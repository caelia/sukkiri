extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use rusqlite::SqliteConnection;
use rusqlite::SQLITE_OPEN_READ_ONLY as READ_ONLY;
// mod sql_queries;
// mod base;

pub struct SKSqliteStore<'a> {
    path: &'a str,
    conn: Option<SqliteConnection>,
    writeable: bool
}

impl<'a> SKSqliteStore<'a> {
    pub fn new(file: &'a str) -> SKSqliteStore {
        let store = SKSqliteStore { path: file, conn: None, writeable: false };
        store
    }
    /*
    fn connect_unsafe(&mut self) {
        self.conn = Some(SqliteConnection::open_with_flags(self.path, READ_ONLY).unwrap());
        self.writeable = false;
    }
    fn connect_rw_unsafe(&mut self) {
        self.conn = Some(SqliteConnection::open(self.path).unwrap());
        self.writeable = true;
    }
    */
    fn read_action(self) {
    }
    fn write_action(self) {
    }
}

impl<'a> SKStore for SKSqliteStore<'a> {
    fn initialize(&self) {
        println!("Initializing database.");
    }
    fn connect(&mut self) {
        fn connect_unsafe(this: &mut SKSqliteStore) {
            this.conn = Some(SqliteConnection::open_with_flags(this.path, READ_ONLY).unwrap());
            this.writeable = false;
        }
        let matchvals = (self.conn, self.writeable);
        match matchvals {
            (Some(_), false) => (),
            (Some(_), true) => {
                self.disconnect();
                connect_unsafe(self);
            },
            (None, _) => connect_unsafe(self)
        };
        /*
        match self {
            &SKSqliteStore { conn: Some(_), writeable: false, .. } => (),
            &SKSqliteStore { conn: Some(_), writeable: true, .. } => {
                self.disconnect();
                connect_unsafe(self);
            },
            &SKSqliteStore { conn: None, .. } => connect_unsafe(self)
        };
        */
    }
    fn connect_rw(&mut self) {
        fn connect_rw_unsafe(this: &mut SKSqliteStore) {
            this.conn = Some(SqliteConnection::open(this.path).unwrap());
            this.writeable = true;
        };
        match (self.conn, self.writeable) {
            (Some(_), true) => (),
            (Some(_), false) => {
                self.disconnect();
                connect_rw_unsafe(self);
            },
            (None, _) => connect_rw_unsafe(self)
        };
    }
    fn disconnect(&mut self) {
        let conn = self.conn.take();
        match conn {
            Some(c) => c.close().unwrap(),
            None => ()
        };
    }
}

mod tests {
    use super::*;

    #[test]
    fn test_setup_new_db() {
        let store = SKSqliteStore::new("test.db");
        assert_eq!(store.path, "test.db");
    }
}
