extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use rusqlite::SqliteConnection;
// mod sql_queries;
// mod base;

pub struct SKSqliteStore<'a> {
    path: &'a str,
    conn: Option<SqliteConnection>
}

impl<'a> SKSqliteStore<'a> {
    pub fn new(file: &'a str) -> SKSqliteStore {
        let store = SKSqliteStore { path: file, conn: None };
        store
    }
}

impl<'a> SKStore for SKSqliteStore<'a> {
    fn init(&self) {
        println!("Initializing database.");
    }
    fn connect(&mut self) {
        match self.conn {
            Some(_) => (),
            None => self.conn = Some(SqliteConnection::open(self.path).unwrap())
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
