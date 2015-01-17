extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use rusqlite::SqliteConnection;
// mod sql_queries;
// mod base;

pub struct SKSqliteStore<'a, 'b> {
    path: &'a str,
    conn: Option<SqliteConnection>
}

impl<'a, 'b> SKSqliteStore<'a, 'b> {
    pub fn new(file: &'a str) -> SKSqliteStore {
        let store = SKSqliteStore { path: file, conn: &None };
        store
    }
}

impl<'a, 'b> SKStore for SKSqliteStore<'a, 'b> {
    fn init(&self) {
        println!("Initializing database.");
    }
    fn connect(&mut self) {
        /*
        let conn = match self.conn {
            &Some(ref c) => c,
            &None => {
                &SqliteConnection::open(self.path).unwrap()
            }
        };
        &Some(*conn);
        */
        /*
        match self {
            &mut SKSqliteStore { conn: Some(_), .. } => true,
            &mut SKSqliteStore { conn: None, path: p } => {
                /*
                let c = SqliteConnection::open(p).unwrap();
                self.conn = Some(&mut c);
                */
                self.conn = Some(&mut SqliteConnection::open(p).unwrap());
                false
            }
        };
        */
        if *self.conn == &None {
            self.conn = &Some(SqliteConnection::open(self.path).unwrap());
        }
        println!("Connected to {}.", self.path);
    }
    fn disconnect(&mut self) {
        match self.conn {
            &Some(c) => { c.close(); self.conn = &None; true }
            &None => false
        };
        println!("Disconnected from {}.", self.path);
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
