extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use rusqlite::SqliteConnection;
// mod sql_queries;
// mod base;

pub struct SKSqliteStore<'a> {
    path: &'a str,
    conn: Option<Box<SqliteConnection>>
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
            Some(ref c) => true,
            None => {
                let c = SqliteConnection::open(self.path).unwrap();
                self.conn = Some(Box::new(c));
                false
            }
        };
        println!("Connected to {}.", self.path);
    }
    fn disconnect(&mut self) {
        match self.conn {
            // Some(ref c) => { c.close(); self.conn = None; true }
            Some(ref c) => { let conn = *c; conn.close(); self.conn = None; true }
            None => false
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
