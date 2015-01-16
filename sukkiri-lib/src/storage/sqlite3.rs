extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use rusqlite::SqliteConnection;
// mod sql_queries;
// mod base;

pub struct SKSqliteStore<'a, 'b> {
    path: &'a str,
    conn: Option<&'b SqliteConnection>
}

impl<'a, 'b> SKSqliteStore<'a, 'b> {
    pub fn new(file: &'a str) -> SKSqliteStore {
        let store = SKSqliteStore { path: file, conn: None };
        store
    }
}

impl<'a, 'b> SKStore for SKSqliteStore<'a, 'b> {
    fn init(&self) {
        println!("Initializing database.");
    }
    fn connect(&self) {
        let conn =
            match self.conn {
                Some(&c) => c,
                None => {
                    let c = SqliteConnection::open(self.path).unwrap();
                    self.conn = Some(&c);
                    c
                }
            };
        println!("Connected to {}.", self.path);
    }
    fn disconnect(&self) {
        match self.conn {
            Some(&c) => { c.close(); self.conn = None; true }
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
