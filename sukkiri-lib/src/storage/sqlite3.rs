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
        println!("Connecting to {}.", self.path);
    }
    fn disconnect(&self) {
        println!("Disconnecting from {}.", self.path);
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
