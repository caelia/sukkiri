extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use rusqlite::SqliteConnection;

pub struct SKSqliteStore<'a> {
    path: &'a str,
    conn: Option<SqliteConnection>
}

impl<'a> SKSqliteStore<'a> {
    pub fn new(path: &'a str) -> SKSqliteStore<'a> {
        SKSqliteStore { path: path, conn: None }
    }
}

impl<'a> SKStore for SKSqliteStore<'a> {
    fn connect(store: SKSqliteStore<'a>) -> SKSqliteStore<'a> {
        let result = match store.conn {
            Some(_) => store,
            None => {
                let c = SqliteConnection::open(store.path).unwrap();
                SKSqliteStore { conn: Some(c), path: store.path }
            }
        };
        println!("Connected to {}.", store.path);
        result
    }
    fn disconnect(store: SKSqliteStore<'a>) -> SKSqliteStore<'a> {
        let result = match store.conn {
            Some(c) => {
                c.close();
                SKSqliteStore { conn: None, path: store.path }
            },
            None => store
        };
        println!("Disconnected from {}.", store.path);
        result
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
