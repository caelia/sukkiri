extern crate rusqlite;
use super::sql_queries::sqlite3 as queries;
pub use super::base::SKStore;
use std::mem;
use rusqlite::{SqliteConnection, SqliteRows};
use rusqlite::SQLITE_OPEN_READ_ONLY as READ_ONLY;
use self::OptSqliteConnection::{ReadOnly, ReadWrite, NoConn};

pub enum OptSqliteConnection {
    ReadOnly(SqliteConnection),
    ReadWrite(SqliteConnection),
    NoConn,
}

impl OptSqliteConnection {
    fn take(&mut self) -> OptSqliteConnection {
        mem::replace(self, NoConn)
    }
    fn expect(self, writeable: bool) -> SqliteConnection {
        match self {
            ReadWrite(c) => {
                if writeable {
                    c
                } else {
                    panic!("Expected ReadOnly connection, ReadWrite found.");
                }
            },
            ReadOnly(c) => {
                if writeable {
                    panic!("Expected ReadWrite connection, ReadOnly found.");
                } else {
                    c
                }
            },
            NoConn => {
                if writeable {
                    panic!("Expected ReadWrite connection, no connection found.");
                } else {
                    panic!("Expected ReadOnly connection, no connection found.");
                }
            }
        }
    }
}

pub struct SKSqliteStore<'a> {
    path: &'a str,
    conn: OptSqliteConnection
}

impl<'a> SKSqliteStore<'a> {
    pub fn new(file: &'a str) -> SKSqliteStore {
        SKSqliteStore { path: file, conn: NoConn }
    }
    fn read_action<'b, F: Fn(SqliteConnection) -> SqliteRows<'b>>(&mut self, f: F) -> SqliteRows<'b> {
        self.connect();
        let konn = self.conn.expect(false);
        f(konn)
    }
    fn write_action<F: Fn(SqliteConnection)>(&mut self, f: F) {
        self.connect_rw();
        let konn = self.conn.expect(true);
        f(konn)
    }
    fn rw_action<'b, F: Fn(SqliteConnection) -> SqliteRows<'b>>(&mut self, f: F) -> SqliteRows<'b> {
        self.connect_rw();
        let konn = self.conn.expect(true);
        f(konn)
    }
}

impl<'a> SKStore for SKSqliteStore<'a> {
    fn initialize(&self) {
        println!("Initializing database.");
    }
    fn connect(&mut self) {
        fn connect_unsafe(this: &mut SKSqliteStore) {
            this.conn = ReadOnly(SqliteConnection::open_with_flags(this.path, READ_ONLY).unwrap());
        }
        match self.conn {
            ReadOnly(_) => (),
            ReadWrite(_) => {
                self.disconnect();
                connect_unsafe(self);
            },
            NoConn => connect_unsafe(self)
        };
    }
    fn connect_rw(&mut self) {
        fn connect_rw_unsafe(this: &mut SKSqliteStore) {
            this.conn = ReadWrite(SqliteConnection::open(this.path).unwrap());
        };
        match self.conn {
            ReadWrite(_) => (),
            ReadOnly(_) => {
                self.disconnect();
                connect_rw_unsafe(self);
            },
            NoConn => connect_rw_unsafe(self)
        };
    }
    fn disconnect(&mut self) {
        let conn = self.conn.take();
        match conn {
            ReadOnly(c) | ReadWrite(c) => c.close().unwrap(),
            NoConn => ()
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
