extern crate rusqlite;
extern crate "rustc-serialize" as rustc_serialize;
use storage::sqlite3::*;
mod storage;

fn poke_sqlite3(path: &'static str) {
    let mut store = SKSqliteStore::new(path);
    store = SKStore::connect(store);
    SKStore::disconnect(store);
}

#[test]
fn it_works() {
}
