extern crate rusqlite;
extern crate "rustc-serialize" as rustc_serialize;
use storage::sqlite3::*;
mod storage;

fn poke_sqlite3(file: &str) {
    // let store = sqlite3::SKSqliteStore::new(file);
    let mut store = SKSqliteStore::new(file);
    store.init();
    store.connect();
    store.disconnect();
}

#[test]
fn it_works() {
}
