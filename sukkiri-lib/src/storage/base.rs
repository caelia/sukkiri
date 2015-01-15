pub trait SKStore {
    fn init(&self);
    fn connect(&self);
    fn disconnect(&self);
}
