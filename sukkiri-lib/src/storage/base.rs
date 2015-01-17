pub trait SKStore {
    fn init(&self);
    fn connect(&mut self);
    fn disconnect(&mut self);
}
