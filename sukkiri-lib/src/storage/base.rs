pub trait SKStore {
    fn initialize(&self);
    fn connect(&mut self);
    fn connect_rw(&mut self);
    fn disconnect(&mut self);
}
