pub trait SKStore {
    //fn connect<T: SKStore>(T) -> T;
    //fn disconnect<T: SKStore>(T) -> T;
    fn connect(Self) -> Self;
    fn disconnect(Self) -> Self;
}
