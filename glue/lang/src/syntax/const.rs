#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstExprType {
    Primitive(PrimitiveType),
    List(Box<ConstExprType>),
}
