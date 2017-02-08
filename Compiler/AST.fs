namespace Compiler

type Type =
| Unknown
| Float
| Int
| Boolean
| String
| Unit
| Array of Length:int option
| Block of Return:Type * Arguments:(Type list option)

type Operation =
| Not
| Negate
| Add
| Subtract
| Multiply
| Divide
| Modulo
| And
| Or
| Xor

type Value = 
| Float of single
| Int of int
| Boolean of bool
| String of string
| Unit

type GlobalFlag =
| Uniform
| Varying
| Attribute

type Node =
| Value of Value
| WordReference of string
| LocalReference of string
| Block of TypedNode list
| GlobalReference of Name:string * Flags:(GlobalFlag list)
| Unary of Operation:Operation * RValue:TypedNode
| Binary of Operation:Operation * LValue:TypedNode * RValue:TypedNode
| Cast of To:Type * RValue:TypedNode
| Call of Callee:TypedNode * Arguments:(TypedNode list)
| Array of TypedNode list
| Assignment of Target:TypedNode * RValue:TypedNode
| Index of Base:TypedNode * Index:TypedNode
| MemberReference of Base:TypedNode * Member:string
| Swizzle of Base:TypedNode * Elements:(Value list) // Values must be Int or String
| If of Condition:TypedNode * Then:TypedNode * Else:TypedNode
 // The following always have type Unit!
| When of Condition:TypedNode * Then:TypedNode
| Unless of Condition:TypedNode * Then:TypedNode
| While of Condition:TypedNode * Body:TypedNode
| Return of TypedNode
| Break
| Continue
and TypedNode = Type * Node
