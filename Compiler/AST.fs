namespace Compiler


type Token =
    | Float of single
    | Integer of int
    | String of string
    | Other of string
type Location = { File : string; Position : int; Line : int; Column : int; Length : int } with
    static member Generated = { File="Generated"; Position=0; Line=0; Column=0; Length=0 }
type LocatedToken = Token * Location

type Type =
| Unknown
| Float
| Int
| Boolean
| String
| Unit
| Array of Length:int option
| Block of Return:Type * Arguments:(Type list option)
| Magic // Used only for internal recordkeeping!

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
| GlobalReference of Name:string * Flags:(GlobalFlag list)
| ArgumentReference of int // These can be positive (0 == first) or negative (-1 == last)
| Block of TypedNode list
| RawBlock of LocatedToken list
| Unary of Operation:Operation * RValue:TypedNode
| Binary of Operation:Operation * LValue:TypedNode * RValue:TypedNode
| Cast of To:Type * RValue:TypedNode
| Call of Callee:TypedNode * Arguments:(TypedNode list)
| Array of TypedNode list
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
| Assignment of Target:TypedNode * RValue:TypedNode
| Word of Name:string * Returns:(Type list) * Arguments:((string * Type) list)
// Magical nodes used for internal purposes
| ArrayStart
and TypedNode = Type * Location * Node
