### Declaring a struct
```rust
// In-line requires commas
type MyStruct = (a: String, b: Foo)
// No commas needed!
type Foo = (
   a: String
   b: Bar
)
// Create an alias
// TODO: Decide whether this is a usual alias or whether it's a "copy", i.e. whether 
// objects of type Bar can be passed to functions expecting type Foo, or whether this
// Just says that Bar has the same structure as Foo but is a separate type, which I could
// see being useful in certain cases if you want to use the type system to enforce certain
// behaviors that don't actually influence the structure of the data. (E.g. I could imagine 
// an "OwnedPtr" type that is actually just a pointer, but functions taking OwnedPtr should reject
// things of type Ptr. Could maybe have both concepts as separate things maybe, but don't know 
// how useful that is.
type Bar = Foo
// TODO: Consider using "struct" instead of "type" as keyword
```
### Variable creation/assignment
```rust
// Type-deduction works most of the time
let x = 23 // int
let x = 2.3 // double
let x = FnReturningFoo() // Foo
// All blocks of { } are expressions that can return something
// TODO: type inference needs to work for this, which means it 
// needs to work for functions as well... 
let x = {
   let a = 5
   let b = 10
   a + b
} 

// Struct creation requires type specifier
let my_struct : MyStruct = ("hello", "world")
// TODO: Do I like this?
let my_struct : MyStruct = (
   "hello" // a
   "world" // b
)
// TODO: Allow struct init with field names? Leaning towards no...
```
### Function declaration
```rust
// "Add is a function from (i32, i32) to i32"
fn Add = (a: i32, b: i32) -> i32 {
   // No "return" required because all block expressions ({ }) can return a value
   a + b 
}

// TODO: Consider this syntax
let Add = fn (a: i32, b: i32) -> i32 {
   // No "return" required because all block expressions ({ }) can return a value
   a + b 
}
// And this syntax along with it
let MyStruct = struct (
    a: int
    b: int
)

// Or even
let Add = (a: i32, b: i32) -> i32 {
   // No "return" required because all block expressions ({ }) can return a value
   a + b 
}

let MyStruct = (
    a: int
    b: int
)
```


Open questions:
* Do I want to allow nested scopes, or should I force you to call a function?
* Do I want to allow variable shadowing?
* Pass by val or by pointer?
    * I want to make handling structs and heap allocated pointers the same...
* Do I want inc/dec operators?
* Do I want to allow named struct as input parameter that does unpacking? E.g. 
    fn Foo = Bar -> Baz { /* ... */ }
    where Bar is some type Bar = (a : A, b: B) and then the function receives a/b as inputs
* Constant  ?
* enum? 
* var/val vs. let and let mut
* can i get away without variable reassignment
* Assignments as expressions



