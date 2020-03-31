### Declaring a struct
```rust
// In-line requires commas
type MyStruct = (a: String, b: Foo)
// No commas needed!
type Foo = (
   a: String
   b: Bar
)
```
### Variable creation/assignment
```rust
// Type-deduction works most of the time
let x = 23 // int
let x = 2.3 // double
let x = FnReturningFoo() // Foo

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
* Assignments as expressions?



