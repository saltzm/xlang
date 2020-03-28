

Open questions:
* Do I want to allow nested scopes, or should I force you to call a function?
* Do I want to allow variable shadowing?
* Pass by val or by pointer?
    * I want to make handling structs and heap allocated pointers the same...
* Do I want inc/dec operators?
* Do I want to allow named struct as input parameter that does unpacking? E.g. 
    fn Foo = Bar -> Baz { /* ... */ }
    where Bar is some type Bar = (a : A, b: B) and then the function receives a/b as inputs

