## `Option` type and `Match`

This is just to expand on what we talked about in the session so that everyone is hopefully clearer. 

### Using `match` with an `Option`.

Let's begin with a little theory. We'll come to type parameters and inheritance later. For now, be clear that an `Option` is a type that may or may not contain a value of a given type. 

Scala defines the following (simplified: There's some more subtleties in the actual implementation)

```aidl
sealed trait Option[T]
case class Some[T](t: T) extends Option[T] 
case class None extends Option[T]
```

There's several things that we'll come back to in detail later but for now

* The `trait` keyword is a little bit like `interface` in java. You can't instantiate one of them
* The `sealed` keyword means this trait cannot be extended outside of the file where it is declared. This is important when using `match`. An `Option` can only be subclass `Some` or `None`
* `T` is the type parameter. It specifies the type of value that be contained by this value. The scala compiler is good at interpolating this so
    
    *`val s = Some("Hello")` //Type `Option[String]`
    *`val i = Some(23)`      //Type `Option[Int]`  
  
* The following dummy function always returns a legitimate type of `Option[Int]`

```aidl
def maybeEven(i: Int): Option[Int] = {
    if( i % 2 == 0) Some(i)
    else None
}

```

The first example im the [Worksheet](option_and_match.sc) takes a `Option[String]` and prints a message depending on which subclass of `Option` is passed at runtime.

```aidl
def checkOption(maybeString: Option[String]) = maybeString match {
  case Some(stringValue) =>
    println(s"Morning $stringValue")
  case None =>
    println("You gave me nothing I give you nothing it return")
}
```

There's a couple of things to note here: 

* `maybeString` can only ever be an object of type `Some[String]` or of `None` so the match is exhaustive. This is why the `sealed` keyword in the cod `Option` implementation matters
* when we match an object of type `Some[String]` we know it will contain a value of type `String` and so we can use a placeholder `val` to reference the actual value: `stringValue`. This value remains in scope until the next `case` statement
* The `=>` symbol is scala's way of denoting an anonymous function. This is a very specialised instance of this. For each possible type that can be matched, what we essentially do here is define an anonymous function that takes a value of that type as a parameter. There's no explicit return value here. Although we'll come to that later
* Notice that the `match` block as a whole is a single statement so there's no need for additional braces around the method body. The last statement is always returned so the following is equivalent and you'll see this a lot in our codebfases


```aidl
def checkOption(maybeString: Option[String]) = {
   maybeString match {
    case Some(stringValue) =>
      println(s"Morning $stringValue")
    case None =>
      println("You gave me nothing I give you nothing it return")
  }
}  
```
