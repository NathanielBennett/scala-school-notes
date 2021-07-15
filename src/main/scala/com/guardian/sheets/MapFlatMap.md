# Map and flatmap# 


Let's write some simple examples to see if we can build up an understanding of how `map` and `flatMap` work on `Option` and `List`

## Option `Map`

The `Option` method `map` has the signiture `def map[B](f: A => B): Option[B]`. If this is confusing let's right a series of cod functions that mimic how map works on option to convert and Option of type `Int` to an Option of type `String`. The key thing to recall here is that `map` on an option will always return you an option. You cannot use `map` to retrieve the wrapped value

Lets first write the simplest invocation we can without any generic types. This way we can build out way up and explain all the stuff we had to brush over in class

Remember what we are attempting to approximate would be a method on the `Option` class which in the 'real world' would already encasulate the value we pass in

1. Lets have the following function:
```scala
def intToString(i: Int) : String = s"Number ${i}"
```
So far so good? If we ran run this code in the worksheet we can see we get the following values:

```scala {.line-numbers}
val n = intToString(10)
val m = intToString(2)
```

Outputs: 

```scala
val n: String = Number 10
val m: String = Number 2
```

2. Now let's write a function that approximates what might happen if we wanted to mimic what map does on an `Option[Int]` using the function that we already have.

```scala
def basicOptionIntMap(maybeInt: Option[Int]) : Option[String] = maybeInt match {
  case Some(n) => Some(intToString(n))
  case None => None
}

```

Hopefully this all makes sense now. The `match` tests for the two possible values of an `Option[Int]` and in the case where there is a value contained in the Option passes that actual value to our conversion function and wraps that in a `Some`. When `None` is is passed `None` is returned. 

Lets test this out:

code:
```scala
val maybeOne = Some(1)
val maybeTen = Some(10)

val basicMapOne = basicOptionIntMap(maybeOne)
val basicMapTen = basicOptionIntMap(maybeTen)
val none = basicOptionIntMap(None)
```

Output:
```
val maybeOne: Some[Int] = Some(1)
val maybeTen: Some[Int] = Some(10)

val basicMapOne: Option[String] = Some(Number 1)
val basicMapTen: Option[String] = Some(Number 10)
val none: Option[String] = None
```

Note that when we pass a `Some` in we get a `Some` out and when we pass `None` we get `None` in return.

3. The function can be re-written so that instead of calling `intToString` it can call any function that takes an `Int` and returns a `String`. We'll call this function `convertFunction` to keep this clear and write another function with the same signature

Lets write another function that takes and `Int` and returns a `String`

```scala
def makeXString(x: Int): String = (0 to x).foldLeft(""){ case (acc, _) => s"${acc}X" }
```

I'll leave you to figure out how this works but

```scala
val xString = makeXString(10)
```

will give you 

```scala
val xString: String = XXXXXXXXXXX
```

Let's make a new version of the cod `map` function to use either of the `Int` to `String` functions

```scala
def optionIntMap(maybeInt: Option[Int])(convertFunction: Int => String) : Option[String] =
  maybeInt match {
    case Some(n) => Some(convertFunction(n))
    case None => None
  }
```

This works as before, so that where a `Some[Int]` is passed the value of that `Int` is available in the local variable `n` in the block following the `=>`. But now we pass in the function we want to use to make a `String` with as `convertFunction`. `Int => String` specifies that `convertFunction` takes an `Int` and returns a `String`

Note also how the whole expression `convertFunction(n)` evaluates to a `val` of type `String` so that when it is wrapped in `Some` this becomes the mapped return type of `Option[String]`

Now both of out `Int` to `String` functions can be passed to our cod `map` function

code:
```scala
val numberString = optionIntMap(Some(10))(intToString)
val xString = optionIntMap(Some(10))(makeXString)
```

output:
```scala
val numberString: Option[String] = Some(Number 10)
val xString: Option[String] = Some(XXXXXXXXXXX)```
```

4. But, that's not often how code with curly braces and arrows looks in our actual codebases, right? Right. That's because more often than not, when we use a function like `Map` on an `Option` or `List` we pass our 'converter` function anonymously inline. We can do this with our two existing functions. 

```scala
val numberStringInline = optionIntMap(Some(10)) { i =>
  s"Number $i"
}

val xStringInline = optionIntMap(Some(10)) { i =>
     (0 to i).foldLeft(""){ case(acc, _) => s"${acc}X"}
}
```

This produces exactly the same output as when the predefined functions are passed. The code between the curly braces is what's passed to` optionIntMap` as to convert function the variable `i` being used to reference the `Int` value that is passed in.


5. Finally, realworld `Option.map` lets  map an Option of anything we like. Giving us (a) an `Option[SomethingElse]` of  a `Some[Something]` with a function `Something => SomethingElse`is passed or  `None` when non is passed. `Something` and `SomethingElse` being the `types` inferred for us by the compiler. Let's change out cod `Option` function to demonstrate how this works

```scala
def optionMap[A, B](maybeA: Option[A])(f: A => B): Option[B] = maybeA match {
  case Some(a) => Some(f(a))
  case None => None
}
```

Here's your first taste of scala generics. Here we define two type parameters `A` and `B` which the compiler converts for us to use real types at run time. (Later) this enables us to pass on option of whatever we like in. As long as we supply a function that dealis with an object of whatever type A is we can return whatever we like ( This will be something of type `B`). It's also typical to see function parameters defined as single letters. `convertFunction` becomes `f`. `A` and `B` can then represent the same type, or different types.
Be aware that your actual `Option` type can take a type parameter at class level. 



