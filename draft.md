
# Generics SOP introduction

A fascinating aspect of Lisp-based programming languages is that [code is data and data is code](https://en.wikipedia.org/wiki/Code_as_data). This property, called [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity), is what makes Lisp macros so powerful. In Haskell, this sort of runtime operation on arbitrary datatypes (called [polytypism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Polytypism) or datatype genericity) is provided by generics, of which there are two varieties:

1. [GHC Generics](https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Generics.html)
2. [generics-sop](https://hackage.haskell.org/package/generics-sop)

While the former comes with `base`, the latter is much easier to write generics code in, and this blog post gives an introduction to using `generics-sop`.

- [Generics SOP introduction](#generics-sop-introduction)
  - [Motivation](#motivation)
  - [Basics](#basics)
    - [Datatypes are SOPs under the hood](#datatypes-are-sops-under-the-hood)
    - [SOPs are tables](#sops-are-tables)
    - [Interlude: a foray into type-level programming](#interlude-a-foray-into-type-level-programming)
  - [Let's play with SOPs](#lets-play-with-sops)
    - [Interlude: `NS` & `NP`](#interlude-ns--np)
    - [Code as data; data as code](#code-as-data-data-as-code)
  - [Example: generic equality](#example-generic-equality)
    - [Naive implementation](#naive-implementation)
    - [Combinators](#combinators)
  - [Further information](#further-information)

## Motivation

Generic programming is useful in avoiding having to manually write some implementation for each datatype. This could be a ([polytypic](https://www.sciencedirect.com/science/article/pii/S0167642304000152)) function or a typeclass instance. For example, instead of having to manually write `FromJSON` and `ToJSON` instances for each of your datatypes, you can use generics to derive them automatically. Other examples include pretty printers, parsers, equality functions and route encoders.

## Basics

Before diving, we must understand the "SOP" in generics-**sop**.

### Datatypes are SOPs under the hood

Haskell has two kinds of datatypes:

1. [Algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type), or ADTs
2. Newtypes

Both of these can be converted to what is known as a "sum-of-product" (SOP). When writing generics-sop code, we operate on these SOPs rather than the datatype directly, because every datatype is "polymorphic" in their SOP representation. The basic idea is that if you can write a function `SOP -> a`, then you get `SomeDataType -> a` for free for *any* `SomeDataType`. This is called [polytypism](https://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Polytypism).

Consider the following ADT (from [the `these` package](https://hackage.haskell.org/package/these-1.1.1.1/docs/Data-These.html)):

```haskell
data These a b
  = This a 
  | That a
  | These a b
```

This is a [**sum** type](https://en.wikipedia.org/wiki/Tagged_union), with `This`, `That` and `These` being its three sum constructors. Each sum constructor themselves are [**product** types](https://en.wikipedia.org/wiki/Product_type) - inasmuch as, say, the `a` and `b` in the 3rd constructor together represent a product type associated with that constructor. The type `These` is, under the hood, a "sum of product".

### SOPs are tables

We can visualize the `These` SOP in a table form:

| Constructor | Arg 1 | Arg 2 | ... |
| ----------- | ----- | ----- | --- |
| This        | a     |       |
| That        | a     |       |
| These       | a     | b     |

As every Haskell datatype is a SOP, they can be reduced to a table like the above. Each row represents the sum constructor, and the individual cells to the right represents the arguments to the constructors (product type). We can drop the constructor *names* entirely, and simplify the table as:

|     |     |
| --- | --- |
| a   |     |
| a   |     |
| a   | b   |

Every cell in this table is an unique type. If we are to define this table in Haskell, we could use type-level lists, specifically a type-level *list of lists*. The outter list represents the sum constructor, and the inner list represents the products. The *kind* of this table type would then be `[[Type]]`. Indeed [this](https://hackage.haskell.org/package/generics-sop-0.5.1.2/docs/Generics-SOP.html#t:Code) is what generics-sop uses. We can define the table type for `These` in Haskell as follows:

```haskell
type TheseTable a b =
  '[ '[ a ]
     '[ a ]
     '[ a, b ]
  ]
```

If you are confused about this syntax, read the following "Interlude" section.

### Interlude: a foray into type-level programming

What is a "kind"? Kinds are to types what types are to terms. As an example, the *type of* of the term `"Hello world"` is `String`. The latter is a "type" whereas the former is a "term". We can go one level up, and ask - what the *kind of* of the type `String` is; and the answer is `Type`. We can clarify this further by explicitly annotating the kinds of types when defining them (just as we annotate the types of terms when defining them):

```haskell
-- Here, we define a term (2nd line) and declare its type (1st line)
someBool :: Bool
someBool = True 

-- Here, we define a type (2nd line) and declare its kind (1st line)
type Bool :: Type
data Bool = False | True
```

Parametrized types, such as `Maybe`, have type-level function as their kind:

```haskell
type Maybe :: Type -> Type 
data Maybe a = Nothing | Just a
```

This says that "the type `Maybe` is of kind `Type -> Type`". In other words, `Maybe` is a *type-level function* that takes a type of kind `Type` as argument, and returns another type of the same kind `Type` as its result. 

Finally, we are now in a position to understand the kind of `TheseTable` described in the prior section:

```haskell
type TheseTable :: Type -> Type -> [[Type]]
type TheseTable a b =
  '[ '[ a ]
     '[ a ]
     '[ a, b ]
  ]
```

`[Type]` is the kind of type-level lists; and `[[Type]]` is the kind of type-level lists of lists. The tick (`'`) lifts a term into a type. So, while `True` represents a term of type `Bool`, `'True` on the other hand represents a *type* of *kind* `Bool`, just as `'[a]` represents a type of the kind `[Type]`. The tick "promotes" a term to be a type. See [Datatype promotion](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#) in GHC user guide for details.

See [An introduction to typeclass metaprogramming](https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/) as well as [Thinking with Types](https://thinkingwithtypes.com/) for more on type-level programming.

## Let's play with SOPs

Enough theory, let's get our hands dirty in GHCi. If you use Nix, you can clone [this repo](https://github.com/srid/generics-sop-examples) and run `bin/repl` to get GHCi with everything configured ahead for you.

```sh-session
$ git clone https://github.com/srid/generics-sop-examples.git 
$ cd ./generics-sop-examples
$ bin/repl
[1 of 1] Compiling Main             ( src/Main.hs, interpreted )
Ok, one module loaded.
*Main>
```

The project already has `generics-sop` and `sop-core` added to the .cabal file, so you should be able to import it:

```haskell
> import Generics.SOP
```

We also have [the `these` package](https://hackage.haskell.org/package/these-1.1.1.1/docs/Data-These.html) added to the .cabal file, because it provides the above `These` type, from `Data.These` module. In order to explore the SOP representation of the `These` type, let's do some bootstrapping:

```haskell
> import Data.These 
> instance Generic (These a b) -- Derive generics-sop instance
> let breakfast = These "Egg" "Sausage" :: These String String
```

We derived `Generic` on the type, and created a term value called `breakfast` (we are eating both eggs and sausages). To get the SOP representation of this value, we can use `from`:

```haskell
> unSOP . from $ breakfast
S (S (Z (I "Egg" :* I "Sausage" :* Nil)))
```

We'll explain the above value structure in a bit; but the important thing to realize is that this value corresponds to the 3rd row in the SOP table for `These`:

|        |        |
| ------ | ------ |
| String |        |
| String |        |
| String | String |

Because `breakfast` is a value of the 3rd constructor of `These`, and it contains two values (the product of "Egg" and "Sausage").  The corresponding Haskell type for this table:

```haskell
type TheseTable :: [[Type]]
type TheseTable =
  '[ '[ String ]
     '[ String ]
     '[ String, String ]
  ]
```

This type is automatically provide by `generics-sop` whenever we derive a `Generic` instance for the type in question. We did exactly that further above by evaluating `instance Generic (These a b)` in GHCi. Instead of manually defining `TheseTable` as above, deriving `Generic` gives it for free, in the form of `Code a` (viz. `Code (These a b)`).

```haskell
> :k Code (These String String)
Code (These String String) :: [[Type]]
```

In brief, remember this: `Code a` gives us the SOP table *type* for the datatype `a`. Now how do we get the SOP table *value*? That's what `from` is for:

```haskell
> :t (unSOP . from $ breakfast)
(unSOP . from $ breakfast)
  :: NS
       @[Type]
       (NP @Type I)
       ((':)
          @[Type]
          ((':) @Type [Char] ('[] @Type))
          ((':)
             @[Type]
             ((':) @Type [Char] ('[] @Type))
             ((':)
                @[Type]
                ((':) @Type [Char] ((':) @Type [Char] ('[] @Type)))
                ('[] @[Type]))))
```

That's quite a mouthful, because type-level lists are not represented cleanly in GHCi. But we can reduce it (in our mind) to the following

```haskell
> :t (unSOP . from $ breakfast)
(unSOP . from $ breakfast)
  :: NS (NP I) '[ [String], [String], [String, String] ]
```

Notice how this is more or less isomorphic to the our `TheseTable` definition above. We'll explain the `NS` and `NP` part next.

### Interlude: `NS` & `NP`

You are wondering what the `NS (NP I)` part refers to in our table type above. `NS` is a n-ary sum; and `NP` an n-ary product. These are explained well in section 2 of [Applying Type-Level and Generic Programming in Haskell][ATLGP], but for our purposes - you can treat `NS` as similar to the `Nat` type from the [fin package](https://hackage.haskell.org/package/fin-0.2.1/docs/Data-Nat.html), and `NP` as as being similar to the `Vec` type from the [vec package](https://hackage.haskell.org/package/vec-0.4.1/docs/Data-Vec-Lazy.html#t:Vec).

[ATLGP]: https://github.com/kosmikus/SSGEP/blob/master/LectureNotes.pdf
The difference is that unlike `Vec` (a homogenous list), `NP` is an heterogenous list, whose element types are specified by a type-level list.

```haskell
> :k NP I '[String, Int]
NP I '[String, Int] :: Type
```

Just like `Vec` can enforce, this is a list of exactly size 2 ... however, unlike `Vec` we also say that the first element is of type `String` and the second (and the last) element is of type `Int`. To create a value of this heterogenous list:

```haskell
> I "Meaning" :* I 42 :* Nil  :: NP I '[String, Int]
I "Meaning" :* I 42 :* Nil
```

That's unsurprising because `Nil` and `(:*)` are the constructors of the `NP` type:

```haskell
> :info NP
data NP :: (k -> Type) -> [k] -> Type where
  Nil  :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f (x ': xs)
```

([View haddocks](https://hackage.haskell.org/package/sop-core-0.5.0.2/docs/Data-SOP.html#t:NP))

The `I` is the identity functor, but it could also be something else like `Maybe`:

```haskell
> Nothing :* Just 42 :* Nil  :: NP Maybe '[String, Int]
Nothing :* Just 42 :* Nil
```

`NS` is exactly the same, except now we are representing the same characteristics but for the sum type instead of a product type. A sum of length 'n' over some functor 'f':

```haskell
> :info NS 
data NS :: (k -> Type) -> [k] -> Type where
  Z :: f x -> NS f (x ': xs)
  S :: NS f xs -> NS f (x ': xs)
```

([View haddocks](https://hackage.haskell.org/package/sop-core-0.5.0.2/docs/Data-SOP.html#t:NS))

When the value is `Z`, it indicates the first sum constructor; when it is `S . Z` it is the second, and so on. Our `breakfast` value above uses `These` which is the 3rd constructor. So, to construct the SOP representation of this value directly, we would use `S . S . Z`. This is exactly what we saw above (repeated here):

```haskell
-- Note the `S . S . Z`
> unSOP . from $ breakfast
S (S (Z (I "Egg" :* I "Sausage" :* Nil)))
>
> :t (unSOP . from $ breakfast)
(unSOP . from $ breakfast)
  :: NS (NP I) '[ [String], [String], [String, String] ]
```

`NS`'s functor is a `NP I`, and so the inner value of that sum choice is an n-ary product, whose value is `I "Egg" :* I "Sausage" :* Nil`. 

### Code as data; data as code

Ther SOP representation of `These` can be manually constructed. First we build the constructor arguments (product), and then we build the constructor itself (sum):

```haskell
> let prod = I "Egg" :* I "Sausage" :* Nil :: NP I '[String, String]
> let sum = S $ S $ Z prod :: NP I '[[String], [String], [String, String]]
> :t sum 
sum :: NS (NP I) '[[String], [String], [String, String]]
```

And from this representation we can produce a value of type `These` easily, using `to`:

```haskell
> to @(These String String) (SOP sum)
These "Egg" "Sausage"
```

Let's stop for a moment and reflect on what we just did. By treating the type-definition of `These` ("code") as a generic `SOP` table ("data") -- i.e., code as data -- we are able to generate a value ("code") for that type ("data") -- ie., data as code -- but without using the constructors of that type. This is generic programming in Haskell; you program *generically*, without being privy to the actual type being used. 

This concludes the section on playing with SOPs. Now let's do something actually useful.

## Example: generic equality

GHC's [stock deriving](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_strategies.html#deriving-strategies) can be used to derive instances for builtin type classes, like `Eq`, on user-defined datatypes. This works for builtin type classes, but generics-sop (as well as GHC.Generics) comes in handy when you want to derive generically for arbitrary typeclasses. For a moment, let us assume that GHC had no support for stock deriving; how will we derive our `Eq` instance?

We want a function `geq` that takes *any* datatype `a` (this makes the function [polytypic](https://www.sciencedirect.com/science/article/pii/S0167642304000152)), and does equality check on its arguments. In effect, we want:

```haskell
geq :: Generic a => a -> a -> Bool
```

This function can be further broken down to operate on SOP structures directly, so as to "forget" the specific `a`:

```haskell
geq :: forall a. Generic a => a -> a -> Bool
geq x y = geq' @a (unSOP $ from x) (unSOP $ from y)

geq' :: NS (NP I) (Code a) -> NS (NP I) (Code a) -> Bool
geq' = undefined
```

Our problem has now been reduced to operating on SOP tables, and our task is to implement `geq'`.

At this point you are probably thinking we can just case-match on the arguments, but remember that the n-ary sum type `NS` is a [GADT](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/gadt.html#gadt) (its type index is [dependent](https://en.wikipedia.org/wiki/Dependent_type) on the sum constructor). We have to instead case-match at the *type-level* as it were. This is what type-classes are for. The general pattern is that when wanting a  `foo` that case-match'es at type-level, we write a type-class `Foo` and write instances for each case-match pattern. 

### Naive implementation

For pedagogic reasons, we'll begin with a naive implementation of `geq'` to illustrate the above. What we need is a `sumEq` function that checks equality of first constructor, and then recurses for others; it will case-match on outter list. Likewise, for each sum constructor we will need a `prodEq` that checks equality of its products; and it does so, similarly, by checking equality of first product, and then recurses for the rest; it will case-match on the inner list.

```haskell
geq' :: SumEq (Code a) => NS (NP I) (Code a) -> NS (NP I) (Code a) -> Bool
geq' = sumEq

-- `xss` is a type-level list of lists; `Code a`
class SumEq xss where
  sumEq :: NS (NP I) xss -> NS (NP I) xss -> Bool

instance SumEq '[] where
  sumEq = \case

instance (ProdEq xs, SumEq xss) => SumEq (xs ': xss) where
  -- Both values are the same constructor; so check equality on their products,
  -- using `prodEq`.
  sumEq (Z x) (Z y) = prodEq x y
  -- Recurse on next sum constructor.
  sumEq (S x) (S y) = sumEq x y
  -- Mismatching sum constructor; equality check failed.
  sumEq _ _ = False

class ProdEq xs where
  prodEq :: NP I xs -> NP I xs -> Bool

instance ProdEq '[] where
  prodEq Nil Nil = True

instance (Eq x, ProdEq xs) => ProdEq (x ': xs) where
  -- First product argument should be equal; then we recurse for rest of arguments.
  prodEq (x :* xs) (y :* ys) = x == y && prodEq xs ys
```
 
Notice how in the first instance for `SumEq` we are "pattern matching" as it were at the type-level, and defining the implementation for the scenario of zero sum constructors (not inhabitable). Then, inductively, we define the next instance using recursion. When both arguments are at `Z`, we proceed to match their products, using `prodEq` which is defined similarly. Otherwise, we recurse into the successor constructor (the `x` in `S x`). The story for `ProdEq` is similar.

Finally, we can test that it works:

```haskell
> geq (This True) (That False)
False
> geq (These 42 "Hello") (These 42 "Hello" :: These Int String)
True
```

We just implemented an equality function that works for *any* datatype (with `Generic` instance).

### Combinators

The above naive implementation is hopefully illustratory of how one can "transform" SOP structures straightforwardly using typeclasses. N-ary sums and products need to be processed at type-level, so it is not uncommon to write new type-classes to dispatch on their constructors as shown above. Typically however you don't have to do that, because `generics-sop` provides combinators for common operations. Here, we will rewrite the above implementation using these combinators.

The combinators are explained in depth in [ATLGP]. We will introduce a few in this post. The particular combinators we need for `geq` are:

| Combinator    | Description                                          | Typeclass it replaces |
| ------------- | ---------------------------------------------------- | --------------------- |
| `hcliftA2`    | Lift elements of a NP or NS using given function     | `ProdEq`              |
| `hcollapse`   | Convert heterogenous structure into homogenous value | `ProdEq`              |
| `ccompare_NS` | Compare two `NS` values                              | `SumEq`               |

To appreciate the value of these particular combinators, notice the 3rd column indicating the type-class it intends to replace. Withtout further ado, here is the new implementation:

```haskell
geq :: forall a. (Generic a, All2 Eq (Code a)) => a -> a -> Bool
geq x y = geq' @a (from x) (from y)

geq' :: All2 Eq (Code a) => SOP I (Code a) -> SOP I (Code a) -> Bool
geq' (SOP c1) (SOP c2) =
  ccompare_NS (Proxy @(All Eq)) False eqProd False c1 c2
  where
    eqProd :: All Eq xs => NP I xs -> NP I xs -> Bool
    eqProd p1 p2 =
      foldl' (&&) True $
        hcollapse $ hcliftA2 (Proxy :: Proxy Eq) eqTerm p1 p2
      where
        eqTerm :: forall a. Eq a => I a -> I a -> K Bool a
        eqTerm a b =
          K $ a == b
```

This code introduces two more things:

- **Constraint propagation**: When generically transforming SOP structures we want to be able to "propagate" inner constraints outwardly, and this is what the `Proxy` class is being used for here. `All c xs` simply is an alias for `(c x1, c x2, ...)` where `xs` is a type-level list; likewise, `All2 c xss` is `c x11, c x12, ... ` where `xss` is type-level list of lists (ie., `Code a ~ xss`). Clearly, we want the `Eq` constraint in table elements to apply to the whole table row, and thereon to the table itself. And `All2 Eq (Code a)` on `geq'` specifies this.
- **Constant functor**: The constant functor `K` is defined as `data K a b = K a`; it "discards" the second type parameter, always containing the first. Where you see `K Bool a` we are discarding the polymorphic `a` (the type of the cell in the table), and returning the (constant) type `Bool`. When we transform the structure to be over `K` (using `hcliftA2`), we are essentially making the structure *homogenous* in its elements, which in turn allows us to "collapse" it using `hcollapse` to get a single value out of it (which is what we need to be the result of `geq`).

This is just a brief taste of generics-sop combinators. Read [ATLGP] for details.

Here are some more combinators you might want to use when writing generics code:

| Combinator  | Use                        |
| ----------- | -------------------------- |
| `hcpure`    | Construct a product (`NP`) |
| `hcmap`     | Map a `NS` or `NP`         |
| `ejections` | Destructing a sum (`NS`)   |


## Further information

- [This ZuriHack talk](https://www.youtube.com/watch?v=sQxH349HOik) provides a good introduction to generics-sop
- [Applying Type-Level and Generic Programming in Haskell][ATLGP] by Andres Löh should act as a lengthy tutorial cum documentation for generics-sop
- If haddocks are confusing, read the source. For instance, I found it helpful to scroll through [`NS.hs`](https://github.com/well-typed/generics-sop/blob/master/sop-core/src/Data/SOP/NS.hs) directly so as to understand some of the sum combinators available.
