# Typechecking for higher-rank polymorphism

An implementation of type inference and checking for the polymorphic bidirectional type system presented in the
[Complete and easy bidirectional typechecking for higher-rank polymorphism](https://dl.acm.org/doi/abs/10.1145/2544174.2500582) paper by Jana Dunfield and Neelakantan R. Krishnaswami. 

Types can be inferred for almost all terms inferrable with the [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley–Milner_type_system) algorithm (need a little tweak to fully subsume HM). 
Additionaly can infer SystemF types of an arbitrary rank, as long as enough type annotations have been supplied by the user. 


# Usage 

Need to have regular Haskell tools like `stack` and `cabal` installed.

Edit the term in the `example.term`. The syntax is mix between classic lambda
calculus sytax and the haskell syntax for lambda abstractions.  
For example write `\x.x` for the id term.  

You can also add annotations, annotated terms should be in parens.  
For example term `ω: ∀a.a` can be typed as `(\x.x x): [a]a`  
Term `(K I ω) : ∀a. a -> a` as `((\x.\y.x) (\x.x) (\x.x x)): [a] a -> a`

After you type the term you want inference for in the `example.test` file, 
use `stack run` to run the main program, it will print you a report on your term.

You can also use it via `ghci`.  
After installation run `stack build`. 
Then enter interactive mode with `stack ghci`. 

Main tools to use are 
```Haskell
infer :: Term -> Maybe Type
inferWith :: Context -> Term -> Maybe Type`

infer2 :: Term -> Maybe Type
inferWith2 :: Context -> Term -> Maybe Type`
```
They return the inferred type for the provided term. Or `Nothing` if the type cannot be infered. `inferWith` allows to supply initial context. `infer` uses an empty one.

Functions with the prefix `2` are a more elaborate rule for lambda type inference, so that
the system can fully subsume the Hindely-Milner type inference system, at leas in theory that is... If you find cases where it doesn't work, you can tell about them to me

### Examples

```Haskell
ghci> infer id1
Just ∀a.(a -> a)
ghci> infer k
Just ∀a.∀b.(a -> (b -> a))
ghci> infer s
Just ∀a.∀b.∀c.((a -> (b -> c)) -> ((a -> b) -> (a -> c)))
ghci> infer $ App s k  -- same bind names are fine in this case, but not recommended
Just ∀a.∀b.((a -> b) -> (a -> a))

ghci> top
(∀a.a -> ∀a.a)

ghci> infer omega
Nothing
ghci> infer $ Anno omega (top) -- annotation helps
Just (a -> a)

infer $ App (App k id3) omega
Nothing
ghci> infer $ App (App k id3) (Anno omega top) -- annotation helps
Just ∀a.(a -> a)
```


### Constructing Input
> For details see [Syntax.hs](src/Syntax.hs).

Terms are from simple lambda calculus. Except for the `Anno term type` constructor, which is used to supply user annotations.

Types support binding via the `ForAll` constructor. To refer to a previously bound type variable use the `Basic tvar` constructor. The `TVar` constructor is used in the internal inference process.

Construct contexts as lists of `ContextEntry` type. As a user, you should only use
`EBasic tvar` constructor to add a type variable and `EAnno var type` to add an annotation to the context. 


# Todo

- Enhance print (remove unneeded parentheses)
- Provide error messages
