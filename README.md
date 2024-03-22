HTML documentation [here](https://uvm-plaid.github.io/uvmhs/).

# Getting Started

## Unicode

UVMHS uses lots of unicode symbols. It's strongly encouraged to use a
fully featured unicode input mode when editing or using UVMHS. Most
people who use UVMHS use the input mode
[here](https://github.com/davdar/darais-unicode-input).

## Cross-Reference Table

Here is a cross-reference table between common datatypes and their
equivalents in both standard Haskell and UVMHS:

| Datatype          | UVMHS   | Standard Haskell  |
|-------------------|---------|-------------------|
| `bool`            | `𝔹`     | `Bool`            |
| `char`            | `ℂ`     | `Char`            |
| `nat (unbounded)` | `ℕ`     | `Natural`         |
| `int (unbounded)` | `ℤ`     | `Integer`         |
| `nat 64-bit`      | `ℕ64`   | `Word64`          |
| `int 64-bit`      | `ℤ64`   | `Int64`           |
| `nat 32-bit`      | `ℕ32`   | `Word32`          |
| `int 32-bit`      | `ℤ32`   | `Int32`           |
| `string`          | `𝕊`     | `String = [Char]` |
| `list`            | `𝐿 a`   | `[a]`             |
| `iterator`        | `𝐼 a`   | `[a]`             |
| `pair`            | `a ∧ b` | `(a,b)`           |
| `tagged union`    | `a ∨ b` | `Either a b`      |
| `optional`        | `𝑂 a`   | `Maybe a`         |
| `dictionary`      | `k ⇰ a` | `Map k a`         |
| `set`             | `𝑃 a`   | `Set a`           |
| `vector`          | `𝕍 a`   | `Vector a`        |

## Common Type Classes

In addition to datatypes, UVMHS uses its own basis of standard type
classes upon which the rest of the library builds.

### Collections

| UVMHS Type Class Function               | Standard Haskell Class |
|-----------------------------------------|------------------------|
| `single ∷ (Single a t) ⇒ a → t`         | N/A                    |
| `iter   ∷ (ToIter a t) ⇒ t → 𝐼 a`       | `Foldable t`           |
| `(⋕?)   ∷ (Lookup k v t) ⇒ t → k → 𝑂 v` | N/A                    |
| `(⋕)    ∷ (Access k v t) ⇒ t → k → v`   | N/A                    |

### Arithmetic

| UVMHS Type Class Function       | Standard Haskell Class |
|---------------------------------|------------------------|
| `zero ∷ (Zero a) ⇒ a`           | TODO                   |
| `(+)  ∷ (Plus a) ⇒ a → a → a`   | TODO                   |
| `(-)  ∷ (Minus a) ⇒ a → a → a`  | TODO                   |
| `one  ∷ (One a) ⇒ a`            | TODO                   |
| `(×)  ∷ (Times a) ⇒ a → a → a`  | TODO                   |
| `(/)  ∷ (Divide a) ⇒ a → a → a` | TODO                   |
| `(⌿)  ∷ (DivMod a) ⇒ a → a → a` | TODO                   |
| `(÷)  ∷ (DivMod a) ⇒ a → a → a` | TODO                   |
| `(^^) ∷ (Pon a) ⇒ a → a → a`    | TODO                   |
| `(^)  ∷ (Pow a) ⇒ a → a → a`    | TODO                   |
| `root ∷ (Root a) ⇒ a → a`       | TODO                   |
| `log  ∷ (Log a) ⇒ a → a`        | TODO                   |
| `efn  ∷ (Efn a) ⇒ a → a         | TODO                   |
| `sin  ∷ (Sin a) ⇒ a → a`        | TODO                   |
| `cos  ∷ (Cos a) ⇒ a → a`        | TODO                   |

### Monoids and Lattices

| UVMHS Type Class Function       | Standard Haskell Class |
|---------------------------------|------------------------|
| `null ∷ (Null a) ⇒ a`           | TODO                   |
| `(⧺)  ∷ (Append a) ⇒ a → a → a` | TODO                   |
| `bot  ∷ (Bot a) ⇒ a`            | TODO                   |
| `(⊔)  ∷ (Join a) ⇒ a → a → a`   | TODO                   |
| `top  ∷ (Top a) ⇒ a`            | TODO                   |
| `(⊓)  ∷ (Meet a) ⇒ a → a → a`   | TODO                   |
| `(⊑)  ∷ (POrd a) ⇒ a → a → 𝔹`   | TODO                   |

### Functors and Monads

| UVMHS Type Class Function                                 | Standard Haskell Class |
|-----------------------------------------------------------|------------------------|
| `map ∷ (Functor t) ⇒ t a → t b`                           | TODO                   |
| `return ∷ (Return m) ⇒ a → m a`                           | TODO                   |
| `(≫=) ∷ (Bind m) ⇒ m a → (a → m b) → m b`                 | TODO                   |
| `mapM ∷ (FunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b)` | TODO                   |
