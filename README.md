HTML documentation [here](https://uvm-plaid.github.io/uvmhs/).

# Contributing and Versioning

Current Version: 0.0.1.0

- The `master` branch tracks with library versions, as specified in the
  `package.yaml`.
- The `future` branch is were new changes are made and collected before being
  rolled up into version releases. The `CHANGELOG.md` in that branch will track
  changes. Any merge of `future` into `master` should bump the version in
  `package.yaml` and at the top of this README. Version bumps should be at
  least in `X` position for the version structure `*.*.X.*`.
- Every single commit to `master` that isn't a version bump to `*.*.X.*` (e.g.,
  by merging `future`) must bump the minor version `*.*.*.X` and document
  changes in `CHANGELOG.md`. These minor version bumps are intended to be
  reserved only for hotfixes, and should be upstreamed to `future` ASAP.
- Things to do before cutting a version:
  - run `make docs`
  - run `make fixity-levels`

# Getting Started

## Unicode

UVMHS uses lots of unicode symbols. It's strongly encouraged to use a
fully featured unicode input mode when editing or using UVMHS. Most
people who use UVMHS use the input mode
[here](https://github.com/davdar/darais-unicode-input).

Note that the Symbola font has an issue where it swaps the symbols for
U+2A4F (Double Square Union) `⩏` and U+2A4E (Double Square Intersection)
`⩎`, and Symbola is sometimes used as a default fallback font in Linux
distributions.

# Quick Reference

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

| UVMHS Primitive Type Class Functions    |
|-----------------------------------------|
| `single ∷ (Single a t) ⇒ a → t`         |
| `iter   ∷ (ToIter a t) ⇒ t → 𝐼 a`       |
| `(⋕?)   ∷ (Lookup k v t) ⇒ t → k → 𝑂 v` |
| `(⋕)    ∷ (Access k v t) ⇒ t → k → v`   |

### Arithmetic

| UVMHS Type Class Function       |
|---------------------------------|
| `zero ∷ (Zero a) ⇒ a`           |
| `one  ∷ (One a) ⇒ a`            |
| `(+)  ∷ (Plus a) ⇒ a → a → a`   |
| `(-)  ∷ (Minus a) ⇒ a → a → a`  |
| `(×)  ∷ (Times a) ⇒ a → a → a`  |
| `(/)  ∷ (Divide a) ⇒ a → a → a` |
| `(⌿)  ∷ (DivMod a) ⇒ a → a → a` |
| `(÷)  ∷ (DivMod a) ⇒ a → a → a` |
| `(^^) ∷ (Pon a) ⇒ a → a → a`    |
| `(^)  ∷ (Pow a) ⇒ a → a → a`    |
| `root ∷ (Root a) ⇒ a → a`       |
| `log  ∷ (Log a) ⇒ a → a`        |
| `efn  ∷ (Efn a) ⇒ a → a`        |
| `sin  ∷ (Sin a) ⇒ a → a`        |
| `cos  ∷ (Cos a) ⇒ a → a`        |

### Monoids and Lattices

| UVMHS Type Class Function       |
|---------------------------------|
| `null ∷ (Null a) ⇒ a`           |
| `(⧺)  ∷ (Append a) ⇒ a → a → a` |
| `bot  ∷ (Bot a) ⇒ a`            |
| `(⊔)  ∷ (Join a) ⇒ a → a → a`   |
| `top  ∷ (Top a) ⇒ a`            |
| `(⊓)  ∷ (Meet a) ⇒ a → a → a`   |
| `(⊑)  ∷ (POrd a) ⇒ a → a → 𝔹`   |

### Functors and Monads

| UVMHS Type Class Function                                   |
|-------------------------------------------------------------|
| `map    ∷ (Functor t) ⇒ t a → t b`                          |
| `return ∷ (Return m) ⇒ a → m a`                             |
| `(≫=)   ∷ (Bind m) ⇒ m a → (a → m b) → m b`                 |
| `mapM   ∷ (FunctorM t,Monad m) ⇒ (a → m b) → t a → m (t b)` |
