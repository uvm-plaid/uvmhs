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

(When you see `<pipe>` in the table, this represents the `|`
character, which when used directly messes up the `vim-table-mode`
plugin I use to edit the table.)

|-------------------|--------------------------------------------|------------------------|----------------------|-------------------|--------------------------|-----------------------|
| Datatype          | Standard Haskell                           | UVMHS                  | Constructor Patterns | Common Operations | Unicode Characters  UVMS | Relevant Source Files |
|-------------------|--------------------------------------------|------------------------|----------------------|-------------------|--------------------------|-----------------------|
| `bool`            | `Bool`            (from `Prelude`)         | `𝔹 = Bool` [alias]     |                      | `⩓`, `⩔`          | `\bbB`                   | TODO                  |
| `char`            | `Char`            (from `Prelude`)         | `ℂ = Char` [alias]     |                      |                   | `\bbC`                   | TODO                  |
| `nat (unbounded)` | `Natural`         (from `Numeric.Natural`) | `ℕ = Natural` [alias]  |                      | `+`, `×`          | `\bbN`                   | TODO                  |
| `int (unbounded)` | `Integer`         (from `Prelude`)         | `ℤ = Integer` [alias]  |                      | `+`, `×`          | `\bbZ`                   | TODO                  |
| `nat 64-bit`      | `Word64`          (from `Data.Word`)       | `ℕ64 = Word64` [alias] |                      | `+`, `×`          | `\bbN`                   | TODO                  |
| `int 64-bit`      | `Int64`           (from `Data.Int`)        | `ℤ64 = Int64` [alias]  |                      | `+`, `×`          | `\bbZ`                   | TODO                  |
| `nat 32-bit`      | `Word32`          (from `Data.Word`)       | `ℕ32 = Word32` [alias] |                      | `+`, `×`          | `\bbN`                   | TODO                  |
| `int 32-bit`      | `Int32`           (from `Data.Int`)        | `ℤ32 = Int32` [alias]  |                      | `+`, `×`          | `\bbZ`                   | TODO                  |
| `string`          | `String = [Char]` (from `Prelude`)         | `𝕊 = Text` [alias]     |                      | `⧺`               | `\bbS`                   | TODO                  |
| `list`            | `[a]`             (from `Prelude`)         | `𝐿 a` [new type]       | `Nil`, `:&`          | `list`,`⧺`,       | `\itL`                   | TODO                  |
| `iterator`        | `[a]`             (from `Prelude`)         | `𝐼 a` [new type]       |                      | `iter`,`⧺`,       | `\itI`                   | TODO                  |
| `dictionary`      | `Map k a`         (from `Data.Map`)        | `k ⇰ a` [new type]     |                      | `dict`,`⋿` `⩌`    | `\r<pipe>=`              | TODO                  |
| `set`             | `Set a`           (from `Data.Set`)        | `𝑃 a` [new type]       |                      | `pow`,`∈`,`∪`,`∩` | `\itP`                   | TODO                  |
| `vector`          | `Vector a`        (from `Data.Vector`)     | `𝕍 a` [new type]       |                      | `vec`,`⋕?`,`⋕!`   | `\bbV`                   | TODO                  |
| `pair`            | `(a,b)`           (from `Prelude`)         | `a ∧ b` [new type]     | `:*`                 |                   | `\and`                   | TODO                  |
| `tagged union`    | `Either a b`      (from `Prelude`)         | `a ∨ b` [new type]     | `Inl`, `Inr`         |                   | `\or`                    | TODO                  |
| `optional`        | `Maybe a`         (from `Prelude`)         | `𝑂 a` [new type]       | `None`, `Some`       |                   | `\itO`                   | TODO                  |
|-------------------|--------------------------------------------|------------------------|----------------------|-------------------|--------------------------|-----------------------|

## Common Functions

Here are some common functions in UVMHS.

Some of the function types are instantiated to specific types (e.g.,
lists `𝐿`) to make things simple, but their actual types are more
generic and parameterized by type classes (e.g., iterable things
`ToIter`). Such types are annotated with "(generic)".

|--------------------|-------------------------------|-----------------------------|-------------------------------------------------------------------------------------|-----------------------------------|
| UVMHS FunctionName | Type                          | Example                     | Description                                                                         | Standard Haskell Analog           |
|--------------------|-------------------------------|-----------------------------|-------------------------------------------------------------------------------------|-----------------------------------|
| `out`              | `𝕊 → IO ()`                   | `out "hello world"`         | Print a string to the terminal followed by a newline                                | `putStrLn` (from `Prelude`)       |
| `pow`              | `𝐿 t → 𝑃 a` (generic)         | `pow [1,2]`                 | Convert something that is iterable with elements to a set of those elements         | `Set.fromList` (from `Data.Set`)  |
| `(↦)`              | `k → a → k ⇰ a`               | `"a" ↦ 1`                   | Create a singleton dictionary                                                       | `Map.singleton` (from `Data.Map`) |
| `dict`             | `𝐿 (k ⇰ a) → k ⇰ a` (generic) | `dict ["a"↦1,"b"↦2]`        | Convert something that is iterable with dictionaries inside into a dictionary       | `Map.unions` (from `Data.Map`)    |
| `iter`             | `𝐿 a → 𝐼 a` (generic)         | `iter [1,2]                 | Convert something that is iterable with elements to an iterator over those elements | N/A                               |
| `makePrettyRecord` | `<macro>`                     | `makePrettyRecord ''MyType` | Generate a `Pretty` instance that prints keys and values in record notation         | N/A                               |
| `makePrettySum`    | `<macro>`                     | `makePrettySum ''MyType`    | Generate a `Pretty` instance that prints constructor names                          | N/A                               |
| `makePrettyUnion`  | `<macro>`                     | `makePrettyUnion ''MyType`  | Generate a `Pretty` instance that omits constructor names                           | N/A                               |
|--------------------|-------------------------------|-----------------------------|-------------------------------------------------------------------------------------|-----------------------------------|

And common type classes:

- `Zero a`, `Plus a`, `Additive a`:

  Relevant Source Files: TODO

  Functions primitive to the `Zero a` class:

  |---------------|----------------|
  | Function Name | Type           |
  |---------------|----------------|
  | `zero`        | `(Zero a) ⇒ a` |
  |---------------|----------------|

  Functions primitive to the `Plus a` class:

  |---------------|------------------------|
  | Function Name | Type                   |
  |---------------|------------------------|
  | `(+)`         | `(Plus a) ⇒ a → a → a` |
  |---------------|------------------------|

  `Additive a` is equivalent to `(Zero a,Plus a)`

- `One a`, `Times a`, `Multiplicative a`:

  Relevant Source Files: TODO

  Functions primitive to the `One a` class:

  |---------------|---------------|
  | Function Name | Type          |
  |---------------|---------------|
  | `one`         | `(One a) ⇒ a` |
  |---------------|---------------|

  Functions primitive to the `Times a` class:

  |---------------|----------------------------------|
  | Function Name | Type                             |
  |---------------|----------------------------------|
  | `(×)`         | `(Multiplicative a) ⇒ a → a → a` |
  |---------------|----------------------------------|

  `Multiplicative a` is equivalent to `(Additive a,One a,Times a)`

- `Null a`, `Append a`, `Monoid a`:

  Relevant Source Files: TODO

  Functions primitive to the `Null a` class:

  |---------------|----------------|
  | Function Name | Type           |
  |---------------|----------------|
  | `null`        | `(Null a) ⇒ a` |
  |---------------|----------------|

  Functions primitive to the `Plus a` class:

  |---------------|--------------------------|
  | Function Name | Type                     |
  |---------------|--------------------------|
  | `(⧺)`         | `(Append a) ⇒ a → a → a` |
  |---------------|--------------------------|

  `Monoid a` is equivalent to `(Null a,Append a)`

- `Bot a`, `Join a`, `JoinLattice a`:

  Relevant Source Files: TODO

  Functions primitive to the `Bot a` class:

  |---------------|---------------|
  | Function Name | Type          |
  |---------------|---------------|
  | `bot`         | `(Bot a) ⇒ a` |
  |---------------|---------------|

  Functions primitive to the `Plus a` class:

  |---------------|------------------------|
  | Function Name | Type                   |
  |---------------|------------------------|
  | `(⊔)`         | `(Join a) ⇒ a → a → a` |
  |---------------|------------------------|

  `JoinLattice a` is equivalent to `(Bot a,Join a)`

- `Top a`, `Join a`, `MeetLattice a`:

  Relevant Source Files: TODO

  Functions primitive to the `Top a` class:

  |---------------|---------------|
  | Function Name | Type          |
  |---------------|---------------|
  | `top`         | `(Top a) ⇒ a` |
  |---------------|---------------|

  Functions primitive to the `Plus a` class:

  |---------------|------------------------|
  | Function Name | Type                   |
  |---------------|------------------------|
  | `(⊓)`         | `(Meet a) ⇒ a → a → a` |
  |---------------|------------------------|

  `MeetLattice a` is equivalent to `(Top a,Meet a)`

- `Functor`
- `Monad`

