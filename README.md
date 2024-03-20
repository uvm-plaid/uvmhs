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

| Datatype          | Standard Haskell                           | UVMHS                                     |
|-------------------|--------------------------------------------|-------------------------------------------|
| `bool`            | `Bool`            <br> (`Prelude`)         | `𝔹`            <br> [alias to `Bool`]     |
| `char`            | `Char`            <br> (`Prelude`)         | `ℂ`            <br> [alias to `Char`]     |
| `nat (unbounded)` | `Natural`         <br> (`Numeric.Natural`) | `ℕ`            <br> [alias to `Natural`]  |
| `int (unbounded)` | `Integer`         <br> (`Prelude`)         | `ℤ`            <br> [alias to `Integer`]  |
| `nat 64-bit`      | `Word64`          <br> (`Data.Word`)       | `ℕ64`          <br> [alias to `Word64`]   |
| `int 64-bit`      | `Int64`           <br> (`Data.Int`)        | `ℤ64`          <br> [alias to `Int64`]    |
| `nat 32-bit`      | `Word32`          <br> (`Data.Word`)       | `ℕ32`          <br> [alias to `Word32`]   |
| `int 32-bit`      | `Int32`           <br> (`Data.Int`)        | `ℤ32`          <br> [alias to `Int32`]     |
| `string`          | `String = [Char]` <br> (`Prelude`)         | `𝕊`            <br> [alias to `Text`]      |
| `list`            | `[a]`             <br> (`Prelude`)         | `𝐿 a`          <br> [new datatype]        |
| `iterator`        | `[a]`             <br> (`Prelude`)         | `𝐼 a`          <br> [new datatype]        |
| `pair`            | `(a,b)`           <br> (`Prelude`)         | `a ∧ b`        <br> [new datatype]        |
| `tagged union`    | `Either a b`      <br> (`Prelude`)         | `a ∨ b`        <br> [new datatype]        |
| `optional`        | `Maybe a`         <br> (`Prelude`)         | `𝑂 a`          <br> [new datatype]        |
| `dictionary`      | `Map k a`         <br> (`Data.Map`)        | `k ⇰ a`        <br> [newtype to `Map`]    |
| `set`             | `Set a`           <br> (`Data.Set`)        | `𝑃 a`          <br> [newtype to `Set`]    |
| `vector`          | `Vector a`        <br> (`Data.Vector`)     | `𝕍 a`          <br> [newtype to `Vector`] |

## Common Functions

Here are some common functions in UVMHS.

Some of the function types are instantiated to specific types (e.g.,
lists `𝐿`) to make things simple, but their actual types are more
generic and parameterized by type classes (e.g., iterable things
`ToIter`). Such types are annotated with "(generic)".

| Standard Haskell Function         | UVMHS FunctionName | Type                          |
|-----------------------------------|--------------------|-------------------------------|
| `putStrLn`      <br> (`Prelude`)  | `out`              | `𝕊 → IO ()`                   |
| `Set.fromList`  <br> (`Data.Set`) | `pow`              | `𝐿 t → 𝑃 a` (generic)         |
| `Map.singleton` <br> (`Data.Map`) | `(↦)`              | `k → a → k ⇰ a`               |
| `Map.unions`    <br> (`Data.Map`) | `dict`             | `𝐿 (k ⇰ a) → k ⇰ a` (generic) |


| Less Standard Haskell Functions | UVMHS Function Name | Type                  |
|---------------------------------|---------------------|-----------------------|
| N/A                             | `iter`              | `𝐿 a → 𝐼 a` (generic) |
| N/A                             | `makePrettyRecord`  | `<macro>`             |
| N/A                             | `makePrettySum`     | `<macro>`             |
| N/A                             | `makePrettyUnion`   | `<macro>`             |

And common type classes:

- `Zero a`, `Plus a`, `Additive a`:

  Relevant Source Files: TODO

  Functions primitive to the `Zero a` class:

  | Function Name | Type           |
  |---------------|----------------|
  | `zero`        | `(Zero a) ⇒ a` |

  Functions primitive to the `Plus a` class:

  | Function Name | Type                   |
  |---------------|------------------------|
  | `(+)`         | `(Plus a) ⇒ a → a → a` |

  `Additive a` is equivalent to `(Zero a,Plus a)`

- `One a`, `Times a`, `Multiplicative a`:

  Relevant Source Files: TODO

  Functions primitive to the `One a` class:

  | Function Name | Type          |
  |---------------|---------------|
  | `one`         | `(One a) ⇒ a` |

  Functions primitive to the `Times a` class:

  | Function Name | Type                             |
  |---------------|----------------------------------|
  | `(×)`         | `(Multiplicative a) ⇒ a → a → a` |

  `Multiplicative a` is equivalent to `(Additive a,One a,Times a)`

- `Null a`, `Append a`, `Monoid a`:

  Relevant Source Files: TODO

  Functions primitive to the `Null a` class:

  | Function Name | Type           |
  |---------------|----------------|
  | `null`        | `(Null a) ⇒ a` |

  Functions primitive to the `Plus a` class:

  | Function Name | Type                     |
  |---------------|--------------------------|
  | `(⧺)`         | `(Append a) ⇒ a → a → a` |

  `Monoid a` is equivalent to `(Null a,Append a)`

- `Bot a`, `Join a`, `JoinLattice a`:

  Relevant Source Files: TODO

  Functions primitive to the `Bot a` class:

  | Function Name | Type          |
  |---------------|---------------|
  | `bot`         | `(Bot a) ⇒ a` |

  Functions primitive to the `Plus a` class:

  | Function Name | Type                   |
  |---------------|------------------------|
  | `(⊔)`         | `(Join a) ⇒ a → a → a` |

  `JoinLattice a` is equivalent to `(Bot a,Join a)`

- `Top a`, `Join a`, `MeetLattice a`:

  Relevant Source Files: TODO

  Functions primitive to the `Top a` class:

  | Function Name | Type          |
  |---------------|---------------|
  | `top`         | `(Top a) ⇒ a` |

  Functions primitive to the `Plus a` class:

  | Function Name | Type                   |
  |---------------|------------------------|
  | `(⊓)`         | `(Meet a) ⇒ a → a → a` |

  `MeetLattice a` is equivalent to `(Top a,Meet a)`

- `Functor`
- `Monad`

