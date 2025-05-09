module UVMHS.Lib.Graph where

import UVMHS.Core

-- | A `Graph` has node elements `a`, and is just a relation between nodes.
--
-- Notation: 
--
--     kvss : Graph a
--
-- (same as general dictionaries)
type Graph a = a ⇰ 𝑃 a

-- | Property:
--
--     (v ↦ ks) ∈ graph-transpose(kvss) ⟺ ∀ (k ↦ vs) ∈ kvss. k ∈ ks ⟺ v ∈ vs
graphTranspose ∷ ∀ a. (Ord a) ⇒ Graph a → Graph a
graphTranspose kvss = joins
  [ dict $ mapOn (iter $ dkeys kvss) $ \ k → k ↦ pø
  , joins $ mapOn (iter kvss) $ \ (k :* vs) →
      dict $ mapOn (iter vs) $ \ v → v ↦ single k
  ]

-- | Run the Kosaraju algorithm, which computes strongly connected components.
-- The algorithm computes a canonical node for each strongly connected
-- component, and maps each node to the canonical node for strongly connected
-- component in which it lives.
--
-- Definition: 
--
--     strongly-connected ∈ ℘(Graph)
--     strongly-connected(kvss) ≜ 
--       ∀ k₁,k₂ ∈ nodes(kvss). ∃ path(k₁,k₂)
--     strongly-connected-components ∈ ℘(℘(Graph))
--     strongly-connected-components(kvsss) ≜ 
--         (∀ kvss ∈ kvsss. strongly-connected(kvss))
--       ∧ (∀ kvss₁,kvss₂ ∈ kvsss. ¬strongly-connected(kvss₁ ∪kvss₂))
--
-- Property: 
-- 
--     ∀ kvss. 
--       strongly-connected-components(
--         values(
--           graph-transpose(
--             { k ↦ {v} 
--             | (k ↦ v) ∈ kosaraju(kvss)
--             }
kosaraju ∷ ∀ a. (Ord a) ⇒ Graph a → a ⇰ a
kosaraju g =
  let gᵀ = graphTranspose g

      visit ∷ a → 𝑃 a → 𝐿 a → 𝑃 a ∧ 𝐿 a
      visit u visited stack =
        if u ∈ visited
        then visited :* stack
        else
          let visited' = single u ∪ visited
              visited'' :* stack' =
                foldOnFrom (g ⋕! u) (visited' :* stack) $ \ v (visitedᵢ :* stackᵢ) →
                  visit v visitedᵢ stackᵢ
              stack'' = u :& stack'
          in visited'' :* stack''

      assign ∷ a → a → a ⇰ a → a ⇰ a
      assign u anchor sccs =
        if u ⋿ sccs
        then sccs
        else
          let sccs' = (u ↦ anchor) ⩌ sccs
              sccs'' = foldOnFrom (gᵀ ⋕! u) sccs' $ \ v sccsᵢ → assign v anchor sccsᵢ
          in sccs''

      visited₀ ∷ 𝑃 a
      visited₀ = pø
      stack₀ ∷ 𝐿 a
      stack₀ = Nil
      sccs₀ = dø

      stackᵣ = snd $ foldOnFrom (dkeys g) (visited₀ :* stack₀) $ \ u (visitedᵢ :* stackᵢ) →
        visit u visitedᵢ stackᵢ

      sccsᵣ = foldOnFrom stackᵣ sccs₀ $ \ u sccsᵢ → assign u u sccsᵢ

  in sccsᵣ

-- | In the context of this function, we treat the element `a` like variables
-- that have a definition/use structure. The first argument `deps` encodes
-- dependencies from definitions of things to uses of things.
--
-- From a dependency relation, compute two elements:
-- 1. The strongly connected components for the dependency relation,
--    represented as a map from variables to a canonical SCC elements for its
--    dependency group
-- 2. A map from canonical SCC elements to:
--    (a). All variables in its dependency group (i.e., the inverse of (1))
--    (b). All dependencies of all variables in the dependnecy group
--
-- (1) is computed using `kosaraju`, but with an additional bit of information
-- in the node representation that encodes def/use information, following these
-- two semantic ideas:
-- - *Use* of any variable `x` depends on the *definition* of that variable `x`
-- - If `{x ↦ y}` appears in `deps`, this means that *definition* of variable
--   `x` depends on *use* of variable `y`.
sccGroups ∷ ∀ a. (Ord a) ⇒ a ⇰ 𝑃 a → (a ⇰ a) ∧ (a ⇰ 𝑃 a ∧ 𝑃 a)
sccGroups deps =
  let graph ∷ a ∧ 𝔹 ⇰ 𝑃 (a ∧ 𝔹)
      graph = joins $ mapOn (iter deps) $ \ (x :* xs) → dict
        [ -- variable use (False) depends (↦) on its definition (True)
          (x :* False) ↦ single (x :* True)
        , -- variable definition (True) depends (↦) on dependency uses (False)
          (x :* True) ↦ pow (map (:* False) $ iter xs)
        ]
      -- mapping from def/use to a canonical representative for its scc equivalence class
      sccsDefuse ∷ a ∧ 𝔹 ⇰ a ∧ 𝔹
      sccsDefuse = kosaraju graph
      -- throw out def/use information and just map variables to groups
      sccs ∷ a ⇰ a
      sccs = dict $ mapOn (iter sccsDefuse) $ \ ((x₁ :* b) :* (x₂ :* _)) →
        if b then x₁ ↦ x₂ else null
      -- map group ids to variables in that group, and all dependencies of
      -- that group
      groups ∷ a ⇰ 𝑃 a ∧ 𝑃 a
      groups = joins $ mapOn (iter sccs) $ \ (x₁ :* x₂) →
        x₂ ↦ single x₁ :* (deps ⋕! x₁)
  in sccs :* groups

-- | Compute strongly connected components following a dependency map, and
-- process each component with a monadic action.
-- 
--     sccEachGroupM xs deps f
--
-- - `xs` represents a desired ordering in which to process variables (e.g., if
--   definitions `x` and `y` do not depend on each other, and `x` comes before
--   `y` in `xs`, then call `f` on the SCC group for `x` before doing the same
--   for `y`.)
-- - `deps` represents dependency information. See `sccGroups` for how these
--   are processed.
-- - `f b xs′` represents an action to perform on each SCC group. If the group
--   is non-recursive, then `b` will be `False` and `|xs′| = 1`. If the group
--   is recursive, then `b` will be `True` and `|xs′| ≥ 1`.
sccEachGroupM ∷ ∀ a b m. (Ord a,Monad m) ⇒ 𝐼 a → a ⇰ 𝑃 a → (𝔹 → 𝐼 a → m (𝐼 b)) → m (𝐼 b)
sccEachGroupM xs deps f =
  let sccs :* groups = sccGroups deps
      visitVar ∷ a → RWST () (𝐼 b) (𝑃 a) m ()
      visitVar x = do
        -- lookup the group this element is in
        let g = sccs ⋕! x
        -- if we have already processed this group, then skip
        seen ← get
        if g ∈ seen then skip
        else do
          -- mark that we have already processed this group
          modify $ (∪) $ single g
          -- look up elements and dependencies in this group
          let gdefs :* gdeps = groups ⋕! g
              cyclic = gdefs ∩ gdeps ≢ pø
          -- sequentialize all dependencies (that aren't cyclic)
          eachOn (gdeps ∖ gdefs) visitVar
          -- build a list of results
          tell *$ lift $ f cyclic $ iter gdefs
    in evalRWST () pø $ retOut $ eachOn xs visitVar

-- | Similar to `sccEachGroupM`, but specialized to the identity monad.
sccEachGroup ∷ ∀ a b. (Ord a) ⇒ 𝐼 a → a ⇰ 𝑃 a → (𝔹 → 𝐼 a → 𝐼 b) → 𝐼 b
sccEachGroup xs deps f = unID $ sccEachGroupM xs deps $ ID ∘∘ f
