module UVMHS.Lib.Graph where

import UVMHS.Core

type Graph a = a ⇰ 𝑃 a

graphTranspose ∷ ∀ a. (Ord a) ⇒ Graph a → Graph a
graphTranspose kvs = joins
  [ dict $ mapOn (iter $ keys kvs) $ \ k → k ↦ pø
  , joins $ mapOn (iter kvs) $ \ (k :* vs) → 
      dict $ mapOn (iter vs) $ \ v → v ↦ single k
  ]

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
        if u ⋵ sccs
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

      stackᵣ = snd $ foldOnFrom (keys g) (visited₀ :* stack₀) $ \ u (visitedᵢ :* stackᵢ) →
        visit u visitedᵢ stackᵢ

      sccsᵣ = foldOnFrom stackᵣ sccs₀ $ \ u sccsᵢ → assign u u sccsᵢ

  in sccsᵣ

sccGroups ∷ ∀ a. (Ord a) ⇒ a ⇰ 𝑃 a → (a ⇰ a) ∧ (a ⇰ 𝑃 a ∧ 𝑃 a)
sccGroups deps =
  let graph ∷ a ∧ 𝔹 ⇰ 𝑃 (a ∧ 𝔹)
      graph = joins $ mapOn (iter deps) $ \ (x :* xs) → dict
        [ -- variable use (False) depends (↦) on its definition (True)
          (x :* False) ↦ single (x :* True)
        , -- varianble definition (True) depends (↦) on dependency uses (False)
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

sccEachGroup ∷ ∀ a b. (Ord a) ⇒ 𝐼 a → a ⇰ 𝑃 a → (𝔹 → 𝐼 a → 𝐼 b) → 𝐼 b
sccEachGroup xs deps f = unID $ sccEachGroupM xs deps $ ID ∘∘ f
