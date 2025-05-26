module UVMHS.Lib.Substitution 
    -- * Datatypes
    -- ** Raw Variables
  ( 𝕎(..),varMarkL,varNameL,var,gensymVar
    -- ** Scoped Variables
  , 𝕏(..),d_SVarL,n_SVarL,g_SVarL,znsvar,znsvarL,gensymSVar
  , SVarView(..),svarScopeL
    -- ** Contextual Variables
  , 𝕐(..),s_UVarL,m_UVarL,duvar,duvarL,nuvar,nuvarL,znuvar,znuvarL,guvar,guvarL,gensymUVar
    -- ** Substitutions
  , Subst
    -- ** Metavar Substitutions
  , MetaSubst
    -- ** Substitutable Types
  , SubstyM,Substy(..)
    -- * Variable Operations
    -- ** Validating Variables
  , wfUVar
    -- ** Canonicalizing Variables
  , canonUVar
    -- * Substitution Operations
    -- ** Validating Substitutions
  , wfSubst
    -- ** Canonicalizing Substitutions
  , canonSubst
    -- ** Building Substitutions
  , shiftDSsSubst
  , shiftDSSubst
  , shiftDSubst
  , shiftNsSsSubst
  , shiftNsSSubst
  , shiftNsSubst
  , shiftNSSubst
  , shiftNSubst
  , introDSsSubst
  , introDSSubst
  , introDSubst
  , introNsSsSubst
  , introNsSSubst
  , introNsSubst
  , introNSSubst
  , introNSubst
  , bindDSsSubst
  , bindDSSubst
  , bindDSubst
  , bindsNsSsSubst
  , bindsNsSSubst
  , bindsNsSubst
  , bindsNSSubst
  , bindsNSubst
  , bindNsSsSubst
  , bindNsSSubst
  , bindNsSubst
  , bindNSSubst
  , bindNSubst
  , bindGsSsSubst
  , bindGsSSubst
  , bindGsSubst
  , bindGSSubst
  , bindGSubst
  , bindMsSsSubst
  , bindMsSSubst
  , bindMsSubst
  , bindMSSubst
  , bindMSubst
    -- ** Computing Free Variables
  , fvsWith,fvs,fvsMetas,fvsSMetas
    -- ** Converting Between Nameless and Named
  , todbr,tonmd
    -- ** Applying Substitutions
  , subst,msubst
    -- * Implementing @'Substy'@ for user-defined types.
  , substyDBdr
  , substyNBdr
  , substyBdr
  , substyVar
  , substyDVar
  , substyNVar
  , substyGVar
  , substyMVar
  , substy𝕏
  , substy𝕐
    -- * Pretty Printing
    -- ** De Bruijn Variables
  , ppDVar
    -- ** Named Variables
  , ppNVar
    -- * Parsing
    -- ** Raw Variables
  , syntaxVar,cpVar,cpVarWS
    -- ** De Bruijn Variables
  , syntaxDVar,cpDVarRaw,cpDVarRawInf,cpDVar,cpDVarInf
    -- ** Scoped Variables
  , syntaxSVar
  , cpZNSVar,cpGSVar,cpZNSVarWS,cpGSVarWS
  , cpSVarNGVarTail,cpSVarNGVar,cpSVarNGVarInfTail,cpSVarNGVarInf
  , cpSVarRaw,cpSVarRawInf,cpSVar,cpSVarInf
    -- ** Contextual Variables
  , syntaxUVar
    -- ** 
  , cpVar
  , cpUVarRaw
  ) where
  -- ( module UVMHS.Lib.Substitution.Subst
  -- , module UVMHS.Lib.Substitution.SubstElem
  -- , module UVMHS.Lib.Substitution.SubstScoped
  -- , module UVMHS.Lib.Substitution.SubstSpaced
  -- , module UVMHS.Lib.Substitution.Substy
  -- , module UVMHS.Lib.Substitution.UVar
  -- , module UVMHS.Lib.Substitution.Var
  -- ) where

import UVMHS.Lib.Substitution.Subst
import UVMHS.Lib.Substitution.SubstElem
import UVMHS.Lib.Substitution.SubstScoped
import UVMHS.Lib.Substitution.SubstSpaced
import UVMHS.Lib.Substitution.Substy
import UVMHS.Lib.Substitution.UVar
import UVMHS.Lib.Substitution.Var
