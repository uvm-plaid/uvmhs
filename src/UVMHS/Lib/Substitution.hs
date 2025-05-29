module UVMHS.Lib.Substitution 
  -- * Names
  ( Name(..),nameMarkL,nameNameL,name,gensymName,syntaxName,pName,pNameWS
  -- * Individual Variable Types
  , DVar(..),unDVarL,syntaxDVar,pDVarTail,pDVar
  , NVar(..),nvarIndexL,nvarNameL,nameNVar,nameNVarL,gensymNVar,syntaxNVar,pNVarTail,pNVar
  , GVar(..),unGVarL,syntaxGVar,pGVarTail,pGVar
  , MVar(..),mvarSubstL,mvarNameL,wfMVar,canonMVar,substMVar,syntaxMVar,pMVarTail,pMVar
  -- * Combined Variable Types
  , SVar(..),d_SVarL,n_SVarL,mkSVar,svarName,svarLevel
  , Var(..),d_VarL,n_VarL,g_VarL,nameVar,nameVarL,gensymVar,syntaxVar,pVar
  , UVar(..),d_UVarL,n_UVarL,g_UVarL,m_UVarL,nameUVarL,nameUVar,svar_UVar,svar_UVarL,var_UVar,var_UVarL
  , gensymUVar,wfUVar,canonUVar
  , syntaxUVar,pUVar
  -- * Variable Lens Classes
  , SVarView(..),svarScopeL
  -- * De Bruijn, Named and Global and Substitutions
  , Subst,isNullSubst,wfSubst,canonSubst,syntaxSubst,pSubst
  , dshiftsSubst,dshiftSubst,dintrosSubst,dintroSubst,dbindsSubst,dbindSubst
  , nshiftsSubst,nshiftSubst,nintrosSubst,nintroSubst,nbindsSubst,nbindSubst
  , gbindsSubst,gbindSubst
  -- * Metavar Substitutions
  , MetaSubst
  , mbindsSubst,mbindSubst
  -- * Substy Interface
  , SubstyM,Substy(..)
  , substyDBdr,substyNBdr,substyBdr
  , substyDVar,substyNVar,substyGVar,substySVar,substyVar,substyMVar,substyUVar
  , fvssWith,fvsWith,fvss,fvs,fvssMetas,fvsMetas
  , todbr,tonmd
  , subst,msubst
  ) where

import UVMHS.Lib.Substitution.Name
import UVMHS.Lib.Substitution.Subst
import UVMHS.Lib.Substitution.Substy
import UVMHS.Lib.Substitution.UVar
import UVMHS.Lib.Substitution.Var
