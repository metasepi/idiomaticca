-- | Some utility for ATS AST
module Language.Idiomaticca.ATSUtils
  ( Pos
  , dPos
  , AAts
  , ADecl
  , AExpr
  , AType
  , AArgs
  , AArg
  , APat
  , ALamT
  , AUni
  ) where

import qualified Language.ATS as A

type Pos = A.AlexPosn

dPos :: Pos
dPos = A.AlexPn 0 0 0

type AAts = A.ATS Pos
type ADecl = A.Declaration Pos
type AExpr = A.Expression Pos
type AType = A.Type Pos
type AArgs = A.Args Pos
type AArg = A.Arg Pos
type APat = A.Pattern Pos
type ALamT = A.LambdaType Pos
type AUni = A.Universal Pos
