-- | Some utility for ATS AST
module Language.Idiomaticca.ATSUtils
  ( Pos
  , dPos
  , AArg
  , AArgs
  , AAts
  , ADecl
  , AExpr
  , AFunc
  , ALamT
  , APat
  , AType
  , AUni
  ) where

import qualified Language.ATS as A

type Pos = A.AlexPosn

dPos :: Pos
dPos = A.AlexPn 0 0 0

type AArg = A.Arg Pos
type AArgs = A.Args Pos
type AAts = A.ATS Pos
type ADecl = A.Declaration Pos
type AExpr = A.Expression Pos
type AFunc = A.Function Pos
type ALamT = A.LambdaType Pos
type APat = A.Pattern Pos
type AType = A.Type Pos
type AUni = A.Universal Pos
