module Evoke.Hs
  ( app,
    bindStmt,
    doExpr,
    explicitList,
    explicitTuple,
    fieldOcc,
    funBind,
    grhs,
    grhss,
    importDecls,
    lam,
    lastStmt,
    lit,
    match,
    mg,
    opApp,
    par,
    qual,
    qualTyVar,
    qualVar,
    recField,
    recFields,
    recordCon,
    string,
    tupArg,
    tyVar,
    unqual,
    var,
    varPat,
  )
where

import qualified GHC.Hs as Ghc
import qualified GHC.Plugins as Ghc

app ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs
app s f = Ghc.L s . Ghc.HsApp Ghc.noExtField f

bindStmt ::
  Ghc.SrcSpan ->
  Ghc.LPat Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LStmt Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
bindStmt s p e =
  Ghc.L s $ Ghc.BindStmt Ghc.noExtField p e

doExpr :: Ghc.SrcSpan -> [Ghc.ExprLStmt Ghc.GhcPs] -> Ghc.LHsExpr Ghc.GhcPs
doExpr s = Ghc.L s . Ghc.HsDo Ghc.noExtField (Ghc.DoExpr Nothing) . Ghc.L s

explicitList ::
  Ghc.SrcSpan -> [Ghc.LHsExpr Ghc.GhcPs] -> Ghc.LHsExpr Ghc.GhcPs
explicitList s = Ghc.L s . Ghc.ExplicitList Ghc.noExtField Nothing

explicitTuple ::
  Ghc.SrcSpan -> [Ghc.LHsTupArg Ghc.GhcPs] -> Ghc.LHsExpr Ghc.GhcPs
explicitTuple s xs = Ghc.L s $ Ghc.ExplicitTuple Ghc.noExtField xs Ghc.Boxed

fieldOcc :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LFieldOcc Ghc.GhcPs
fieldOcc s = Ghc.L s . Ghc.FieldOcc Ghc.noExtField

funBind ::
  Ghc.SrcSpan ->
  Ghc.OccName ->
  Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) ->
  Ghc.LHsBind Ghc.GhcPs
funBind s f g =
  Ghc.L s $ Ghc.FunBind Ghc.noExtField (unqual s f) g []

grhs ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
grhs s = Ghc.L s . Ghc.GRHS Ghc.noExtField []

grhss ::
  Ghc.SrcSpan ->
  [Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)] ->
  Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
grhss s xs =
  Ghc.GRHSs Ghc.noExtField xs . Ghc.L s $ Ghc.EmptyLocalBinds Ghc.noExtField

importDecl ::
  Ghc.SrcSpan ->
  Ghc.ModuleName ->
  Ghc.ModuleName ->
  Ghc.LImportDecl Ghc.GhcPs
importDecl s m n =
  Ghc.L s $
    Ghc.ImportDecl
      Ghc.noExtField
      Ghc.NoSourceText
      (Ghc.L s m)
      Nothing
      Ghc.NotBoot
      False
      Ghc.QualifiedPre
      False
      (Just $ Ghc.L s n)
      Nothing

importDecls ::
  Ghc.SrcSpan ->
  [(Ghc.ModuleName, Ghc.ModuleName)] ->
  [Ghc.LImportDecl Ghc.GhcPs]
importDecls = fmap . uncurry . importDecl

lam ::
  Ghc.SrcSpan ->
  Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) ->
  Ghc.LHsExpr Ghc.GhcPs
lam s = Ghc.L s . Ghc.HsLam Ghc.noExtField

lastStmt ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LStmt Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
lastStmt s e = Ghc.L s $ Ghc.LastStmt Ghc.noExtField e Nothing noSyntaxExpr

lit :: Ghc.SrcSpan -> Ghc.HsLit Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
lit s = Ghc.L s . Ghc.HsLit Ghc.noExtField

noSyntaxExpr :: Ghc.SyntaxExpr Ghc.GhcPs
noSyntaxExpr = Ghc.noSyntaxExpr

match ::
  Ghc.SrcSpan ->
  Ghc.HsMatchContext (Ghc.NoGhcTc Ghc.GhcPs) ->
  [Ghc.LPat Ghc.GhcPs] ->
  Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) ->
  Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
match s c ps = Ghc.L s . Ghc.Match Ghc.noExtField c ps

mg ::
  Ghc.Located [Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)] ->
  Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
mg ms = Ghc.MG Ghc.noExtField ms Ghc.Generated

opApp ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs
opApp s l o = Ghc.L s . Ghc.OpApp Ghc.noExtField l o

par :: Ghc.SrcSpan -> Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
par s = Ghc.L s . Ghc.HsPar Ghc.noExtField

qual :: Ghc.SrcSpan -> Ghc.ModuleName -> Ghc.OccName -> Ghc.LIdP Ghc.GhcPs
qual s m = Ghc.L s . Ghc.mkRdrQual m

qualTyVar ::
  Ghc.SrcSpan -> Ghc.ModuleName -> Ghc.OccName -> Ghc.LHsType Ghc.GhcPs
qualTyVar s m = tyVar s . qual s m

qualVar ::
  Ghc.SrcSpan -> Ghc.ModuleName -> Ghc.OccName -> Ghc.LHsExpr Ghc.GhcPs
qualVar s m = var s . qual s m

recFields ::
  [Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)] ->
  Ghc.HsRecFields Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
recFields = flip Ghc.HsRecFields Nothing

recField ::
  Ghc.SrcSpan ->
  Ghc.LFieldOcc Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
recField s f e = Ghc.L s $ Ghc.HsRecField f e False

recordCon ::
  Ghc.SrcSpan ->
  Ghc.LIdP Ghc.GhcPs ->
  Ghc.HsRecordBinds Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs
recordCon s c = Ghc.L s . Ghc.RecordCon Ghc.noExtField c

string :: String -> Ghc.HsLit Ghc.GhcPs
string = Ghc.HsString Ghc.NoSourceText . Ghc.mkFastString

tupArg :: Ghc.SrcSpan -> Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsTupArg Ghc.GhcPs
tupArg s = Ghc.L s . Ghc.Present Ghc.noExtField

tyVar :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LHsType Ghc.GhcPs
tyVar s = Ghc.L s . Ghc.HsTyVar Ghc.noExtField Ghc.NotPromoted

unqual :: Ghc.SrcSpan -> Ghc.OccName -> Ghc.LIdP Ghc.GhcPs
unqual s = Ghc.L s . Ghc.mkRdrUnqual

var :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
var s = Ghc.L s . Ghc.HsVar Ghc.noExtField

varPat :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LPat Ghc.GhcPs
varPat s = Ghc.L s . Ghc.VarPat Ghc.noExtField
