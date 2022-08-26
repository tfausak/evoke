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
import qualified GHC.Types.SourceText as Ghc

app ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs
app s f = Ghc.reLocA . Ghc.L s . Ghc.HsApp Ghc.noAnn f

bindStmt ::
  Ghc.SrcSpan ->
  Ghc.LPat Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LStmt Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
bindStmt s p e =
  Ghc.reLocA . Ghc.L s $ Ghc.BindStmt Ghc.noAnn p e

doExpr :: Ghc.SrcSpan -> [Ghc.ExprLStmt Ghc.GhcPs] -> Ghc.LHsExpr Ghc.GhcPs
doExpr s = Ghc.reLocA . Ghc.L s . Ghc.HsDo Ghc.noAnn (Ghc.DoExpr Nothing) . Ghc.reLocA . Ghc.L s

explicitList ::
  Ghc.SrcSpan -> [Ghc.LHsExpr Ghc.GhcPs] -> Ghc.LHsExpr Ghc.GhcPs
explicitList s = Ghc.reLocA . Ghc.L s . Ghc.ExplicitList Ghc.noAnn

explicitTuple ::
  Ghc.SrcSpan -> [Ghc.HsTupArg Ghc.GhcPs] -> Ghc.LHsExpr Ghc.GhcPs
explicitTuple s xs = Ghc.reLocA . Ghc.L s $ Ghc.ExplicitTuple Ghc.noAnn xs Ghc.Boxed

fieldOcc :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LFieldOcc Ghc.GhcPs
fieldOcc s = Ghc.L s . Ghc.FieldOcc Ghc.noExtField

funBind ::
  Ghc.SrcSpan ->
  Ghc.OccName ->
  Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) ->
  Ghc.LHsBind Ghc.GhcPs
funBind s f g =
  Ghc.reLocA . Ghc.L s $ Ghc.FunBind Ghc.noExtField (unqual s f) g []

grhs ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
grhs s = Ghc.L s . Ghc.GRHS Ghc.noAnn []

grhss ::
  Ghc.SrcSpan ->
  [Ghc.LGRHS Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)] ->
  Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
grhss _ xs =
  Ghc.GRHSs Ghc.emptyComments xs $ Ghc.EmptyLocalBinds Ghc.noExtField

importDecl ::
  Ghc.SrcSpan ->
  Ghc.ModuleName ->
  Ghc.ModuleName ->
  Ghc.LImportDecl Ghc.GhcPs
importDecl s m n =
  Ghc.reLocA . Ghc.L s $
    Ghc.ImportDecl
      Ghc.noAnn
      Ghc.NoSourceText
      (Ghc.reLocA $ Ghc.L s m)
      Nothing
      Ghc.NotBoot
      False
      Ghc.QualifiedPre
      False
      (Just . Ghc.reLocA $ Ghc.L s n)
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
lam s = Ghc.reLocA . Ghc.L s . Ghc.HsLam Ghc.noExtField

lastStmt ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LStmt Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
lastStmt s e = Ghc.reLocA . Ghc.L s $ Ghc.LastStmt Ghc.noExtField e Nothing noSyntaxExpr

lit :: Ghc.SrcSpan -> Ghc.HsLit Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
lit s = Ghc.reLocA . Ghc.L s . Ghc.HsLit Ghc.noAnn

noSyntaxExpr :: Ghc.SyntaxExpr Ghc.GhcPs
noSyntaxExpr = Ghc.noSyntaxExpr

match ::
  Ghc.SrcSpan ->
  Ghc.HsMatchContext (Ghc.NoGhcTc Ghc.GhcPs) ->
  [Ghc.LPat Ghc.GhcPs] ->
  Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) ->
  Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
match s c ps = Ghc.reLocA . Ghc.L s . Ghc.Match Ghc.noAnn c ps

mg ::
  Ghc.Located [Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)] ->
  Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
mg ms = Ghc.MG Ghc.noExtField (Ghc.reLocA ms) Ghc.Generated

opApp ::
  Ghc.SrcSpan ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs
opApp s l o = Ghc.reLocA . Ghc.L s . Ghc.OpApp Ghc.noAnn l o

par :: Ghc.SrcSpan -> Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
par s = Ghc.reLocA . Ghc.L s . Ghc.HsPar Ghc.noAnn

qual :: Ghc.SrcSpan -> Ghc.ModuleName -> Ghc.OccName -> Ghc.LIdP Ghc.GhcPs
qual s m = Ghc.reLocA . Ghc.L s . Ghc.mkRdrQual m

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
recField s f e = Ghc.reLocA . Ghc.L s $ Ghc.HsRecField Ghc.noAnn f e False

recordCon ::
  Ghc.SrcSpan ->
  Ghc.LIdP Ghc.GhcPs ->
  Ghc.HsRecordBinds Ghc.GhcPs ->
  Ghc.LHsExpr Ghc.GhcPs
recordCon s c = Ghc.reLocA . Ghc.L s . Ghc.RecordCon Ghc.noAnn c

string :: String -> Ghc.HsLit Ghc.GhcPs
string = Ghc.HsString Ghc.NoSourceText . Ghc.mkFastString

tupArg :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.HsTupArg Ghc.GhcPs
tupArg = Ghc.Present Ghc.noAnn

tyVar :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LHsType Ghc.GhcPs
tyVar s = Ghc.reLocA . Ghc.L s . Ghc.HsTyVar Ghc.noAnn Ghc.NotPromoted

unqual :: Ghc.SrcSpan -> Ghc.OccName -> Ghc.LIdP Ghc.GhcPs
unqual s = Ghc.reLocA . Ghc.L s . Ghc.mkRdrUnqual

var :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
var s = Ghc.reLocA . Ghc.L s . Ghc.HsVar Ghc.noExtField

varPat :: Ghc.SrcSpan -> Ghc.LIdP Ghc.GhcPs -> Ghc.LPat Ghc.GhcPs
varPat s = Ghc.reLocA . Ghc.L s . Ghc.VarPat Ghc.noExtField
