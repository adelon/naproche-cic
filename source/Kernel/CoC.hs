{-# LANGUAGE OverloadedStrings #-}
module Kernel.CoC where

import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Map as Map

data LambdaCalculus
  = Var Text
  | App LambdaCalculus LambdaCalculus
  | Lam Text LambdaCalculus LambdaCalculus
  | Pi Text LambdaCalculus LambdaCalculus
  | Star
  | Box
  deriving (Eq, Ord, Show)

format :: LambdaCalculus -> IO ()
format = putStrLn . go 0
  where
    go _ (Var v) = Text.unpack v
    go n (App t1 t2) = go n t1 ++ newLine (n + 2)
                    ++ go (n + 2) t2
    go n (Lam v a t) = "λ(" ++ Text.unpack v ++ " : "
                    ++ go n a ++ ") ->" ++ newLine (n + 2)
                    ++ go (n + 2) t
    go n (Pi v a t) = "Λ(" ++ Text.unpack v ++ " : "
                   ++ go n a ++ ") -> " ++ go (n + 2) t
    go _ Star = "*"
    go _ Box = "☐"
    newLine n = '\n' : replicate n ' '

-- | Free variables in an expression
fv :: LambdaCalculus -> Set Text
fv (Var v) = Set.singleton v
fv (App l1 l2) = fv l1 <> fv l2
fv (Lam v a l) = Set.delete v (fv a <> fv l)
fv (Pi v a l) = Set.delete v (fv a <> fv l)
fv _ = Set.empty

-- | Find a name not in the list of bound names
renameVar :: Text -> Set Text -> Text
renameVar var bound = head $ filter (flip Set.notMember bound)
  $ var : map (\n -> Text.append var (Text.pack $ show n)) [1::Int ..]

-- | Rename all variables in an expression.
renameVars :: Set Text -> LambdaCalculus -> LambdaCalculus
renameVars taken = go Set.empty
  where
    go bound (Var w) | w `Set.notMember` bound = Var (renameVar w taken)
    go bound (App l1 l2) = App (go bound l1) (go bound l2)
    go bound (Lam w a l) = Lam w (go (Set.insert w bound) a) (go (Set.insert w bound) l)
    go bound (Pi w a l) = Pi w (go (Set.insert w bound) a) (go (Set.insert w bound) l)
    go _ x = x

-- | Function arrow
infixr -->
(-->) :: LambdaCalculus -> LambdaCalculus -> LambdaCalculus
(-->) a b = Pi (renameVar "x" (fv b)) a b

-- | Substitute a free variable with an expression.
substFor :: Text -> LambdaCalculus -> LambdaCalculus
         -> LambdaCalculus
v `substFor` substitute = go Set.empty
  where
    go bound (Var w) | v == w = renameVars bound substitute
    go bound (App l1 l2) = App (go bound l1) (go bound l2)
    go bound (Lam w a l) | v /= w = Lam w (go (Set.insert w bound) a) (go (Set.insert w bound) l)
    go bound (Pi w a l) | v /= w = Pi w (go (Set.insert w bound) a) (go (Set.insert w bound) l)
    go _ x = x

betaReduce :: LambdaCalculus -> LambdaCalculus
betaReduce (App l1 l2) = case App (betaReduce l1)
                                  (betaReduce l2) of
  App (Lam v _ l1') l2' -> betaReduce $ (v `substFor` l2') l1'
  App (Pi v _ l1') l2' -> betaReduce $ (v `substFor` l2') l1'
  app -> app
betaReduce (Lam v a l) = Lam v (betaReduce a) (betaReduce l)
betaReduce (Pi v a l) = Pi v (betaReduce a) (betaReduce l)
betaReduce x = x

-- | The maximum amount of de-bruijn indices needed.
depth :: LambdaCalculus -> Int
depth (App t1 t2) = max (depth t1) (depth t2)
depth (Lam _ t1 t2) = max (depth t1) (depth t2) + 1
depth (Pi _ t1 t2) = max (depth t1) (depth t2) + 1
depth _ = 0

alphaReduce :: LambdaCalculus -> LambdaCalculus
alphaReduce t = go Map.empty (depth t) t
  where
    go m _ (Var x) = case Map.lookup x m of
      Nothing -> Var x
      Just i -> Var i
    go m n (App t1 t2) = App (go m n t1) (go m n t2)
    go m n (Lam x t1 t2) =
      let x' = Text.pack $ 'i':show n
          m' = Map.insert x x' m
          n' = n - 1
      in Lam x' (go m' n' t1) (go m' n' t2)
    go m n (Pi x t1 t2) =
      let x' = Text.pack $ 'i':show n
          m' = Map.insert x x' m
          n' = n - 1
      in Pi x' (go m' n' t1) (go m' n' t2)
    go _ _ x = x

etaReduce :: LambdaCalculus -> LambdaCalculus
etaReduce (Lam v a l) = case Lam v (etaReduce a)
                                   (etaReduce l) of
  lam@(Lam _ _ (App f (Var w))) ->
    if v == w && Prelude.not (Set.member v (fv f))
      then etaReduce f else lam
  lam@(Pi _ _ (App f (Var w))) ->
    if v == w && Prelude.not (Set.member v (fv f))
      then etaReduce f else lam
  lam -> lam
etaReduce (App l1 l2) = App (etaReduce l1) (etaReduce l2)
etaReduce (Pi v a l) = Pi v (etaReduce a) (etaReduce l)
etaReduce x = x

reduce :: LambdaCalculus -> LambdaCalculus
reduce = alphaReduce . betaReduce . etaReduce

-- | A context maps variable names to their types.
newtype Context = Context
  { fromContext :: Map Text LambdaCalculus
  } deriving (Eq, Ord, Show)

instance Semigroup Context where
  (Context m1) <> (Context m2) = Context (m1 <> m2)

instance Monoid Context where
  mempty = Context Map.empty

checked :: Text -> LambdaCalculus -> Context
checked x t = Context (Map.singleton x t)

data TypeMessage
  = UnboundVariable
  | AppOnNonFunction
  | LambdaCalculus `CantDependOn` LambdaCalculus
  | TypeMismatch LambdaCalculus LambdaCalculus
  | BoxAtValueLevel
  deriving (Eq, Ord, Show)

data TypeError = TypeError
  { context     :: Context
  , current     :: LambdaCalculus
  , typeMessage :: TypeMessage
  } deriving (Eq, Ord, Show)

type TypeTheory = LambdaCalculus -> LambdaCalculus -> Either TypeMessage LambdaCalculus

stlc :: TypeTheory
stlc Star Star = pure Star
stlc Star x = Left (x `CantDependOn` Star)
stlc x y = Left (y `CantDependOn` x)

typeCheck :: TypeTheory -> Context -> LambdaCalculus -> Either TypeError LambdaCalculus
typeCheck rule = go
  where
    go ctx e = case e of
      Star -> pure Box
      Box  -> Left $ TypeError ctx Box BoxAtValueLevel
      Var x -> case Map.lookup x (fromContext ctx) of
        Nothing -> Left (TypeError ctx e UnboundVariable)
        Just a  -> pure a
      Lam x _A t -> do
        go ctx _A
        _B <- go ((checked x _A) <> ctx) t
        let p = Pi x _A _B
        go ctx p
        pure p
      Pi  x _A _B -> do
        s <- go ctx _A
        t <- go ((checked x _A) <> ctx) _B
        case rule s t of
          Right r -> Right r
          Left tm -> Left (TypeError ctx e tm)
      App f a -> do
        e' <- go ctx f
        (x, _A, _B) <- case e' of
          Pi x _A _B -> pure (x, _A, _B)
          _          -> Left (TypeError ctx e AppOnNonFunction)
        _A' <- go ctx a
        if reduce _A == reduce _A'
          then pure $ (x `substFor` a) _B
          else Left (TypeError ctx e (TypeMismatch _A _A'))

systemFomega :: TypeTheory
systemFomega Star Star = pure Star
systemFomega Star x = Left (x `CantDependOn` Star)
systemFomega Box x = case x of
  Star -> pure Star
  Box -> pure Box
  _ -> Left (x `CantDependOn` Box)
systemFomega x y = Left (y `CantDependOn` x)

coc :: TypeTheory
coc Star x = case x of
  Star -> pure Star
  Box -> pure Box
  _ -> Left (x `CantDependOn` Star)
coc Box x = case x of
  Star -> pure Star
  Box -> pure Box
  _ -> Left (x `CantDependOn` Box)
coc x y = Left (x `CantDependOn` y)

eval :: LambdaCalculus -> Either TypeError LambdaCalculus
eval t = case typeCheck coc mempty t of
  Left l -> Left l
  Right r -> Right $ reduce r