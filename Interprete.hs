{-# LANGUAGE GADTs, FlexibleInstances #-}

module Interprete (Expr(..), Var, Σ, Ω, DomSem, eval) where

import Control.Applicative ( liftA, liftA2 )

--         ∞
-- fix f = ⊔ fⁱ ⊥
--        i=0
fix :: (a -> a) -> a
fix f = f (fix f)

type Var = String
type Σ   = Var -> Int

{- Dominios semánticos -}
type MInt  = Maybe Int  -- { n | (n = Just m, m ∈ Int)    ∨ (n = Nothing) }
type MBool = Maybe Bool -- { b | (b = Just b', b' ∈ Bool) ∨ (b = Nothing) }

{- Ω ≈ (Σ' + Z × Ω + Z → Ω)⊥ -}
data Ω = Normal Σ | Abort Σ | Out (Int, Ω) | In (Int -> Ω)
{- Notar:
   * Normal : Σ → Ω
   * Abort  : Σ → Ω
   * Out    : (Z, Ω) → Ω
   * In     : (Z → Ω) → Ω
-}

update :: Σ -> Var -> Int -> Σ
update σ v n v' = if v == v' then n else σ v'

data Expr a where
  -- # Expresiones enteras
  CInt :: Int       -> Expr MInt                -- n
  V    :: Var       -> Expr MInt                -- v
  Plus :: Expr MInt -> Expr MInt -> Expr MInt   -- e + e'
  Dif   :: Expr MInt -> Expr MInt -> Expr MInt     -- e - e'
  Times :: Expr MInt -> Expr MInt -> Expr MInt     -- e * e'
  Div   :: Expr MInt -> Expr MInt -> Expr MInt     -- e / e' (división entera)

  -- # Expresiones booleanas
  Eq   :: Expr MInt  -> Expr MInt  -> Expr MBool -- e = e'
  Lt   :: Expr MInt  -> Expr MInt  -> Expr MBool -- e < e'
  Neq  :: Expr MInt  -> Expr MInt  -> Expr MBool -- e /= e'
  And  :: Expr MBool -> Expr MBool -> Expr MBool -- b && b'
  Or   :: Expr MBool -> Expr MBool -> Expr MBool -- b || b'
  Not  :: Expr MBool               -> Expr MBool -- ¬b

  -- # Comandos LIS
  Skip   :: Expr Ω                                -- skip
  Assign :: Var -> Expr MInt  -> Expr Ω    -- v := e
  Seq    :: Expr Ω  -> Expr Ω    -> Expr Ω    -- c ; c'
  If     :: Expr MBool -> Expr Ω   -> Expr Ω -> Expr Ω -- if b then c else c'
  Newvar :: Var -> Expr MInt -> Expr Ω -> Expr Ω-- newvar v := e in e'
  While  :: Expr MBool -> Expr Ω -> Expr Ω -- while b do c

  -- # Comandos Fallas
  Fail   :: Expr Ω-- fail
  Catch  :: Expr Ω -> Expr Ω -> Expr Ω  -- catch c with c'

  -- # Comandos IO
  Output  :: Expr MInt -> Expr Ω    -- !e
  Input   :: Var -> Expr Ω          -- ?v

class DomSem dom where
  sem :: Expr dom -> Σ -> dom

instance DomSem MInt where
  sem (CInt a) σ      =  Just a
  sem (V v) σ         = Just (σ v)
  sem (Plus e1 e2) σ  = (-^-) (+) (sem e1 σ) (sem e2 σ)
  sem (Dif e1 e2) σ   = (-^-) (-) (sem e1 σ) (sem e2 σ)
  sem (Times e1 e2) σ = (-^-) (*) (sem e1 σ) (sem e2 σ)
  sem (Div e1 e2) σ   =
    case (sem e1 σ, sem e2 σ) of
      (Just v1, Just 0) -> Nothing
      (Just v1, Just v2) -> Just (v1 `div` v2)
      _                  -> Nothing

instance DomSem MBool where
  sem (Eq e1 e2) σ  = (-^-) (==) (sem e1 σ) (sem e2 σ)
  sem (Lt e1 e2) σ  = (-^-) (<) (sem e1 σ) (sem e2 σ)
  sem (Neq e1 e2) σ = (-^-) (/=) (sem e1 σ) (sem e2 σ)
  sem (And b1 b2) σ = (-^-) (&&) (sem b1 σ) (sem b2 σ)
  sem (Or b1 b2) σ  = (-^-) (||) (sem b1 σ) (sem b2 σ)
  sem (Not b) σ     = (-^) not (sem b σ)

instance DomSem Ω where
  sem Skip σ = Normal σ
  sem Fail σ = Abort σ
  sem (Assign v e) σ = (>>==) (sem e σ, σ) (Normal . update σ v)
  sem (If b c1 c2) σ = (sem b σ, σ) >>== \cond ->
    if cond then sem c1 σ else sem c2 σ
  sem (Seq c1 c2) σ = (*.) (sem c2) (sem c1 σ)
  sem (Catch c1 c2) σ = (+.) (sem c2) (sem c1 σ) --Dual a (Seq c1 c2)
  sem (While b c) σ = fix f σ
    where
      f g σ =
        (sem b σ, σ) >>== \bool -> --si es Notging devuelve Abort σ
                if bool
                  then g *. sem c σ    -- Ejecutar c y luego el resto del bucle
                  else Normal σ      -- Terminar normalmente si la guarda es False

  sem (Newvar v e c) σ =
    (sem e σ, σ) >>== \n ->
      let σ' = update σ v n
          ω  = sem c σ'
      in restore v σ ω

  sem (Input v) σ = In (Normal . update σ v)
  sem (Output e) σ = (sem e σ, σ) >>== \n -> Out (n, Normal σ)

restore :: Var -> Σ -> Ω -> Ω
restore v σ = (†.) (\σ' -> update σ' v (σ v))

(>>==) :: (Maybe a, Σ) -> (a -> Ω) -> Ω
(>>==) (Nothing, σ) _ = Abort σ
(>>==) (Just n, _)  f = f n

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ)  = f σ
(*.) _ (Abort σ)   = Abort σ
(*.) f (Out (n,ω)) = Out (n, f *. ω)
(*.) f (In g)      = In ((f *.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ)  = Normal $ f σ
(†.) f (Abort σ)   = Abort $ f σ
(†.) f (Out (n,ω)) = Out (n, f †. ω)
(†.) f (In g)      = In ((f †.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ)  = Normal σ
(+.) f (Abort σ)   = f σ
(+.) f (Out (n,ω)) = Out (n, f +. ω)
(+.) f (In g)      = In ((f +.) . g)

{- Funciones de evaluación de dom -}

class Eval dom where
  eval :: Expr dom -> Σ -> IO ()

instance Eval MInt where
  eval e = print . sem e

instance Eval MBool where
  eval e = print . sem e

instance Eval Ω where
  eval e = unrollOmega . sem e
    where unrollOmega :: Ω -> IO ()
          unrollOmega (Normal _)   = return ()
          unrollOmega (Abort _)    = putStrLn "Abort"
          unrollOmega (Out (n, ω)) = print n >> unrollOmega ω
          unrollOmega (In f)       = getLine >>= unrollOmega . f . read

{- Funciones auxiliares -}
(-^-) :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
(-^-) = liftA2

(-^) :: (a -> b) -> (Maybe a -> Maybe b)
(-^) = fmap
