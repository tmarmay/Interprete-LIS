module Test where
import Interprete

-- **Ejemplo 1**
-- while x < 10 do
--   !x ;
--   x := x + 1
-- od

ej1 :: Expr Ω
ej1 = While (Lt (V "x") (CInt 10)) $
            Seq (Output $ V "x")
                (Assign "x" (Plus (V "x") (CInt 1)))

eval_ej1 :: IO ()
eval_ej1 = eval ej1 (\_ -> 0)

-- **Ejemplo 2**
-- while y < 10 do
--   ?x ;
--   !x ;
--   !y ;
--   y := y + 1
-- od

ej2 :: Expr Ω
ej2 = While (Lt (V "y") (CInt 10)) $
            Seq (Seq (Seq (Input "x")
                          (Output $ V "x")
                     )
                     (Output $ V "y")
                )
                (Assign "y" (Plus (V "y") (CInt 1)))

eval_ej2 :: IO ()
eval_ej2 = eval ej2 (\_ -> 0)

-- **Ejemplo 3**
-- ?x ;
-- Newvar x := 10 in !x end ;
-- !x

ej3 :: Expr Ω
ej3 = Seq (Seq (Input "x")
               (Newvar "x" (CInt 10)
                       (Output $ V "x")
               )
          )
          (Output $ V "x")

eval_ej3 :: IO ()
eval_ej3 = eval ej3 (\_ -> 0)


-- **Ejemplo 4**
-- ?x ;
-- SKIP ;
-- !x

ej4 :: Expr Ω
ej4 = Seq (Seq (Input "x")
               (Skip)
          )
          (Output $ V "x")

eval_ej4 :: IO ()
eval_ej4 = eval ej4 (\_ -> 0)

-- **Ejemplo 5**
-- ?x ;
-- If x != 5 then x=1 else x=0
-- !x

ej5 :: Expr Ω
ej5 = Seq 
        (Seq (Input "x") 
          (If 
            (Neq (V "x") (CInt 5))
            (Assign "x" (CInt 1)) 
            (Assign "x" (CInt 0))
          )
        )
        (Output $ V "x")

eval_ej5 :: IO ()
eval_ej5 = eval ej5 (\_ -> 0)


