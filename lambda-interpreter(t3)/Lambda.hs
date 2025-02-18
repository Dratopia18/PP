module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
-- vars primeste o expresie lambda si returneaza variabilele
vars :: Lambda -> [String]
vars (Var x) = [x] -- daca e Var, atunci x e variabila
vars (App e1 e2) = vars e1 ++ vars e2 -- daca e aplicatie, atunci variabilele sunt cele din e1 si e2
vars (Abs x e) = if x `elem` vars e then vars e else x : vars e -- daca e abstractie, atunci daca x e in vars(e) atunci vars(e) sunt variabilele din e, altfel x si vars(e)
vars (Macro _) = [] -- daca e Macro, atunci nu are variabile

-- 1.2.
-- freeVars primeste o expresie lambda si returneaza variabilele libere
freeVars :: Lambda -> [String]
freeVars (Var x) = [x] -- daca e Var, atunci x e variabila libera
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2 -- daca e aplicatie, atunci variabilele libere sunt cele din e1 si e2 fara duplicate
freeVars (Abs x e) = freeVars e \\ [x] -- daca e abstractie, atunci variabilele libere sunt cele din e fara x
freeVars (Macro _) = [] -- daca e Macro, atunci nu are variabile libere

-- 1.3.
-- newVar primeste o lista de variabile si returneaza o variabila noua
newVar :: [String] -> String
newVar xs = head $ filter (`notElem` xs) vars -- prima variabila care nu e in xs
  where 
    vars = [x : y | y <- "" : vars, x <- ['a'..'z']] -- toate variabilele de forma [a-z]*

-- 1.4.
-- isNormalForm verifica daca o expresie lambda e in forma normala
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True -- daca e Var, atunci e in forma normala
isNormalForm (Abs _ e) = isNormalForm e -- daca e abstractie, atunci e in forma normala daca e in forma normala variabila e
isNormalForm (App (Abs _ _) _) = False -- daca e aplicatie cu o abstractie, atunci nu e in forma normala
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2 -- daca e aplicatie cu 2 variabile e1 si e2, atunci e in forma normala daca e1 si e2 sunt in forma normala
isNormalForm (Macro _) = True -- daca e Macro, atunci e in forma normala

-- 1.5.
-- reduce primeste o variabila x si 2 expresii lambda si returneaza expresia lambda obtinuta prin inlocuirea tuturor aparitiilor lui x din prima expresie cu a doua expresie
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = case e1 of 
    Var y | y == x -> e2  -- daca e Var si y == x, atunci e2
    Var y -> Var y -- altfel, e y
    App e1' e1'' -> App (reduce x e1' e2) (reduce x e1'' e2) -- daca e aplicatie, atunci aplic reducerea pe e1' si e1''
    Abs y e1' | y == x -> Abs y e1' -- daca e abstractie si y == x, atunci e Abs y e1'
              | y `elem` freeVars e2 -> let z = newVar (freeVars e1' ++ freeVars e2) -- altfel, z e o variabila noua care nu e in freeVars(e1') si freeVars(e2)
                                        in Abs z (reduce x (reduce y e1' (Var z)) e2) -- unde e1' e inlocuita cu z, iar z e inlocuita cu e2
    Abs y e1' -> Abs y (reduce x e1' e2) -- altfel, facem reducerea pe e1' si e2
    Macro y -> Macro y -- daca e Macro, atunci nu facem nimic

-- 1.6.
-- normalStep primeste o expresie lambda si returneaza expresia lambda obtinuta prin aplicarea unei singure reducere
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2 -- daca avem o aplicatie dintre o abstractie cu 2 variabile si e2, atunci facem reducerea
normalStep (App e1 e2) -- daca avem o aplicatie cu 2 variabile e1 si e2
    | isNormalForm e1 = App e1 (normalStep e2) -- daca e1 e in forma normala, atunci aplicam reducerea pe e2
    | otherwise = App (normalStep e1) e2 -- altfel, aplicam reducerea pe e1
normalStep (Abs x e) = Abs x (normalStep e) -- daca e abstractie, aplicam reducerea pe e
normalStep e = e -- altfel, nu facem nimic

-- 1.7.
-- applicativeStep primeste o expresie lambda si returneaza expresia lambda obtinuta prin aplicarea unei singure reducere in mod aplicativ
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1) e2) -- daca avem o aplicatie dintre o abstractie cu 2 variabile si e2
    | isNormalForm e2 = reduce x e1 e2 -- daca e2 e in forma normala, atunci facem reducerea in mod normal
    | otherwise = App (Abs x e1) (applicativeStep e2) -- altfel, aplicam reducerea aplicativa pe e2
applicativeStep (App e1 e2) -- daca avem o aplicatie cu 2 variabile e1 si e2
    | isNormalForm e1 = App e1 (applicativeStep e2) -- daca e1 e in forma normala, atunci aplicam reducerea pe e2
    | otherwise = App (applicativeStep e1) e2 -- altfel, aplicam reducerea pe e1
applicativeStep (Abs x e1) = Abs x (applicativeStep e1) -- daca e abstractie, aplicam reducerea pe e1
applicativeStep e = e -- altfel, nu facem nimic

-- 1.8.
-- simplify primeste o functie de reducere si o expresie lambda si returneaza lista de expresii lambda obtinute prin aplicarea repetata a functiei de reducere
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step expr -- luam expr si aplicam reducerea pe ea
    | expr == next = [expr] -- zicem ca daca expr e egala cu next, atunci lista de expresii e [expr]
    | otherwise = expr : simplify step next -- altfel, adaugam expr la lista de expresii si aplicam reducerea pe next
  where
    next = step expr -- next e expr dupa ce aplicam reducerea pe ea

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
