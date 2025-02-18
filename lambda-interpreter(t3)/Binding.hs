module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
-- simplifyCtx ia un context si o expresie lambda si returneaza o lista de expresii lambda obtinute prin simplificarea expresiei lambda cu ajutorul contextului
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx context step expr = case expr of -- verificam cazurile posibile
    Macro x -> case lookup x context of -- daca e Macro, atunci verificam daca x e in context
        Just e  -> simplifyCtx context step e -- daca e in context, atunci simplificam e
        Nothing -> Left $ "Macro " ++ x ++ " not found in context" -- altfel, returnam eroare
    App (Macro x) e2 -> case lookup x context of -- daca e aplicatie cu un Macro si o expresie e2, atunci verificam daca x e in context
        Just e  -> do -- daca e in context, atunci
            simplifiedY <- simplifyCtx context step e2 -- simplifiedY e simplificarea lui e2
            simplifiedE <- simplifyCtx context step e -- simplifiedE e simplificarea lui e
            Right $ simplify step (App (head simplifiedE) (head simplifiedY)) -- returnam simplificarea aplicatiei
        Nothing -> Left $ "Macro " ++ x ++ " not found in context" -- altfel, returnam eroare
    _ -> Right $ simplify step expr -- altfel, returnam simplificarea expr

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
