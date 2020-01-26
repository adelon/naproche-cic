module Parse.Instruction where


import Base.Parser
import Parse.Expression (Expr(Hole))
import Parse.Token (word, comma, indefinite)
import Parse.Var (var)


fixVars :: Parser ()
fixVars = do
   optional (word "in" *> word "this" *> word "section" *> comma)
   word "let"
   v <- var
   word "denote" *> optional indefinite *> pure () -- TODO parse nominal types.
   let ty = Hole
   fixVar v ty
