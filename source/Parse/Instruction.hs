module Parse.Instruction where


import Base.Parser
import Parse.Expression (Expr(Hole))
import Parse.Token (word, words, comma, indefinite)
import Parse.Var (var)


fixVars :: Parser ()
fixVars = do
   thisSection
   word "let"
   v <- var
   word "denote" *> optional indefinite *> pure () -- TODO parse nominal types.
   let ty = Hole
   fixVar v ty

-- Importing of a separate module.
using :: Parser ()
using = do
   thisSection
   words ["we", "use", "the"]
   word "definitions" <|> word "notation" <|> word "conventions"
   word "of"
   ref <- moduleRef
   importModule ref
   where
      moduleRef = error "Instruction.moduleRef undefined"
      importModule = error "Instruction.importModule undefined"

thisSection :: Parser ()
thisSection = void $ optional do
   word "in" <|> word "throughout"
   words ["this", "section"]
   optional comma
