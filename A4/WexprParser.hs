module WexprParser where

import ParserLib
import WexprDef
import Distribution.Types.SourceRepo (knownRepoTypes)

wexpr :: Parser Wexpr
wexpr = whitespaces *> mainexpr <* eof
  where
    mainexpr = expr
            >>= \e -> (keyword "where"
                        >> def e
                        >>= \d -> return d
                      )
            <|> return e

    def inp = var
            >>= \(Var v) -> operator "="
            >> expr
            >>= \e -> (keyword "and"
                        >> def inp
                        >>= \(Where inpr dr) -> return (Where inp ((v, e) : dr))
                      )
            <|> return (Where inp [(v, e)])

    expr = op <|> var

    op = plus

    var = identifier ["where", "and"]
            >>= \v -> return (Var v)

    nat = natural
            >>= \n -> return (Nat n)

    plus = chainl1 mins (operator "+" *> pure (Plus))
    mins = chainl1 muls (operator "-" *> pure (Minus))
    muls = chainl1 neg (operator "*" *> pure (Times))
    pows = chainr1 atom (operator "^" *> pure (Pow))
    atom = nat <|> (openParen *> mainexpr <* closeParen)

    neg = (operator "-"
                        >> expr
                        >>= \e -> return (Neg e)
          )
          <|> pows



(<|>) :: Parser Wexpr -> Parser Wexpr -> Parser Wexpr
MkParser sf1 <|> p2 = MkParser g
    where
        g inp = case sf1 inp of
            Nothing -> unParser p2 inp
            j -> j
