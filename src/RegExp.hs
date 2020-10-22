{-# LANGUAGE DeriveGeneric #-}

module RegExp where

import GHC.Generics (Generic)

data RE
    = Empty
    | Const Char
    | Paren RE
    | Alt RE
          RE
    | Concat RE
             RE
    | Star RE
    deriving (Eq, Generic, Ord, Read)

instance Show RE where
    show (Const c) = [c]
    show (Paren r) = "(" <> show r <> ")"
    show (Alt r1 r2) = show r1 <> "|" <> show r2
    show (Concat r1 r2) = show r1 <> show r2
    show (Star r) = show r <> "*"
    show Empty = ""

-- | quick & dirty RE parser
parseRE :: String -> RE
parseRE s = simplify $ parseRE' Empty s

parseRE' :: RE -> String -> RE
parseRE' r [] = r
parseRE' Empty [c]
    | c `elem` opSymbols = error "Invalid RE"
    | otherwise = Const c
parseRE' r (c:cs) =
    case c of
        '|' -> parseRE' (Alt r $ parseRE cs) ""
        '(' ->
            let (p, rest) = parseParen 1 "" cs
             in case simplify p of
                    Empty -> error "Invalid RE"
                    _ -> parseRE' (Concat r p) rest
        ')' -> error "Unbalanced parentheses"
        _ ->
            case cs of
                '*':cs' -> parseRE' (Concat r $ Star $ Const c) cs'
                _ -> parseRE' (Concat r $ Const c) cs

parseParen :: Int -> String -> String -> (RE, String)
parseParen 0 s rest = (Paren $ parseRE $ init s, rest)
parseParen n s (c:cs)
    | c == '(' = parseParen (n + 1) (s <> [c]) cs
    | c == ')' = parseParen (n - 1) (s <> [c]) cs
    | otherwise = parseParen n (s <> [c]) cs
parseParen _ _ "" = error "Unbalanced parentheses"

simplify :: RE -> RE
simplify Empty = Empty
simplify (Const c) = Const c
simplify (Concat Empty r) = simplify r
simplify (Concat r Empty) = simplify r
simplify (Concat r1 r2) = Concat (simplify r1) $ simplify r2
simplify (Alt r1 r2) = Alt (simplify r1) $ simplify r2
simplify (Star r) = Star $ simplify r
simplify (Paren Empty) = Empty
simplify (Paren r) = simplify r

-- utils
opSymbols :: String
opSymbols = "()|*"
