{-# OPTIONS_GHC -fno-warn-orphans #-}

module RegExp
    ( RE(..)
    , parseRE
    , size
    ) where

import GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

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

-- | quick & dirty RE parser
parseRE :: String -> RE
parseRE s = simplify $ parseRE' Empty s
  where
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
                        _ ->
                            case rest of
                                '*':_ ->
                                    parseRE' (Concat r (Star p)) $
                                    dropStars rest
                                _ -> parseRE' (Concat r p) rest
            ')' -> error "Unbalanced parentheses"
            _ ->
                case cs of
                    '*':_ -> parseRE' (Concat r $ Star $ Const c) $ dropStars cs
                    _ -> parseRE' (Concat r $ Const c) cs
    parseParen :: Int -> String -> String -> (RE, String)
    parseParen 0 s' rest = (Paren $ parseRE $ init s', rest)
    parseParen n s' (c:cs)
        | c == '(' = parseParen (n + 1) (s' <> [c]) cs
        | c == ')' = parseParen (n - 1) (s' <> [c]) cs
        | otherwise = parseParen n (s' <> [c]) cs
    parseParen _ _ "" = error "Unbalanced parentheses"
    dropStars = dropWhile (== '*')

-- | deletes extraneous pieces of generated RE ASTs
simplify :: RE -> RE
simplify (Concat Empty r) = simplify r
simplify (Concat r Empty) = simplify r
simplify (Concat (Paren (Concat r1 r2)) r3) = Concat (Concat r1 r2) r3
simplify (Concat r1 (Paren (Concat r2 r3))) = Concat (Concat r1 r2) r3
simplify (Concat r1 r2) = Concat (simplify r1) $ simplify r2
simplify (Alt Empty r) = simplify r
simplify (Alt r Empty) = simplify r
simplify (Alt (Paren r1) r2) = Alt (simplify r1) $ simplify r2
simplify (Alt r1 (Paren r2)) = Alt (simplify r1) $ simplify r2
simplify (Alt r1 r2) = Alt (simplify r1) $ simplify r2
simplify (Star Empty) = Empty
simplify (Star (Star r)) = simplify $ Star r
simplify (Star (Paren (Star r))) = simplify $ Star r
simplify (Star r) = Star $ simplify r
simplify (Paren Empty) = Empty
simplify (Paren (Const c)) = Const c
simplify (Paren (Paren r)) = simplify $ Paren r
simplify (Paren (Star r)) = Star $ simplify r
simplify (Paren r) = Paren $ simplify r
simplify r = r

-- utils
opSymbols :: String
opSymbols = "()|*"

size :: RE -> Int
size Empty = 0
size (Const _) = 1
size (Concat r1 r2) = size r1 + size r2
size (Alt r1 r2) = size r1 + size r2 + 1
size (Star r) = size r + 1
size (Paren r) = size r

instance Show RE where
    show (Const c) = [c]
    show (Paren r) = "(" <> show (simplify r) <> ")"
    show (Alt r1 r2) = show (simplify r1) <> "|" <> show (simplify r2)
    show (Concat r1 r2) = show (simplify r1) <> show (simplify r2)
    show (Star (Const c)) = [c] <> "*"
    show (Star (Paren r)) = show (simplify r) <> "*"
    show (Star r) = "(" <> show (simplify r) <> ")*"
    show Empty = ""

instance QC.Arbitrary RE where
    arbitrary = do
        s <- genSize 20
        arbitrary' s Empty
      where
        arbitrary' s r =
            if size r <= s
                then do
                    n <- genNum
                    case n of
                        0 -> pure $ simplify r
                        1 -> do
                            c <- genChar
                            arbitrary' s $ simplify $ Concat r $ Const c
                        2 -> do
                            s' <- genSize $ s - size r
                            r' <- arbitrary' s' Empty
                            arbitrary' s $ simplify $ Concat r r'
                        3 -> do
                            s' <- genSize $ s - size r
                            r' <- arbitrary' s' Empty
                            arbitrary' s $ simplify $ Alt r r'
                        4 -> arbitrary' s $ simplify $ Star r
                        5 -> arbitrary' s $ simplify $ Paren r
                        _ -> error "unreachable"
                else pure $ simplify r
    shrink Empty = [Empty]
    shrink (Const _) = [Empty]
    shrink (Concat r1 r2) = [r1, r2]
    shrink (Alt r1 r2) = [r1, r2]
    shrink (Star r) = [r]
    shrink (Paren r) = [r]

genChar :: QC.Gen Char
genChar = QC.choose ('a', 'z')

genSize :: Int -> QC.Gen Int
genSize s = QC.choose (0, s)

genNum :: QC.Gen Int
genNum = QC.choose (0, 5)
