{-# OPTIONS_GHC -fno-warn-orphans #-}

module RegExp
    ( RE(..)
    , parseRE
    , size
    , simplify
    , recognize
    , recognizeRE
    , recognizeSample
    , recognizeSampleRE
    ) where

import GHC.Generics (Generic)
import qualified Test.QuickCheck as QC

-- TODO: recognize
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
-- makes Concat right associative for recognition
simplify :: RE -> RE
simplify (Concat Empty r) = simplify r
simplify (Concat r Empty) = simplify r
simplify (Concat (Concat r1 r2) r3) = simplify $ Concat r1 $ Concat r2 r3
simplify (Concat (Paren (Concat r1 r2)) r3) =
    simplify $ Concat r1 $ Concat r2 r3
simplify (Concat r1 (Paren (Concat r2 r3))) =
    simplify $ Concat r1 $ Concat r2 r3
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

recognize :: String -> String -> Bool
recognize r = recognizeRE $ parseRE r

recognizeRE :: RE -> String -> Bool
recognizeRE Empty s = s == ""
recognizeRE (Const c) s = s == [c]
recognizeRE (Concat r1 r2) s =
    any (\(a, b) -> recognizeRE r1 a && recognizeRE r2 b) $ slices s
recognizeRE (Alt r1 r2) s = recognizeRE r1 s || recognizeRE r2 s
recognizeRE (Paren r) s = recognizeRE r s
recognizeRE (Star _) "" = True
recognizeRE r@(Star r') s =
    recognizeRE r' s ||
    any (\(a, b) -> recognizeRE r a && recognizeRE r b) (nontrivialSlices s)

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

slices :: String -> [(String, String)]
slices s = [splitAt n s | n <- [0 .. (length s)]]

nontrivialSlices :: String -> [(String, String)]
nontrivialSlices s = filter (\(a, b) -> a /= "" && b /= "") $ slices s

recognizeSample :: String -> [String]
recognizeSample = take 10 . recognizeSampleRE . parseRE

recognizeSampleRE :: RE -> [String]
recognizeSampleRE Empty = []
recognizeSampleRE (Const c) = [[c]]
recognizeSampleRE (Concat r1 r2) =
    take 10 $
    concatMap (\s -> map (<> s) $ recognizeSampleRE r1) (recognizeSampleRE r2)
recognizeSampleRE (Alt r1 r2) =
    take 10 (recognizeSampleRE r1) <> take 10 (recognizeSampleRE r2)
recognizeSampleRE (Paren r) = recognizeSampleRE r
recognizeSampleRE (Star r) =
    let samp = recognizeSampleRE r
        join = concatMap (\s -> map (<> s) samp)
        m1 = join samp
        m2 = join m1
        m3 = join m2
        m4 = join m3
     in take 10 $ [""] <> samp <> m1 <> m2 <> m3 <> m4

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
        s <- genSize 10
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
