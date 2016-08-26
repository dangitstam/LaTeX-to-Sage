{-  
    Tam Dang
    8.06.16
    Practice-it: Math's LaTeX to sagemath syntax converter.
    Breaks down a LaTeX AST and translates it to sagemath.
-}

module Sage where

    import Text.LaTeX
    import Text.LaTeX.Base.Parser
    import Text.LaTeX.Base.Syntax
    import Data.Char
    import Data.Text (unlines)
    import qualified Data.Text as T
    import qualified Data.String as S
    import qualified Data.Map.Strict as Map
    import System.Environment

    -- Commonly used values for building sagemath.
    comma :: Text
    comma = T.pack ","
    leftParen :: Text
    leftParen = T.pack "("
    rightParen :: Text
    rightParen = T.pack ")"
    leftBracket :: Text
    leftBracket = T.pack "["
    rightBracket :: Text
    rightBracket = T.pack "]"
    caret :: Text
    caret = T.pack "^"
    forwardSlash :: Text
    forwardSlash = T.pack "/"
    one :: Text
    one = T.pack "1"
    asterik :: Text
    asterik = T.pack "*"

    {- sageCommandMap(x) maps LaTeX commands to appropriate converter.
       commandLookUp(x) queries and selects appropriate function.
       One set for single arg functions, another for multi-arg functions. -}
    sageCommandMap :: Map.Map [Char] (LaTeX -> Text)
    sageCommandMap = Map.fromList([("log", sageLog), ("int", sageIntegral)])

    sageCommandMapArgs :: Map.Map [Char] ([TeXArg] -> LaTeX -> Text)
    sageCommandMapArgs = Map.fromList([("sqrt", sageRoot), ("frac", sageFrac), ("bmatrix", sageMatrix)])

    commandLookUp :: String -> LaTeX -> Text
    commandLookUp s latex = do
                                let sageCommand = Map.lookup s sageCommandMap
                                case sageCommand of
                                    Just command -> command latex
                                    Nothing -> T.concat[T.pack s, createSage latex]

    commandLookUpArgs :: String -> [TeXArg] -> LaTeX -> Text
    commandLookUpArgs s args latex = do
                                         let sageCommand = Map.lookup s sageCommandMapArgs
                                         case sageCommand of
                                             Just command -> command args latex
                                             Nothing -> createSage latex


    {- 
       No implicit multiplication implemented in sagemath, explicit asterik required.
       Inserts asterik between alphas and nums or right paren and alpha/nums if not already there.
     -}
    insertAsterik :: Text -> Text
    insertAsterik txt = case T.uncons txt of
        -- Check two consecutive characters to see if there should be multiplication
        Just (c, txt') -> case T.uncons txt' of
                              Just (c2, txt'') -> do
                                                      let text_c = T.singleton c
                                                      let text_c2 = T.singleton c2
                                                      if (insertAsterikHelper (c, c2))
                                                      {- insert and continue, 
                                                         otherwise skip current and continue -}
                                                      then T.concat [text_c, asterik, 
                                                             insertAsterik (T.concat [text_c2, txt''])]
                                                      else T.concat [text_c, insertAsterik txt']
                              Nothing          -> T.singleton c -- txt' is empty
        Nothing        ->  txt


    {-
       Takes a tuple of Chars and returns a boolean if there is a case of 
       alpha * num, num * alpha, num * (, ) * (, or ) * alpha/num (correct cases for multiplication).
    -}
    insertAsterikHelper :: (Char, Char) -> Bool
    insertAsterikHelper (c1, c2) = ((((isAlpha(c1) && isDigit(c2)) || (isDigit(c1) && isAlpha(c2))) || 
                                     (((c1 == ')') && isAlphaNum(c2)) || ((c2 == '(') && isDigit(c1)))) ||
                                        ((c1 == ')') && (c2 == '(')))




    -- Converts LaTeX blocks in sagemath Text equivalents.
    createSage :: LaTeX -> Text
    createSage l = case l of
        TeXRaw s     -> s
        -- \, command in latex is a space
        TeXCommS s   -> if (s /= ",") 
                        then (T.pack s)
                        else (T.pack " ")
        TeXComm s args -> commandLookUpArgs s args TeXEmpty
        TeXSeq l1 l2 -> T.concat [createSage l1, createSage l2]
        TeXEnv s args latex -> commandLookUpArgs s args latex
        -- Processes not meant to be separate and contiguous are assumed to be wrapped in braces
        TeXBraces (TeXSeq l1 l2) -> case l1 of
                                        TeXCommS s     -> commandLookUp s l2
                                        TeXComm s args -> commandLookUpArgs s args l2       
                                        _              -> T.concat[createSage l1, createSage l2]
        TeXBraces latex -> createSage latex
        TeXLineBreak _ _ -> T.pack "linebreak" -- for splitting purposes
        TeXEmpty -> T.pack ""
        _            -> T.pack " well shit " -- marks where something went wrong


    {- Converts LaTeX fractions to sagemath. 
       If passed non-null LaTeX block, delegates to sageDerive. -}
    sageFrac :: [TeXArg] -> LaTeX -> Text
    sageFrac args latex = case args of
        [FixArg l1, FixArg l2] -> let eval_l1 = createSage l1
                                      eval_l2 = createSage l2
                                      eval_latex = createSage latex
                                  -- delegate if fraction is differential operator
                                  in if ((T.unpack eval_l1) == "d") 
                                     then sageDerive args latex
                                     else T.concat [leftParen, leftParen, eval_l1, 
                                               rightParen, forwardSlash, leftParen, 
                                               eval_l2, rightParen, rightParen, eval_latex]
        _       -> sageDerive args latex



    {- Converts LaTeX derivative operator (fraction) and expression
       into single-variable sagemath differentiation. 
       Handles differentiation for >= 1 times. -}
    sageDerive :: [TeXArg] -> LaTeX -> Text
    sageDerive [FixArg d1, FixArg d2] latex = do
        let eval_d1 = createSage d1
            eval_d2 = createSage d2
            expression = createSage latex
            pack_d = T.pack "d"
        if (T.take 1 eval_d1 == pack_d && T.take 1 eval_d2 == pack_d)
        then let variable   = T.takeEnd 1 eval_d2
                 diff       = T.pack "diff"
                 differentiation = T.concat [diff, leftParen, expression, 
                                             comma, variable, comma, degree, rightParen]
                     where degree = let poss_degree = T.drop 2 eval_d1 -- ex. d^2
                                    -- assumes degree of differentiation in num/denom are equal
                                    in if poss_degree == T.pack ""
                                       then one
                                       else poss_degree
             in differentiation
        -- resolve fractions that are not differential operators that somehow got here
        else T.concat [eval_d1, forwardSlash, eval_d2, expression]


    {- Converts LaTeX definite/indefinite integrals to sagemath.
       Assumes argument given is body after integral sign. -}
    sageIntegral :: LaTeX -> Text
    sageIntegral l2 = do
        let integral = T.pack "integral"
        let integrand = (\x -> T.dropEnd 2 (T.strip x))
        let differential = (\x -> T.takeEnd 1 x)
        case l2 of
            -- definite case
            TeXSeq (TeXRaw hyphen) 
                   (TeXSeq (TeXBraces a) 
                           (TeXSeq (TeXRaw caret) 
                                   (TeXSeq (TeXBraces b) 
                                           c))) -> do
                                                       let eval_c = createSage c
                                                       T.concat [integral, leftParen, 
                                                                 integrand eval_c, 
                                                                 comma, differential eval_c, 
                                                                 comma,  createSage a, 
                                                                 comma, createSage b, rightParen]
            -- indefinite case, latex will be integrand + differential
            latex -> let eval_latex = createSage latex
                     in T.concat[integral, leftParen, integrand eval_latex, 
                                 comma, differential eval_latex, rightParen]


    {- Converts LaTeX logarithms to sagemath.
       Assumes argument has non-natural base that is wrapped in braces 
       and prefixed with underscore. -}
    sageLog :: LaTeX -> Text
    sageLog (TeXSeq (TeXRaw underscore) (TeXSeq (TeXBraces (TeXRaw base)) (TeXRaw body))) = 
        T.concat [T.pack "log", leftParen, T.strip body, comma, base, rightParen]


    {- Converts LaTeX roots to sagemath.
       Assumes base is wrapped in braces. -}
    sageRoot :: [TeXArg] -> LaTeX -> Text
    sageRoot [OptArg inv_exp, FixArg under_root] latex = let eval_inv_exp = createSage inv_exp
                                                             eval_under_root = createSage under_root
                                                             eval_latex = createSage latex
                                                         in T.concat [leftParen, eval_under_root, caret, 
                                                                      leftParen, one, forwardSlash, 
                                                                      leftParen, createSage inv_exp, rightParen, 
                                                                      rightParen, rightParen, eval_latex]
    sageRoot [FixArg under_root] latex = sageRoot [OptArg (TeXRaw (T.pack "2")), FixArg under_root] latex

    -- Converts LaTeX matrices to sagemath.
    sageMatrix :: [TeXArg] -> LaTeX -> Text
    sageMatrix [] matrix = let matrixStr = createSage matrix
                               splitLine = T.splitOn (T.pack "linebreak")
                               splitAmp = T.splitOn (T.pack "&")
                               trimList :: [Text] -> [Text]
                               trimList = map (\y -> T.strip y)
                               addCommas :: [Text] -> Text
                               addCommas = T.intercalate comma 
                               createArg :: [Text] -> Text
                               createArg = (\x -> T.concat [leftBracket, addCommas (trimList x), rightBracket])
                               splitListByLineAmp :: [[Text]]
                               splitListByLineAmp =  map splitAmp (splitLine matrixStr)
                           in T.concat [T.pack "matrix", leftParen, leftBracket, 
                                        (addCommas (map createArg splitListByLineAmp)), 
                                        rightBracket, rightParen]