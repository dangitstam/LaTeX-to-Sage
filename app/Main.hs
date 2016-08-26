module Main where

    import Lib
    import Sage
    import Text.LaTeX
    import Text.LaTeX.Base.Parser
    import Data.Text (unlines, pack)
    import qualified Data.Text as T
    import qualified Data.Text.IO as TO
    import System.Environment

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            [latex] -> case parseLaTeX (T.pack latex) of
                           Left err -> do
                                        print err
                                        print (parseLaTeX (T.pack latex))
                           Right l -> TO.putStrLn (insertAsterik (createSage l))
            _ -> putStrLn "failed"




    {-    case parseLaTeX example of
        Left err -> print err
        Right l -> do
                    print (createSage l)
                    print (parseLaTeX example)
        _       -> putStrLn "failed" -}


    -- add flag for numerical approximation vs symbolic comparison



    example :: Text
    example = T.pack "{\\sqrt{5x}}"
    --example = T.pack "{\\begin{bmatrix} 23450 & 0 & 2 \\\\ 564 & 7865 & 5425 \\\\ 5452 & 542345 & 134 \\end{bmatrix}} + y = {\\sqrt[12](x^2)} + {\\log_{2} n^2} + {\\int_{3}^{4} \\cos(12x) dx} + {\\int 876*x^4 dx} + \\log(12) + {\\frac{d}{dx}(x^2 + 2x + 1)} + {\\frac{d^100}{d^100x}(x^2 + 2x + 1)} + \\frac{x}{y}"