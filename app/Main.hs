
module Main where

import System.IO()

import Lab2()
import Exp()
import Parsing ( exprParser, parseFirst )
import Printing ( showExp )
import REPLCommand ( replCommand, REPLCommand(Eval, Quit, Load) )

main :: IO ()
main = do
    putStrLn "Insert command: "
    string <- getLine
    let prompt = parseFirst replCommand string
    case prompt of
        Just Quit -> return () 
        Just (Load a) -> do 
            putStrLn "The load command!"
            main
        Just (Eval a) -> do
            putStrLn a
            let rez = parseFirst exprParser a
            case rez of
                Nothing -> do
                    putStrLn "Could not parse!"
                    main
                (Just exp) ->  
                            let text1 = showExp exp
                            in do
                                putStrLn text1
                                main
        Nothing -> undefined