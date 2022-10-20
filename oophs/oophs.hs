import Parser
import Grammar
import Validator

main :: IO ()
main = do let fileName = "files\\in.hclass"
          
          file <- readFile fileName
          
          -- Parse.
          let classes = hclassFileParse file
          
          case classes of Right x  -> print x
                          Left err -> print err
          
          putStrLn ""

          -- Validate.
          case classes of Right x  -> print $ validate x
                          Left err -> print err
          
          return ()
