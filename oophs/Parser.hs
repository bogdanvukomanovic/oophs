module Parser where
import Grammar

import Text.Parsec
import Text.Parsec.String


{-
    
    <lower> ::= 'a' | 'b' | ... | 'z'
    <upper> ::= 'A' | 'B' | ... | 'Z'
    <letter> ::= <lower> | <upper>

    <type> ::= void
            | String
            | char
            | short
            | int
            | long
            | float
            | double

    <class_name> ::= <upper>{<lower>}
    <package_name> ::= {lower}+

    <class> ::= "class" <class_name> "p:"<package_name> <EOL>
                {<attribute>}
                {<association>}
                <constructor>

    <attribute_name> ::= {<lower>}+
    <attribute_type> ::= <type>
    <collection> ::= '*'
    <method> ::= @{<letter>}

    <attribute> ::= <attribute_name> <attribute_type> {<collection>}? {<method>} <EOL>

    <association_name> ::= {<lower>}+
    <cardinality> ::= "@OneToMany" 
                    | "@ManyToOne"
                    | "@OneToOne"
                    | "@ManyToMany"

    <associated_with> ::= <class_name>
    <owner> ::= "@owner"

    <association> ::= <association_name> <cardinality> <associated_with> {<owner>}? {<method>} <EOL>

    <constructor_type> ::= "@NoArgsConstructor"
                        | "@RequiredArgsConstructor"
                        | "@AllArgsConstructor" 

    <constructor> ::= "constructor" {constructor_type}+

-}

-- Parser combinators -------------------------------------------------------

{-|
    Note that since input type for our parsers is always String and we do not
    use state we can write "Parser a" (from Text.Parsec.String) instead of 
    "Parsec String () a" (from Text.Parsec).
-}

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

{-|
    Naming convention is currently not supported... Also, note that this function
    is useded to parse Class name, Association name, Method name...
-}
name :: Parser Name
name = lexeme $ many1 letter

package :: Parser Package
package = do string "p:"
             name

modifier :: Parser Modifier
modifier = do string ":"
              name

method :: Parser Method
method = do char '@'
            notFollowedBy $ string "notNull"
            n <- name
            m <- try $ many modifier
            return $ Method n m

methods :: Parser [Method]
methods = many $ try method

collection :: Parser Collection
collection = lexeme $ option False $ do c <- char '*'
                                        return True

notNull :: Parser Null
notNull = lexeme $ option False $ do c <- string "@notNull"
                                     return True

attribute :: Parser Attribute
attribute = do n <- name
               notFollowedBy $ char '@'
               t <- name
               c <- collection
               ms <- methods
               nn <- notNull
               return $ Attribute n t nn c ms

cardinality :: Parser Cardinality
cardinality = lexeme $ try (string "@OneToMany") <|> try (string "@ManyToOne") <|> try (string "@OneToOne") <|> string "@ManyToMany"

owner :: Parser Owner
owner = lexeme $ option False $ do c <- try $ string "@owner"
                                   return True

association :: Name -> Parser Association
association className = do n <- name
                           c <- cardinality
                           aw <- name
                           o <- owner
                           ms <- methods
                           return $ Association n c aw o ms (className, aw)

constructorType :: Parser Constructor
constructorType = lexeme $ try (string "@RequiredArgsConstructor") <|> try (string "@NoArgsConstructor") <|> try (string "@AllArgsConstructor")

constructor :: Parser [Constructor]
constructor = do lexeme $ string "constructor"
                 many1 constructorType

extends :: Parser [String]
extends = do lexeme $ between (char '[') (char ']') (many name)
             
implements :: Parser [String]
implements = do lexeme $ between (char '[') (char ']') (many name)

hclass :: Parser Class
hclass = do lexeme $ string "class"
            n <- name
            e <- extends
            i <- implements
            p <- package
            atr <- many $ try attribute
            aso <- many $ try $ association n
            cns <- constructor
            return $ Class n e i p cns atr aso

hinterface :: Parser Interface
hinterface = do lexeme $ string "interface"
                n <- name
                p <- package
                m <- many $ try method
                return $ Interface n p []

haclass :: Parser AClass
haclass = do lexeme $ string "abstract"
             n <- name
             p <- package
             m <- many $ try method
             return $ AClass n p m

hclasses :: Parser [Class]
hclasses = do spaces
              c <- many $ try hclass
              i <- many $ try hinterface -- ! 
              a <- many $ try haclass    -- ! 
              eof
              return c

hclassFileParse :: String -> Either ParseError [Class]
hclassFileParse = parse hclasses ""


-- Utility functions used for testing ---------------------------------------

-- testParser :: Parser a -> String -> Either ParseError a
-- testParser p = parse p ""

-- testParserWithEOF :: Parser a -> String -> Either ParseError a
-- testParserWithEOF p = parse (p <* eof) ""

-- testParserWithLeftOver :: Parser a -> String -> Either ParseError (a,String)
-- testParserWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
--         where leftOver = manyTill anyToken eof