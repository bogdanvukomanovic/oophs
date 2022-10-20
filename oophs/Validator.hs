module Validator where
import Grammar
import Utility.Writer
import Control.Monad
import Object (isAttributeValue, isAttributeValues)
import Text.Read
import Data.Maybe

-- Writer helper function(s) ----------------------------------------------------

f :: Int -> Writer [String] Int
f x = if x == 0 then Writer (0, ["OK"])
                else Writer (x, ["NOT OK"])

-- Attribute Validator ----------------------------------------------------------

-- | Note: It needs to fail, so guard won't catch it...
isValidAttribute :: Attribute -> Bool
isValidAttribute atr =
      if isCollection atr then
           "add" `notElem` [getMethodName m | m <- getAttributeMethods atr]
      else "add" `elem` [getMethodName m | m <- getAttributeMethods atr]

validateAttributes :: [Class] -> [Int]
validateAttributes classes = do c <- classes
                                let attributes = getAttributes c
                                case attributes of [] -> return 0
                                                   _  -> do a <- attributes
                                                            guard $ isValidAttribute a
                                                            return 1

-- Association Validator --------------------------------------------------------

data Record = AssociationRecord (Name, Name) Cardinality Owner | None deriving (Show, Eq)

getRecords :: [Class] -> [Record]
getRecords classes = do c <- classes
                        let associations = getAssociations c
                        case associations of [] -> return None
                                             _  -> do a <- associations
                                                      return $ AssociationRecord
                                                                (getAssociationEdge a)
                                                                (getCardinality     a)
                                                                (isOwner            a)

-- | Returns the complement of the Cardinality. 
cardinality' :: Cardinality -> Cardinality
cardinality' cardinality = case cardinality of "@OneToMany" -> "@ManyToOne"
                                               "@ManyToOne" -> "@OneToMany"
                                               _            -> cardinality


-- | Returns the complement of the Record. 
record' :: Record -> Record
record' None = None
record' (AssociationRecord (a, b) ca oa) = AssociationRecord (b, a) cb ob
                                                        where cb = cardinality' ca
                                                              ob = not oa

validateAssociations :: [Class] -> [Record]
validateAssociations classes = [record | record <- records, record' record `notElem` records, record /= None]
                                    where records = getRecords classes

validate :: [Class] -> Writer [String] Int
validate classes = let atr = validateAttributes   classes
                       aso = validateAssociations classes
                   in do tell ["Attributes:"]
                         x <- f $ sum atr
                         tell ["Associations:"]
                         y <- f $ length aso
                         return $ x + y

-- Object Validator -------------------------------------------------------------

getAttributeValues :: [(String, Value)] -> [(String, Value)]
getAttributeValues = filter (\(string, value) -> isAttributeValue value || isAttributeValues value)

int :: String -> Maybe Int
int = readMaybe

float :: String -> Maybe Float
float = readMaybe

boolean :: String -> Maybe Bool
boolean = readMaybe

char :: String -> Maybe Char
char = readMaybe

verifyType :: Attribute -> String -> Bool
verifyType attribute value = case getType attribute of
      "String"  -> True
      "int"     -> isJust $ int     value
      "float"   -> isJust $ float   value
      "boolean" -> isJust $ boolean value
      "char"    -> isJust $ char    value
      _         -> error "Error: Not supported type."

checkType :: Maybe Attribute -> Value -> Bool
checkType Nothing _ = False
checkType (Just attribute) (ATTRValue value) = verifyType attribute value
checkType (Just attribute) (ATTRValues values) = and [verifyType attribute value | value <- values]
checkType _ _ = error "Error: _"

getAttributeByName :: [Attribute] -> Name -> Maybe Attribute
getAttributeByName [] _ = Nothing
getAttributeByName (x:xs) name
      | getAttributeName x == name = Just x
      | otherwise  = getAttributeByName xs name

validateObject :: Object -> Bool
validateObject object =
      and [checkType (getAttributeByName classAttributes name) value  | (name, value) <- objectAttributes]
      where classAttributes  = getAttributes $ getClass object
            objectAttributes = getAttributeValues $ getValues object