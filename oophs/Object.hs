module Object where
import Grammar
import Utility.State


collectionAttributes, nullAttributes :: Class -> [Attribute]
collectionAttributes c = [a | a <- getAttributes c, isCollection a]
nullAttributes       c = [a | a <- getAttributes c, not $ isNull a, not $ isCollection a]

-- | Functions such as collectionAttributes, nullAttributes could be abstracted with filterAttributesBy... 
-- filterAttributesBy :: (Attribute -> Bool) -> Class -> [Attribute]
-- filterAttributesBy f c = [a | a <- getAttributes c, f a]

attributes :: Class -> [String] -> (Class -> [Attribute]) -> [(Name, Value)]
attributes c args f = zipWith (\x y -> (getAttributeName x, ATTRValue y)) (f c) args

-- Utility functions -----------------------------------------------------------------------------

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs)
        | f x       = Just x
        | otherwise = find f xs

findFilter :: String -> (String, a) -> Bool
findFilter x (x', v) = x == x'

replace :: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) new old
    | x == old  = new : xs
    | otherwise = x : replace xs new old

modify :: Object -> Value -> (String, Value) -> Object
modify object newValue oldValue@(a, v) =
    Instance (getClass object)
             (replace (getValues object) (a, newValue) oldValue)

isCardinalityToMany :: Cardinality -> Bool
isCardinalityToMany card = case card of
    "@OneToMany"  -> True
    "@ManyToMany" -> True
    _             -> False

isAttributeValue, isAttributeValues, isAssociationValue, isAssociationValues :: Value -> Bool
isAttributeValue (ATTRValue _) = True
isAttributeValue _             = False

isAttributeValues (ATTRValues _) = True
isAttributeValues _              = False

isAssociationValue (ASSOValue _) = True
isAssociationValue _             = False

isAssociationValues (ASSOValues _) = True
isAssociationValues _              = False

getAssociationValue :: Value -> Maybe Object
getAssociationValue (ASSOValue x) = Just x
getAssociationValue _             = Nothing

-- Constructors ----------------------------------------------------------------------------------

-- | Maybe change to [Attribute] -> [Attribute]?
requiredArgs, allArgs :: Class -> [Attribute]
requiredArgs c = [a | a <- getAttributes c, isNull a]
allArgs      c = [a | a <- getAttributes c, not $ isCollection a]

instantiate :: Class -> [String] -> State Object ()
instantiate c args = State $ \_ ->
                case () of
                    _ | n == 0                       && "@NoArgsConstructor"       `elem` getConstructors c -> ((), Instance c [])
                      | n == length (allArgs      c) && "@AllArgsConstructor"      `elem` getConstructors c -> ((), Instance c (attributes c args allArgs))
                      | n == length (requiredArgs c) && "@RequiredArgsConstructor" `elem` getConstructors c -> ((), Instance c (attributes c args requiredArgs))
                      | otherwise -> ((), Null)
                where n = length args

-- Get, Set and Add ------------------------------------------------------------------------------

safeGet :: String -> Object -> Bool
safeGet name object =
    case value of
        (Just a, Nothing) -> "get" `elem` [getMethodName m | m <- getAttributeMethods   a] -- getAttributeMethods   a
        (Nothing, Just a) -> "get" `elem` [getMethodName m | m <- getAssociationMethods a] -- getAssociationMethods a
        _                 -> False
    where value = (find (\x -> getAttributeName   x == name) (getAttributes   $ getClass object),
                   find (\x -> getAssociationName x == name) (getAssociations $ getClass object))

get :: String -> State Object (Maybe Value)
get name = State $ \object ->
    if safeGet name object then
        case find (findFilter name) (getValues object) of
            Nothing     -> (Nothing, object)
            Just (a, v) -> (Just v,  object)
    else (Nothing, object)

--------------------------------------------------------------------------------------------------

safeSet :: String -> Object -> Value -> Bool
safeSet name object newValue =
    case value of
        (Just a, Nothing)
            | isCollection a && not (isAttributeValue newValue) && "set" `elem` [getMethodName m | m <- getAttributeMethods a] -> True
            | not (isCollection a) && isAttributeValue newValue && "set" `elem` [getMethodName m | m <- getAttributeMethods a] -> True
            | otherwise                                                                                                        -> False
        (Nothing, Just a)
            | isOwner a  && isCardinalityToMany (getCardinality a)       && isAssociationValues newValue && "set" `elem` [getMethodName m | m <- getAssociationMethods a] -> True
            | isOwner a  && not (isCardinalityToMany (getCardinality a)) && isAssociationValue  newValue && "set" `elem` [getMethodName m | m <- getAssociationMethods a] -> True
            | otherwise                                                                                                           -> False
        _                 -> False
    where value = (find (\x -> getAttributeName   x == name) (getAttributes   $ getClass object),
                   find (\x -> getAssociationName x == name) (getAssociations $ getClass object))

set :: String -> Value -> State Object ()
set name newValue = State $ \object ->
    if safeSet name object newValue then
        case find (findFilter name) (getValues object) of
            Nothing            -> ((), Instance (getClass object) (getValues object ++ [(name, newValue)]))
            Just (a, oldValue) -> ((), modify object newValue (a, oldValue))
    else ((), object)

--------------------------------------------------------------------------------------------------

{-
    To be able to add element to Attribute, Attribute needs to be:
        1. Existing
        2. Collection
        3. Attribute needs to have add method in [Mehthods]

    Also, we could check for type here.  
    
    To be able to add Element to Association, Association needs to be:
        1. Existing
        2. Owner
        3. Attribute needs to have add method in [Mehthods]
        4. Cardinality of Association needs to be "@_ToMany"
    
    Also, element (object) we are adding to association needs to be valid.    
-}

safeAdd :: Object -> String -> Value -> Bool
safeAdd object name element =
    case value of
        (Just a, Nothing)
            | isAttributeValue element -> isCollection a && "add" `elem` [getMethodName m | m <- getAttributeMethods a]
            | otherwise                -> False
        (Nothing, Just a)
            | isAssociationValue element ->
                    case getAssociationValue element of
                        Nothing -> False
                        Just ob -> isOwner a && associatedWith a == getClassName (getClass ob) && "add" `elem` [getMethodName m | m <- getAssociationMethods a] && isCardinalityToMany (getCardinality a)
            | otherwise                  -> False
        _                 -> False
    where value = (find (\x -> getAttributeName   x == name) (getAttributes   $ getClass object),
                   find (\x -> getAssociationName x == name) (getAssociations $ getClass object))

add :: String -> Value -> State Object ()
add name element = State $ \object ->
    if safeAdd object name element then
        case find (findFilter name) (getValues object) of
            Nothing
                | isAttributeValue element  -> ((), Instance (getClass object) (getValues object ++ [(name, ATTRValues [] <.> element)]))
                | otherwise                 -> ((), Instance (getClass object) (getValues object ++ [(name, ASSOValues [] <.> element)]))
            Just (a, collection@(ATTRValues xs)) -> ((), modify object (collection <.> element) (a, collection))
            Just (a, collection@(ASSOValues xs)) -> ((), modify object (collection <.> element) (a, collection))
            _                                    -> ((), object)

    else ((), object)



-- set :: String -> Value -> State Object ()
-- set name newValue = State $ \object ->
--     if safeSet name object newValue then
--         case find (findFilter name) (getValues object) of
--             Nothing -> ((), Instance (getClass object) (getValues object ++ [(name, newValue)]))
--                 --  | not (isAttributeValue newValue) -> ((), Instance (getClass object) (getValues object ++ [(name, newValue)]))
--                 --  | isAssociationValue newValue     -> ((), object) 
--                 --  | otherwise                       -> ((), object) 
--                 --  | isAttributeCollection   name object -> ((), object) 
--                 --  | isAttribute             name object -> ((), object)
--                 --  | isAssociationCollection name object -> ((), object)
--                 --  | otherwise                           -> ((), object) -- (It's an association.)
--             -- Just (a, oldValue@(ATTRValue  attr )) -> ((), modify object newValue (a, oldValue))
--             -- Just (a, oldValue@(ATTRValues attrs)) -> ((), modify object newValue (a, oldValue))
--             -- Just (a, oldValue@(ASSOValue  asso )) -> ((), modify object newValue (a, oldValue))
--             -- Just (a, oldValue@(ASSOValues assos)) -> ((), modify object newValue (a, oldValue))
--             Just (a, oldValue)                -> ((), modify object newValue (a, oldValue))
--     else ((), object)