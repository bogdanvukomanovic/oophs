module Grammar where

import Data.List ( intercalate )
import Data.Char ( toUpper )

-- Grammar ------------------------------------------------------------------

data Class = Class { getClassName    :: Name         ,
                     getExtends      :: [Name]       ,
                     getImplements   :: [Name]       ,
                     getPackage      :: Package      ,
                     getConstructors :: [Constructor],
                     getAttributes   :: [Attribute]  ,
                     getAssociations :: [Association]
                   } deriving (Eq)

type Name = String
type Package = String
type Constructor = String

data Attribute = Attribute { getAttributeName       :: Name      ,
                             getType                :: Type      ,
                             isNull                 :: Null      ,
                             isCollection           :: Collection,
                             getAttributeMethods    :: [Method]
                           } deriving (Eq)
type Type = String
type Null = Bool
type Collection = Bool

type AssociationEdge = (Name, Name)

data Association = Association { getAssociationName     :: Name           ,
                                 getCardinality         :: Cardinality    ,
                                 associatedWith         :: AssociatedWith ,
                                 isOwner                :: Owner          ,
                                 getAssociationMethods  :: [Method]       ,
                                 getAssociationEdge     :: AssociationEdge
                               } deriving (Eq)

type Cardinality = String
type Owner = Bool
type AssociatedWith = String


data Interface = Interface { getInterfaceName    :: Name    ,
                             getInterfacePackage :: Package ,
                             getInterfaceMethods :: [Method]
                           } deriving (Eq)

data AClass = AClass { getAClassName    :: Name    ,
                       getAClassPackage :: Package ,
                       getAClassMethods :: [Method]
                     }

data Method = Method { getMethodName      :: Name      ,
                       getMethodModifiers :: [Modifier]
                     } deriving (Eq, Show)

type Modifier = String

---



instance Show Attribute where
    show a = "ATTR " ++ getAttributeName          a  ++ " "
                     ++ getType                   a  ++ " "
                     ++ show (isNull              a) ++ " "
                     ++ show (isCollection        a) ++ " "
                     ++ show (getAttributeMethods a)

instance Show Association where
    show a = "ASSO " ++ getAssociationName          a  ++ " "
                     ++ getCardinality              a  ++ " "
                     ++ associatedWith              a  ++ " "
                     ++ show (isOwner               a) ++ " "
                     ++ show (getAssociationMethods a) ++ " "
                     ++ show (getAssociationEdge    a)

-- Algebraic representation of Object ---------------------------------------  

data Value = ATTRValue String | ATTRValues [String] | ASSOValue Object | ASSOValues [Object] deriving (Show, Eq)

infixr 5 <.>
(<.>) :: Value -> Value -> Value
(ATTRValues xs) <.> (ATTRValue x) = ATTRValues $ xs ++ [x]
(ASSOValues xs) <.> (ASSOValue x) = ASSOValues $ xs ++ [x]
_               <.> _             = error "Error: Cannot concat ASSOValue to ATTRValues (ATTRValue to ASSOValues)."

data Object = Instance { getClass  :: Class             ,
                         getValues :: [(String, Value)]
                       }
            | Null deriving (Eq)

instance Show Object where
    show Null = "Null"
    show o = getClassName (getClass o)  ++ " " ++ show (getValues o)

---

instance Show Class where
    show c = pClassHeader c ++
             pAttributes (getAttributes c)
             ++ "\n" ++
             pAssociations (getAssociations c)
             ++ "\n" ++
             pConstructors c (getConstructors c)
             ++ "\n" ++
             pAttributeMethods (getAttributes c)
             ++"\n}"

pExtends :: [Name] -> String
pExtends [] = ""
pExtends xs = " extends " ++ intercalate ", " xs

pImplements :: [Name] -> String
pImplements [] = ""
pImplements xs = " implements " ++ intercalate ", " xs

pClassHeader :: Class -> String
pClassHeader c = "class " ++ getClassName c ++ pExtends (getExtends c) ++ pImplements (getImplements c) ++ " { \n"

pCollection :: Attribute -> String
pCollection a = if isCollection a then "[]" else ""

pAttributes :: [Attribute] -> String
pAttributes [] = ""
pAttributes (x:xs) = "\t" ++ getType x ++ pCollection x ++  " " ++ getAttributeName x ++ ";\n" ++ pAttributes xs

pCardinality :: Cardinality -> String
pCardinality card = case card of
    "@OneToMany"  -> "[]"
    "@ManyToMany" -> "[]"
    _             -> ""

pAssociations :: [Association] -> String
pAssociations [] = ""
pAssociations (x:xs) = "\t" ++ associatedWith x ++ pCardinality (getCardinality x) ++ " " ++ getAssociationName x ++ ";\n" ++ pAssociations xs

pConstructorArgs :: [Attribute] -> String
pConstructorArgs [] = ""
pConstructorArgs (x:xs)
    | null xs   = getType x ++ " " ++ getAttributeName x ++ pConstructorArgs xs
    | otherwise = getType x ++ " " ++ getAttributeName x ++ ", " ++ pConstructorArgs xs

pConstructorBody :: [Attribute] -> String
pConstructorBody [] = ""
pConstructorBody (x:xs) = "\t\tthis." ++ getAttributeName x ++ " = " ++ getAttributeName x ++ ";\n" ++ pConstructorBody xs

pConstructors :: Class -> [Constructor] -> String
pConstructors _ [] = ""
pConstructors c (x:xs)
    | x == "@NoArgsConstructor" =
        "\tpublic " ++ getClassName c ++ "() {};\n" ++ pConstructors c xs
    | x == "@AllArgsConstructor" =
        "\tpublic " ++ getClassName c ++ "(" ++ pConstructorArgs (getAttributes c) ++ ") {\n"
            ++ "\t\t // TODO: AllArgsConstructor..."
                ++ "\t}" 
    | x == "@RequiredArgsConstructor" =
        "\tpublic " ++ getClassName c ++ "(" ++ pConstructorArgs [a | a <- getAttributes c, isNull a] ++ ") {\n"
            ++ pConstructorBody [a | a <- getAttributes c, isNull a] ++ "\t}"
    | otherwise = ""

pAttributeMethod :: Attribute -> Method -> String
pAttributeMethod a method
    | getMethodName method == "get" =
        "\t" ++ pModifiers (getMethodModifiers method) ++ getType a ++ " get" ++ capitalize (getAttributeName a) ++ "() {\n"
            ++ "\t\treturn " ++ getAttributeName a ++ ";\n"
                ++ "\t}\n\n"
    | getMethodName method == "set" =
        "\t" ++ pModifiers (getMethodModifiers method) ++ "set" ++ capitalize (getAttributeName a) ++ "(" ++ getType a ++ " new" ++ capitalize (getAttributeName a) ++ ") {\n"
            ++ "\t\tthis." ++ getAttributeName a ++ " = new" ++ capitalize (getAttributeName a) ++ ";\n"
                ++ "\t}\n\n"
    | getMethodName method == "add" =
        "\t" ++ "// TODO: Add method...\n\n"
    | otherwise                     = ""

capitalize :: String -> String
capitalize [] = ""
capitalize (x:xs) = toUpper x : xs

pAttributeMethods :: [Attribute] -> String
pAttributeMethods attributes = unwords [concatMap (pAttributeMethod a) methods | (a, methods) <- x]
                                    where x = fmap (\a -> (a, getAttributeMethods a)) attributes

pModifiers :: [Modifier] -> String
pModifiers [] = ""
pModifiers xs = unwords xs