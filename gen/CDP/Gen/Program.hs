{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections, TypeOperators #-}

module CDP.Gen.Program
    ( Program (..)
    , genProgram
    , genProtocolModule

    , ComponentName (..)
    ) where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.Text as T

import qualified CDP.Definition as D
import qualified CDP.Gen.Snippets as Snippets

----------- Start of program generation ----------- 

data Program = Program
    { pComponents       :: Map.Map ComponentName T.Text
    , pComponentImports :: T.Text
    }

data Context = Context { ctxDomainComponents :: DomainComponents }

genProgram :: [D.Domain] -> Program
genProgram delts = Program
    { pComponents        = allComponents ctx $ Map.elems dc
    , pComponentImports  = T.unlines $ allComponentImports ctx
    }
  where
    ctx    = Context dc
    dc     = domainComponents delts

----------- Generation of global types / values ----------- 

genProtocolModule :: [ComponentName] -> T.Text -> T.Text
genProtocolModule names source = T.unlines
    [ "{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE FlexibleContexts #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE DeriveGeneric #-}"
    , ""
    , protocolModuleHeader names
    , Snippets.domainImports
    , source
    ]

allComponentImports :: Context -> [T.Text]
allComponentImports = Set.toList . Set.fromList . 
    concatMap ((\n -> [importDomain False n, importDomain True n]) . unComponentName . cName) . Map.elems . ctxDomainComponents

allComponents :: Context -> [Component] -> Map.Map ComponentName T.Text
allComponents ctx components = Map.fromList . 
    map (cName &&& genComponent ctx) $ 
    components

----------- Generation of domain types / values -----------

genComponent :: Context -> Component -> T.Text
genComponent ctx component = T.intercalate "\n\n" $
    [ Snippets.domainLanguageExtensions
    , formatComponentDescription component
    , domainModuleHeader cn
    , Snippets.domainImports
    , T.unlines $ map (importDomain True) deps
    , T.unlines $ map (genDomain ctx) delts
    ]
  where
    deps  = Set.toList . Set.fromList . map unComponentName . 
        map (domainToComponentName ctx) . concatMap snd . Map.elems $ cDomDeps component
    delts = map fst . Map.elems $ cDomDeps component
    cn    = unComponentName . cName $ component

genDomain :: Context -> D.Domain -> T.Text
genDomain ctx domainElt = T.unlines . intercalate [""] $
    map (genType ctx dn)            (D.domainTypes    domainElt) ++
    map (genEventReturnType ctx dn) (D.domainEvents   domainElt) ++
    map (genCommand ctx dn)         (D.domainCommands domainElt)
  where
    dn    = domainName domainElt

genType :: Context -> T.Text -> D.Type -> [T.Text]
genType ctx domainName telt =
    desc ++
    (case D.typeEnum telt of
        Just enumValues -> genTypeEnum ctx tn enumValues
        Nothing         -> case tytelt of
            "object" -> case tpeltsM of
                Nothing ->  [genTypeSynonynm ctx domainName tn]
                Just tpelts ->
                    genRecordType ctx domainName tn tpelts ++
                    genRecordFromJson tn tpelts ++
                    genRecordToJson tn tpelts
            ty -> [T.unwords ["type", tn, "=", lty]])
  where
    desc     = formatDescription . Just $
        "Type '" <> domainName <> "." <> D.typeId telt <> "'." <>
        maybe "" (\x -> "\n" <> x) (D.typeDescription telt)
    lty      = leftType ctx domainName (Just tytelt) Nothing (D.typeItems telt)
    tytelt   = D.typeType telt
    tpeltsM  = D.typeProperties telt
    tn       = typeNameHS domainName telt 

genTypeEnum :: Context -> T.Text -> [T.Text] -> [T.Text]
genTypeEnum _ typeEnumName values =
    [T.unwords ["data", typeEnumName, "=", T.intercalate " | " constructors]] ++
    indent [derivingOrd] ++
    genFromJSONInstanceEnum typeEnumName values constructors ++
    genToJSONInstanceEnum typeEnumName values constructors
  where
    constructors = map (tyNameHS typeEnumName) values

genEventReturnType :: Context -> T.Text -> D.Event -> [T.Text]
genEventReturnType ctx domainName eventElt =
    desc ++
    (case evelts of
        [] -> ["data " <> evrn <> " = " <> evrn] ++ indent [derivingBase]
        _ -> genRecordType ctx domainName evrn evelts) ++
    (genRecordFromJson evrn evelts) ++
    ["instance Event " <> evrn <> " where"] ++
    indent ["eventName _ = \"" <> eventName domainName eventElt <> "\""]
  where
    desc = formatDescription . Just $ "Type of the '" <>
        (eventName domainName eventElt) <> "' event."
    evelts = D.eventParameters eventElt
    evrn = eventNameHS domainName eventElt

genCommand :: Context -> T.Text -> D.Command -> [T.Text]
genCommand ctx domainName commandElt =
    formatDescription (D.commandDescription commandElt) ++
    [""] ++
    formatDescription (Just $ "Parameters of the '" <> cn <> "' command.") ++
    genRecordType ctx domainName ptn pelts ++
    genRecordSmartConstructor ctx domainName ptn pelts ++
    genRecordToJson ptn pelts ++
    (if null relts then [] else genRecordType ctx domainName rtn relts) ++
    (if null relts then [] else genRecordFromJson rtn relts) ++
    commandInstance
  where
    cn   = commandName domainName commandElt 
    ptn  = commandParamsNameHS domainName commandElt
    rtn  = commandNameHS domainName commandElt

    pelts = D.commandParameters commandElt

    relts = D.commandReturns commandElt

    commandAssociatedType = if null relts then "()" else rtn
    commandInstance =
        ["instance Command " <> ptn <> " where"] ++
        indent ["type CommandResponse " <> ptn <> " = " <> commandAssociatedType] ++
        indent ["commandName _ = \"" <> cn <> "\""] ++
        indent ["fromJSON = const . A.Success . const ()" | (null relts)]

genRecordType :: Context -> T.Text -> T.Text -> [D.Property] -> [T.Text]
genRecordType ctx domainName recName props =
    (do
        p <- props
        e <- maybeToList $ D.propertyEnum p
        genTypeEnum ctx (tyNameHS "" (fieldNameHS recName $ D.propertyName p)) e) ++
    [ "data " <> recName <> " = " <> recName] ++
    (indent $ case props of
        [] -> []
        _ ->
            ["{"] ++
            (indent $ do
                (isLast, prop) <- markLast props
                let (fn, fty) = propertyHsSig ctx domainName recName prop
                    comma = if isLast then "" else ","
                formatDescription (D.propertyDescription prop) ++
                    [fn <> " :: " <> fty <> comma]) ++
            ["}"]) ++
    ["  deriving (Eq, Show)"]

genRecordSmartConstructor :: Context -> T.Text -> T.Text -> [D.Property] -> [T.Text]
genRecordSmartConstructor ctx domainName recName props =
    [conName] ++
    (indent $ do
        (isFirstParam, (mbDesc, ty)) <- markFirst types
        formatDescription mbDesc ++
            [ (if isFirstParam then ":: " else "-> ") <> ty ]) ++
    [conName] ++
    indent args ++
    indent ["= " <> recName] ++
    (indent . indent $ do
        prop <- props
        pure $ if D.propertyOptional prop
            then "Nothing"
            else argName prop)
  where
    conName = uncapitalizeFirst recName
    requiredProps = filter (not . D.propertyOptional) props
    types = (do
        prop <- requiredProps
        let (_, ty) = propertyHsSig ctx domainName recName prop
        pure (D.propertyDescription prop, ty)) ++
        [ (Nothing, recName) ]
    args = do
        prop <- requiredProps
        guard . not $ D.propertyOptional prop
        pure $ argName prop

    argName = ("arg_" <>) . fieldNameHS recName . D.propertyName

genRecordFromJson :: T.Text -> [D.Property] -> [T.Text]
genRecordFromJson recName props =
    ["instance FromJSON " <> recName <> " where"] ++
    (indent $ case props of
        [] -> ["parseJSON _ = pure " <> recName]
        _  ->
            ["parseJSON = A.withObject \"" <> recName <> "\" $ \\o -> " <> recName] ++
            (indent $ do
                (isFirst, prop) <- markFirst props
                pure $
                    (if isFirst then "<$>" else "<*>") <>
                    " o " <> (if D.propertyOptional prop then "A..:?" else "A..:") <>
                    " \"" <> D.propertyName prop <> "\""))

genRecordToJson :: T.Text -> [D.Property] -> [T.Text]
genRecordToJson recName params =
    ["instance ToJSON " <> recName <> " where"] ++
    (case params of
        [] -> ["  toJSON _ = A.Null"]
        _  ->
            ["  toJSON p = A.object $ catMaybes ["] ++
            [
                "    " <> T.intercalate ",\n    "
                [ "(\"" <> D.propertyName prop <> "\" A..=) <$> " <>
                    (if D.propertyOptional prop then "" else "Just ") <>
                    "(" <> fieldNameHS recName (D.propertyName prop) <> " p)"
                | prop <- params
                ]
            ] ++
            ["    ]"])

propertyHsSig :: Context -> T.Text -> T.Text -> D.Property -> (T.Text, T.Text)
propertyHsSig ctx domainName recName prop = case D.propertyEnum prop of
    Just _  -> (fn, (if D.propertyOptional prop then "Maybe " else "") <> ftn)
    Nothing -> (fn, ) $ genEltType ctx domainName (D.propertyOptional prop)
        (D.propertyType prop)
        (D.propertyRef prop) 
        (D.propertyItems prop)
  where
    ftn = tyNameHS "" fn
    fn = fieldNameHS recName . D.propertyName $ prop

genTypeSynonynm :: Context -> T.Text -> T.Text -> T.Text
genTypeSynonynm ctx domainName typeName = T.unwords ["type", typeName, "=", typeCDPToHS ctx domainName "object" Nothing]

----------- Generation rules ----------- 
----- Docs    -----
formatComponentDescription :: Component -> T.Text
formatComponentDescription component = T.unlines $
    ["{- |"] ++
    (do
        (delt, _) <- Map.elems $ cDomDeps component
        ["= " <> domainName delt] <> [""] ++
            maybe [] T.lines (D.domainDescription delt)) ++
    ["-}"]

formatDescription :: Maybe T.Text -> [T.Text]
formatDescription = maybe [] (indentWith "-- | " "--   " . T.lines)


----- Imports -----
importDomain :: Bool -> T.Text -> T.Text 
importDomain as' domainName = T.unwords $ 
    if as' then imp ++ ["as", domainName] else imp
  where
    imp = ["import", domainModuleName domainName]
   
componentToImport :: [ComponentName] -> [T.Text]
componentToImport = map (("module " <>) . domainModuleName . unComponentName)

protocolModuleHeader :: [ComponentName] -> T.Text
protocolModuleHeader names = T.unlines
    [ mod
    , "( " <> T.intercalate "\n, " exports
    , ") where"
    ]
  where
    mod = T.unwords [ "module", protocolModuleName ]

    exports = componentToImport names

protocolModuleName :: T.Text
protocolModuleName = "CDP.Domains"

domainModuleHeader :: T.Text -> T.Text
domainModuleHeader domainName = T.unwords ["module", domainModuleName domainName, "(module", domainModuleName domainName <>")", "where"]

domainModuleName :: T.Text -> T.Text
domainModuleName domainName = T.intercalate "." ["CDP", "Domains", domainName]

domainQualifiedName :: T.Text -> T.Text -> T.Text
domainQualifiedName domainName n =  T.intercalate "." [domainName, n]

domainName :: D.Domain -> T.Text
domainName = D.domainDomain

----- JSON -----

genFromJSONInstanceEnum :: T.Text -> [T.Text] -> [T.Text] -> [T.Text]
genFromJSONInstanceEnum name vals hsVals =
    ["instance FromJSON " <> name <> " where"] ++
    (indent $
        ["parseJSON = A.withText " <> (T.pack . show) name <> " $ \\v -> case v of"] ++
        (indent $ do
            (v, hsv) <- zip vals (map ("pure " <>) hsVals) ++ [emptyCase name]
            pure $ T.pack (show v) <> " -> " <> hsv))

genToJSONInstanceEnum :: T.Text -> [T.Text] -> [T.Text] -> [T.Text]
genToJSONInstanceEnum name vals hsVals =
    ["instance ToJSON " <> name <> " where"] ++
    (indent $
        ["toJSON v = A.String $ case v of"] ++
        (indent $ do
            (v, hsv) <- zip vals hsVals
            pure $ hsv <> " -> " <> T.pack (show v)))

----- Types -----
genEltType :: Context -> T.Text -> Bool -> Maybe T.Text -> Maybe T.Text -> Maybe D.Items -> T.Text
genEltType ctx name isOptional t1 t2 items = (if isOptional then "Maybe " else "") <> (leftType ctx name t1 t2 items)

leftType :: Context -> T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe D.Items -> T.Text
leftType ctx _ (Just ty1) (Just ty2) _ = error "impossible"
leftType ctx domain (Just ty) _ itemsElt = typeCDPToHS ctx domain ty itemsElt
leftType ctx domain _ (Just ty) itemsElt = typeCDPToHS ctx domain ty itemsElt
leftType ctx _ _ _ _ = error "no type found"

typeCDPToHS :: Context -> T.Text -> T.Text -> Maybe D.Items -> T.Text
typeCDPToHS ctx _ "object" _ = "[(String, String)]"
typeCDPToHS ctx domain "array" (Just items) = "[" <> (leftType ctx domain (D.itemsType items) (D.itemsRef items) Nothing) <> "]"
typeCDPToHS ctx _ ty (Just items) = error . T.unpack $ "non-array type with items: " <> ty
typeCDPToHS ctx domain ty Nothing = convertType ctx domain ty

convertType :: Context -> T.Text -> T.Text -> T.Text
convertType _ _ "string"  = "String"
convertType _ _ "integer" = "Int"
convertType _ _ "boolean" = "Bool"
convertType _ _ "number"  = "Double"
convertType _ _ "()" = "()"
convertType _ _ "any" = "Int" -- TODO
convertType _ _ "array" = error "got array conversion" -- TODO
convertType _ _ "object" = error "got object type"
convertType _ _ "" = error "got empty type"
convertType ctx domain s = case T.splitOn "." s of
    [otherDomain, ty] -> if cn otherDomain == cn domain
            then tyNameHS otherDomain ty -- both domains are in the same component, so the type is not qualified
            else domainQualifiedName (cn otherDomain) (tyNameHS otherDomain ty)
    _ -> tyNameHS domain s 
  where
    cn dn = unComponentName . domainToComponentName ctx $ dn

----- Dependencies -----    
{- : Domain dependencies specified in the protocol seem inaccurate:
    https://bugs.chromium.org/p/chromium/issues/detail?id=1354980#c0

    So they are collected by inspecting each domain.
-}

domainToComponentName :: Context -> T.Text -> ComponentName
domainToComponentName ctx domainName = cName . (flip (Map.!) domainName) . ctxDomainComponents $ ctx 

newtype ComponentName = ComponentName { unComponentName :: T.Text }
    deriving (Show, Eq, Ord)

type DomainDependencies = Map.Map T.Text (D.Domain, [T.Text])
data Component = Component {
  cName     :: ComponentName
, cDomDeps  :: DomainDependencies
}

type DomainComponents = Map.Map T.Text Component
    -- original domain name, component the domain belongs to
    -- domain name for each vertex is the modified domain name
type Vertex = (D.Domain, T.Text, [T.Text]) -- domain, domain name, domain dependencies 

domainComponents :: [D.Domain] -> DomainComponents
domainComponents delts = Map.fromList . concatMap verticesToDomainComponents $ vsc2
  where
    vsc2  = map removeSameComponentDependencies vsc1   
    vsc1  = map Graph.flattenSCC . Graph.stronglyConnCompR $ g
    g     = map (\delt -> (delt, domainName delt, deps delt)) delts
    deps  = Set.toList . domainDependencies

verticesToDomainComponents :: [Vertex] -> [(T.Text, Component)]
verticesToDomainComponents vs = map (,c) dns
  where
    dns = map vertexToDomainName vs 
    c   = verticesToComponent vs

vertexToDomainName :: Vertex -> T.Text
vertexToDomainName (_,dn,_) = dn 

verticesToComponent :: [Vertex] -> Component
verticesToComponent vs = Component cn . Map.fromList . map toComponent $ vs
  where
    toComponent (delt,dn,deps) = (dn, (delt, deps))
    cn = componentName vs

componentName :: [Vertex] -> ComponentName
componentName [v] = ComponentName $ vertexToDomainName v 
componentName vs  = ComponentName $ mconcat . map vertexToDomainName $ vs

removeSameComponentDependencies :: [Vertex] -> [Vertex]
removeSameComponentDependencies vs = map (go vs) vs
  where
    go vs (delt, dn, deps) = (delt,dn,) . Set.toList $ 
        (Set.fromList deps) `Set.difference` dns
    dns = Set.fromList $ map vertexToDomainName vs

domainDependencies :: D.Domain -> Set.Set T.Text
domainDependencies delt = removeSelf . Set.fromList . concat $
    [ concatMap typeDependencies    $ D.domainTypes delt
    , concatMap commandDependencies $ D.domainCommands delt
    , concatMap eventDependencies   $ D.domainEvents delt
    ]
  where
    removeSelf = Set.filter (/= dn)
    dn = domainName delt

typeDependencies :: D.Type -> [T.Text]
typeDependencies telt = case D.typeEnum telt of 
    Just _  -> []
    Nothing -> case D.typeType telt of
        "array"  -> maybe [] pure . join $ itemDependencies <$> D.typeItems telt
        "object" -> fromMaybe [] $ catMaybes . map propertyDependencies <$> D.typeProperties telt
        s -> maybe [] pure $ refTypeToDomain s

commandDependencies :: D.Command -> [T.Text]
commandDependencies celt = concat
    [ catMaybes . map propertyDependencies $ D.commandReturns celt
    , catMaybes . map propertyDependencies $ D.commandParameters celt
    ]

eventDependencies :: D.Event -> [T.Text]
eventDependencies evelt =
    catMaybes . map propertyDependencies $ D.eventParameters evelt

propertyDependencies :: D.Property -> Maybe T.Text
propertyDependencies pelt = case D.propertyEnum pelt of
        Just _  -> Nothing
        Nothing -> case D.propertyRef pelt of
            Just r  -> refTypeToDomain r
            Nothing -> case D.propertyType pelt of
                Just "object" -> Nothing
                Just "array"  -> itemDependencies =<< D.propertyItems pelt
                Just s -> refTypeToDomain s
                _      -> Nothing

itemDependencies :: D.Items -> Maybe T.Text
itemDependencies itelt = refTypeToDomain =<< D.itemsRef itelt

----- Names -----
eventName   :: T.Text -> D.Event -> T.Text
eventName   domainName ev = (domainName <>) . ("." <>) . D.eventName $ ev
eventNameHS :: T.Text -> D.Event -> T.Text
eventNameHS domainName ev = tyNameHS domainName (D.eventName ev)

commandName   :: T.Text -> D.Command -> T.Text
commandName domainName c = (domainName <>) . ("." <>) . D.commandName $ c
commandNameHS :: T.Text -> D.Command -> T.Text
commandNameHS domainName c = tyNameHS domainName (D.commandName c)
commandParamsNameHS :: T.Text -> D.Command -> T.Text
commandParamsNameHS domainName c = paramsTypePrefix $ commandNameHS domainName c

typeNameHS :: T.Text -> D.Type -> T.Text
typeNameHS domainName t = tyNameHS domainName (D.typeId t)

tyNameHS :: T.Text -> T.Text -> T.Text
tyNameHS prefix tyName =
    capitalizeFirst prefix <> capitalizeFirst (hyphensToCapitalize tyName)

fieldNameHS :: T.Text -> T.Text -> T.Text
fieldNameHS tyName fieldName = uncapitalizeFirst tyName <> capitalizeFirst fieldName

paramsTypePrefix :: T.Text -> T.Text
paramsTypePrefix = ("P" <>)

refTypeToDomain :: T.Text -> Maybe T.Text
refTypeToDomain r = case T.splitOn "." r of
    [domain, _] -> Just domain
    _           -> Nothing

-----
emptyCase :: T.Text -> (T.Text, T.Text)
emptyCase name = ("_", T.unwords ["fail", (T.pack . show) $ "failed to parse " <> name])

derivingBase    :: T.Text
derivingBase    = "deriving (Eq, Show, Read)"
derivingOrd     :: T.Text
derivingOrd     = "deriving (Ord, Eq, Show, Read)"

-----------  Utilities ----------- 

indent :: [T.Text] -> [T.Text]
indent = indentWith "  " "  "

indentWith :: T.Text -> T.Text -> [T.Text] -> [T.Text]
indentWith _         _          []       = []
indentWith firstLine otherLines (x : xs) =
    (firstLine <> x) : map (otherLines <>) xs

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t = maybe t (\(first, rest) -> T.singleton (toUpper first) `T.append` rest) . T.uncons $ t

uncapitalizeFirst :: T.Text -> T.Text
uncapitalizeFirst t = maybe t (\(first, rest) -> T.singleton (toLower first) `T.append` rest) . T.uncons $ t

markFirst :: [a] -> [(Bool, a)]
markFirst = zip (True : repeat False)

markLast :: [a] -> [(Bool, a)]
markLast []          = []
markLast (x : [])    = [(True, x)]
markLast (x : y : z) = (False, x) : markLast (y : z)

-- | Changes things like "a-rate" to "aRate"
hyphensToCapitalize :: T.Text -> T.Text
hyphensToCapitalize txt
    | T.null after = before
    | otherwise    =
        before <> capitalizeFirst (hyphensToCapitalize (T.drop 1 after))
  where
    (before, after) = T.break (== '-') txt
