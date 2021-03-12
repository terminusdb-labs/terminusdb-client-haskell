{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module TerminusDBClient where

import           Control.Monad.IO.Class
import           Data.Aeson                 hiding (String)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text                  as T
import           Network.HTTP.Req


data TerminusClient = TerminusClient { serverURL :: T.Text, key :: T.Text, accountId :: T.Text, databaseId :: T.Text } deriving (Show)
newtype QueryListElement = QueryListElement WOQLExpression deriving (Show)
data QueryResource = RemoteResource String | FileResource String deriving (Show)
data WOQLValue = WOQLNode String | WOQLVar String | WOQLString String | WOQLInt Int deriving (Show)
data NamedAsVar = NamedAsVar String String deriving (Show) -- identifier and variable name, should this be record syntax?
data WOQLExpression = And [WOQLExpression] |
                      Triple WOQLValue WOQLValue WOQLValue |
                      Quad   WOQLValue WOQLValue WOQLValue String |
                      Using  T.Text WOQLExpression |
                      WOQLGet [NamedAsVar] QueryResource

                    deriving (Show)
newtype Query = Query { query:: WOQLExpression } deriving (Show)


woqlStringJSON value = object ["@type" .= "xsd:string", "@value" .= value]

woqlLiteralJSON dataType value = object ["@type" .= "woql:Datatype",
                                         "woql:datatype" .= object ["@type" .= dataType, "@value" .= value]
                                    ]

instance ToJSON NamedAsVar where
  toJSON (NamedAsVar id var) = object ["@type" .= "woql:NamedAsVar",
                                       "woql:identifier" .= woqlStringJSON id,
                                       "woql:variable_name" .= woqlStringJSON id
                                      ]

instance ToJSON QueryListElement where
  -- TODO: add woql:index, probably should have used vectors instead of lists
  toJSON (QueryListElement expression) = object ["@type" .= "woql:QueryListElement",
                                                 "woql:query" .= expression
                                                ]

instance ToJSON QueryResource where
  toJSON (FileResource path) = object ["@type" .= "woql:FileResource",
                                        "woql:file" .= woqlStringJSON path
                                      ]
  toJSON (RemoteResource uri) = object ["@type" .= "woql:RemoteResource",
                                        "woql:remote_uri" .= object ["@type" .= "xsd:anyURI", "@value" .= uri]
                                      ]

instance ToJSON Query where
  toJSON (Query query) = object ["query" .= query]


instance ToJSON WOQLValue where
  toJSON (WOQLNode node) = object ["@type" .= "woql:Node", "woql:node" .= node]
  toJSON (WOQLVar var) = object ["@type" .= "woql:Variable",
                                 "woql:variable_name" .= woqlStringJSON var
                                ]
  toJSON (WOQLString str) = woqlLiteralJSON "xsd:string" str
  toJSON (WOQLInt int) = woqlLiteralJSON "xsd:integer" int


instance ToJSON WOQLExpression where
  toJSON (WOQLGet namedVars queryExpression) = object ["@type" .= "woql:Get",
                                                       "woql:query_resource" .= queryExpression,
                                                       "woql:as_vars" .= namedVars]
  toJSON (Using graph expression) = object ["@type" .= "woql:Using",
                                            "woql:collection" .= woqlStringJSON graph,
                                             "woql:query" .= expression
                                           ]
  toJSON (And expressions) = object ["@type" .= "woql:And", "woql:query_list" .= listElements]
    where
      listElements = Prelude.map QueryListElement expressions
  toJSON (Triple x y z) =
    object ["@type" .= "woql:Triple",
            "woql:subject" .= x,
            "woql:predicate" .= y,
            "woql:object" .= z
           ]
  toJSON (Quad x y z graph) =
    object ["@type" .= "woql:Quad",
            "woql:subject" .= x,
            "woql:predicate" .= y,
            "woql:object" .= z,
            "woql:graph" .= graph
           ]

printJSON :: Query -> IO ()
printJSON = BL8.putStrLn . encode

queryToJSON :: Query -> BL.ByteString
queryToJSON = encode

createQuery :: WOQLExpression -> Query
createQuery = Query


executeQuery :: TerminusClient -> Query -> IO Value
executeQuery (TerminusClient serverUrl key accountId dbId) query =
  runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (http serverUrl /: "api" /: "woql" /: accountId /: dbId /: "local" /: "branch" /: "main")
      (ReqBodyJson query) -- use built-in options or add your own
      jsonResponse
      (basicAuthUnsafe "admin" "root" <> port 6363 <> header "Content-Type" "application/json")
  return (responseBody r :: Value)


-- A function because things like a regex url check would be nice
initClient :: T.Text -> T.Text -> T.Text -> T.Text -> TerminusClient
initClient = TerminusClient

queryAllDatabases = Query (Using "_system"
                            (Triple
                             (WOQLVar "X")
                             (WOQLNode "rdf:type")
                             (WOQLNode "system:Database")
                            )
                           )

queryAlternativeSyntax = Query $ Using "_system" $
                         Triple (WOQLVar "X") (WOQLNode "rdf:type") (WOQLNode "system:Database")

queryBikeExample = Query ( WOQLGet
                           [(NamedAsVar "Bike number" "Bike")]
                           (RemoteResource "https://terminusdb.com/t/data/bike_tutorial.csv")
                         )

queryWithWOQLAnd = Query $ Using
    "_system"
    ( And
        [ Triple
            (WOQLVar "X")
            (WOQLNode "rdf:type")
            (WOQLNode "system:Database"),
          Triple
            (WOQLVar "X")
            (WOQLVar "Y")
            (WOQLVar "Z")
        ]
    )


