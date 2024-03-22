module Main where

import Prelude

import Data.String as String
import Data.Array as Array
import Data.Argonaut as Json
import Effect.Class.Console as Console
import Data.Argonaut.Encode (class EncodeJson, (:=), (~>))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Halogen.HTML.Properties as HP
import Helpers (class_)
import MetaDataForm as Form
import StudentForm as StudentForm
import Effect (Effect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Events (onClick)
import Type.Proxy (Proxy(..))
import Web.HTML as HTML
import Web.Storage.Storage as Storage
import Web.HTML.Window as Window

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI parent unit body

type Slots =
  ( form :: Form.Slot Unit
  , studentForm :: StudentForm.Slot Unit
  )

_form = Proxy :: Proxy "form"
_studentForm = Proxy :: Proxy "studentForm"

type ParentState =
  { metaDataNFT :: Maybe Form.MetaDataNFT
  , students :: Array StudentForm.Student
  }

data ParentAction
  = Initialize
  | HandleForm Form.Output
  | HandleStudentForm StudentForm.Output
  | DeleteStudent Int

parent :: forall query input output. H.Component query input output Aff
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  key :: String
  key = "local-storage-parent"

  initialState :: input -> ParentState
  initialState _ =
    { metaDataNFT: Nothing
    , students: []
    }

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output Aff Unit
  handleAction = case _ of
    Initialize -> do
      storedState <- H.liftEffect $ Storage.getItem key =<< Window.localStorage =<< HTML.window
      case Json.decodeJson =<< Json.parseJson =<< note (Json.TypeMismatch "No data") storedState of
        Left err ->
          Console.log $ Json.printJsonDecodeError err
        Right parentState -> do
          H.put parentState

    HandleForm output ->
      case output of
        _ -> H.modify_ \st -> st { metaDataNFT = Just output.newMetaDataNFT }

    HandleStudentForm { newStudent } -> do
      let stateJson st = Json.stringify $ Json.encodeJson st
      st <- H.modify \st -> st { students = [ newStudent ] <> st.students }
      H.liftEffect $ Storage.setItem key (stateJson st) =<< Window.localStorage =<< HTML.window
      H.put st

    DeleteStudent idx -> do
      let stateJson st = Json.stringify $ Json.encodeJson st
      st <- H.modify \st -> st { students = deleteAt idx st.students }
      H.liftEffect $ Storage.setItem key (stateJson st) =<< Window.localStorage =<< HTML.window
      H.put st

deleteAt :: forall a. Int -> Array a -> Array a
deleteAt idx xs = 
  fromMaybe [] $ Array.deleteAt idx xs
   
  
button :: forall w i. { label :: String } -> HH.HTML w i
button { label } = HH.button [] [ HH.text label ]

render :: ParentState -> H.ComponentHTML ParentAction Slots Aff
render parentState =
  HH.div [ HP.classes [ HH.ClassName "bg-base-200 min-h-screen min-w-screen flex" ] ]
    [ HH.div [ class_ "bg-base-200 flex flex-col gap-8 m-auto max-w-screen-xl	w-full p-8" ]
        [ HH.div [ class_ "bg-base-200 flex flex-col gap-8 w-full" ]
            [ HH.div [ class_ "flex flex-col gap-8" ]
                [ HH.slot _form unit Form.form unit HandleForm
                , HH.div []
                    [ HH.p [ class_ "text-xl" ]
                        [ HH.p [ class_ "text-xl" ] [ HH.text "List of Students" ]
                        , HH.div [ class_ "overflow-x-auto" ]
                            [ HH.table [ class_ "table table-zebra table-xs" ]
                                [ HH.thead []
                                    [ HH.th_ [ HH.text "#" ]
                                    , HH.th_ [ HH.text "NFT Name" ]
                                    , HH.th_ [ HH.text "Name" ]
                                    , HH.th_ [ HH.text "Image" ]
                                    , HH.th_ [ HH.text "File" ]
                                    , HH.th_ [ HH.text "Wallet Address" ]
                                    , HH.th_ [ HH.text "Actions"]
                                    ]

                                , HH.tbody [] (renderStudents parentState.students)
                                ]

                            ]
                        ]
                    ]
                ]
            , HH.div []
                [ HH.slot _studentForm unit StudentForm.form unit HandleStudentForm ]
            ]
        , HH.div [ class_ "w-full bg-base-200" ]
            [ HH.h1 [ class_ "md:text-lg lg:text-2xl gap-8 flex justify-between my-4" ]
                [ HH.span_ [HH.text "NFT Metadata"]
                , HH.a [HP.href "https://cips.cardano.org/cip/CIP-25/", class_ "link link-info px-2"] [HH.text "CIP-25"] ]
            , HH.div [ class_ "mockup-code" ]
                [ HH.pre [ class_ "my-8" ]
                    [ HH.code_ [ HH.text fieldsJson ]
                    ]
                ]
            ]
        , HH.div [ class_ "w-full bg-base-500" ]
          [ HH.button [class_ "btn btn-lg btn-accent"] [ HH.text "Mint NFTs"]]
        ]
    ]
  where
  renderStudents xs = do
    Array.mapWithIndex renderStudent xs

  fieldsJson =
    Json.stringifyWithIndent 2 json

  json =
    Json.encodeJson
      $ policy "<policy_id>" metaData

  metaData :: Array MetaData
  metaData =
    case parentState.metaDataNFT of
      Just mdn ->
        map (fromMetaDataNFT mdn) parentState.students

      Nothing ->
        [] 



renderStudent :: forall w . Int -> StudentForm.Student -> HH.HTML w ParentAction
renderStudent ind { nftName, studentName, walletAddr, image, file } =
    HH.tr []
      [ HH.td_ [ HH.text $ show ind ]
      , HH.td_ [ HH.text nftName]
      , HH.td_ [ HH.text studentName ]
      , HH.td_ [ renderLongText image ]
      , HH.td_ [ renderLongText file ]
      , HH.td_ [ renderLongText walletAddr ]
      , HH.td  [ onClick \_ -> (DeleteStudent ind)]
        [ HH.button [class_ "btn btn-xs btn-ghost"] [ HH.text "delete"]]
      ]


-- rednerLongText :: forall w i. String -> HH.HTML w i

renderLongText :: forall w i. String -> HH.HTML w i 
renderLongText txt =
    HH.a [ HP.href txt ]
      [ HH.text $ String.take 17 txt <> "....." <> String.drop ((String.length txt) - 5) txt ]


-- students =
--   [ { studentName: "Ivan Ivanovich"
--     , walletAddr: "addr_test1qr08n9sv89qfl2su6k09n7h0468k0lw34v93yll9l96ndz7lnfaneyma0qh06m0eerlyus7qtav5nnjhxyrpk4x57c3s0dgpus"
--     , image: "ipfs://bafybeieabktidmdmli2rstzfemv76wzpyg3bn3youftmxzq7xkekidfb2q"
--     , file: "ipfs://QmY2pnUNSdMxBGLi1qgwZy8iWka2RSrJU72FPXYGtZhmk5"
--     }
--   , { studentName: "Ivan Ivanovich"
--     , walletAddr: "addr_test1qr08n9sv89qfl2su6k09n7h0468k0lw34v93yll9l96ndz7lnfaneyma0qh06m0eerlyus7qtav5nnjhxyrpk4x57c3s0dgpus"
--     , image: "ipfs://bafybeieabktidmdmli2rstzfemv76wzpyg3bn3youftmxzq7xkekidfb2q"
--     , file: "ipfs://QmY2pnUNSdMxBGLi1qgwZy8iWka2RSrJU72FPXYGtZhmk5"
--     }
--   , { studentName: "Ivan Ivanovich"
--     , walletAddr: "addr_test1qr08n9sv89qfl2su6k09n7h0468k0lw34v93yll9l96ndz7lnfaneyma0qh06m0eerlyus7qtav5nnjhxyrpk4x57c3s0dgpus"
--     , image: "ipfs://bafybeieabktidmdmli2rstzfemv76wzpyg3bn3youftmxzq7xkekidfb2q"
--     , file: "ipfs://QmY2pnUNSdMxBGLi1qgwZy8iWka2RSrJU72FPXYGtZhmk5"
--     }
--   ]

fromMetaDataNFT :: Form.MetaDataNFT -> StudentForm.Student -> MetaData
fromMetaDataNFT d s =
  let file = fileExm { src= s.file } in
  metaDataExm
    { courseName = d.courseName
    , courseDescription = d.courseDesc
    , courseStartDate = d.courseStartDate
    , courseEndDate = d.courseEndDate
    , issuer = d.orgName
    , issuerDescription = d.orgDesc
    -- personal
    , name = s.nftName
    , studentName = s.studentName
    , image = s.image
    , files = [file]
    }

-- CIP-25 Version 2
-- General structure
-- {
--   "721": {
--     "<policy_id>": {
--       "<asset_name>": {
--         "name": <string>,
--         "image": <uri | array>,
--         "mediaType": image/<mime_sub_type>,
--         "description": <string | array>,
--         "files": [{
--           "name": <string>,
--           "mediaType": <mime_type>,
--           "src": <uri | array>,
--           <other_properties>
--         }],
--         <other properties>
--       }
--     },
--     "version": 2 
--   }
-- }

-- {
--   "721": {
--     "<policy_id>": {
--       "<asset_name>": {
--         "name": "Диплом Магистра",
--         "image": "https://gateway.lighthouse.storage/ipfs/QmdheTcXUS1mbQbefENuwp6Y6YDocgymYpq4u4FA1GwSM9",
--         "mediaType": "image/jpg",
--         "description": "Министерство науки и высшего образования российской федерации приказ от 22 июля 2021 г. n 645 об утверждении образцов и описания документов о высшем образовании и о квалификации и приложений к ним",
--         "files": [{
--           "name": "pdfcertificate",
--           "mediaType": "application/pdf"
--           "src": "https://gateway.lighthouse.storage/ipfs/QmY2pnUNSdMxBGLi1qgwZy8iWka2RSrJU72FPXYGtZhmk5",
--         }],
--       }
--     },
--     "version": 2 
--   }
-- }

data NFT = NFT Policy

data Policy = Policy PolicyId (Array MetaData)

instance encodeJsonTeam :: EncodeJson Policy where
  encodeJson (Policy (PolicyId id) metaData ) =
    "721" :=
      ( "version" := "2"
          ~> id := map encodeMetaData metaData
          ~> jsonEmptyObject
      )
      ~>
        jsonEmptyObject


encodeMetaData :: MetaData -> Json.Json 
encodeMetaData metaData =
  let name = metaData.name in
  ( name := metaData ~> jsonEmptyObject)

policy :: String -> Array MetaData -> Policy
policy policyId metaData =
  Policy (PolicyId policyId) metaData 

toAssets :: StudentForm.Student -> Asset
toAssets {nftName, studentName, image, file } =
  { name: nftName
  , studentName: studentName
  , image: image
  , file: file
  }

data PolicyId = PolicyId String

type Asset =
  { name :: String
  , studentName :: String
  , image :: String
  , file :: String
  }

type MetaData =
  { name :: String
  , courseName :: String
  , courseDescription :: String
  , courseUrl :: String
  , courseStartDate :: String
  , courseEndDate :: String
  , issuer :: String
  , issuerDescription :: String
  , studentName :: String
  , image :: String
  , mediaType :: String
  , files :: Array File
  }

metaDataExm :: MetaData
metaDataExm =
  { name: "Диплом Магистра"
  , courseName: ""
  , courseDescription: "Министерство науки и высшего образования российской федерации приказ от 22 июля 2021 г. n 645 об утверждении образцов и описания документов о высшем образовании и о квалификации и приложений к ним"
  , courseUrl: ""
  , courseStartDate: ""
  , courseEndDate: ""
  , issuer: "ТУСУР"
  , issuerDescription: "Томский государственный университет систем управления и радиоэлектроники"
  , studentName: "Сидоров Петр Сергеевич"
  , image: "ipfs://bafybeieabktidmdmli2rstzfemv76wzpyg3bn3youftmxzq7xkekidfb2q"
  , mediaType: "image/webp"
  , files: [ fileExm ]
  }

type File =
  { name :: String
  , mediaType :: String
  , src :: String
  }

fileExm :: File
fileExm =
  { name: "pdfcertificate"
  , mediaType: "application/pdf"
  , src: "https://gateway.lighthouse.storage/ipfs/QmY2pnUNSdMxBGLi1qgwZy8iWka2RSrJU72FPXYGtZhmk5"
  }
