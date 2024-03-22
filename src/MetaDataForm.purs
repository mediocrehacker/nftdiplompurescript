module MetaDataForm where

import Data.Argonaut as Json
import Data.Either (Either(..), note)
import Effect.Class.Console as Console
import Prelude
import Helpers (class_)
import Formless as F
import Effect.Aff (Aff)
import Data.Either (Either(..))
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
-- import Web.HTML.Common (AttrName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Const (Const)
import Web.HTML as HTML
import Web.Storage.Storage as Storage
import Web.HTML.Window as Window

type Slot id = H.Slot Query Output id

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( orgName :: f String String String
  , orgDesc :: f String String String
  , courseName :: f String String String
  , courseDesc :: f String String String
  , courseUrl :: f String String String
  , courseStartDate :: f String String String
  , courseEndDate :: f String String String
  --                input  error  output
  )

type MetaDataNFT = { | Form F.FieldOutput }

type Query :: forall k. k -> Type
type Query = Const Void

type Input = Unit

type Output = { newMetaDataNFT :: MetaDataNFT }
-- component :: H.Component Query Input Output Aff

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Initialize
  | Receive FormContext
  | Eval FormlessAction

type State = FormContext

form :: H.Component Query Input Output Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , initialize = Just Initialize
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

  where
  key :: String
  key = "local-storage-form"

  handleAction
    :: Action
    -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) Aff Unit
  handleAction = case _ of
    Initialize -> do
      storedState <- H.liftEffect $ Storage.getItem key =<< Window.localStorage =<< HTML.window
      case Json.decodeJson =<< Json.parseJson =<< note (Json.TypeMismatch "No data") storedState of
        Left err ->
          Console.log $ Json.printJsonDecodeError err
        Right fields -> do
          setFields <- H.gets _.formActions.setFields
          handleAction $ setFields fields

    -- When we receive new form context we need to update our form state.
    Receive context -> do
      let fieldsJson = Json.stringify $ Json.encodeJson context.fields
      H.liftEffect $ Storage.setItem key fieldsJson =<< Window.localStorage =<< HTML.window
      H.put context

    -- When a `FormlessAction` has been triggered we must raise it up to
    -- Formless for evaluation. We can do this with `F.eval`.
    Eval action ->
      F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      validation :: { | Form F.FieldValidation }
      validation =
        { orgName: Right
        , orgDesc: Right
        , courseName: Right
        , courseDesc: Right
        , courseUrl: Right
        , courseStartDate: Right
        , courseEndDate: Right
        }

      handleSuccess :: MetaDataNFT -> H.HalogenM _ _ _ _ _ Unit
      handleSuccess metaDataNFT = do
        let
          output :: Output
          output = { newMetaDataNFT: metaDataNFT }

        F.raise output

    F.handleSubmitValidate handleSuccess F.validate validation

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit
      , HP.classes [ HH.ClassName "flex flex-col gap-2 w-full" ]
      ]
      [ input "Organization Name" (actions.orgName) (fields.orgName)
      , textarea "Organization Description" (actions.orgDesc) (fields.orgDesc)
      , input "Course Name" (actions.courseName) (fields.courseName)
      , textarea "Course Description" (actions.courseDesc) (fields.courseDesc)
      , input "Course Url" (actions.courseUrl) (fields.courseUrl)
      , input "Course Start Date" (actions.courseStartDate) (fields.courseStartDate)
      , input "Course End Date" (actions.courseEndDate) (fields.courseEndDate)
      ,HH.button
          [ HP.type_ HP.ButtonSubmit, class_ "btn btn-primary max-w-96 mt-4" ]
          [ HH.text "Save" ]
      ]

  input label action field =
    HH.div_
      [ HH.label [ HP.classes [ HH.ClassName "label" ] ] [ HH.span [ HP.classes [ HH.ClassName "label-text" ] ] [ HH.text label ] ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.classes [ HH.ClassName "input w-full max-w-xl input-bordered" ]
          , HP.value field.value
          , HE.onValueInput action.handleChange
          -- , HH.attr (AttrName "required") ""
          ]
      , case field.result of
          Just (Left err) -> HH.small_ [ HH.text err ]
          _ -> HH.text ""
      ]

  textarea label action field =
    HH.div_
      [ HH.label [ HP.classes [ HH.ClassName "label" ] ] [ HH.span [ HP.classes [ HH.ClassName "label-text" ] ] [ HH.text label ] ]
      , HH.textarea
          [ HP.classes [ HH.ClassName "textarea w-full max-w-xl textarea-bordered" ]
          , HP.value field.value
          , HE.onValueInput action.handleChange
          -- , HH.attr (AttrName "required") ""
          ]
      , case field.result of
          Just (Left err) -> HH.small_ [ HH.text err ]
          _ -> HH.text ""
      ]
