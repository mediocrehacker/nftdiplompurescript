module StudentForm where

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

type Slot id = H.Slot Query Output id

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( studentName :: f String String String
  , nftName :: f String String String
  , walletAddr :: f String String String
  , image :: f String String String
  , file :: f String String String
  --                input  error  output
  )

type Student = { | Form F.FieldOutput }

type Query :: forall k. k -> Type
type Query = Const Void

type Input = Unit

type Output = { newStudent :: Student }
-- component :: H.Component Query Input Output Aff

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Input Action

type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

type State = FormContext

form :: H.Component Query Input Output Aff
form = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: \context -> context
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }

  where
  handleAction
    :: Action
    -> H.HalogenM State Action () (F.FormOutput (Form F.FieldState) Output) Aff Unit
  handleAction = case _ of
    -- When we receive new form context we need to update our form state.
    Receive context ->
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
        { studentName: Right
        , nftName: Right
        , walletAddr: Right
        , image: Right
        , file: Right
        }

      handleSuccess :: Student -> H.HalogenM _ _ _ _ _ Unit
      handleSuccess student = do
        let
          output :: Output
          output = { newStudent: student }

        F.raise output

    F.handleSubmitValidate handleSuccess F.validate validation

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit
      , HP.classes [ HH.ClassName "flex flex-col gap-2 w-full" ]
      ]
      [ input "Student Name" (actions.studentName) (fields.studentName)
      , input "NFT Name" (actions.nftName) (fields.nftName)
      , input "Wallet Address" (actions.walletAddr) (fields.walletAddr)
      , input "Image (webp)" (actions.image) (fields.image)
      , input "File (pdf)" (actions.file) (fields.file)
      , HH.button
          [ HP.type_ HP.ButtonSubmit, class_ "btn btn-primary max-w-96 mt-4" ]
          [ HH.text "Add Student" ]
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
