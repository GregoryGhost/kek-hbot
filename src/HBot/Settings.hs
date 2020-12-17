module HBot.Settings
  ( JsonSettings,
  )
where

import Data.Aeson.Casing
import Data.Aeson.Lens
import Data.Aeson.Parser
import Data.Typeable
import Deriving.Aeson

type JsonSettings = CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "_", CamelToSnake)]
