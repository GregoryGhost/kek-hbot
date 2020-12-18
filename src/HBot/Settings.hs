module HBot.Settings
  ( JsonSettings,
    ChatJsonSettings,
  )
where

import Data.Aeson.Casing
import Data.Aeson.Lens
import Data.Aeson.Parser
import Data.Typeable
import Deriving.Aeson

type JsonSettings = CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "_", CamelToSnake)]

type ChatJsonSettings = CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "_update", CamelToSnake)]
