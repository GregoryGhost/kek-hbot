module HBot.Messangers.Telegram.Types where

import Data.Time
import Data.Aeson.TH
import Data.Aeson.Types as AesonT
import Data.Aeson.Casing
import Data.Aeson.Lens
import Data.Aeson.Parser
import Deriving.Aeson
import Control.Lens.TH
import GHC.Generics
import HBot.Settings


data User
  = User
      { _id :: Int,
        _isBot :: Bool,
        _firstName :: String,
        _username :: String,
        _canJoinGroups :: Bool,
        _canReadAllGroupMessages :: Bool,
        _supportsInlineQueries :: Bool
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings User

makeFieldsNoPrefix ''User

data ResultTelegram a
  = ResultTelegram
      { _ok :: Bool,
        _result :: a
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings (ResultTelegram a)

makeFieldsNoPrefix ''ResultTelegram

type GetMeResult = ResultTelegram User

-- -- Presents an incoming update
-- data Update = Update
--     {
--         updateId :: Int
--         -- New incoming message of any kind — text, photo, sticker, etc
--         , message :: Maybe Message
--         -- New version of a message that is known to the bot and was edited
--         , editedMessage :: Maybe Message
--         -- New version of a message that is known to the bot and was edited
--         , channelPost :: Maybe Message
--         -- New incoming channel post of any kind — text, photo, sticker, etc.
--         , editedChannelPost :: Maybe Message
--         -- -- New incoming inline query
--         -- , inlineQuery :: Maybe InlineQuery
--         -- -- The result of an inline query that was chosen by a user and sent to their chat partner.
--         -- -- Please see our documentation on the feedback collecting for details on how to enable these updates for your bot
--         -- , chosenInlineResult :: Maybe ChosenInlineResult
--         -- -- New incoming callback query
--         -- , callbackQuery :: Maybe CallbackQuery
--         -- -- New incoming shipping query. Only for invoices with flexible price
--         -- , shippingQuery :: Maybe ShippingQuery
--         -- -- New incoming pre-checkout query. Contains full information about checkout
--         -- , preCheckoutQuery :: Maybe PreCheckoutQuery
--         -- -- New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
--         -- , poll :: Maybe Poll
--         -- -- A user changed their answer in a non-anonymous poll. Bots receive new votes only in polls that were sent by the bot itself
--         -- , pollAnswer :: Maybe PollAnswer
--     } deriving (Show, Generic)

-- -- Represents a Telegram user or bot
-- data User = User
--   { -- Unique identifier for this user or bot
--     id :: Int
--     -- True, if this user is a bot
--     ,isBot :: Bool
--     -- User‘s or bot’s first name
--     ,firstName :: String
--     -- User‘s or bot’s last name
--     ,lastName :: Maybe String
--     -- User‘s or bot’s username
--     ,username :: Maybe String
--     -- IETF language tag of the user's language
--     ,languageCode :: Maybe String
--     } deriving (Show, Generic)

-- -- Represents a chat.
-- data Chat = Chat
--     {
--         -- Unique identifier for this chat.
--         id :: Int
--     } deriving (Show, Generic)

-- -- Represents a message
-- data Message = Message
--     {
--     -- Unique message identifier inside this chat
--     messageId :: Int
--     -- Sender, can be empty for messages sent to channels
--     , from :: Maybe User
--     -- Date the message was sent in Unix time
--     ,date :: DateTime
--     -- Conversation the message belongs to
--     ,chat :: Chat
--     -- -- For forwarded messages, sender of the original message
--     -- ,forwardFrom :: Maybe User
--     -- -- For messages forwarded from a channel, information about the original channel
--     -- ,forwardFromChat :: Maybe Chat
--     -- -- For forwarded channel posts, identifier of the original message in the channel
--     -- ,forwardFromMessageId :: Maybe Int
--     -- -- For messages forwarded from channels, signature of the post author if present
--     -- ,forwardSignature :: Maybe String
--     -- -- Sender's name for messages forwarded from users who disallow adding a link to their account in forwarded messages
--     -- ,forwardSenderName :: Maybe String
--     -- -- For forwarded messages, date the original message
--     -- ,forwardDate :: Maybe DateTime
--     -- -- For replies, the original message. 
--     -- -- Note that the Message object in this field will not contain further 
--     -- -- ReplyToMessage fields even if it itself is a reply.
--     -- ,replyToMessage :: Maybe Message
--     -- -- Date the message was last edited
--     -- ,editDate :: Maybe DateTime
--     -- -- Signature of the post author for messages in channels
--     -- ,authorSignature :: Maybe String
--     -- -- For text messages, the actual UTF-8 text of the message, 0-4096 characters.
--     -- ,text :: Maybe String
--     -- -- For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
--     -- ,entities :: Maybe [MessageEntity]
--     -- -- For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
--     -- ,captionEntities :: Maybe [MessageEntity]
--     -- -- Message is an audio file, information about the file
--     -- ,audio :: Maybe Audio
--     -- -- Message is a general file, information about the file
--     -- ,document:: Maybe Document
--     -- -- Message is an animation message, information about the animation
--     -- ,animation:: Maybe Animation
--     -- -- Message is a game, information about the game
--     -- ,game:: Maybe Game
--     -- -- Message is a photo, available sizes of the photo
--     -- ,photo:: Maybe [PhotoSize]
--     -- -- Message is a sticker, information about the sticker
--     -- ,sticker:: Maybe Sticker
--     -- -- Message is a video, information about the video
--     -- ,video:: Maybe Video
--     -- -- Message is a voice message, information about the file
--     -- ,voice:: Maybe Voice
--     -- -- Message is a video note, information about the video message
--     -- ,videoNote:: Maybe VideoNote
--     -- -- Caption for the document, photo or video, 0-200 characters
--     -- ,caption:: Maybe String
--     -- -- Message is a shared contact, information about the contact
--     -- ,contact:: Maybe Contact
--     -- -- Message is a shared location, information about the location
--     -- ,location:: Maybe Location
--     -- -- Message is a venue, information about the venue
--     -- ,venue:: Maybe Venue
--     -- -- Message is a native poll, information about the poll
--     -- ,poll:: Maybe Poll
--     -- -- New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
--     -- ,newChatMembers:: Maybe [User]
--     -- -- A member was removed from the group, information about them (this member may be the bot itself)
--     -- ,leftChatMember:: Maybe User
--     -- -- A chat title was changed to this value
--     -- ,newChatTitle:: Maybe String
--     -- -- A chat photo was change to this value
--     -- ,newChatPhoto:: Maybe [PhotoSize]
--     -- -- Service message:: the chat photo was deleted
--     -- ,deleteChatPhoto:: Maybe Bool
--     -- -- Service message:: the group has been created
--     -- ,groupChatCreated:: Maybe Bool
--     -- -- Service message:: the supergroup has been created. This field can‘t be received 
--     -- -- in a message coming through updates, because bot can’t be a member of a supergroup 
--     -- -- when it is created. It can only be found in ReplyToMessage if someone replies to a 
--     -- -- very first message in a directly created supergroup.
--     -- ,supergroupChatCreated:: Maybe Bool
--     -- -- Service message: the channel has been created. This field can‘t be received 
--     -- -- in a message coming through updates, because bot can’t be a member of a channel 
--     -- -- when it is created. It can only be found in ReplyToMessage if someone replies 
--     -- -- to a very first message in a channel.
--     -- ,channelChatCreated:: Maybe Bool
--     -- -- The group has been migrated to a supergroup with the specified identifier
--     -- ,migrateToChatId:: Maybe Int
--     -- -- The supergroup has been migrated from a group with the specified identifier
--     -- ,migrateFromChatId:: Maybe Int
--     -- -- Specified message was pinned. Note that the Message object in this field will not contain further ReplyToMessage fields even if it is itself a reply.
--     -- ,pinnedMessage:: Maybe Message
--     -- -- Message is an invoice for a payment, information about the invoice
--     -- ,invoice:: Maybe Invoice
--     -- -- Message is a service message about a successful payment, information about the payment
--     -- ,successfulPayment:: Maybe SuccessfulPayment
--     -- -- The domain name of the website on which the user has logged in.
--     -- ,connectedWebsite:: Maybe String
--     -- -- Telegram Passport data
--     -- ,passportData :: Maybe PassportData
--     -- -- Inline keyboard attached to the message. login_url buttons are represented as ordinary url buttons
--     -- ,replyMarkup :: Maybe InlineKeyboardMarkup
--     } deriving (Show, Generic)