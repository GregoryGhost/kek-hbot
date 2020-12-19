module HBot.Messangers.Telegram.Types where

import Control.Lens
import Control.Lens.TH
import Data.Aeson.Casing
import Data.Aeson.Lens
import Data.Aeson.Parser
import Data.Aeson.TH
import Data.Aeson.Types as AesonT
import Data.DateTime
import Deriving.Aeson
import GHC.Generics
import HBot.Settings

-- Represents a Telegram user or bot
data User = User
  { -- Unique identifier for this user or bot
    _id :: Int,
    -- True, if this user is a bot
    _isBot :: Bool,
    -- User‘s or bot’s first name
    _firstName :: String,
    -- User‘s or bot’s last name
    _lastName :: Maybe String,
    -- User‘s or bot’s username
    _username :: Maybe String,
    -- IETF language tag of the user's language
    _languageCode :: Maybe String,
    -- True, if the bot can be invited to groups
    _canJoinGroups :: Bool,
    -- True, if privacy mode is disabled for the bot
    _canReadAllGroupMessages :: Bool,
    -- True, if the bot supports inline queries
    _supportsInlineQueries :: Bool
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings User

data ResultTelegram a = ResultTelegram
  { _ok :: Bool,
    _result :: a
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings (ResultTelegram a)

type GetMeResult = ResultTelegram User

-- Represents a message
data Message = Message
  { -- Unique message identifier inside this chat
    _messageId :: Int,
    -- Sender, can be empty for messages sent to channels
    _from :: Maybe User,
    -- Date the message was sent in Unix time
    _date :: DateTime,
    -- Conversation the message belongs to
    _chat :: Chat,
    -- -- For forwarded messages, sender of the original message
    _forwardFrom :: Maybe User,
    -- For messages forwarded from a channel, information about the original channel
    _forwardFromChat :: Maybe Chat,
    -- For forwarded channel posts, identifier of the original message in the channel
    _forwardFromMessageId :: Maybe Int,
    -- For messages forwarded from channels, signature of the post author if present
    _forwardSignature :: Maybe String,
    -- Sender's name for messages forwarded from users who disallow adding a link to their account in forwarded messages
    _forwardSenderName :: Maybe String,
    -- For forwarded messages, date the original message
    _forwardDate :: Maybe DateTime,
    -- For replies, the original message.
    -- Note that the Message object in this field will not contain further
    -- ReplyToMessage fields even if it itself is a reply.
    _replyToMessage :: Maybe Message,
    -- Date the message was last edited
    _editDate :: Maybe DateTime,
    -- Signature of the post author for messages in channels
    _authorSignature :: Maybe String,
    -- For text messages, the actual UTF-8 text of the message, 0-4096 characters.
    _text :: Maybe String,
    -- -- For text messages, special entities like usernames, URLs, bot commands, etc. that appear in the text
    -- ,entities :: Maybe [MessageEntity]
    -- -- For messages with a caption, special entities like usernames, URLs, bot commands, etc. that appear in the caption
    -- ,captionEntities :: Maybe [MessageEntity]
    -- -- Message is an audio file, information about the file
    -- ,audio :: Maybe Audio
    -- -- Message is a general file, information about the file
    -- ,document:: Maybe Document
    -- -- Message is an animation message, information about the animation
    -- ,animation:: Maybe Animation
    -- -- Message is a game, information about the game
    -- ,game:: Maybe Game
    -- -- Message is a photo, available sizes of the photo
    -- ,photo:: Maybe [PhotoSize]
    -- -- Message is a sticker, information about the sticker
    -- ,sticker:: Maybe Sticker
    -- -- Message is a video, information about the video
    -- ,video:: Maybe Video
    -- -- Message is a voice message, information about the file
    -- ,voice:: Maybe Voice
    -- -- Message is a video note, information about the video message
    -- ,videoNote:: Maybe VideoNote
    -- Caption for the document, photo or video, 0-200 characters
    _caption :: Maybe String,
    -- -- Message is a shared contact, information about the contact
    -- ,contact:: Maybe Contact
    -- Message is a shared location, information about the location
    _location :: Maybe Location,
    -- -- Message is a venue, information about the venue
    -- ,venue:: Maybe Venue
    -- Message is a native poll, information about the poll
    _poll :: Maybe Poll,
    -- New members that were added to the group or supergroup and information about them (the bot itself may be one of these members)
    _newChatMembers :: Maybe [User],
    -- A member was removed from the group, information about them (this member may be the bot itself)
    _leftChatMember :: Maybe User,
    -- A chat title was changed to this value
    _newChatTitle :: Maybe String,
    -- -- A chat photo was change to this value
    -- ,newChatPhoto:: Maybe [PhotoSize]
    -- Service message:: the chat photo was deleted
    _deleteChatPhoto :: Maybe Bool,
    -- Service message:: the group has been created
    _groupChatCreated :: Maybe Bool,
    -- Service message:: the supergroup has been created. This field can‘t be received
    -- in a message coming through updates, because bot can’t be a member of a supergroup
    -- when it is created. It can only be found in ReplyToMessage if someone replies to a
    -- very first message in a directly created supergroup.
    _supergroupChatCreated :: Maybe Bool,
    -- Service message: the channel has been created. This field can‘t be received
    -- in a message coming through updates, because bot can’t be a member of a channel
    -- when it is created. It can only be found in ReplyToMessage if someone replies
    -- to a very first message in a channel.
    _channelChatCreated :: Maybe Bool,
    -- The group has been migrated to a supergroup with the specified identifier
    _migrateToChatId :: Maybe Int,
    -- The supergroup has been migrated from a group with the specified identifier
    _migrateFromChatId :: Maybe Int,
    -- Specified message was pinned. Note that the Message object in this field will not contain further ReplyToMessage fields even if it is itself a reply.
    _pinnedMessage :: Maybe Message,
    -- -- Message is an invoice for a payment, information about the invoice
    -- ,invoice:: Maybe Invoice
    -- -- Message is a service message about a successful payment, information about the payment
    -- ,successfulPayment:: Maybe SuccessfulPayment
    -- The domain name of the website on which the user has logged in.
    _connectedWebsite :: Maybe String
    -- -- Telegram Passport data
    -- ,passportData :: Maybe PassportData
    -- -- Inline keyboard attached to the message. login_url buttons are represented as ordinary url buttons
    -- ,replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings Message

-- This object represents a point on the map
data Location = Location
  { -- Longitude as defined by sender
    _longitude :: Double,
    -- Latitude as defined by sender
    _latitude :: Double
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings Location

-- This object represents an incoming inline query.
-- When the user sends an empty query, your bot could return some default or trending results.
data InlineQuery = InlineQuery
  { -- Unique identifier for this query
    _id :: String,
    -- Sender
    _from :: User,
    -- Sender location, only for bots that request user location
    _location :: Maybe Location,
    -- Text of the query (up to 512 characters)
    _query :: String,
    -- Offset of the results to be returned, can be controlled by the bot
    _offset :: String
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings InlineQuery

-- Represents a result of an inline query that was chosen by the user and sent to their chat partner.
data ChosenInlineResult = ChosenInlineResult
  { -- The unique identifier for the result that was chosen
    _resultId :: String,
    -- The user that chose the result
    _from :: User,
    -- Sender location, only for bots that require user location
    _location :: Maybe Location,
    -- Identifier of the sent inline message. Available only if there is an inline keyboard attached to the message.
    -- Will be also received in callback queries and can be used to edit the message.
    _inlineMessageId :: Maybe String,
    -- The query that was used to obtain the result
    _query :: String
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings ChosenInlineResult

-- This object represents an incoming callback query from a callback button in an inline keyboard. If the button that originated the query was attached to a message sent by the bot, the field message will be present.  If the button was attached to a message sent via the bot (in inline mode), the field InlineMessageId will be present. Exactly one of the fields data or GameShortName will be present.
data CallbackQuery = CallbackQuery
  { -- Unique identifier for this query
    _cbId :: String,
    -- Sender
    _cbFrom :: User,
    -- Message with the callback button that originated the query.
    -- Note that message content and message date will not be available if the message is too old
    _cbMessage :: Maybe Message,
    -- Identifier of the message sent via the bot in inline mode, that originated the query.
    _cbInlineMessageId :: Maybe String,
    -- Global identifier, uniquely corresponding to the chat to which the message
    -- with the callback button was sent. Useful for high scores in games.
    _cbChatInstance :: String,
    -- Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
    _cbData :: Maybe String,
    -- Short name of a Game to be returned, serves as the unique identifier for the game
    _cbGameShortName :: Maybe String
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CallbackQueryJsonSettings CallbackQuery

-- This object represents a shipping address
data ShippingAddress = ShippingAddress
  { -- ISO 3166-1 alpha-2 country code
    _countryCode :: String,
    -- State, if applicable
    _state :: String,
    -- City
    _city :: String,
    -- First line for the address
    _streetLine1 :: String,
    -- Second line for the address
    _streetLine2 :: String,
    -- Address post code
    _postCode :: String
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings ShippingAddress

-- This object contains information about an incoming shipping query.
data ShippingQuery = ShippingQuery
  { -- Unique query identifier
    _id :: String,
    -- User who sent the query
    _from :: User,
    -- Bot specified invoice payload
    _invoicePayload :: String,
    -- User specified shipping address
    _shippingAddress :: ShippingAddress
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings ShippingQuery

-- This object represents information about an order.
data OrderInfo = OrderInfo
  { -- User name
    _name :: Maybe String,
    -- Optional. User's phone number
    _phoneNumber :: Maybe String,
    -- User email
    _email :: Maybe String,
    -- User shipping address
    _shippingAddress :: Maybe ShippingAddress
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings OrderInfo

-- This object contains information about an incoming pre-checkout query
data PreCheckoutQuery = PreCheckoutQuery
  { -- Unique query identifier
    _id :: String,
    -- User who sent the query
    _from :: User,
    -- Three-letter ISO 4217 currency code
    _currency :: String,
    -- Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
    _totalAmount :: Int,
    -- Bot specified invoice payload
    _invoicePayload :: String,
    -- Identifier of the shipping option chosen by the user
    _shippingOptionId :: Maybe String,
    -- Order info provided by the user
    _orderInfo :: Maybe OrderInfo
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings PreCheckoutQuery

-- This object contains information about one answer option in a poll.
data PollOption = PollOption
  { -- Option text, 1-100 characters
    _text :: String,
    -- Number of users that voted for this option
    _voterCount :: Int
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings PollOption

-- This object contains information about a poll
data Poll = Poll
  { -- Unique poll identifier
    _id :: String,
    -- Poll question, 1-255 characters
    _question :: String,
    -- List of poll options
    _options :: [PollOption],
    -- True, if the poll is closed
    _isClosed :: Bool
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings Poll

-- Presents an incoming update
data Update = Update
  { -- The update's unique identifier
    _updateId :: Int,
    -- New incoming message of any kind — text, photo, sticker, etc
    _message :: Maybe Message,
    -- New version of a message that is known to the bot and was edited
    _editedMessage :: Maybe Message,
    -- New version of a message that is known to the bot and was edited
    _channelPost :: Maybe Message,
    -- New incoming channel post of any kind — text, photo, sticker, etc
    _editedChannelPost :: Maybe Message,
    -- New incoming inline query
    _inlineQuery :: Maybe InlineQuery,
    -- The result of an inline query that was chosen by a user and sent to their chat partner.
    -- Please see our documentation on the feedback collecting for details on how to enable these updates for your bot
    _chosenInlineResult :: Maybe ChosenInlineResult,
    -- New incoming callback query
    _callbackQuery :: Maybe CallbackQuery,
    -- New incoming shipping query. Only for invoices with flexible price
    _shippingQuery :: Maybe ShippingQuery,
    -- New incoming pre-checkout query. Contains full information about checkout
    _preCheckoutQuery :: Maybe PreCheckoutQuery,
    -- New poll state. Bots receive only updates about stopped polls and polls, which are sent by the bot
    _poll :: Maybe Poll
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings Update

data ChatType = Private | Group | Supergroup | Channel | Unknown
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings ChatType

--- Describes actions that a non-administrator user is allowed to take in a chat.
data ChatPermissions = ChatPermissions
  { --- True, if the user is allowed to send text messages, contacts, locations and venues
    _canSendMessages :: Maybe Bool,
    --- True, if the user is allowed to send audios, documents, photos, videos, video notes and voice notes, implies can_send_messages
    _canSendMediaMessages :: Maybe Bool,
    --- True, if the user is allowed to send polls, implies can_send_messages
    _canSendPools :: Maybe Bool,
    --- True, if the user is allowed to send animations, games, stickers and use inline bots, implies can_send_media_messages
    _canSendOtherMessages :: Maybe Bool,
    --- True, if the user is allowed to add web page previews to their messages, implies can_send_media_messages
    _canAddWebPagePreviews :: Maybe Bool,
    --- True, if the user is allowed to change the chat title, photo and other settings. Ignored in public supergroups
    _canChangeInfo :: Maybe Bool,
    --- True, if the user is allowed to invite new users to the chat
    _canInviteUsers :: Maybe Bool,
    --- True, if the user is allowed to pin messages. Ignored in public supergroups
    _canPinMessages :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings ChatPermissions

-- This object represents a chat photo
data ChatPhoto = ChatPhoto
  { -- Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
    _smallFileId :: String,
    -- Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
    _bigFileId :: String
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings ChatPhoto

-- Represents a chat.
data Chat = Chat
  { -- Unique identifier for this chat.
    _chatId :: Int,
    -- Type of chat, can be either "private", "group", "supergroup" or "channel"
    _chatType :: ChatType,
    -- Title, for supergroups, channels and group chats
    _chatTitle :: Maybe String,
    -- Username, for private chats, supergroups and channels if available
    _chatUsername :: Maybe String,
    -- First name of the other party in a private chat
    _chatFirstName :: Maybe String,
    -- Last name of the other party in a private chat
    _chatLastName :: Maybe String,
    -- True if a group has ‘All Members Are Admins’ enabled.
    _chatAllMembersAreAdministrators :: Maybe Bool,
    -- Chat photo. Returned only in getChat
    _chatPhoto :: Maybe ChatPhoto,
    -- Description, for supergroups and channel chats. Returned only in getChat.
    _chatDescription :: Maybe String,
    -- Chat invite link, for supergroups and channel chats. Returned only in getChat.
    _chatInviteLink :: Maybe String,
    -- Pinned message, for supergroups. Returned only in getChat.
    _chatPinnedMessage :: Maybe Message,
    -- Default chat member permissions, for groups and supergroups
    _chatPermissions :: Maybe ChatPermissions,
    -- For supergroups, name of group sticker set
    _chatStickerSetName :: Maybe String,
    -- True, if the bot can change the group sticker set.
    _chatCanSetStickerSet :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via ChatJsonSettings Chat

makeFieldsNoPrefix ''User
makeFieldsNoPrefix ''ResultTelegram
makeFieldsNoPrefix ''Message
makeFieldsNoPrefix ''Location
makeFieldsNoPrefix ''InlineQuery
makeFieldsNoPrefix ''ChosenInlineResult
makeFieldsNoPrefix ''CallbackQuery
makeFieldsNoPrefix ''ShippingAddress
makeFieldsNoPrefix ''ShippingQuery
makeFieldsNoPrefix ''OrderInfo
makeFieldsNoPrefix ''PreCheckoutQuery
makeFieldsNoPrefix ''PollOption
makeFieldsNoPrefix ''Poll
makeFieldsNoPrefix ''Update
makeFieldsNoPrefix ''Chat
makeFieldsNoPrefix ''ChatType
makeFieldsNoPrefix ''ChatPermissions
makeFieldsNoPrefix ''ChatPhoto
