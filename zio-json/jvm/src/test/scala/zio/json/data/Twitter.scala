package testzio.json.data.twitter

import ai.x.play.json.Encoders.encoder
import ai.x.play.json.{ Jsonx => Playx }
import com.github.ghik.silencer.silent
import io.circe
import play.api.libs.{ json => Play }
import zio.json._

case class Urls(
  url: String,
  expanded_url: String,
  display_url: String,
  indices: List[Int]
)
@silent("Block result was adapted via implicit conversion")
object Urls {
  implicit val jJsonDecoder: JsonDecoder[Urls] = DeriveJsonDecoder.gen[Urls]
  implicit val jEncoder: JsonEncoder[Urls]     = DeriveJsonEncoder.gen[Urls]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Urls] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Urls]
  implicit val circeEncoder: circe.Encoder[Urls] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Urls]
  implicit val playFormatter: Play.Format[Urls] = Play.Json.format[Urls]
}
case class Url(urls: List[Urls])
@silent("Block result was adapted via implicit conversion")
object Url {
  implicit val jJsonDecoder: JsonDecoder[Url] = DeriveJsonDecoder.gen[Url]
  implicit val jEncoder: JsonEncoder[Url]     = DeriveJsonEncoder.gen[Url]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Url] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Url]
  implicit val circeEncoder: circe.Encoder[Url] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Url]
  implicit val playFormatter: Play.Format[Url] = Play.Json.format[Url]
}

case class UserEntities(url: Url, description: Url)
@silent("Block result was adapted via implicit conversion")
object UserEntities {
  implicit val jJsonDecoder: JsonDecoder[UserEntities] = DeriveJsonDecoder.gen[UserEntities]
  implicit val jEncoder: JsonEncoder[UserEntities]     = DeriveJsonEncoder.gen[UserEntities]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[UserEntities] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[UserEntities]
  implicit val circeEncoder: circe.Encoder[UserEntities] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[UserEntities]
  implicit val playFormatter: Play.Format[UserEntities] =
    Play.Json.format[UserEntities]
}

case class UserMentions(
  screen_name: String,
  name: String,
  id: Long,
  id_str: String,
  indices: List[Int]
)
@silent("Block result was adapted via implicit conversion")
object UserMentions {
  implicit val jJsonDecoder: JsonDecoder[UserMentions] = DeriveJsonDecoder.gen[UserMentions]
  implicit val jEncoder: JsonEncoder[UserMentions]     = DeriveJsonEncoder.gen[UserMentions]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[UserMentions] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[UserMentions]
  implicit val circeEncoder: circe.Encoder[UserMentions] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[UserMentions]
  implicit val playFormatter: Play.Format[UserMentions] =
    Play.Json.format[UserMentions]
}

case class User(
  id: Long,
  id_str: String,
  name: String,
  screen_name: String,
  location: String,
  description: String,
  url: String,
  entities: UserEntities,
  `protected`: Boolean,
  followers_count: Int,
  friends_count: Int,
  listed_count: Int,
  created_at: String,
  favourites_count: Int,
  utc_offset: Int,
  time_zone: String,
  geo_enabled: Boolean,
  verified: Boolean,
  statuses_count: Int,
  lang: String,
  contributors_enabled: Boolean,
  is_translator: Boolean,
  is_translation_enabled: Boolean,
  profile_background_color: String,
  profile_background_image_url: String,
  profile_background_image_url_https: String,
  profile_background_tile: Boolean,
  profile_image_url: String,
  profile_image_url_https: String,
  profile_banner_url: String,
  profile_link_color: String,
  profile_sidebar_border_color: String,
  profile_sidebar_fill_color: String,
  profile_text_color: String,
  profile_use_background_image: Boolean,
  has_extended_profile: Boolean,
  default_profile: Boolean,
  default_profile_image: Boolean,
  following: Boolean,
  follow_request_sent: Boolean,
  notifications: Boolean,
  translator_type: String
)
@silent("Block result was adapted via implicit conversion")
object User {
  implicit val jJsonDecoder: JsonDecoder[User] = DeriveJsonDecoder.gen[User]
  implicit val jEncoder: JsonEncoder[User]     = DeriveJsonEncoder.gen[User]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[User] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[User]
  implicit val circeEncoder: circe.Encoder[User] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[User]
  implicit val playFormatter: Play.Format[User] = Playx.formatCaseClass[User]
}

case class Entities(
  hashtags: List[String],
  symbols: List[String],
  user_mentions: List[UserMentions],
  urls: List[Urls]
)
@silent("Block result was adapted via implicit conversion")
object Entities {
  implicit val jJsonDecoder: JsonDecoder[Entities] = DeriveJsonDecoder.gen[Entities]
  implicit val jEncoder: JsonEncoder[Entities]     = DeriveJsonEncoder.gen[Entities]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Entities] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Entities]
  implicit val circeEncoder: circe.Encoder[Entities] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Entities]
  implicit val playFormatter: Play.Format[Entities] = Play.Json.format[Entities]
}

case class RetweetedStatus(
  created_at: String,
  id: Long,
  id_str: String,
  text: String,
  truncated: Boolean,
  entities: Entities,
  source: String,
  in_reply_to_status_id: Option[String],
  in_reply_to_status_id_str: Option[String],
  in_reply_to_user_id: Option[String],
  in_reply_to_user_id_str: Option[String],
  in_reply_to_screen_name: Option[String],
  user: User,
  geo: Option[String],
  coordinates: Option[String],
  place: Option[String],
  contributors: Option[String],
  is_quote_status: Boolean,
  retweet_count: Int,
  favorite_count: Int,
  favorited: Boolean,
  retweeted: Boolean,
  possibly_sensitive: Boolean,
  lang: String
)
@silent("Block result was adapted via implicit conversion")
object RetweetedStatus {
  implicit val jJsonDecoder: JsonDecoder[RetweetedStatus] =
    DeriveJsonDecoder.gen[RetweetedStatus]
  implicit val jEncoder: JsonEncoder[RetweetedStatus] =
    DeriveJsonEncoder.gen[RetweetedStatus]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[RetweetedStatus] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[RetweetedStatus]
  implicit val circeEncoder: circe.Encoder[RetweetedStatus] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[RetweetedStatus]
  implicit val playFormatter: Play.Format[RetweetedStatus] =
    Playx.formatCaseClass[RetweetedStatus]
}

case class Tweet(
  created_at: String,
  id: Long,
  id_str: String,
  text: String,
  truncated: Boolean,
  entities: Entities,
  source: String,
  in_reply_to_status_id: Option[String],
  in_reply_to_status_id_str: Option[String],
  in_reply_to_user_id: Option[String],
  in_reply_to_user_id_str: Option[String],
  in_reply_to_screen_name: Option[String],
  user: User,
  geo: Option[String],
  coordinates: Option[String],
  place: Option[String],
  contributors: Option[String],
  retweeted_status: RetweetedStatus,
  is_quote_status: Boolean,
  retweet_count: Int,
  favorite_count: Int,
  favorited: Boolean,
  retweeted: Boolean,
  possibly_sensitive: Boolean,
  lang: String
)

@silent("Block result was adapted via implicit conversion")
object Tweet {
  implicit val zioJsonJsonDecoder: JsonDecoder[Tweet] = DeriveJsonDecoder.gen[Tweet]
  implicit val zioJsonEncoder: JsonEncoder[Tweet]     = DeriveJsonEncoder.gen[Tweet]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeJsonDecoder: circe.Decoder[Tweet] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Tweet]
  implicit val circeEncoder: circe.Encoder[Tweet] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Tweet]
  implicit val playFormatter: Play.Format[Tweet] = Playx.formatCaseClass[Tweet]
}
