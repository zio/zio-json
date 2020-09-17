package zio.json.data.twitter

import io.circe
import zio.json
import play.api.libs.{ json => Play }
import ai.x.play.json.{ Jsonx => Playx }
import ai.x.play.json.Encoders.encoder

case class Urls(
  url: String,
  expanded_url: String,
  display_url: String,
  indices: List[Int]
)
object Urls {
  implicit val jDecoder: json.Decoder[Urls] = json.DeriveDecoder.gen[Urls]
  implicit val jEncoder: json.Encoder[Urls] = json.DeriveEncoder.gen[Urls]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Urls] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Urls]
  implicit val circeEncoder: circe.Encoder[Urls] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Urls]
  implicit val playFormatter: Play.Format[Urls] = Play.Json.format[Urls]
}
case class Url(urls: List[Urls])
object Url {
  implicit val jDecoder: json.Decoder[Url] = json.DeriveDecoder.gen[Url]
  implicit val jEncoder: json.Encoder[Url] = json.DeriveEncoder.gen[Url]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Url] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Url]
  implicit val circeEncoder: circe.Encoder[Url] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Url]
  implicit val playFormatter: Play.Format[Url] = Play.Json.format[Url]
}

case class UserEntities(url: Url, description: Url)
object UserEntities {
  implicit val jDecoder: json.Decoder[UserEntities] = json.DeriveDecoder.gen[UserEntities]
  implicit val jEncoder: json.Encoder[UserEntities] = json.DeriveEncoder.gen[UserEntities]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[UserEntities] =
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
object UserMentions {
  implicit val jDecoder: json.Decoder[UserMentions] = json.DeriveDecoder.gen[UserMentions]
  implicit val jEncoder: json.Encoder[UserMentions] = json.DeriveEncoder.gen[UserMentions]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[UserMentions] =
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
object User {
  implicit val jDecoder: json.Decoder[User] = json.DeriveDecoder.gen[User]
  implicit val jEncoder: json.Encoder[User] = json.DeriveEncoder.gen[User]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[User] =
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
object Entities {
  implicit val jDecoder: json.Decoder[Entities] = json.DeriveDecoder.gen[Entities]
  implicit val jEncoder: json.Encoder[Entities] = json.DeriveEncoder.gen[Entities]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Entities] =
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
object RetweetedStatus {
  implicit val jDecoder: json.Decoder[RetweetedStatus] =
    json.DeriveDecoder.gen[RetweetedStatus]
  implicit val jEncoder: json.Encoder[RetweetedStatus] =
    json.DeriveEncoder.gen[RetweetedStatus]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[RetweetedStatus] =
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

object Tweet {
  implicit val zioJsonDecoder: json.Decoder[Tweet] = json.DeriveDecoder.gen[Tweet]
  implicit val zioJsonEncoder: json.Encoder[Tweet] = json.DeriveEncoder.gen[Tweet]
  implicit val customConfig: circe.generic.extras.Configuration =
    circe.generic.extras.Configuration.default
  implicit val circeDecoder: circe.Decoder[Tweet] =
    circe.generic.extras.semiauto.deriveConfiguredDecoder[Tweet]
  implicit val circeEncoder: circe.Encoder[Tweet] =
    circe.generic.extras.semiauto.deriveConfiguredEncoder[Tweet]
  implicit val playFormatter: Play.Format[Tweet] = Playx.formatCaseClass[Tweet]
}
