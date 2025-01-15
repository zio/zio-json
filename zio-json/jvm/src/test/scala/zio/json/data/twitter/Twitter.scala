package zio.json.data.twitter

import com.github.ghik.silencer.silent
import io.circe
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
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

  implicit val circeCodec: Codec[Urls] = deriveCodec
}
case class Url(urls: List[Urls])
@silent("Block result was adapted via implicit conversion")
object Url {
  implicit val jJsonDecoder: JsonDecoder[Url] = DeriveJsonDecoder.gen[Url]
  implicit val jEncoder: JsonEncoder[Url]     = DeriveJsonEncoder.gen[Url]

  implicit val circeCodec: Codec[Url] = deriveCodec
}

case class UserEntities(url: Url, description: Url)
@silent("Block result was adapted via implicit conversion")
object UserEntities {
  implicit val jJsonDecoder: JsonDecoder[UserEntities] = DeriveJsonDecoder.gen[UserEntities]
  implicit val jEncoder: JsonEncoder[UserEntities]     = DeriveJsonEncoder.gen[UserEntities]

  implicit val circeCodec: Codec[UserEntities] = deriveCodec
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

  implicit val circeCodec: Codec[UserMentions] = deriveCodec
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

  implicit val circeCodec: Codec[User] = deriveCodec
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

  implicit val circeCodec: Codec[Entities] = deriveCodec
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

  implicit val circeCodec: Codec[RetweetedStatus] = deriveCodec
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

  implicit val circeCodec: Codec[Tweet] = deriveCodec
}
