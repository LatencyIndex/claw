module Claw.Time (
    TimeZone,
    UTCTime,
    ZonedTime,
    getCurrentTimeZone,
    showTime,
    utcToZonedTime,
) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (FormatTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, getCurrentTimeZone, utcToZonedTime)

-- | Show time in yyyy-mm-dd hh:mm format.
showTime :: (FormatTime t) => t -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
