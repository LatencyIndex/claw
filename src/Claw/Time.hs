module Claw.Time (
    Duration,
    TimeZone,
    UTCTime,
    ZonedTime,
    getCurrentTimeZone,
    showDuration,
    showTime,
    utcToZonedTime,
    hmsToDuration,
) where

import Data.Time.Clock (DiffTime, UTCTime, secondsToDiffTime)
import Data.Time.Format (FormatTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, ZonedTime, getCurrentTimeZone, utcToZonedTime)

type Duration = DiffTime

-- | Format according to format string, per https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime
fmtTime :: (FormatTime t) => String -> t -> String
fmtTime = formatTime defaultTimeLocale

-- | Show time in yyyy-mm-dd hh:mm format.
showTime :: (FormatTime t) => t -> String
showTime = fmtTime "%Y-%m-%d %H:%M"

-- | Show duration in h:mm:ss format.
showDuration :: Duration -> String
showDuration = fmtTime "%h:%M:%S"

-- | Convert hours:minutes:seconds to a Duration.
hmsToDuration :: Int -> Int -> Int -> Duration
hmsToDuration h m s = secondsToDiffTime $ fromIntegral $ 3600 * h + 60 * m + s
