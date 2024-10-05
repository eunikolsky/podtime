module AudioDuration (AudioDuration(..)) where

-- | Duration of an MP3 file, in seconds.
newtype AudioDuration = AudioDuration { getAudioDuration :: Double }
  deriving newtype (Eq, Ord, Fractional, Num)

instance Show AudioDuration where
  show (AudioDuration d) = show d <> " s"
