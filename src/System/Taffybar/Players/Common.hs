module System.Taffybar.Players.Common where

import Text.Printf

data TrackInfo = TrackInfo
  { trackArtist :: Maybe String -- ^ Artist name, if available.
  , trackTitle  :: Maybe String -- ^ Track name, if available.
  , trackAlbum  :: Maybe String -- ^ Album name, if available.
  }

data PlayerConfig = PlayerConfig
  { trackLabel :: TrackInfo -> String -- ^ Calculate a label to display.
  }


defaultPlayerConfig :: PlayerConfig
defaultPlayerConfig = PlayerConfig
  { trackLabel = display
  }
  where artist track  = maybe "[unknown]" id (trackArtist track)
        title  track  = maybe "[unknown]" id (trackTitle  track)
        display :: TrackInfo -> String
        display track = "<span fgcolor='yellow'>▶</span> " ++
                        printf "%s - %s" (artist track) (title track)

