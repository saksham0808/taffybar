{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This is a "Now Playing"-style widget that listens for MPRIS
-- events on DBus.  Various media players implement this.  This widget
-- works with version 2 of the MPRIS protocol
-- (http://www.mpris.org/2.0/spec.html).
--
module System.Taffybar.Players.MPRIS2 ( mpris2New
                              ) where

import Data.Maybe ( listToMaybe )
import DBus
import DBus.Client
import Data.List (isPrefixOf)
import Graphics.UI.Gtk hiding ( Signal, Variant )
import System.Taffybar.Players.Common

mpris2New :: PlayerConfig -> IO Widget
mpris2New cfg = do
  label <- labelNew (Nothing :: Maybe String)
  widgetShowAll label
  _ <- on label realize $ initLabel cfg label
  return (toWidget label)

unpack :: IsVariant a => Variant -> a
unpack var = case fromVariant var of
  Just x -> x
  Nothing -> error("Could not unpack variant: " ++ show var)

initLabel :: PlayerConfig -> Label -> IO ()
initLabel cfg w = do
  client <- connectSession
  -- Set initial song state/info
  reqSongInfo cfg w client
  listen client propMatcher (callBack w)
  return ()
    where callBack label s = do
            let items = dictionaryItems $ unpack (signalBody s !! 1)
            updatePlaybackStatus label items
            updateMetadata cfg label items
            return ()
          propMatcher = matchAny { matchSender = Nothing
                                 , matchDestination = Nothing
                                 , matchPath = Just "/org/mpris/MediaPlayer2"
                                 , matchInterface = Just "org.freedesktop.DBus.Properties"
                                 , matchMember = Just "PropertiesChanged"
                                 }

reqSongInfo :: PlayerConfig -> Label -> Client -> IO ()
reqSongInfo cfg w client = do
  rep <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
                         { methodCallDestination = Just "org.freedesktop.DBus" }
  let plist = unpack $ methodReturnBody rep !! 0
  let players = filter (isPrefixOf "org.mpris.MediaPlayer2.") plist
  case length players of
    0 -> return ()
    _ -> do
      reply <- getProperty client (players !! 0) "Metadata"
      updateSongInfo cfg w $ dictionaryItems $ (unpack . unpack) (methodReturnBody reply !! 0)
      reply' <- getProperty client (players !! 0) "PlaybackStatus"
      let status = (unpack . unpack) (methodReturnBody reply' !! 0) :: String
      case status of
        "Playing" -> postGUIAsync $ widgetShow w
        "Paused"  -> postGUIAsync $ widgetHide w
        "Stopped" -> postGUIAsync $ widgetHide w
        _         -> return ()

getProperty :: Client -> String -> String -> IO MethodReturn
getProperty client name property = do
  call_ client (methodCall "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "Get")
    { methodCallDestination = Just (busName_ name)
    , methodCallBody = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String),
                         toVariant property ]
    }

setSongInfo :: PlayerConfig -> Label -> Maybe String -> Maybe String -> Maybe String
                -> IO ()
setSongInfo (PlayerConfig { trackLabel = getTrackLabel }) w artist album title = do
  let txt = "<span fgcolor='yellow'>▶</span> " ++
          (getTrackLabel $ TrackInfo { trackArtist = artist
                                     , trackTitle = title
                                     , trackAlbum = album
                                     })
  postGUIAsync $ do
    labelSetMarkup w txt

updatePlaybackStatus :: Label -> [(Variant, Variant)] -> IO ()
updatePlaybackStatus w items = do
  case lookup (toVariant ("PlaybackStatus" :: String)) items of
    Just a -> do
      case (unpack . unpack) a :: String of
        "Playing" -> postGUIAsync $ widgetShow w
        "Paused"  -> postGUIAsync $ widgetHide w
        "Stopped" -> postGUIAsync $ widgetHide w
        _         -> return ()
    Nothing -> do
      return ()

updateSongInfo :: PlayerConfig -> Label -> [(Variant, Variant)] -> IO ()
updateSongInfo cfg w items = setSongInfo cfg w readArtist readAlbum readTitle
  where
    readArtist :: Maybe String
    readArtist = do
      artist <- lookup (toVariant ("xesam:artist" :: String)) items
      listToMaybe $ ((unpack . unpack) artist :: [String])
    readTitle :: Maybe String
    readTitle = do
      title <- lookup (toVariant ("xesam:title" :: String)) items
      Just $ (unpack . unpack) title
    readAlbum :: Maybe String
    readAlbum = do
      album <- lookup (toVariant ("xesam:album" :: String)) items
      Just $ (unpack . unpack) album

updateMetadata :: PlayerConfig -> Label -> [(Variant, Variant)] -> IO ()
updateMetadata cfg w items = do
  case lookup (toVariant ("Metadata" :: String)) items of
    Just meta -> do
      let metaItems = dictionaryItems $ (unpack . unpack) meta
      updateSongInfo cfg w metaItems
    Nothing -> return ()
