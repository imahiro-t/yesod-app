module Handler.NotifyChecker
 ( checkNotification
 ) where

import Import

--import Control.Concurrent
import Database.Persist.Sqlite
import Network.Mail.Mime (Mail)
import Network.Mail.SMTP
import Data.Time.Clock
import Data.Time.LocalTime

mail :: AppSettings -> Notification -> TimeZone -> Mail
mail appSettings notification timeZone = simpleMail from to cc bcc subject [body]
  where
    from = Address (Just "NOTIFY SENDER") (appMailFrom appSettings)
    to = [Address Nothing (appMailTo appSettings)]
    cc = []
    bcc = []
    subject = notificationSubject notification
    summary = case notificationSummary notification of
                Just a -> unTextarea a
                Nothing -> ""
    body = plainTextPart $
              "[件名]" ++
              "\r\n" ++
              fromStrict subject ++
              "\r\n" ++
              "[日付]" ++
              "\r\n" ++
              (pack $ show $ notificationActionDate notification) ++
              "\r\n" ++
              "[時間]" ++
              "\r\n" ++
              (pack $ show $ notificationActionTime notification) ++
              "\r\n" ++
              "[通知]" ++
              "\r\n" ++
--              (pack $ formatTime defaultTimeLocale "%T, %F (%Z)" (utcToZonedTime timeZone (notificationNotifyDateTime notification))) ++
              (pack $ show (utcToZonedTime timeZone (notificationNotifyDateTime notification))) ++
              "\r\n" ++
              "[説明]" ++
              "\r\n" ++
              fromStrict summary ++
              "\r\n"

notify :: AppSettings -> IO ()
notify appSettings = do
  now <- getCurrentTime
  notifications <- runSqlite dbName $ selectList [NotificationNotifyDateTime <. now, NotificationSent ==. False] []
  timeZone <- getCurrentTimeZone
  mapM_ (\(Entity notificationId notification) -> do
             runSqlite dbName $ update notificationId [NotificationSent =. True]
             sendMail' mailHost mailPort (mail appSettings notification timeZone)
        ) notifications
  where
    dbName = sqlDatabase $ appDatabaseConf $ appSettings
    mailHost = unpack $ appMailHost appSettings
    mailPort = fromIntegral $ appMailPort appSettings

checkNotification :: AppSettings -> IO ()
checkNotification appSettings = do
  forever $ do
    ct <- getCurrentTime
    let sec = 60 :: Integer
    let dayTimeSecondToZero = realToFrac $ (truncate $ utctDayTime ct) `div` sec * sec
    let nt = addUTCTime (realToFrac sec) (UTCTime (utctDay ct) dayTimeSecondToZero)
    threadDelay (truncate $ 10^(6::Integer) * (diffUTCTime nt ct))
    notify appSettings
