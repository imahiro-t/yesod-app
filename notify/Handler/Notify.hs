module Handler.Notify
  ( getNotifyR
  , postNotifyR
  ) where

import Import

import Data.Time.Clock
import Data.Time.LocalTime

import Handler.Forms

getNotifyR :: Handler Html
getNotifyR = do
  notificationsSentNotYet <- runDB $ selectList [NotificationSent ==. False] [Desc NotificationNotifyDateTime]
  notificationsSent <- runDB $ selectList [NotificationSent ==. True] [Desc NotificationNotifyDateTime]
  (widget, enctype) <- generateFormPost $ notifyMForm "create" Nothing
  defaultLayout $ do
    setTitle "登録画面"
    $(widgetFile "notifications")

postNotifyR :: Handler Html
postNotifyR = do
  ((res,widget),enctype) <- runFormPost $ notifyMForm "" Nothing
  case res of
    FormSuccess (_, notification) -> do
      timeZone <- liftIO getCurrentTimeZone
      let localTime = LocalTime (notificationActionDate notification) (notificationActionTime notification)
      let actionDateTime =  localTimeToUTC timeZone localTime
      let notifyDateTime =  addUTCTime (-realToFrac (60*notificationNotifyBefore notification)) actionDateTime
      let notification' = notification {notificationNotifyDateTime = notifyDateTime}
      _ <- runDB $ insert notification'
      setMessage $ toHtml $ "[" ++ (notificationSubject notification) ++ "]" ++ "を作成しました"
      redirect $ NotifyR
    _ -> do
      setMessage $ toHtml $ ("入力に誤りがあります"::Text)
      notificationsSentNotYet <- runDB $ selectList [NotificationSent ==. False] [Desc NotificationNotifyDateTime]
      notificationsSent <- runDB $ selectList [NotificationSent ==. True] [Desc NotificationNotifyDateTime]
      defaultLayout $ do
        setTitle "登録画面"
        $(widgetFile "notifications")
