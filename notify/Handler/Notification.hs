module Handler.Notification
  ( getNotificationR
  , postNotificationR
  ) where

import Import

import Data.Time.Clock
import Data.Time.LocalTime

import Handler.Forms

getNotificationR :: NotificationId -> Handler Html
getNotificationR notificationId = do
  notification <- runDB $ get404 notificationId
  ((_,widget), enctype) <- runFormPost $ notifyMForm "update" $ Just notification
  defaultLayout $ do
    setTitle "編集画面"
    $(widgetFile "notification")

postNotificationR :: NotificationId -> Handler Html
postNotificationR notificationId = do
  ((res,widget),enctype) <- runFormPost $ notifyMForm "" $ Nothing
  case res of
    FormSuccess (command, notification) -> do
      case command of
        "update" -> do
          timeZone <- liftIO getCurrentTimeZone
          let localTime = LocalTime (notificationActionDate notification) (notificationActionTime notification)
          let actionDateTime =  localTimeToUTC timeZone localTime
          let notifyDateTime =  addUTCTime (-realToFrac (60*notificationNotifyBefore notification)) actionDateTime
          let notification' = notification {notificationNotifyDateTime = notifyDateTime, notificationSent = False}
          runDB $ replace notificationId notification'
          setMessage $ toHtml $ "[" ++ (notificationSubject notification) ++ "]" ++ "を更新しました"
        "delete" -> do
          runDB $ delete notificationId
          setMessage $ toHtml $ "[" ++ (notificationSubject notification) ++ "]" ++ "を削除しました"
        _ -> notFound
      redirect $ NotifyR
    _ -> do
      setMessage $ toHtml $ ("入力に誤りがあります"::Text)
      defaultLayout $ do
        setTitle "編集画面"
        $(widgetFile "notification")
