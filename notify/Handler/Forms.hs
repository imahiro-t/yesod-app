module Handler.Forms
  ( notifyMForm
  , notifyAForm
  ) where

import Import

type FormM x = MForm (HandlerT App IO) (FormResult x, Widget)

commandFieldSettings :: FieldSettings master
commandFieldSettings = FieldSettings
    { fsLabel = "コマンド"
    , fsTooltip = Nothing
    , fsId = Just "command"
    , fsName = Just "command"
    , fsAttrs = []
    }

subjectFieldSettings :: FieldSettings master
subjectFieldSettings = FieldSettings
    { fsLabel = "件名"
    , fsTooltip = Nothing
    , fsId = Just "subject"
    , fsName = Just "subject"
    , fsAttrs = []
    }

actionDateFieldSettings :: FieldSettings master
actionDateFieldSettings = FieldSettings
    { fsLabel = "日付"
    , fsTooltip = Nothing
    , fsId = Just "actionDate"
    , fsName = Just "actionDate"
    , fsAttrs = []
    }

actionTimeFieldSettings :: FieldSettings master
actionTimeFieldSettings = FieldSettings
    { fsLabel = "時間"
    , fsTooltip = Nothing
    , fsId = Just "actionTime"
    , fsName = Just "actionTime"
    , fsAttrs = []
    }

notifyBeforeFieldSettings :: FieldSettings master
notifyBeforeFieldSettings = FieldSettings
    { fsLabel = "通知"
    , fsTooltip = Nothing
    , fsId = Just "notifyBefore"
    , fsName = Just "notifyBefore"
    , fsAttrs = []
    }

summaryFieldSettings :: FieldSettings master
summaryFieldSettings = FieldSettings
    { fsLabel = "説明"
    , fsTooltip = Nothing
    , fsId = Just "summary"
    , fsName = Just "summary"
    , fsAttrs = []
    }

notifyBefores :: [(Text,Int)]
notifyBefores =
  [ ("5分前",5)
  , ("10分前",10)
  , ("15分前",15)
  , ("20分前",20)
  , ("30分前",30)
  , ("45分前",45)
  , ("1時間前",60)
  ]

notifyMForm :: Text -> Maybe Notification -> Html -> FormM (Text, Notification)
notifyMForm command mnotification html = do
  (commandRes, commandView) <- mreq hiddenField commandFieldSettings (Just command)
  (subjectRes, subjectView) <- mreq textField subjectFieldSettings (notificationSubject <$> mnotification)
  (actionDateRes, actionDateView) <- mreq dayField actionDateFieldSettings (notificationActionDate <$> mnotification)
  (actionTimeRes, actionTimeView) <- mreq timeFieldTypeTime actionTimeFieldSettings (notificationActionTime <$> mnotification)
  (notifyBeforeRes, notifyBeforeView) <- mreq (selectFieldList notifyBefores) notifyBeforeFieldSettings (notificationNotifyBefore <$> mnotification)
  (summaryRes, summaryView) <- mopt textareaField summaryFieldSettings (notificationSummary <$> mnotification)
  notifyDateTime <- lift (liftIO getCurrentTime)
  let notifyDateTimeRes = FormSuccess notifyDateTime
  sent <- pure False
  let sentRes = FormSuccess sent
  let notificationRes = (,) <$> commandRes
        <*> (Notification
              <$> subjectRes
              <*> actionDateRes
              <*> actionTimeRes
              <*> notifyBeforeRes
              <*> summaryRes
              <*> notifyDateTimeRes
              <*> sentRes
            )
  let widget = $(widgetFile "notify-form")
  return (notificationRes, widget)

notifyAForm :: Text -> Maybe Notification -> Form (Text, Notification)
notifyAForm command mnotification = renderDivs $ (,)
  <$> areq hiddenField commandFieldSettings (Just command)
  <*> (Notification
        <$> areq textField subjectFieldSettings (notificationSubject <$> mnotification)
        <*> areq dayField actionDateFieldSettings (notificationActionDate <$> mnotification)
        <*> areq timeFieldTypeTime actionTimeFieldSettings (notificationActionTime <$> mnotification)
        <*> areq (selectFieldList notifyBefores) notifyBeforeFieldSettings (notificationNotifyBefore <$> mnotification)
        <*> aopt textareaField summaryFieldSettings (notificationSummary <$> mnotification)
        <*> lift (liftIO getCurrentTime)
        <*> pure False
      )
