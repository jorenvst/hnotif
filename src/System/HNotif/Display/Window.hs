module System.HNotif.Display.Window where

import System.HNotif.Configuration
import System.HNotif.Types

import Graphics.UI.Gtk

-- TODO: a function onDismiss should be passed, so when the dismiss button is pressed, the notification gets closed
notificationWindow :: HNotifConfig -> Notification -> IO Window
notificationWindow config n = do
    c <- notificationContainer config n
    w <- windowNewPopup
    set w
        [ windowAcceptFocus := False
        , containerChild := c
        , containerBorderWidth := (padding . notificationView $ config)
        ]
    uncurry (windowSetDefaultSize w) (defaultSize . notificationView $ config)
    return w

summaryView :: HNotifConfig -> Notification -> IO Widget
summaryView config n = do
    label <- labelNew (Nothing :: Maybe String)
    labelSetMarkup label ("<b>" ++ summary n ++ "</b>")
    align <- makeAlignment (summaryAlignment . notificationView $ config)
    containerAdd align label
    return $ toWidget align

-- TODO: onDismiss
dismissButton :: IO Button
dismissButton = buttonNewWithLabel "x"

bodyView :: HNotifConfig -> Notification -> IO Widget
bodyView config n = do
    label <- labelNew $ Just (body n)
    labelSetLineWrap label True
    align <- makeAlignment (bodyAlignment . notificationView $ config)
    containerAdd align label
    return $ toWidget align

notificationContainer :: HNotifConfig -> Notification -> IO Widget
notificationContainer config n = do
    vbox <- vBoxNew False (padding . notificationView $ config)
    hbox <- hBoxNew False (padding . notificationView $ config)
    summaryView config n >>= \w -> boxPackStart hbox w PackGrow 0
    dismissButton >>= \w -> boxPackStart hbox w PackNatural 0
    boxPackStart vbox hbox PackGrow 0
    bodyView config n >>= \w -> boxPackStart vbox w PackGrow 0
    return $ toWidget vbox

makeAlignment :: TextAlignment -> IO Alignment
makeAlignment LeftAlignment = alignmentNew 0 0.5 0 0
makeAlignment RightAlignment = alignmentNew 1 0.5 0 0
makeAlignment CenterAlignment = alignmentNew 0.5 0.5 0 0
