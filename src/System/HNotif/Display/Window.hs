module System.HNotif.Display.Window where

import System.HNotif.Configuration
import System.HNotif.Types

import Graphics.UI.Gtk

notificationWindow :: HNotifConfig -> Notification -> IO Window
notificationWindow config n = do
    c <- notificationContainer config n
    w <- windowNewPopup
    set w
        [ windowAcceptFocus := False
        , containerChild := c
        , containerBorderWidth := 10
        ]
    uncurry (windowSetDefaultSize w) (defaultSize config)
    return w

title :: HNotifConfig -> Notification -> IO Widget
title config n = do
    label <- labelNew (Nothing :: Maybe String)
    labelSetMarkup label ("<b>" ++ summary n ++ "</b>")
    align <- alignmentNew 0 0.5 0 0
    containerAdd align label
    return $ toWidget align

content :: HNotifConfig -> Notification -> IO Widget
content config n = do
    label <- labelNew $ Just (body n)
    labelSetLineWrap label True
    align <- alignmentNew 0 0.5 0 0
    containerAdd align label
    return $ toWidget align

notificationContainer :: HNotifConfig -> Notification -> IO Widget
notificationContainer config n = do
    vbox <- vBoxNew False 10
    title config n >>= \w -> boxPackStart vbox w PackGrow 0
    content config n >>= \w -> boxPackStart vbox w PackGrow 0
    return $ toWidget vbox

