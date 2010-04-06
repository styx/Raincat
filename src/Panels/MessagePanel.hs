module Panels.MessagePanel
    (MessagePanel(MessagePanel),
     messageDisplay) where

data MessagePanel = MessagePanel
    {
        messageDisplay  :: [Char]
    }
