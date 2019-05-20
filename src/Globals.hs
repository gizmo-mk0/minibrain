module Globals where

import NanoVG

editorBackgroundColor, backgroundLines, background, perceptronBodyColor,
    selectionLineColor, selectionFillColor, pinColor,
    perceptronLabelColor :: Color
editorBackgroundColor = rgba  23  34  40 255
backgroundLines       = rgba  72  90 102 255
background            = rgba  47  68  81 255
perceptronBodyColor   = rgba  79 127 141 255
perceptronSelectedBodyColor = rgba 117 187 206 255
selectionLineColor    = rgba 113 180 198 255
selectionFillColor    = rgba 113 180 198 63
pinColor              = rgba 206 201 193 255
knobBaseColor         = rgba  46  73  81 255
knobColor             = pinColor -- rgba 226 221 213 255
perceptronLabelColor  = editorBackgroundColor

-- Sizes
perceptronWidth, perceptronModuleHeight, perceptronBodyRoundness, pinWidth,
    pinHeight, connectionWidth, knobWidth, knobHeight, knobRoundness :: Float
perceptronWidth         = 100
perceptronModuleHeight  = 40
perceptronBodyRoundness = 10
pinWidth                = 20
pinHeight               = perceptronModuleHeight * 0.6
connectionWidth         = 10
knobWidth               = editorGridSizeF
knobHeight              = editorGridSizeF / 2
knobRoundness           = perceptronBodyRoundness / 2

editorGridSizeF :: Float
editorGridSizeF = 60
connectorSegmentCount :: Float
connectorSegmentCount = 20
tuneMouseDistance :: Float
tuneMouseDistance = 100
perceptronLabelSize :: Float
perceptronLabelSize = 18