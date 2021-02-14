module Main where

import GI.Gtk (Label(..), Window(..), Frame(..), ShadowType(..), Box(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data Event = Noop | Closed

type State = ()

mainWindow :: Widget Event -> Bin Window Event
mainWindow = bin Window [ #title := "Hack simulator",
                          #widthRequest := 640, #heightRequest := 480,
                          on #deleteEvent (const (True, Closed)) ]

romPane :: Widget Event
romPane =
  bin Frame [ #label := "ROM", #labelXalign := 0.5, #shadowType := ShadowTypeOut ] $
    widget Label [ #label := "No ROM loaded" ]

ramPane :: Widget Event
ramPane =
  bin Frame [ #label := "RAM", #labelXalign := 0.5, #shadowType := ShadowTypeOut ] $
    widget Label [ #label := "No RAM loaded" ]

initialView :: Widget Event
initialView = container Box []
  [ BoxChild defaultBoxChildProperties { expand = True } romPane
  , BoxChild defaultBoxChildProperties { expand = True } romPane
  ]


update' :: State -> Event -> Transition State Event
update' _ Noop = Transition () (pure Nothing)
update' _ Closed = Exit

view' :: State -> AppView Window Event
view' _ = mainWindow initialView

main :: IO ()
main = run App {
  view = view',
  update = update',
  inputs = [],
  initialState = ()
}
