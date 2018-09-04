module RhineGloss.Arrowized.Model.DifficultyManager where

difficultyValue :: Float -> Int
difficultyValue timePassed
  | timePassed <= 30                       = 90
  | timePassed >  30  && timePassed <= 60  = 80
  | timePassed >  60  && timePassed <= 90  = 70
  | timePassed >  90  && timePassed <= 120 = 60
  | timePassed >  120 && timePassed <= 150 = 50
  | timePassed >  150 && timePassed <= 180 = 40
  | timePassed >  210 && timePassed <= 240 = 30
  | timePassed >  240 && timePassed <= 270 = 20
  | timePassed >  270                      = 10
  | otherwise                              = 50
