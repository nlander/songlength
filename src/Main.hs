module Main where

import Data.Monoid ( (<>)
                   , mconcat
                   )

data SongLength = Integer :. Integer

instance Show SongLength where
  show ( a :. b ) = show a
                 <> ":"
                 <> show ( b `div` 10 )
                 <> show ( b `mod` 10 )

instance Monoid SongLength where
  mempty = 0 :. 0
  mappend ( min1 :. sec1 ) ( min2 :. sec2 ) =
    let seconds = sec1 + sec2
     in ( min1 + min2 + div seconds 60 )
          :. ( mod seconds 60 )

aftertouchesSongLengths =
  [ 4 :. 27
  , 3 :. 30
  , 3 :. 32
  , 5 :. 20
  , 4 :. 30
  , 3 :. 53
  , 5 :. 23
  , 4 :. 56
  , 4 :. 32
  ]

main :: IO ()
main = do
  print $ mconcat $ aftertouchesSongLengths
