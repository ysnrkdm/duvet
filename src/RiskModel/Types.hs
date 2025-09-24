module RiskModel.Types (Ticker, Bar (..)) where

import Data.Text (Text)
import Data.Time (Day)

type Ticker = Text

data Bar = Bar {bDate :: Day, bClose :: Double, bVolume :: Double} deriving (Show)
