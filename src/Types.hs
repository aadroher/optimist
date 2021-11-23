module Types where

import Data.Time
import RIO
import qualified RIO.Map as M
import qualified RIO.Map.Partial as M'
import RIO.Process
import qualified RIO.Set as S

-- | Domain models
newtype Person = Person
  { personName :: Text
  }
  deriving (Show, Eq)

newtype Room = Room
  { roomName :: Text
  }
  deriving (Show, Eq)

data Meeting = Meeting
  { meetingStart :: UTCTime,
    meetingEnd :: UTCTime,
    meetingPersons :: Set Person,
    meetingRoom :: Room
  }
  deriving (Show, Eq)

nameToNum :: M.Map OrderedDayOfWeek Int
nameToNum =
  M.fromList
    [ (OrderedDayOfWeek Monday, 0),
      (OrderedDayOfWeek Tuesday, 1),
      (OrderedDayOfWeek Wednesday, 2),
      (OrderedDayOfWeek Thursday, 3),
      (OrderedDayOfWeek Friday, 4),
      (OrderedDayOfWeek Saturday, 5),
      (OrderedDayOfWeek Sunday, 6)
    ]

newtype OrderedDayOfWeek = OrderedDayOfWeek DayOfWeek
  deriving (Eq)

instance Ord OrderedDayOfWeek where
  compare (OrderedDayOfWeek d0) (OrderedDayOfWeek d1) = compare (nameToNum M'.! OrderedDayOfWeek d0) (nameToNum M'.! OrderedDayOfWeek d1)

defaultDaysOfWeek :: Set OrderedDayOfWeek
defaultDaysOfWeek =
  S.fromList
    [ OrderedDayOfWeek Monday,
      OrderedDayOfWeek Tuesday,
      OrderedDayOfWeek Wednesday,
      OrderedDayOfWeek Thursday,
      OrderedDayOfWeek Friday,
      OrderedDayOfWeek Saturday,
      OrderedDayOfWeek Sunday
    ]

data Calendar = Calendar
  { calendarName :: Text,
    calendarMeetings :: Set Meeting,
    calendarDaysOfWeeek :: Set DayOfWeek
  }
  deriving (Show, Eq)

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    -- Add other app-specific configuration information here
    appSomeVar :: !Text
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
