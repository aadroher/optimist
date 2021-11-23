module Types where

import Data.Time
import RIO
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

defaultDaysOfWeek :: Set DayOfWeek
defaultDaysOfWeek =
  S.fromList
    [ Monday,
      Tuesday,
      Wednesday,
      Thursday,
      Friday,
      Saturday,
      Sunday
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
