import Time
import Data.Time.Calendar (gregorianMonthLength)
import Data.List (intercalate)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import Data.Char (isDigit)

dayToInt :: Day -> Bool -> Int
dayToInt day mondayFirst =
    if mondayFirst then
        if i == 0 then 6 else i - 1
    else i
    where i = fromEnum day

intToDay :: Int -> Bool -> Day
intToDay x mondayFirst =
    let i = if mondayFirst then
                if x == 6 then 0 else x + 1
            else x
    in toEnum i :: Day

getCal :: CalendarTime -> Bool -> [[Maybe Int]]
getCal time mondayFirst =
    let wday = dayToInt (ctWDay time) mondayFirst
        fwday' =  wday - ((ctDay time - 1) `mod` 7)
        fwday = if fwday' < 0 then fwday' + 7 else fwday'
        year = toInteger $ ctYear time
        month = (fromEnum $ ctMonth time) + 1
        mlen = gregorianMonthLength year month
        initialWeek = replicate fwday Nothing
    in build [] initialWeek 1 mlen

build :: [[Maybe Int]] -> [Maybe Int] -> Int -> Int -> [[Maybe Int]]
build month week day maxday =
    if length week + 1 == 7 then
        if day == maxday then
            month ++ [week']
        else build (month ++ [week']) [] (day + 1) maxday
    else
        if day == maxday then
            month ++ [week' ++ (replicate (6 - length week) Nothing)]
        else build month week' (day + 1) maxday
    where week' = week ++ [Just day]

showDay :: (Maybe Int) -> String
showDay (Just day) = if length s == 1 then ' ':s else s
    where s = show day
showDay Nothing = "  "

showWeek :: [Maybe Int] -> String
showWeek = intercalate " " . map showDay

getYearAndMonth :: [String] -> Maybe (Int,Int)
getYearAndMonth (y:m:args) =
    if (digitsOnly y) && (digitsOnly m) && (month >= 1) && (month <= 12) then
        Just (read y,month)
    else Nothing
    where digitsOnly s = filter (not . isDigit) s == ""
          month = read m
getYearAndMonth _ = Nothing

getCalendarTime :: [String] -> IO CalendarTime
getCalendarTime args = do
    let ym = getYearAndMonth args
    case ym of Just (year, month) -> do
                   let ct = (CalendarTime year (toEnum $ month - 1) 1 0 0 0 0 Sunday 0 "UTC" 0 False)
                   toCalendarTime $ toClockTime $ ct
               _ -> do
                   clock <- getClockTime
                   ctNow <- toCalendarTime clock
                   return ctNow

data Flag = StartWithMonday | StartWithSunday | Help deriving Eq

options :: [OptDescr Flag]
options = [Option ['M'] [] (NoArg StartWithMonday) "Weeks start on Monday",
           Option ['S'] [] (NoArg StartWithSunday) "Weeks start on Sunday",
           Option ['h'] ["help"] (NoArg Help) "Show usage information"]

usage progName = usageInfo header options
  where header = "Usage: " ++ progName ++ " [-M|-S] [<year> <month>]"

getOpts :: String -> [String] -> IO ([Flag], [String])
getOpts progName argv =
    case getOpt RequireOrder options argv of
        (o,n,[]) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usage progName))

main = do
    argv <- getArgs
    progName <- getProgName
    (opts, args) <- getOpts progName argv
    time <- getCalendarTime args
    if Help `elem` opts then putStrLn $ usage progName
        else do
        let mondayFirst = not $ StartWithSunday `elem` opts
            header = (show $ ctMonth time) ++ " " ++ (show $ ctYear time)
            labels = intercalate " " $ map (\x -> take 2 $ show $ intToDay x mondayFirst) [0..6]
            dif = fromIntegral $ (length labels) - (length header)
            header' = if dif > 0 then (replicate (floor (dif / 2)) ' ') ++ header else header
        mapM putStrLn (header':labels:(map showWeek $ getCal time mondayFirst))
        return ()

