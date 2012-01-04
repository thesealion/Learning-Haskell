import Time
import Data.Time.Calendar (gregorianMonthLength)
import Data.List (intercalate)
import System.Environment (getArgs)

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

getCalendarTime :: [String] -> IO CalendarTime
getCalendarTime args = do
    if (length args) == 2 then
        getCalendarTimeFromArgs args
        else do
            clock <- getClockTime
            ctNow <- toCalendarTime clock
            return ctNow

getCalendarTimeFromArgs :: [String] -> IO CalendarTime
getCalendarTimeFromArgs args = do
    let year = read $ args !! 0
        month = toEnum ((read $ args !! 1) - 1)
        ct = CalendarTime year month 1 0 0 0 0 Sunday 0 "UTC" 0 False
    toCalendarTime $ toClockTime ct

main = do
    args <- getArgs
    let firstDayFlag = (length args) > 0 && ((args !! 0) `elem` ["-M", "-S"])
        mondayFirst = if firstDayFlag then args !! 0 == "-M" else True
        args' = if firstDayFlag then drop 1 args else args
    time <- getCalendarTime args'
    let header = (show $ ctMonth time) ++ " " ++ (show $ ctYear time)
        labels = intercalate " " $ map (\x -> take 2 $ show $ intToDay x mondayFirst) [0..6]
        dif = fromIntegral $ (length labels) - (length header)
        header' = if dif > 0 then (replicate (floor (dif / 2)) ' ') ++ header else header
    mapM putStrLn (header':labels:(map showWeek $ getCal time mondayFirst))
    return ()

