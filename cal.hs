import Time
import Data.Time.Calendar (gregorianMonthLength)
import Data.List (intercalate)
import System.Environment (getArgs)

days :: [(Day, Int)]
days = [(Monday, 1),
        (Tuesday, 2),
        (Wednesday, 3),
        (Thursday, 4),
        (Friday, 5),
        (Saturday, 6),
        (Sunday, 7)]

months :: [(Month, Int)]
months = [(January, 1),
          (February, 2),
          (March, 3),
          (April, 4),
          (May, 5),
          (June, 6),
          (July, 7),
          (August, 8),
          (September, 9),
          (October, 10),
          (November, 11),
          (December, 12)]

toRange :: Int -> Int
toRange d
    | d < 1 = toRange $ d + 7
    | d > 7 = toRange $ d - 7
    | otherwise = d

getCal :: CalendarTime -> [[Maybe Int]]
getCal time =
    let wday = case lookup (ctWDay time) days of (Just x) -> x
        fwday = toRange $ wday - (ctDay time - 1)
        year = toInteger $ ctYear time
        month = case lookup (ctMonth time) months of (Just x) -> x
        mlen = gregorianMonthLength year month
    in build fwday mlen

build :: Int -> Int -> [[Maybe Int]]
build fwday mlen =
    let month = []
        week = replicate (fwday - 1) Nothing
    in build' month week 1 mlen

build' :: [[Maybe Int]] -> [Maybe Int] -> Int -> Int -> [[Maybe Int]]
build' month week day maxday =
    if length week + 1 == 7 then
        if day == maxday then
            month ++ [week']
        else build' (month ++ [week']) [] (day + 1) maxday
    else
        if day == maxday then
            month ++ [week' ++ (replicate (6 - length week) Nothing)]
        else build' month week' (day + 1) maxday
    where week' = week ++ [Just day]

showDay :: (Maybe Int) -> String
showDay (Just day) = if length s == 1 then ' ':s else s
    where s = show day
showDay Nothing = "  "

showWeek :: [Maybe Int] -> String
showWeek week = intercalate " " (map showDay week)

getCalendarTime :: IO CalendarTime
getCalendarTime = do
    args <- getArgs
    if (length args) == 2 then
        getCalendarTimeFromArgs args
        else do
            clock <- getClockTime
            ctNow <- toCalendarTime clock
            return ctNow

getCalendarTimeFromArgs :: [String] -> IO CalendarTime
getCalendarTimeFromArgs args = do
    let year = read $ args !! 0
        month = getMonth $ read $ args !! 1
        ct = CalendarTime year month 1 0 0 0 0 Sunday 0 "UTC" 0 False
    toCalendarTime $ toClockTime ct
    where getMonth n = case lookup n (map swap months) of (Just x) -> x
          swap (a,b) = (b,a)

main = do
    time <- getCalendarTime
    let header = (show $ ctMonth time) ++ " " ++ (show $ ctYear time)
        days = "Mo Tu We Th Fr Sa Su"
        dif = fromIntegral $ (length days) - (length header)
        header' = if dif > 0 then (replicate (floor (dif / 2)) ' ') ++ header else header
    mapM putStrLn (header':days:(map showWeek (getCal time)))

