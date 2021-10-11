{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.List (find)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Simple
import GHC.Generics (Generic)

-- API Weather
newtype OnecallWeather = OnecallWeather { 
    daily :: [DailyWeather]                
} deriving (Show, Generic, FromJSON)

data DailyWeather = DailyWeather {
    daily_temp :: DailyTemp,    
    wind_speed :: Double,       
    desc_weather :: [DescWeather] 
} deriving Show

instance FromJSON DailyWeather where      
    parseJSON (Object v) =
        DailyWeather <$> v .: "temp"
                     <*> v .: "wind_speed"  
                     <*> v .: "weather"

data DailyTemp = DailyTemp {
    temp_min :: Double,
    temp_max :: Double
} deriving Show

instance FromJSON DailyTemp where
    parseJSON (Object v) =
        DailyTemp <$> v .: "min"
                  <*> v .: "max"

newtype DescWeather = DescWeather { 
    description :: T.Text
} deriving (Show, Generic, FromJSON)

-- API Telegram
newtype TelegramResponse = TelegramResponse { 
    result :: [TelegramResult]   
} deriving (Show, Generic, FromJSON)

data TelegramResult = TelegramResult {  
    update_id :: Int,                         
    message :: TelegramMessage                
} deriving (Show, Generic, FromJSON)

data TelegramMessage = TelegramMessage {
    text :: T.Text,         
    chat :: TelegramChat    
} deriving (Show, Generic, FromJSON)

newtype TelegramChat = TelegramChat {
    chat_id :: Int      
} deriving Show

instance FromJSON TelegramChat where
    parseJSON (Object v) =
        TelegramChat <$> v .: "id"

-- API City List
data CityList = CityList {
    name :: T.Text,         
    coord :: CityCoord      
} deriving (Show, Generic, FromJSON)

data CityCoord = CityCoord {
    lon :: Double,              
    lat :: Double
} deriving (Show, Generic, FromJSON)

-- Requests (запросы)
weatherHost :: BS.ByteString
weatherHost = "api.openweathermap.org"  

telegramHost :: BS.ByteString
telegramHost = "api.telegram.org"       

updatesPath :: BS.ByteString            
updatesPath = "/bot[id]/getUpdates?offset="

sendPath :: BS.ByteString               
sendPath = "/bot[id]/sendMessage?chat_id="

buildRequest :: BS.ByteString -> BS.ByteString -> Request
buildRequest host path = setRequestMethod "GET"  
    $ setRequestHost host   
    $ setRequestPath path   
    $ setRequestSecure True 
    $ setRequestPort 443 defaultRequest 

requestToday :: BS.ByteString -> Request  
requestToday city = buildRequest weatherHost ("/data/2.5/weather?appid=43adb5963ce43b21967e05eeece24e92&q=" `BS.append` city)

request7days :: Double -> Double -> Request    
request7days lon lat = buildRequest weatherHost weatherPath where 
    weatherPath = BS.pack $ "/data/2.5/onecall?appid=43adb5963ce43b21967e05eeece24e92&lang=ru&units=metric&lon=" ++ show lon ++ "&lat=" ++ show lat

requestSend :: Int -> BS.ByteString -> Request     
requestSend chat_id text = buildRequest telegramHost (sendPath `BS.append` (BS.pack . show $ chat_id) `BS.append` "&text=" `BS.append` text)

requestUpdates :: Int -> Request        
requestUpdates offset = buildRequest telegramHost (BS.append updatesPath (BS.pack . show $ offset))

-- Main Logic
showWeather :: Either String OnecallWeather -> Int -> BS.ByteString
showWeather (Left error) _ = BS.pack error
showWeather (Right result) days | days == 1 = dailyConvert (head $ daily result)
                                | days == 2 = dailyConvert (daily result !! 1)
                                | otherwise = BS.intercalate "%0A" (fmap (\(a, b) -> BS.pack (show a) `BS.append` ". " `BS.append` b)
                                                (zip [1..] (fmap dailyConvert (init $ daily result))))
                                where
                                    dailyConvert weather = encodeUtf8 $ (T.toTitle . description . head . desc_weather $ weather) `T.append` ", минимум " `T.append` 
                                     T.pack (show (temp_min (daily_temp weather))) `T.append` " %C2%B0C максимум " `T.append`
                                     T.pack (show (temp_max (daily_temp weather))) `T.append` " %C2%B0C, скорость ветра "
                                     `T.append` T.pack (show (wind_speed weather)) `T.append` T.pack " м/с"

parsingCoordCities :: Either String [CityList] -> T.Text -> (Double, Double)
parsingCoordCities (Left error) _ = (0, 0)
parsingCoordCities (Right cityList) city = safePrint (find (\c -> name c == city) cityList) where
    safePrint (Just city) = (lon (coord city), lat (coord city))
    safePrint Nothing = (0, 0)

parsingCommands :: TelegramResult -> IO ()
parsingCommands result 
    | "/today" `T.isPrefixOf` (text . message) result = do  
        print "today"
        parsingCommands' 1
    | "/tomorrow" `T.isPrefixOf` (text . message) result = do 
        print "tomorrow"
        parsingCommands' 2
    | "/7days" `T.isPrefixOf` (text . message) result = do
        print "7days"
        parsingCommands' 7       
    | otherwise = print "Unexpected command" >> threadDelay 5000000 >> getUpdates (update_id result + 1)
    where
        parsingCommands' days = do
            let city = last $ (T.words . text . message) result 
            jsonData <- BS.readFile "city.list.json" 
            let (cityLon, cityLat) = parsingCoordCities (eitherDecodeStrict jsonData) city 
            if (cityLon, cityLat) == (0, 0)
                then httpBS (requestSend (chat_id . chat . message $ result) "Error") 
                else do
                    response <- httpBS $ request7days cityLon cityLat 
                    let text = showWeather (eitherDecodeStrict (getResponseBody response)) days
                    print text
                    httpBS (requestSend (chat_id . chat . message $ result) text) 
            threadDelay 5000000
            getUpdates (update_id result + 1) 

parsingUpdates :: Either String [TelegramResult] -> IO () 
parsingUpdates (Left error) = print error  
parsingUpdates (Right []) = print "No message" >> threadDelay 5000000 >> getUpdates 0 
parsingUpdates (Right results) = mapM_ parsingCommands results

getUpdates :: Int -> IO () 
getUpdates offset = do
    response <- httpBS (requestUpdates offset)      
    let status = getResponseStatusCode response     
    if status == 200                                
        then do
            print "Getting updates" 
            (parsingUpdates . fmap result . eitherDecodeStrict . getResponseBody) response 
        else print "Request updates with error"     

main :: IO ()
main = getUpdates 0
