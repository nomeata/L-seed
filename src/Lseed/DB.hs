module Lseed.DB 
	( 
	getCodeToRun
	)
	where

import Database.HDBC
import Database.HDBC.ODBC
import Data.Map((!))

data DBCode = DBCode
	{ dbcUserName :: String
	, dbcUserID :: Int
	, dbcPlantName :: String
	, dbcPlantID :: Int
	, dbcCode :: String
	}
	deriving (Show)

withLseedDB ::  (Connection -> IO t) -> IO t
withLseedDB what = do
	dn <- readFile "../db.conf"
	conn <- connectODBC dn	
	res <- what conn
	disconnect conn
	return res

getCodeToRun ::  IO [DBCode]
getCodeToRun = withLseedDB $ \conn -> do
	let getCodeQuery = "SELECT plant.ID AS plantid, user.ID AS userid, code, plant.Name AS plantname, user.Name AS username from plant, user WHERE user.NextSeed = plant.ID;"
	stmt <- prepare conn getCodeQuery
	execute stmt []
	result <- fetchAllRowsMap' stmt
	return $ flip map result $ \m -> 
		DBCode (fromSql (m ! "username"))
		       (fromSql (m ! "userid"))
		       (fromSql (m ! "plantname"))
		       (fromSql (m ! "plantid"))
		       (fromSql (m ! "code"))

