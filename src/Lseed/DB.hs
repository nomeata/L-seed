module Lseed.DB 
	( DBCode(..)
	, getCodeToRun
	, addFinishedSeasonResults
	) where

import Database.HDBC
import Database.HDBC.ODBC
import Data.Map((!))

import Lseed.Data
import Lseed.Data.Functions

data DBCode = DBCode
	{ dbcUserName :: String
	, dbcUserID :: Integer
	, dbcPlantName :: String
	, dbcPlantID :: Integer
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

addFinishedSeasonResults garden = withLseedDB $ \conn -> do 
	run conn "INSERT INTO SEASON VALUES (NULL, False)" []
	stmt <- prepare conn "SELECT LAST_INSERT_ID()"
	execute stmt []
	id <- (head . head) `fmap` fetchAllRows' stmt
	stmt <- prepare conn "INSERT INTO seasonscore VALUES (NULL, ?, ?, ?)"
	executeMany stmt $ map (\planted ->
		[ toSql $ plantOwner planted
		, id
		, toSql $ plantLength (phenotype planted)]
		) garden

