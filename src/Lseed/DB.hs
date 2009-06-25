module Lseed.DB 
	( DBCode(..)
	, getCodeToRun
	, getUpdatedCodeFromDB
	, addFinishedSeasonResults
	) where

import Database.HDBC
import Database.HDBC.ODBC
import Data.Map((!))
import qualified Data.Map as M

import Lseed.Data
import Lseed.Data.Functions
import Data.Maybe

data DBCode = DBCode
	{ dbcUserName :: String
	, dbcUserID :: Integer
	, dbcPlantName :: String
	, dbcPlantID :: Integer
	, dbcCode :: String
	}
	deriving (Show)

withLseedDB :: FilePath -> (Connection -> IO t) -> IO t
withLseedDB conf what = do
	dn <- readFile conf
	conn <- connectODBC dn	
	res <- what conn
	disconnect conn
	return res

getCodeToRun :: FilePath -> IO [DBCode]
getCodeToRun conf = withLseedDB conf $ \conn -> do
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

getUpdatedCodeFromDB :: FilePath -> Integer -> IO (Maybe DBCode)
getUpdatedCodeFromDB conf userid = withLseedDB conf $ \conn -> do
	let query = "SELECT plant.ID AS plantid, user.ID AS userid, code, plant.Name AS plantname, user.Name AS username from plant, user WHERE user.NextSeed = plant.ID AND user.ID = ?;"
	stmt <- prepare conn query
	execute stmt [toSql userid]
	result <- fetchAllRowsMap' stmt
	return $ listToMaybe $ flip map result $ \m -> 
		DBCode (fromSql (m ! "username"))
		       (fromSql (m ! "userid"))
		       (fromSql (m ! "plantname"))
		       (fromSql (m ! "plantid"))
		       (fromSql (m ! "code"))

addFinishedSeasonResults conf garden = withLseedDB conf $ \conn -> do 
	let owernerscore = M.toList $ foldr go M.empty garden
		where go p = M.insertWith (+) (plantOwner p) (plantLength (phenotype p))
	run conn "INSERT INTO season VALUES (NULL, False)" []
	stmt <- prepare conn "SELECT LAST_INSERT_ID()"
	execute stmt []
	id <- (head . head) `fmap` fetchAllRows' stmt
	stmt <- prepare conn "INSERT INTO seasonscore VALUES (NULL, ?, ?, ?)"
	executeMany stmt $ map (\(o,l)->
		[ toSql $ o
		, id
		, toSql $ l
		]) owernerscore

