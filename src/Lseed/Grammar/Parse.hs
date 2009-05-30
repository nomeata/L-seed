module Lseed.Grammar.Parse ( parseGrammar ) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Expr
import Control.Monad

import Lseed.Grammar

-- The lexer
lexer       = P.makeTokenParser $ javaStyle
	{ P.reservedNames = ["RULE", "WHEN", "SET", "Tag", "Light", "Branch", "At",
			     "Length", "Light", "Sublength", "Sublight", "Direction", "Angle",
			     "BY", "TO", "IMPORTANCE", "WEIGHT", "Blossom"]
	}

parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
natural     = P.natural lexer
integer     = P.integer lexer
stringLiteral = P.stringLiteral lexer
naturalOrFloat = P.naturalOrFloat lexer
float	    = P.float lexer
comma	    = P.comma lexer
whiteSpace  = P.whiteSpace lexer

-- Expression

-- The parser

parseGrammar :: String -> String -> Either ParseError GrammarFile
parseGrammar = parse pFile

type Parser = Parsec String ()

pFile :: Parser GrammarFile
pFile = do
	whiteSpace 
	gf <- many1 pRule
	eof
	return gf

pRule :: Parser GrammarRule
pRule = do
	reserved "RULE" 
	name <- pString
	condition <- option (Always True) $ do
		reserved "WHEN"
		pCondition
	action <- pAction
	-- maybe (return ()) fail (actionIsInvalid action)
	priority <- option 1 $ do
		reserved "IMPORTANCE"
		fromIntegral `fmap` natural
	weight <- option 1 $ do
		reserved "WEIGHT"
		fromIntegral `fmap` natural
	return $ GrammarRule name priority weight condition action

pCondition :: Parser Condition
pCondition = buildExpressionParser table term
  where term = parens pCondition <|> pNumCond <|> pTagTest
	table = [[ Infix (do{ reserved "AND"; return And }) AssocLeft ]
	        ,[Infix (do{ reserved "OR";  return Or  }) AssocLeft ]
		]
pNumCond = do
	what <- pMatchable
	cmp <- pCmp
	value <- pFloat
	return (NumCond what cmp value)	

pTagTest = do
	reserved "TAG"
	reservedOp "="
	value <- pString
	return (UserTagIs value)

pAction :: Parser GrammarAction
pAction = pBranch <|> pGrow <|> pBlossom

pBranch :: Parser GrammarAction
pBranch = do
	reserved "BRANCH"
	fraction <- (do
		reserved "AT"
		fraction <- pFloat
		unless (0 <= fraction && fraction <= 100) $
			fail "Fork position has to be in between 0% and 100%."
		reservedOp "%"
		return fraction
		) <|> (return 100)
	branches <- many1 $ do
		reserved "ANGLE"
		reservedOp "="
		angle <- pFloat
		comma
		reserved "LENGTH"
		reservedOp "="
		length <- pFloat
		mTag <- optionMaybe $ do
			comma
			reserved "TAG"
			reservedOp "="
			pString
		return (angle, length, mTag)
	mTag <- optionMaybe $ do
		reserved "SET"
		reserved "TAG"
		reservedOp "="
		pString
	return (AddBranches mTag (fraction/100) branches)

pGrow :: Parser GrammarAction
pGrow = do
	reserved "GROW"
	desc <- by <|> to
	mTag <- optionMaybe $ do
		reserved "SET"
		reserved "TAG"
		reservedOp "="
		pString
	return (SetLength mTag desc)
  where by = do
		reserved "BY"
		value <- pFloat
		(reservedOp "%" >> return (AdditionalRelative value)) <|>
		                   return (Additional value)
	to = do
		reserved "TO"
		value <- pFloat
		return (Absolute value)

pBlossom :: Parser GrammarAction
pBlossom = do
	reserved "BLOSSOM"
	return Blossom

pMatchable =
	choice $ map (\(a,b) -> const b `fmap` reserved a) $
		[ ("LIGHT", MatchLight)
		, ("LENGTH", MatchLength)
		, ("SUBLENGTH", MatchSubLength)
		, ("SUBLIGHT", MatchSubLight)
		, ("ANGLE", MatchAngle)
		, ("DIRECTION", MatchDirection)
		]

pCmp = 
	choice $ map (\(a,b) -> const b `fmap` reservedOp a) $
		[ ("<=", LE)
		, ("<",  Less)
		, ("=",  Equals)
		, (">",  Greater)
		, (">=", GE)
		]

pString = identifier <|> stringLiteral

pFloat = do value <- try (do 
			i <- fromIntegral `fmap` integer
			notFollowedBy (char '.')
			return i
		     )  <|> float
	    (deg >> return (value / 180 * pi)) <|> return value

deg = reservedOp "\194\176"
