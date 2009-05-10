module Lseed.Grammar.Parse ( parseGrammar ) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)
import Text.Parsec.Expr
import Control.Monad

import Lseed.Grammar

-- The lexer
lexer       = P.makeTokenParser $ javaStyle
	{ P.reservedNames = ["RULE", "WHEN", "Tag", "Light", "Branch", "At",
			     "Length", "Light", "Sublength", "Sublight", "Direction", "Angle",
			     "BY", "TO", "IMPORTANCE", "WEIGHT"]
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
pFile = whiteSpace >> many1 pRule

pRule :: Parser GrammarRule
pRule = do
	reserved "RULE" 
	name <- pString
	condition <- option (Always True) $ do
		reserved "WHEN"
		pCondition
	actions <- many1 pAction
	maybe (return ()) fail (actionsAreInvalid actions)
	priority <- option 1 $ do
		reserved "IMPORTANCE"
		fromIntegral `fmap` natural
	weight <- option 1 $ do
		reserved "WEIGHT"
		fromIntegral `fmap` natural
	skipMany nl
	return $ GrammarRule name priority weight condition actions

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
pAction = pBranch <|> pGrow

pBranch :: Parser GrammarAction
pBranch = do
	reserved "BRANCH"
	reserved "AT"
	fraction <- pFloat
	unless (0 <= fraction && fraction <= 100) $
		fail "Fork position has to be in between 0% and 100%."
	reservedOp "%"
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
	return (AddBranch (fraction/100) angle length mTag)

pGrow :: Parser GrammarAction
pGrow = do
	reserved "GROW"
	desc <- by <|> to
	return (SetLength desc Nothing)
  where by = do
		reserved "BY"
		value <- pFloat
		(reservedOp "%" >> return (AdditionalRelative value)) <|>
		                   return (Additional value)
	to = do
		reserved "TO"
		value <- pFloat
		return (Absolute value)
		

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
	
nl = char '\n'
