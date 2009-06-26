{- 
 
Helper program: Expects a L-seed grammar on stdin and outputs its validity in JSON format:
$ echo "RULE is invalid" | ./validate 
{"valid":false,"line":1,"column":9,"msg":"\nunexpected \"i\"\nexpecting \"WHEN\", \"BRANCH\" or \"GROW\""}
$ echo "RULE trivial GROW BY 1" | ./validate 
{"valid":true}
-}

import Text.Parsec.Error
import Text.Parsec.Pos
import Lseed.Grammar.Parse
import Text.JSON
import System.Exit

valid = encode $ makeObj [ ("valid", showJSON True) ]

invalid error = encode $ makeObj
	[ ("valid", showJSON False)
	, ("line",  showJSON . sourceLine .   errorPos $ error) 
	, ("column",showJSON . sourceColumn . errorPos $ error) 
	, ("msg",   showJSON .
	            showErrorMessages "or" "unknown parse error"
                                      "expecting" "unexpected" "end of input" .
                    errorMessages $ error)
	]

main = do
	file <- getContents
	case (parseGrammar "stdin" file) of
		Left err -> do
			putStr (invalid err)
			exitWith (ExitFailure 1)
		Right _ -> do
			putStr valid
			exitWith ExitSuccess
