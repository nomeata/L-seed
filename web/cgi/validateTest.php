<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html>
	<head>
		<title>ValidateTest</title>
		
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	</head>
	<body>

<?php 
	$GLOBALS['WINDOWS'] = false;
	
	function ValidateRuleSet($ruleset) {
		$result = "{valid: false, line: 0, column: 0, msg: 'Internal Server Error'}";
		
		$descriptorspec = array(
		   0 => array("pipe", "r"),  // STDIN ist eine Pipe, von der das Child liest
		   1 => array("pipe", "w"),  // STDOUT ist eine Pipe, in die das Child schreibt
		   2 => array("pipe", "w")   // STDERR
		);

		$cwd = realpath(".");

		$filename = '../cgi/validate';
		if ($GLOBALS['WINDOWS'] == true) {
			$filename = '../cgi/validate.exe';
		}

		$process = proc_open($filename, $descriptorspec, $pipes, $cwd, array());

		if (is_resource($process)) {
			// $pipes sieht nun so aus:
			// 0 => Schreibhandle, das auf das Child STDIN verbunden ist
			// 1 => Lesehandle, das auf das Child STDOUT verbunden ist
			// Jedwede Fehlerausgaben werden an /tmp/error-output.txt angefügt

			fwrite($pipes[0], $ruleset);
			fclose($pipes[0]);

			$result = stream_get_contents($pipes[1]);
			fclose($pipes[1]);

			echo stream_get_contents($pipes[2]);
			fclose($pipes[2]);

			$return_value = proc_close($process);
		}
		
		return $result;
	}
	
	echo ValidateRuleSet("RyULE MyRule GROW BY 1");

?>
	</body>
</html>
