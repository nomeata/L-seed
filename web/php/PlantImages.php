<?php
	session_start();
	if (get_magic_quotes_gpc()) { $_POST = array_map( 'stripslashes', $_POST ); }
	
	$_GLOBALS['WINDOWS'] = false;
	
	include("Plant.php");
	include("User.php");
	include("Season.php");
	include("SeasonScore.php");
	include("Controller.php");
	include("Database.php");

	function main()
	{
		$controller = new Controller();

		$user = $controller->GetUser();
		if ($user == null) {
			header("HTTP/1.0 404 Not Found");
			header("Content-type: text/plain; charset=utf-8");
			echo "User not found";
			return;
		}

		$plantid = $_GET[plantid];
		if ($plantid == null) {
			header("HTTP/1.0 404 Not Found");
			header("Content-type: text/plain; charset=utf-8");
			echo "No Plant id given";
			return;
		}

		$plant = $user->GetPlantById($plantid);
		if ($plant == null) {
			header("HTTP/1.0 404 Not Found");
			header("Content-type: text/plain; charset=utf-8");
			echo "No Plant found";
			return;
		}

		$descriptorspec = array(
		   0 => array("pipe", "r"),
		   1 => array("pipe","w"),
		   2 => array("pipe","w"),
		);

		$cwd = realpath(".");

		$process = proc_open("../cgi/renderAsPNG ".escapeshellarg($plant->Name), $descriptorspec, $pipes, $cwd, array());

		if (is_resource($process)) {
			// $pipes sieht nun so aus:
			// 0 => Schreibhandle, das auf das Child STDIN verbunden ist
			// 1 => Lesehandle, das auf das Child STDOUT verbunden ist
			// Jedwede Fehlerausgaben werden an /tmp/error-output.txt angefügt
			header("Content-type: image/png");

			fwrite($pipes[0], $plant->Code);
			fclose($pipes[0]);

			echo stream_get_contents($pipes[1]);
                        fclose($pipes[1]);
                        fclose($pipes[2]);

		    	$return_value = proc_close($process);
		}
	}

	main();
?>
