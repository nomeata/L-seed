<?php
	header("Content-type: text/html; charset=utf-8");

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

		$response = null;
		
		switch ($_POST["cmd"]) {
			case "RPC":
				$username = $_POST["user"];
				if ($username == null) {
					$username = $_SESSION['user'];
				}
				$pw = $_POST["pw"];
				$plantname = $_POST["plant"];
				$code = $_POST["code"];
				$plantid = $_POST["id"];
				$response = $controller->HandleRemoteProcedureCall($_POST["func"], $username, $pw, $plantname, $code, $plantid);
				break;

			case "ContentRequest":
				if ($controller->IsLoggedIn() != "false") {
					$response = new ContentMessage($_POST["content"]);
				} else {
					$func = "function() { this.showLoginDialog(); this.showMessage('Sie sind nicht eingeloggt bitte einloggen', 'error'); }";
					$response = new RemoteProcedureCall($func);
				}
				break;
			
			default:
				$response = new Message('error', 'unknown Command');
				break;
		}
		
		if ($response != null) {
			$response->send();
		} else {
			echo "Error! no response was generated";
		}
	}

	main();

?>
