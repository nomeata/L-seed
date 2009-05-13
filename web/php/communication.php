<?php
	header("Content-type: text/html; charset=utf-8");

	session_start();
	if (get_magic_quotes_gpc()) { $_POST = array_map( 'stripslashes', $_POST ); }

	// DB
	function CreateConnection() {
		$db = @new mysqli( 'localhost', 'lseed', 'GCp:rtAaN8nwTFMP', 'lseed' );;
		if (mysqli_connect_errno() != 0) {
			die( "Argh what did you do? -> SERVERERR=0");
		}
		return $db;
	}
	function CreateUser($user, $md5pw) {
		$result = false;
		$db = CreateConnection();

		$stmt = $db -> prepare("INSERT INTO user (Name, Password, IsAdmin) VALUES (?, ?, ?)");
		$isadmin = false;
		$stmt->bind_param("ssb", $user, $md5pw, $isadmin);
		$stmt->execute();

		if ($stmt->affected_rows == 1) {
			$result = true;
		}

		$db->close();

		return $result;
	}
	function GetUser($user) {
		$result = null;
		$db = CreateConnection();

		$stmt = $db->prepare("SELECT ID, Name, Password, IsAdmin FROM user WHERE Name=?");
		$stmt->bind_param("s", $user);
		$stmt->execute();
		$stmt->bind_result( $id, $name, $pw, $isadmin);

		if ($stmt->fetch()) {
			$result = new User($id, $name, $pw, $isadmin);
		}

    	$db->close();

		return $result;
	}
	function InsertNewPlant($userid, $name, $code) {
		$result = false;
		$db = CreateConnection();

		$stmt = $db -> prepare("INSERT INTO plant (UserID, Name, Code) VALUES (?, ?, ?)");
		$stmt->bind_param("dss", $userid, $name, $code);
		$stmt->execute();

		if ($stmt->affected_rows == 1) {
			$result = true;
		}

		$db->close();

		return $result;
	}
	function UpdatePlant($plant) {
		$result = false;
		$db = CreateConnection();

		$stmt = $db->prepare("UPDATE plant SET Code=? WHERE ID=?");
		$stmt->bind_param("sd", $code, $plant->ID);
		$stmt->execute();

		if ($stmt->affected_rows == 1) {
			$result = true;
		}

    		$db->close();

		return $result;
	}
	function DropPlant($plant) {
		$result = false;
		$db = CreateConnection();

		$stmt = $db->prepare("DELETE FROM plant WHERE ID=?");
		$stmt->bind_param("d", $plant->ID);
		$stmt->execute();

		if ($stmt->affected_rows == 1) {
			$result = true;
		}

    		$db->close();

		return $result;
	}
	function GetPlant($userid, $name) {
		$result = null;
		$db = CreateConnection();

		$stmt = $db->prepare("SELECT ID, UserID, Name, Code FROM plant WHERE UserID=? AND Name=?");
		$stmt->bind_param("ds", $userid, $name);
		$stmt->execute();
		$stmt->bind_result( $id, $userid, $name, $code);

		if ($stmt->fetch()) {
			$result = new Plant($id, $userid, $name, $code);
		}

    		$db->close();

		return $result;
	}
	function GetPlantsForUser($userid) {
		$result = array();
		$db = CreateConnection();

		$stmt = $db->prepare("SELECT ID, UserID, Name, Code FROM plant WHERE UserID=?");
		$stmt->bind_param("d", $userid);
		$stmt->execute();
		$stmt->bind_result( $id, $userid, $name, $code);

		while ($stmt->fetch()) {
			$plant = new Plant($id, $userid, $name, $code);
			$result[] = $plant;
		}

    		$db->close();
    		//echo count($result);

		return $result;

	}

	//Plant management

	function SavePlant($name, $code) {
		$result = "{ success: true, msg: '' }";

		$user = GetUser($_SESSION['user']);
		$plant = GetPlant($user->ID, $name);
		if ($plant == null) {
			if (!InsertNewPlant($user->ID, $name, $code)) {
				$result = "{ success: false, msg: 'Pflanze konnte nicht erstellt werden.' }";
			}
		} else {
			$plant->Code = $code;
			if (!UpdatePlant($plant)) {
				$result = "{ success: false, msg: 'Pflanze konnte nicht aktualisiert werden.' }";
			}
		}

		return $result;
	}

	function GetPlantList() {
		$result = "{ list: [] }";

		$userid = GetUser($_SESSION['user'])->ID;
		$list = GetPlantsForUser($userid);

		if (count($list) > 0) {
			$result = "{ list: [";
			$first = true;
			foreach ($list as $plant) {
				if (!$first) {
					$result .= ", ";
				}
				//$result .= "{ ID: " . $plant->ID . ", Name: '" . $plant->Name . "', Code: '" . $plant->Code . "' }";
				$result .= $plant->ToJson();
				$first = false;
			}
			$result .= "] }";
		}

		return $result;
	}
	
	function GetPlantById($id) {
		$result = null;
		
		$userid = GetUser($_SESSION['user'])->ID;
		$list = GetPlantsForUser($userid);

		foreach ($list as $plant) {
			if ($plant->ID == $id) {
				$result = $plant;
				break;
			}
		}
		
		return $result;
	}
	
	function DeletePlant($id) {
		$result = "{ success: false }";

		$plantToBeDeleted = GetPlantById($id);
		if ($plantToBeDeleted != null) {
			if (DropPlant($plantToBeDeleted)) {
				$result = "{ success: true }";
			}
		}

		return $result;
	}
	
	function ValidatePlant($id) {
		$plantToBeValidated = GetPlantById($id);
		
		return ValidatePlantCode($plantToBeValidated->Code);
	}
	
	function ValidatePlantCode($code) {
		$result = "{valid: false, line: 0, column: 0, msg: 'Internal Server Error'}";
		
		$descriptorspec = array(
		   0 => array("pipe", "r"),  // STDIN ist eine Pipe, von der das Child liest
		   1 => array("pipe", "w"),  // STDOUT ist eine Pipe, in die das Child schreibt
		   2 => array("pipe", "w")   // STDERR
		);

		$cwd = realpath("..\\cgi");


		$process = proc_open('validate.exe', $descriptorspec, $pipes, $cwd, array());

		if (is_resource($process)) {
			// $pipes sieht nun so aus:
			// 0 => Schreibhandle, das auf das Child STDIN verbunden ist
			// 1 => Lesehandle, das auf das Child STDOUT verbunden ist
			// Jedwede Fehlerausgaben werden an /tmp/error-output.txt angefügt

			fwrite($pipes[0], $code);
			fclose($pipes[0]);

			$output = stream_get_contents($pipes[1]);
			fclose($pipes[1]);

			//echo stream_get_contents($pipes[2]);
			fclose($pipes[2]);

			$return_value = proc_close($process);
			
			//echo $return_value;
			if ($return_value == 0) {
				$result = $output;
			}
		}

		return $result;
	}

	//Communication

	function RPCAnswer($calledFunc, $data) {
		echo "{ cmd: 'RPC-Response', calledFunc: '".$calledFunc."', data: ".$data." }";
	}

	function ReturnContent($content) {
		switch ($content) {
			case "myplants":
			case "createplant":
			case "testplant":
				LoadContent($content);
				break;
			case "nav":
				if (IsAdmin() != "false") {
					LoadContent("adminnav");
				} else {
					LoadContent($content);
				}
			break;
			case "debug":
				if (IsAdmin() != "false") {
					LoadContent($content);
				}
			break;
		}
	}

	function LoadContent($content) {
			echo "{ cmd: 'Content', contentname: '".$content."', content: ";
			readfile( "page/".$content.".pg" );
			echo "}";
	}

	function ReturnMessage($msg, $type) {
		echo "{ cmd: 'Message', type: '".$type."', msg: '".$msg."' }";
	}

	function ReturnRequest($func) {
		echo "{ cmd: 'RPC', func: ".$func." }";
	}

	function IsLoggedIn() {
		$result = "false";
		if (isset($_SESSION['user']) and isset($_SESSION['pw'])) {
			$user = $_SESSION['user'];
			$md5pw = $_SESSION['pw'];

			$userobj = GetUser($user);
			if ($userobj != null and $userobj->Password == $md5pw) {
				$result = "true";
			}
		}

		return $result;
	}

	function IsAdmin() {
		$result = "false";
		if (isset($_SESSION['user']) and isset($_SESSION['pw'])) {
			$user = $_SESSION['user'];

			$userobj = GetUser($user);
			if ($userobj != null and $userobj->IsAdmin) {
				$result = "true";
			}
		}

		return $result;
	}

	function LogIn($user, $md5pw) {
		$result = "false";
		$userobj = GetUser($user);

		if ($userobj != null and $userobj->Password == $md5pw) {
			$_SESSION['user'] = $user;
			$_SESSION['pw'] = $md5pw;

			$result = "true";
		}

		return $result;
	}

	function LogOut() {
		$_SESSION['user'] = "";
		$_SESSION['pw'] = "";
	}

	function Register($user, $md5pw) {
		$result = "{ success: true, msg: '' }";
		$userobj = GetUser($user);

		if ($userobj == null) {
			if (!CreateUser($user, $md5pw)) {
				$result = "{ success: false, msg: 'Benutzer konnte nicht erstellt werden.' }";
			} else {
				LogIn($user, $md5pw);
			}
		} else {
			$result = "{ success: false, msg: 'Benutzername bereits vergeben.' }";
		}

		return $result;
	}

	switch ($_POST["cmd"]) {
		case "RPC":
			switch ($_POST["func"]) {
				case "IsLoggedIn":
					$res = IsLoggedIn();
					RPCAnswer($_POST["func"], $res);
					break;

				case "Auth":
					$res = LogIn($_POST["user"], $_POST["pw"]);
					RPCAnswer($_POST["func"], $res);
					break;

				case "Logout":
					LogOut();
					echo "{}";
					break;

				case "Register":
					$res = Register($_POST["user"], $_POST["pw"]);
					RPCAnswer($_POST["func"], $res);
					break;

				case "SavePlant":
					$res = SavePlant($_POST["name"], $_POST["code"]);
					RPCAnswer($_POST["func"], $res);
					break;

				case "CreatePlant":
					break;

				case "GetPlantList":
					$res = GetPlantList();
					RPCAnswer($_POST["func"], $res);
					break;

				case "DeletePlant":
					$res = DeletePlant($_POST["id"]);
					RPCAnswer($_POST["func"], $res);
					break;

				case "TestPlant":
					break;

				case "ValidatePlant":
					$res = null;
					if (isset($_POST["id"])) {
						$res = ValidatePlant($_POST["id"]);
					} else {
						$res = ValidatePlantCode($_POST["code"]);
					}
					RPCAnswer($_POST["func"], $res);
					break;
			}
			break;

		case "ContentRequest":
			if (IsLoggedIn() != "false") {
				ReturnContent($_POST["content"]);
			} else {
				$func = <<<EOS
function() {
	this.showLoginDialog();
	this.showMessage('Sie sind nicht eingeloggt bitte einloggen', 'error');
}
EOS;
				ReturnRequest($func);
			}
			break;
	}

	class User
	{
	    public $ID;
	    public $Name;
	    public $Password;
	    public $IsAdmin;

		public function __construct($id, $user, $md5pw, $isadmin) {
			$this->ID = $id;
			$this->Name = $user;
			$this->Password = $md5pw;
			$this->IsAdmin = $isadmin;
		}
	}

	class Plant
	{
	    public $ID;
	    public $UserID;
	    public $Name;
	    public $Code;

		public function __construct($id, $userid, $name, $code) {
			$this->ID = $id;
			$this->UserID = $userid;
			$this->Name = $name;
			$this->Code = $code;
		}

		public function ToJson() {
			return "{ ID: " . $this->ID . ", Name: '" . $this->Name . "', Code: '" . $this->Code . "' }";
		}

		public function ToJsonArray() {
			return "[ " . $this->ID . ", '" . $this->Name . "', '" . $this->Code . "' ]";
		}
	}


?>