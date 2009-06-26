<?php
	include("Response.php");
	include("Server.php");
	
	class Controller
	{
		protected $m_Database;
		protected $m_Server;
		protected $m_User;
		protected $m_IsLoggedIn;

		public function	__construct() {
			$this->m_Database = new Database();
			$this->m_Server = new Server($this);
			
			if (isset($_SESSION['user'])) {
				$this->m_User = $this->m_Database->GetUser($_SESSION['user']);
				$this->m_IsLoggedIn = $this->m_Server->IsLoggedIn();
			}
		}
		
		public function GetDatabase() { return $this->m_Database; }
		public function GetUser() { return $this->m_User; }
		public function SetUser($user) { $this->m_User = $user; }
		public function IsLoggedIn() { $this->m_Server->IsLoggedIn(); }

		private function CreatePlant($plantname, $code) {
			$res = "{ success: false, msg: 'Unbekannter Fehler' }";
			
			if (isset($this->m_User)) {
				if ($plantname != null) {
					if ($this->m_User->CreatePlant($plantname, $code) == "true") {
						$res = "{ success: true, msg: 'Es wurde eine neue Pflanze für sie angelegt.' }";
					} else {
						$res = "{ success: false, msg: 'Pflanze konnte nicht erstellt werden.' }";
					}
				} else {
					$res = "{ success: false, msg: 'Kein Name wurde übergeben:' }";
				}
			} else {
				$res = "{ success: false, msg: 'Sie sind nicht angemeldet' }";
			}
			
			return $res;
		}
		
		private function GetSeasonScoreList() {
			$result = "{ success: false, msg: 'Unbekannter Fehler' }";
			
			$list = $this->m_Database->GetAllSeasonScores();
			
			if (count($list) > 0) {
				$result = "{ list: [";
				$first = true;
				foreach ($list as $seasonscore) {
					if (!$first) {
						$result .= ", ";
					}
					$result .= $seasonscore->ToJson();
					$first = false;
				}
				$result .= "] }";
			}
			
			return $result;
		}

		public function HandleRemoteProcedureCall($func, $username, $pw, $plantname, $code, $plantid) {
			$plant = null;
			if ($this->m_User == null) {
				$this->m_User = $this->m_Database->GetUser($username);
			}
			if ($this->m_User != null) {
				if ($plantid != null) {
					$plant = $this->m_User->GetPlantById($plantid);
				} else {
					$plant = $this->m_User->GetPlant($plantname);
				}
			}
			
			$res = "";
			error_log($func);

			switch ($func) {
				case "IsLoggedIn":
					$res = $this->m_Server->IsLoggedIn();
					break;

				case "Auth":
					$res = $this->m_Server->LogIn($username, $pw);
					break;

				case "Logout":
					$res = $this->m_Server->LogOut();
					break;

				case "Register":
					$res = $this->m_Server->Register($username, $pw);
					break;

				case "SavePlant":
					error_log("Blubb");
					if ($plant != null) {
						$res = $plant->Save($code);
					} else {
					$res = $this->CreatePlant($plantname, $code);
					}
					break;

				case "CreatePlant":
					$res = $this->CreatePlant($plantname, $code);
					break;

				case "ActivatePlant":
					$res = $plant->Activate();;
					break;

				case "GetPlantList":
					if (isset($this->m_User)) {
						$res = $this->m_User->GetPlantList();
					} else {
						$res = "{ success: false, msg: 'Sie sind nicht angemeldet' }";
					}
					break;

				case "GetSeasonList":
					$res = $this->GetSeasonScoreList();
					break;

				case "DeletePlant":
					if ($plant != null) {
						$res = $plant->Delete();
					} else {
						$res = "{ success: false, msg: \"Keine Pflanze mit dem Namen '".$plantname."' für den Nutzer '".$username."' gefunden.\" }";
					}
					break;

				case "TestPlant":
					break;

				case "CheckSyntax":
					# Create a temporary plant		
					$plant = new Plant(0,0,$plantname,$code,0);
					$res = $plant->Validate();
					break;

				default:
					$res = "{ success : false, msg: \"Unkown RPC call '".$func."'\"}";
			}
		
			return new RPCAnswer($func, $res);
		}
	}
?>
