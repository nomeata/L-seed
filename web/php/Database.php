<?php
	
	class DataBase
	{
		public $m_Connection;

		public function __construct() {
			$this->m_Connection = @new mysqli( 'localhost', 'lseed', 'GCp:rtAaN8nwTFMP', 'lseed' );
			if (mysqli_connect_errno() != 0) {
				die( "Argh what did you do?->SERVERERR=0, ".mysqli_connect_error());
			}
		}
		
		public function Clear() {
			$res1 = $this->m_Connection->query("DELETE FROM user");
			$res2 = $this->m_Connection->query("DELETE FROM plant");
			return $res1 && $res2;
		}
		public function Close() {
			$this->m_Connection->close();
		}

		public function CreateUser($user, $md5pw) {
			$result = false;

			$stmt = $this->m_Connection->prepare("INSERT INTO user (Name, Password, IsAdmin) VALUES (?, ?, ?)");
			
			if ($stmt) {
				$isadmin = false;
				$stmt->bind_param("ssb", $user, $md5pw, $isadmin);
				$stmt->execute();

				if ($stmt->affected_rows == 1) {
					$result = true;
				}
				
				$stmt->close();
			} else {
				die("You little Nerd. How could you?");
			}

			return $result;
		}
		public function GetUser($user) {
			$result = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID, Name, Password, IsAdmin FROM user WHERE Name=?");
			
			if ($stmt) {
				$stmt->bind_param("s", $user);
				$stmt->execute();
				$stmt->bind_result( $id, $name, $pw, $isadmin);

				if ($stmt->fetch()) {
					$result = new User($id, $name, $pw, $isadmin, $this);
				}
				$stmt->close();
			} else {
				die("You silly bastard. Not again!!!!");
			}

			return $result;
		}
		public function GetUserByID($userid) {
			$result = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID, Name, Password, IsAdmin FROM user WHERE ID=?");
			
			if ($stmt) {
				$stmt->bind_param("d", $userid);
				$stmt->execute();
				$stmt->bind_result( $id, $name, $pw, $isadmin);

				if ($stmt->fetch()) {
					$result = new User($id, $name, $pw, $isadmin, $this);
				}
				$stmt->close();
			} else {
				die("AAARRRRRRR!!!!");
			}

			return $result;
		}
		public function InsertNewPlant($userid, $name, $code) {
			$result = false;
			
			$stmt = $this->m_Connection->prepare("INSERT INTO plant (UserID, Name, Code) VALUES (?, ?, ?)");
			
			if ($stmt) {
				$stmt->bind_param("dss", $userid, $name, $code);
				$stmt->execute();
				
				if ($stmt->affected_rows == 1) {
					$result = true;
				}
				$stmt->close();
			} else {
				die("DAAAAAAMN.");
			}
			
			return $result;
		}
		public function GetPlant($userid, $name) {
			$result = null;

			$stmt = $this->m_Connection->prepare("SELECT ID, UserID, Name, Code FROM plant WHERE UserID=? AND Name=?");
			
			if ($stmt) {
				$stmt->bind_param("ds", $userid, $name);
				$stmt->execute();
				$stmt->bind_result( $id, $theuserid, $thename, $code);
				
				if ($stmt->fetch()) {
					$result = new Plant($id, $theuserid, $thename, $code, $this);
				} else {
					//echo "nope no plant like that found.";
				}
				$stmt->close();
			} else {
				die("the server you requested is currently unavailable. Please engage in nose picking...");
			}
			return $result;
		}
		public function GetPlantByID($userid, $plantid) {
			$result = null;

			$stmt = $this->m_Connection->prepare("SELECT ID, UserID, Name, Code FROM plant WHERE UserID=? AND ID=?");
			
			if ($stmt) {
				$stmt->bind_param("ds", $userid, $plantid);
				$stmt->execute();
				$stmt->bind_result( $id, $theuserid, $thename, $code);
				
				if ($stmt->fetch()) {
					$result = new Plant($id, $theuserid, $thename, $code, $this);
				} else {
					//echo "nope no plant like that found.";
				}
				$stmt->close();
			} else {
				die("the server you requested is currently unavailable. Please engage in nose picking...");
			}
			return $result;
		}
		public function UpdatePlant($plant) {
			$result = false;
			
			$stmt = $this->m_Connection->prepare("UPDATE plant SET Code=? WHERE ID=?");
			
			if ($stmt) {
				$stmt->bind_param("sd", $plant->Code, $plant->ID);
				$stmt->execute();

				if ($stmt->affected_rows == 1) {
					$result = true;
				}
				$stmt->close();
			} else {
				die("Look there, behind you. A three-headed monkey!");
			}
			
			return $result;
		}
		public function DropPlant($plant) {
			$result = false;
			
			$stmt = $this->m_Connection->prepare("DELETE FROM plant WHERE ID=?");
			if ($stmt) {
				$stmt->bind_param("d", $plant->ID);
				$stmt->execute();

				if ($stmt->affected_rows == 1) {
					$result = true;
				}
				$stmt->close();
			} else {
				die("Au man, that hurt.");
			}

			return $result;
		}
		public function GetPlantsForUser($userid) {
			$result = array();
			
			$stmt = $this->m_Connection->prepare("SELECT ID, UserID, Name, Code FROM plant WHERE UserID=?");
			if ($stmt) {
				$stmt->bind_param("d", $userid);
				$stmt->execute();
				$stmt->bind_result( $id, $userid, $name, $code);

				while ($stmt->fetch()) {
					$plant = new Plant($id, $userid, $name, $code, $this);
					$result[] = $plant;
				}
				$stmt->close();
			} else {
				die("None of that young lady!");
			}
			
			return $result;
		}
	}
?>
