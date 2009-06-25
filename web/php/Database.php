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
			$res3 = $this->m_Connection->query("DELETE FROM season");
			$res4 = $this->m_Connection->query("DELETE FROM seasonscore");
			return $res1 && $res2 && $res3 && $res4;
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
		public function GetUser($username) {
			$result = null;
			$userid = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID FROM user WHERE Name=?");
			
			if ($stmt) {
				$stmt->bind_param("s", $username);
				$stmt->execute();
				$stmt->bind_result( $userid);

				$stmt->fetch();
				$stmt->close();
			} else {
				die("You silly bastard. Not again!!!!");
			}
			
			if ($userid != null) {
				$result = $this->GetUserByID($userid);
			}

			return $result;
		}
		public function GetUserByID($userid) {
			$result = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID, Name, Password, IsAdmin, NextSeed FROM user WHERE ID=?");
			
			if ($stmt) {
				$stmt->bind_param("d", $userid);
				$stmt->execute();
				$stmt->bind_result( $id, $name, $pw, $isadmin, $nextseedid);

				if ($stmt->fetch()) {
					$result = new User($id, $name, $pw, $isadmin, $nextseedid, $this);
				}
				$stmt->close();
			} else {
				die("AAARRRRRRR!!!!");
			}
			
			if ($result != null) {
				$result->SeasonScore = $this->GetCurrentSeasonScore($result);
			}

			return $result;
		}
		public function SetUsersNextSeed($userid, $plantid) {
			$result = false;
			
			$stmt = $this->m_Connection->prepare("UPDATE user SET NextSeed=? WHERE ID=?");
			
			if ($stmt) {
				$stmt->bind_param("dd", $plantid, $userid);
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
		public function InsertNewPlant($userid, $name, $code) {
			$result = false;

			$plant = new Plant(0,$userid,$plantname,$code,this);
			$valid = $plant->IsValid();
			
			$stmt = $this->m_Connection->prepare("INSERT INTO plant (UserID, Name, Code, Valid) VALUES (?, ?, ?, ?)");
			
			if ($stmt) {
				$stmt->bind_param("dssi", $userid, $name, $code, $valid);
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
			$plantid = null;

			$stmt = $this->m_Connection->prepare("SELECT ID FROM plant WHERE UserID=? AND Name=?");
			
			if ($stmt) {
				$stmt->bind_param("ds", $userid, $name);
				$stmt->execute();
				$stmt->bind_result( $plantid);
				
				$stmt->fetch();
				$stmt->close();
			} else {
				die("The server you requested is currently unavailable. Please engage in nosepicking...");
			}
			
			if ($plantid != null) {
				$result = $this->GetPlantByID($userid, $plantid);
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
				die("WTF?");
			}
			
			if ($result != null) {
				$user = $this->GetUserByID($userid);
				if ($user != null) {
					$result->IsActive = $user->NextSeedID == $result->ID;
				} else {
					die("Invisible? No waaaay.");
				}
			}
			
			return $result;
		}
		public function UpdatePlant($plant) {
			$result = false;
			
			die($plant);

			$stmt = $this->m_Connection->prepare("UPDATE plant SET Code=?, Valid=? WHERE ID=?");

			if ($stmt) {
				$stmt->bind_param("sid", $plant->Code, $plant->IsValid(), $plant->ID);
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
			$plantids = array();
			
			$stmt = $this->m_Connection->prepare("SELECT ID FROM plant WHERE UserID=?");
			if ($stmt) {
				$stmt->bind_param("d", $userid);
				$stmt->execute();
				$stmt->bind_result($id);

				while ($stmt->fetch()) {
					$plantids[] = $id;
				}
				$stmt->close();
			} else {
				die("None of that young lady!");
			}
			
			foreach ($plantids as $id) {
				$result[] = $this->GetPlantByID($userid, $id);
			}
			
			return $result;
		}
		public function GetCurrentSeasonScore($userid) {
			$result = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID FROM seasonscore WHERE UserID=?");
			if ($stmt) {
				$stmt->bind_param("d", $userid);
				$stmt->execute();
				$stmt->bind_result($id);

				while ($stmt->fetch()) {
					$result = $this->GetSeasonScoreByID($id);
				}
				$stmt->close();
			} else {
				die("WHY YOU LITTLE...!");
			}
			
			return $result;
		}
		public function GetSeasonScoreByID($ssid) {
			$result = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID, UserID, SeasonID, Score FROM seasonscore WHERE ID=?");
			if ($stmt) {
				$stmt->bind_param("d", $ssid);
				$stmt->execute();
				$stmt->bind_result( $id, $userid, $seasonid, $score);

				while ($stmt->fetch()) {
					$result = new SeasonScore($id, $userid, $seasonid, $score, $this);
				}
				$stmt->close();
			} else {
				die("WHY YOU LITTLE...!");
			}
			
			if ($result != null) {
				$result->Season = $this->GetSeasonForSeasonScore($result);
			}
			
			return $result;
		}
		
		public function GetSeasonForSeasonScore($score) {
			$season = null;
			
			$stmt = $this->m_Connection->prepare("SELECT ID, IsRunning FROM season WHERE ID=?");
			if ($stmt) {
				$stmt->bind_param("d", $score->SeasonID);
				$stmt->execute();
				$stmt->bind_result( $id, $isrunnning);

				while ($stmt->fetch()) {
					$season = new Season($id, $isrunnning, $this);
				}
				$stmt->close();
			} else {
				die("You Crack me up little buddy.");
			}
			
			return $season;
		}
		
		public function GetAllSeasonScores() {
			$result = array();
			$idlist = array();
			
			$stmt = $this->m_Connection->query("SELECT ID FROM seasonscore");
		
			if ($stmt) {
				while ($obj = $stmt->fetch_object()) {
					$idlist[] = $obj->ID;
				}
				$stmt->close();
			} else {
				die("WHY YOU LITTLE...!");
			}
			
			foreach ($idlist as $id) {
				$result[] = $this->GetSeasonScoreByID($id);
			}
			
			return $result;
		}
	}
?>
