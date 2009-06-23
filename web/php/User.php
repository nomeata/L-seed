<?php
	
	class User
	{
	    public $ID;
	    public $Name;
	    public $Password;
	    public $IsAdmin;
	    public $SeasonScore;
		public $NextSeedID;
	    public $m_Database;

		public function __construct($id, $user, $md5pw, $isadmin, $nextseedid, $database) {
			$this->ID = $id;
			$this->Name = $user;
			$this->Password = $md5pw;
			$this->IsAdmin = $isadmin;
			$this->NextSeedID = $nextseedid;
			$this->m_Database = $database;
		}
		
		public function GetPlantList() {
			$result = "{ list: [] }";

			$list = $this->m_Database->GetPlantsForUser($this->ID);

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
		
		public function GetPlantById($id) {
			$result = null;
			
			$list = $this->m_Database->GetPlantsForUser($this->ID);

			foreach ($list as $plant) {
				if ($plant->ID == $id) {
					$result = $plant;
					break;
				}
			}
			
			return $result;
		}
		
		public function CreatePlant($name, $code) {
			$result = "false";
			
			$res = $this->m_Database->InsertNewPlant($this->ID, $name, $code);
			if ($res) {
				$result = "true";
			}
			
			return $result;
		}
		
		public function GetPlant($name) {
			return $this->m_Database->GetPlant($this->ID, $name);
		}
	}
?>
