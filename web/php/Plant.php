<?php
	
	class Plant
	{
	    public $ID;
	    public $UserID;
	    public $Name;
	    public $Code;
		public $IsActive;
		public $m_Database;

		public function __construct($id, $userid, $name, $code, $database) {
			$this->ID = $id;
			$this->UserID = $userid;
			$this->Name = $name;
			$this->Code = $code;
			$this->IsActive = false;
			$this->m_Database = $database;
			if (!isset($GLOBALS['ValidatorFile'])) {
				$GLOBALS['ValidatorFile'] = '/home/lseed/.cabal/bin/validate';
				#if ($GLOBALS['WINDOWS']) {
				#	$GLOBALS['ValidatorFile'] = '../cgi/validate.exe';
				#}
			}
		}

		public function ToJson() {
			
			$active = 'false';
			if ($this->IsActive) { $active = 'true'; } else { $active = 'false'; }
			return "{ ID: " . $this->ID . ", Name: '" . $this->Name . "', Code: '" . $this->Code . "', IsActive: " . $active . " }";
		}

		public function ToJsonArray() {
			return "[ " . $this->ID . ", '" . $this->Name . "', '" . $this->Code . "' ]";
		}

		public function Save($code) {
			$result = "{ success: true, msg: '' }";
			
			$this->Code = $code;

			$user = $this->m_Database->GetUserByID($this->UserID);
			if ($user) {
				$plant = $this->m_Database->GetPlant($user->ID, $this->Name);
				
				if ($plant == null) {
					if (!$this->m_Database->InsertNewPlant($user->ID, $this->Name, $this->Code)) {
						$result = "{ success: false, msg: 'Pflanze konnte nicht erstellt werden.' }";
					}
				} else {
					$plant->Code = $code;
					if (!$this->m_Database->UpdatePlant($plant)) {
						$result = "{ success: false, msg: 'Pflanze konnte nicht aktualisiert werden.' }";
					}
				}
			} else {
				$result = "{ success: false, msg: 'User (id: \'".$this->UserID."\' could not be found.' }";
			}

			return $result;
		}

		public function Delete() {
			$result = "{ success: false, msg: 'Pflanze konnte nicht gelöscht werden.' }";

			if ($this->m_Database->DropPlant($this)) {
				$result = "{ success: true, msg: '' }";
			}

			return $result;
		}
		
		public function Validate() {
			return $this->ValidateCode();
		}
		
		public function ValidateCode() {
			$result = "{valid: false, line: 0, column: 0, msg: 'Interner Server Fehler'}";
			
			$descriptorspec = array(
			   0 => array("pipe", "r"),  // STDIN ist eine Pipe, von der das Child liest
			   1 => array("pipe", "w"),  // STDOUT ist eine Pipe, in die das Child schreibt
			   2 => array("pipe", "w")   // STDERR
			);

			$cwd = realpath(".");

			$process = proc_open($GLOBALS['ValidatorFile'], $descriptorspec, $pipes, $cwd, array());

			if (is_resource($process)) {
				// $pipes sieht nun so aus:
				// 0 => Schreibhandle, das auf das Child STDIN verbunden ist
				// 1 => Lesehandle, das auf das Child STDOUT verbunden ist
				// Jedwede Fehlerausgaben werden an /tmp/error-output.txt angefügt

				fwrite($pipes[0], $this->Code);
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
		
		public function Activate() {
			$result = "{ success: false, msg: 'Ihre Pflanze konnte nicht aktiviert werden.' }";
			
			if ($this->IsActive) {
				$result = "{ success: false, msg: 'Ihre Pflanze ist bereits aktiv.' }";
			} else {
				if ($this->m_Database->SetUsersNextSeed($this->UserID, $this->ID)) {
					$result = "{ success: true, msg: '' }";
					$this->IsActive = true;
				}
			}
			
			return $result;
		}
	}
?>
