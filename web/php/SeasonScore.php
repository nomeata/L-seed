<?php

	class SeasonScore
	{
	    public $ID;
		public $UserID;
		public $User;
		public $SeasonID;
		public $Season;
		public $Score;
		public $DataBase;
		
		public function __construct($id, $userid, $seasonid, $score, $database) {
			$this->ID = $id;
			$this->UserID = $userid;
			$this->SeasonID = $seasonid;
			$this->Score = $score;
			$this->DataBase = $database;
		}

		public function ToJson() {
			$running = 'false';
			$username = 'unknown';
			if ($this->User) { $username = $this->User->Name; }
			if ($this->Season) { if ($this->Season->IsRunning) { $running = 'true'; } else { $running = 'false'; } }
			return "{ ID: " . $this->ID . ", User: '" . $username . "', IsRunning: " . $running . ", Score: " . sprintf("%f", $this->Score) . " }";
		}
	}
?>

