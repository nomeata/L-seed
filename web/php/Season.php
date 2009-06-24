<?php
	
	class Season
	{
	    public $ID;
		public $IsRunning;
		public $DataBase;
		
		public function __construct($id, $isrunning, $database) {
			$this->ID = $id;
			$this->IsRunning = $isrunning;
			$this->DataBase = $database;
		}
	}
?>
