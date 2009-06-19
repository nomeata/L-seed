<?php
	include("../Database.php");

	class TestOfDataBase extends UnitTestCase
	{
		private $m_Database;
		
		function __construct() {
		}
		
		function setUp() {
			$this->m_Database = new DataBase();
			$this->m_Database->Clear();
		}
		
		function tearDown() {
			$this->m_Database->Clear();
			$this->m_Database->Close();
		}
		
		function testClear() {
			$username = "argletam";
			$this->m_Database->CreateUser($username, "42aa");
			$user = $this->m_Database->GetUser($username);
			$userid = $user->ID;
			
			$res = $this->m_Database->Clear();
			$this->assertTrue($res, "Clear failed");
			
			$user = $this->m_Database->GetUser($username);
			$this->assertTrue($user == null, "A User was found even though the database was cleared");
			
			$plants = $this->m_Database->GetPlantsForUser($userid);
			$this->assertTrue(count($user) == 0, "Even though the database was cleared we found plants for a user");
		}
		
		function testUser() {
			$username = "testuser";
			
			$result = $this->m_Database->CreateUser($username, "test");
			$this->assertTrue($result);
			
			$user = $this->m_Database->GetUser($username);
			$this->assertTrue($user != null);
			
			$user = $this->m_Database->GetUser("a");
			$this->assertTrue($user == null);
			
			$user = $this->m_Database->GetUser(null);
			$this->assertTrue($user == null);
		}
		
		function testPlant() {
			$username = "testuser";
			
			$result = $this->m_Database->CreateUser($username, "test");
			$user = $this->m_Database->GetUser($username);
			
			$data_name = "myplant";
			$data_code = "my fancy plant code!";
			
			$result = $this->m_Database->InsertNewPlant($user->ID, $data_name, $data_code);
			$this->assertTrue($result);
			
			$plant = $this->m_Database->GetPlant($user->ID, $data_name);
			$this->assertTrue($plant != null);
			$this->assertTrue($plant->UserID == $user->ID);
			$this->assertTrue($plant->Name == $data_name);
			$this->assertTrue($plant->Code == $data_code);
			
			$plants = $this->m_Database->GetPlantsForUser($user->ID);
			$this->assertTrue($plants != null);
			$this->assertTrue(count($plants) == 1);
			$this->assertTrue($plants[0]->ID == $plant->ID);
			
			$data_code = $plant->Code = "My New and improved Plantcode. Featuring: even fancier code!";
			$result = $this->m_Database->UpdatePlant($plant);
			$this->assertTrue($result);
			
			$plant = $this->m_Database->GetPlant($user->ID, $data_name);
			$this->assertTrue($plant != null);
			$this->assertTrue($plant->UserID == $user->ID);
			$this->assertTrue($plant->Name == $data_name);
			$this->assertTrue($plant->Code == $data_code);
			
			$result = $this->m_Database->DropPlant($plant);
			$this->assertTrue($result);
			
			$plant = $this->m_Database->GetPlant($user->ID, $data_name);
			$this->assertTrue($plant == null);
		}
	}

?>
