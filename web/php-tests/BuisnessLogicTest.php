<?php
	include("../php/User.php");
	include("../php/Plant.php");
	include("../php/Controller.php");
	include("../php/Season.php");
	include("../php/SeasonScore.php");

	class TestOfBuisnessLogic extends UnitTestCase
	{
		private $m_Controller;
		
		function __construct() {
		}
		
		function setUp() {
			$this->m_Controller = new Controller();
			$db = new DataBase();
			$db->Clear();
			$db->Close();
		}
		
		function tearDown() {
			$db = new DataBase();
			$db->Clear();
			$db->Close();
			unset($_SESSION['user']);
			unset($_SESSION['pw']);
		}
		
		function validateRPCResponseWithoutData($res, $func) {
			$this->assertTrue($res != null, "No Result given");
			$this->assertTrue($res->m_Command == "RPC-Response", "Wrong Commandtype");
			$this->assertTrue($res->m_CalledFunction == $func, "Wrong CalledFunction");
		}
		function validateRPCResponse($res, $func, $data) {
			$this->validateRPCResponseWithoutData($res, $func);
			$this->assertTrue($res->m_Data == $data, "'".$res->m_Data."' was unexpected, '".$data."' expected");
		}
		function testAuthentication() {
			$res = $this->m_Controller->HandleRemoteProcedureCall("IsLoggedIn", "", "", "", "");
			$this->validateRPCResponse($res, "IsLoggedIn", "false");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("Register", "test", "test", "", "");
			$this->validateRPCResponse($res, "Register", "{ success: true, msg: 'Benutzer erstellt und erfolgreich angemeldet.' }");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("IsLoggedIn", "", "", "", "");
			$this->validateRPCResponse($res, "IsLoggedIn", "true");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("Logout", "", "", "", "");
			$this->validateRPCResponse($res, "Logout", "true");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("IsLoggedIn", "", "", "", "");
			$this->validateRPCResponse($res, "IsLoggedIn", "false");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("Auth", "test", "test", "", "");
			$this->validateRPCResponse($res, "Auth", "true");
		}
		
		function testPlantCreation() {
			$res = $this->m_Controller->HandleRemoteProcedureCall("CreatePlant", "", "", "", "");
			$this->validateRPCResponse($res, "CreatePlant", "{ success: false, msg: 'Sie sind nicht angemeldet' }");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("Register", "test", "test", "", "");
			$res = $this->m_Controller->HandleRemoteProcedureCall("Auth", "test", "test", "", "");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("CreatePlant", "", "", "newplant", "");
			$this->validateRPCResponse($res, "CreatePlant", "{ success: true, msg: 'Es wurde eine neue Pflanze für sie angelegt.' }");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("GetPlantList", "", "", "", "");
			$this->validateRPCResponseWithoutData($res, "GetPlantList");
			$expectedEnd = "Name: 'newplant', Code: '' }] }";
			substr($res->m_Data, -count($expectedEnd), 0);
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("SavePlant", "test", "", "newplant", "a b c");
			$this->validateRPCResponse($res, "SavePlant", "{ success: true, msg: '' }");
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("GetPlantList", "", "", "", "");
			$this->validateRPCResponseWithoutData($res, "GetPlantList");
			$expectedEnd = "Name: 'newplant', Code: 'a b c' }] }";
			substr($res->m_Data, -count($expectedEnd), 0);
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("DeletePlant", "test", "", "newplant", "");
			$this->validateRPCResponse($res, "DeletePlant", "{ success: true, msg: '' }");
		}
		
		function testValidation() {
			
			$res = $this->m_Controller->HandleRemoteProcedureCall("Register", "test", "test", "", "");
			$res = $this->m_Controller->HandleRemoteProcedureCall("Auth", "test", "test", "", "");
			$res = $this->m_Controller->HandleRemoteProcedureCall("CreatePlant", "", "", "newplant", "RULE a GROW BY 1");
			
			$GLOBALS['ValidatorFile'] = '../cgi/validate';
			$res = $this->m_Controller->HandleRemoteProcedureCall("ValidatePlant", "test", "", "newplant", "");
			$this->validateRPCResponse($res, "ValidatePlant", "{\"valid\":true}");
			
		}
	}
?>
