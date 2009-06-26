<?php

	class Server
	{
		protected $m_Controller;
		
		public function	__construct($controller) {
			$this->m_Controller = $controller;
		}

		public function ReturnContent($content) {
			switch ($content) {
				case "myplants":
				case "createplant":
				case "editplant":
				case "testplant":
					new ContentMessage($content).send();
					break;
				case "nav":
					if ($this->IsAdmin() != "false") {
						new ContentMessage("adminnav").send();
					} else {
						new ContentMessage($content).send();
					}
				break;
				case "debug":
					if ($this->IsAdmin() != "false") {
						new ContentMessage($content).send();
					}
				break;
			}
		}

		public function IsLoggedIn() {
			$result = "false";
			
			if (isset($_SESSION['user']) and isset($_SESSION['pw'])) {
				$user = $_SESSION['user'];
				$md5pw = $_SESSION['pw'];

				$userobj = $this->m_Controller->GetDatabase()->GetUser($user);
				if ($userobj != null and $userobj->Password == $md5pw) {
					$result = "true";
				}
			}

			return $result;
		}

		public function IsAdmin() {
			$result = "false";
			
			$user = $controller->GetUser();
			if ($user != null) {
				if ($user->IsAdmin) {
					$result = "true";
				}
			}

			return $result;
		}

		public function LogIn($username, $md5pw) {
			$result = "false";
			$user = $this->m_Controller->GetDatabase()->GetUser($username);
			
			if ($user != null and $user->Name == $username and $user->Password == $md5pw) {
				$_SESSION['user'] = $user->Name;
				$_SESSION['pw'] = $md5pw;

				$result = "true";
			}

			return $result;
		}

		public function LogOut() {
			$_SESSION['user'] = "";
			$_SESSION['pw'] = "";
			$user = $this->m_Controller->SetUser(null);
			return "true";
		}

		public function Register($username, $md5pw) {
			$result = "{ success: true, msg: '' }";
			
			$userobj = $this->m_Controller->GetDatabase()->GetUser($username);

			if ($userobj == null) {
				if (!$this->m_Controller->GetDatabase()->CreateUser($username, $md5pw)) {
					$result = "{ success: false, msg: 'Benutzer konnte nicht erstellt werden.' }";
				} else {
					if ($this->LogIn($username, $md5pw) == "true") {
						$result = "{ success: true, msg: 'Benutzer erstellt und erfolgreich angemeldet.' }";
					} else {
						$result = "{ success: true, msg: 'Benutzer erstellt, die Anmeldung schlug jedoch fehl.' }";
					}
				}
			} else {
				$result = "{ success: false, msg: 'Benutzername bereits vergeben.' }";
			}

			return $result;
		}
	}
?>
