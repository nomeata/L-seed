<?php
	
	class Response
	{
		public $m_Command;
		
		public function __construct($cmd) {
			$this->m_Command = $cmd;
		}
		
		public function send() {
			echo "{ cmd: '".$this->m_Command."' }";
		}
	}
	
	class Message extends Response
	{
		public $m_Type;
		public $m_Message;
		
		public function __construct($type, $msg) {
			parent::__construct("Message");
			$this->m_Type = $type;
			$this->m_Message = $msg;
		}
		
		public function send() {
			echo "{ cmd: '".$this->m_Command."', type: '".$this->m_Type."', msg: '".$this->m_Message."' }";
		}
	}
	
	class ContentMessage extends Response
	{
		public $m_Type;
		public $m_ContentName;
		
		public function __construct($name, $plantid) {
			parent::__construct("Content");
			$this->m_ContentName = $name;
			$this->m_PlantID = $plantid;
		}
		
		public function send() {
			echo "{ cmd: '".$this->m_Command."', contentname: '".$this->m_ContentName."', content: ";
			readfile( "page/".$this->m_ContentName.".pg" );
			if ($this->m_PlantID != null) {
				echo ", plantid: ".$this->m_PlantID;
			};
			echo "}";
		}
	}
	
	class RemoteProcedureCall extends Response
	{
		public $m_Function;
		
		public function __construct($func) {
			parent::__construct("RPC");
			$this->m_Function = $func;
		}
		
		public function send() {
			echo "{ cmd: '".$this->m_Command."', func: '".$this->m_Function."' }";
		}
	}
	
	class RPCAnswer extends Response
	{
		public $m_CalledFunction;
		public $m_Data;
		
		public function __construct($calledFunc, $data) {
			parent::__construct("RPC-Response");
			$this->m_CalledFunction = $calledFunc;
			$data = str_replace("\r", "\\r", $data);
			$data = str_replace("\n", "\\n", $data);
			$data = str_replace("\t", "\\t", $data);
			//TODO escape further
			$this->m_Data = $data;
		}
		
		public function send() {
			echo "{ cmd: '".$this->m_Command."', calledFunc: '".$this->m_CalledFunction."', data: ".$this->m_Data."}";
		}
	}

?>
