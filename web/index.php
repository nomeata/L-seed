<?php
	function __autoload($class_name) { require_once $class_name . '.php'; }

//require("php/Communication.php");

//$res = GetPlantList();
//echo $res;


?><!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html>
	<head>
		<title>L-Seed</title>

		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

		<link rel="stylesheet" type="text/css" href="ext/resources/css/ext-all.css" />
		<link rel="stylesheet" type="text/css" href="ext/resources/css/xtheme-slate.css" />
		<style type="text/css">
			#loading {
			  position: absolute;
			  left: 45%;
			  top: 40%;
			  margin-left: -45px;
			  padding: 2px;
			  z-index: 20001;
			  height: auto;
			  border: 1px solid #ccc;
			}

			#loading a {
			  color: #225588;
			}

			#loading .loading-indicator {
			  background: white;
			  color: #444;
			  font: bold 13px tahoma, arial, helvetica;
			  padding: 10px;
			  margin: 0;
			  height: auto;
			  width:130px;
			}

			#loading .loading-indicator img {
			  margin-right:8px;
			  float:left;
			  vertical-align:top;
			}

			#loading-msg {
			  font: normal 10px arial, tahoma, sans-serif;
			}

			#ContentPanel_start h1 {
				margin-bottom:1em;
			}
			#ContentPanel_start p {
				margin-bottom:1em;
			}

		</style>

		<script type="text/javascript" src="ext/adapter/ext/ext-base.js"></script>
		<script type="text/javascript" src="ext/ext-all-debug.js"></script>
		<script type="text/javascript" src="js/md5.js"></script>
		<script type="text/javascript" src="js/communication.js"></script>
		<script type="text/javascript" src="js/codepress/codepress.js"></script>
		<script type="text/javascript">
			communication = new Lseed.Communication();
			editor = new Lseed.Editor();

			Ext.onReady(function() {
				Ext.QuickTips.init();

				new Ext.Viewport({
				    layout: 'border'
				    ,items: [{
						region: 'north'
						,html: '<h1 class="x-panel-header">L-Seed Manager</h1>'
						,autoHeight: true
						,border: false
						,margins: '0 0 5 0'
					},{
						id: 'navTree'
						,region: 'west'
						,collapsible: true
						,title: 'Navigation'
						,xtype: 'treepanel'
						,width: 200
						,autoScroll: true
						,split: true
						,tbar: [{
							xtype: "button"
							,text: "Logout"
							,handler: communication.logout.createDelegate(communication)
						}]
						,loader: new Ext.tree.TreeLoader()
						,root: new Ext.tree.AsyncTreeNode({
							expanded: true
							,children: [{
								text: 'Start'
								,leaf: true
							}]
						})
						,rootVisible: false
						,listeners: {
							click: function(obj) {
								communication.showTab(obj.attributes.id);
							}
						}
						,bbar: [{
							id: 'loadPageProgressbar'
							,xtype: 'progress'
						}]
					},{
						id: 'contentTabPanel'
						,region: 'center'
						,xtype: 'tabpanel'
						,activeTab: 0
						,items: [{
							id: 'ContentPanel_start'
							,style: 'padding: 5px;'
							,title: 'Start'
							,html: ' <h1>Willkommen bei L-Seed, dem Spiel zur GPN</h1> <p> Auf dieser Webseite kannst du die Genome für deine Pflanzen speichern, die dann laufend auf dem Beamer angezeigt werden, sowie Statistiken dazu betrachen. Mehr Informationen findest du auf <a href="http://entropia.de/wiki/L-seed">http://entropia.de/wiki/L-seed</a>.  </p> <p> Der <a href="/git">Quellcode</a> (als Git-Repository) zu L-seed ist auch (vermutlich schneller) von diesem Server zu beziehen.  </p> <p> Bei Fragen wendest du dich bitte an Joachim Breitner, <tt>nomeata</tt> im IRC, <tt>nomeata@joachim-breitner.de</tt> im Jabber oder gern auch persönlich, wenn du mir über den Weg läufst.  </p> '
						}]
					},{
						region: 'south'
						,title: 'Statusanzeige'
						,collapsible: true
						,collapsed: true
						,html: 'Statusanzeige'
						,split: true
						,height: 100
						,minHeight: 100
					}]
				});

				communication.Init(editor);
			});
		</script>
	</head>
	<body style="overflow: hidden">


		<div id="content"/>
	</body>
</html>
