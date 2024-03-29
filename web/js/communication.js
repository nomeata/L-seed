if (typeof console == "undefined") {
    console = {
        dir: function() { }
		, monitor: function() { }
		, debug: function() { }
		, profile: function() { }
		, log: function() { }
		, error: function() { }
		, warn: function() { }
		, info: function() { }
		, profileEnd: function() { }
		, unmonitor: function() { }
    };
}
if (typeof Lseed == "undefined") {
    Lseed = {};
}

Lseed.MessageCommands = {
	RPC: 'RPC'
	,ContentRequest: 'ContentRequest'
};

Lseed.Communication = function() {
	
	this.Username = '';

	// ----- Framework -----
	
	this.RequestCallbacks = {};
	
	this.sendMessage = function (cmd, opts) {
		var params = {};
		Ext.apply(params, opts, { cmd: cmd });
		Ext.Ajax.request({
			url: 'php/Communication.php',
			success: this.handleResponse.createDelegate(this)
			,failure: function(response, opts) {
				Ext.MessageBox.alert("Fehler", "Es konnte keine Verbindung zum Server hergestellt werden.")
							  .setIcon(Ext.MessageBox.ERROR);
			}
			,params: params
		});
	};

	this.handleResponse = function (response, opts) {
		Ext.MessageBox.hide();
		console.log("'"+response.responseText+"'");
		try {
			if (!response.responseText) {
				console.error("empty answer");
				return;
			}
			var obj = Ext.decode(response.responseText);
			var cmd = obj.cmd;

			switch (cmd) {
				case 'RPC':
					obj.func.CreateDelegate(this)(obj.data);
					break;
				case 'RPC-Response':
					this.RequestCallbacks[obj.calledFunc](obj.data);
					break;
				case 'Message':
					this.showMessage(obj.msg, obj.type);
					break;
				case 'Content':
					this.stopWaitingForPage();
					
					this.showContent(obj);
					break;
			}
		} catch (e) {
			console.error(e);
		}
	};
	
	this.showMessage = function (msg, type) {
		var title = "Unbekannte Nachricht";
		if (type == 'error') {
			title = "Fehler";
		} else if (type == "info") {
			title = "Information";
		}
		var msgbox = Ext.MessageBox.alert(title, msg);
		if (type == 'error') {
			msgbox.setIcon(Ext.MessageBox.ERROR);
		} else if (type == "info") {
			msgbox.setIcon(Ext.MessageBox.INFO);
		}
	};
	
	this.AddCallback = function (hook, func) {
		this.RequestCallbacks[hook] = func;
	};


	// ----- Application -----
	
	this.showContent = function (contentmsg) {
		switch(contentmsg.contentname) {
			case "nav":
			case "adminnav":
				this.loadNavigation(contentmsg.content);
				break;
			default:
				content = contentmsg.content;
				if (contentmsg.contentname == "previewplant") {
					content.plantid = contentmsg.plantid;
					var dd = new Date().getTime();
					content.html = '<img src="php/PlantImages.php?plantid='+contentmsg.plantid+'&dd='+dd+'"/>';
				}
				this.loadTab(content, contentmsg.contentname);
				if (contentmsg.contentname == "editplant") {
					editor.EditCallback();
				}
				break;
		}
	};
	
	// === LOGIN ===
	
	this.isLoggedInCallback = function (loggIn) {
		if (loggIn) {
			Ext.MessageBox.wait("Navigation wird geladen.", "Wird geladen...");
			
			this.sendMessage(Lseed.MessageCommands.ContentRequest, {content: 'nav'});
		} else {
			this.showLoginDialog();
		}
	};
	
	this.AuthCallback = function (success) {
		if (success) {
			Ext.MessageBox.wait("Navigation wird geladen.", "Wird geladen...");
			
			this.sendMessage(Lseed.MessageCommands.ContentRequest, {content: 'nav'});
		} else {
			this.showLoginDialog();
			this.showMessage(
				"Es trat ein Fehler beim einloggen auf. Bitte überprüfen sie ihre eingaben und versuchen sie es erneut.", 
				"error");
		}
	};

	this.showLoginDialog = function () {
		var loginDialog = Ext.getCmp("loginDialog");
		if (!loginDialog) {
			loginDialog = new Ext.Window({
				id: 'loginDialog'
				,title: 'Login'
				,modal: true
				,closable: false
				,tbar: ['->',{
					text: 'Register'
					,icon: 'img/icons/user_add.png'
					,cls: 'x-btn-text-icon'
					,handler: function() {
						loginDialog.hide();
						this.showRegisterDialog();
					}.createDelegate(this)
				}]
				,items: [{
					id: 'loginDialogForm'
					,xtype: 'form'
					,labelWidth: 75
					,bodyStyle:'padding:5px 5px 0'
					,width: 350
					,defaults: {
						width: 230
						,labelStyle: 'padding-right:5px'
					}
					,defaultType: 'textfield'
					,items: [{
						id: 'loginDialogFormUsername'
						,fieldLabel: 'Benutzername'
						,name: 'username'
						,allowBlank:false
					},{
						id: 'loginDialogFormPassword'
						,fieldLabel: 'Password'
						,name: 'password'
						,inputType: 'password'
						,allowBlank:false
					}]
					,buttons: [{
						text: 'OK'
						,handler: this.login.createDelegate(this)
					}]
				}]
			});
		}
		
		loginDialog.show();
	};
	
	this.login = function() {
		var cmpUser = Ext.getCmp('loginDialogFormUsername');
		var cmpPw = Ext.getCmp('loginDialogFormPassword');
		if (cmpUser && cmpUser.isValid() && 
		    cmpPw && cmpPw.isValid()) {
			var user = cmpUser.getValue();
			var pw = cmpPw.getValue();
			pw = MD5(pw);
			Ext.MessageBox.wait("Authentifiziere.", "Wird geladen...");
				
			this.Username = user;
			
			this.sendMessage(Lseed.MessageCommands.RPC, { func: 'Auth', user: user, pw: pw });
			this.hideLoginDialog();
		} else {
			this.showMessage("Ungültige Eingabe. Bitte überprüfen.", "error");
		}
	};
	
	this.logout = function() {
		Ext.MessageBox.wait("Deauthentifiziere.", "Wird geladen...");
		
		this.sendMessage(Lseed.MessageCommands.RPC, { func: 'Logout' });
		this.clearNavigation();
		this.closeAllTabs();
		this.showLoginDialog();
	};

	this.hideLoginDialog = function () {
		var cmp = Ext.getCmp('loginDialog');
		if (cmp) {
			cmp.hide();
		} else {
			console.error("Lseed.Communication.hideLoginDialog: loginDialog couldn't be found");
		}
	};
	
	// === REGISTER ===

	this.showRegisterDialog = function () {
		var registerDialog = Ext.getCmp("registerDialog");
		if (!registerDialog) {
			registerDialog = new Ext.Window({
				id: 'registerDialog'
				,title: 'Register'
				,modal: true
				,closable: true
				,listeners: {
					close: this.showLoginDialog
				}
				,items: [{
					id: 'registerDialogForm'
					,xtype: 'form'
					,labelWidth: 75
					,bodyStyle:'padding:5px 5px 0'
					,width: 350
					,defaults: {
						width: 230
						,labelStyle: 'padding-right:5px'
					}
					,defaultType: 'textfield'
					,items: [{
						id: 'registerDialogFormUsername'
						,fieldLabel: 'Benutzername'
						,name: 'username'
						,allowBlank:false
					},{
						id: 'registerDialogFormPassword'
						,fieldLabel: 'Password'
						,name: 'password'
						,inputType: 'password'
						,allowBlank:false
					},{
						id: 'registerDialogFormPasswordRepeat'
						,fieldLabel: 'Password Wiederholung'
						,name: 'passwordRepeat'
						,inputType: 'password'
						,allowBlank:false
					}]
					,buttons: [{
						text: 'Register'
						,handler: this.register.createDelegate(this)
					},{
						text: 'Cancel'
						,handler: function() {
							this.hideRegisterDialog();
							this.showLoginDialog();
						}.createDelegate(this)
					}]
				}]
			});
		}
		
		registerDialog.show();
	};

	this.hideRegisterDialog = function () {
		var cmp = Ext.getCmp('registerDialog');
		if (cmp) {
			cmp.hide();
		} else {
			console.error("Lseed.Communication.hideRegisterDialog: registerDialog couldn't be found");
		}
	};
	
	this.register = function() {
		var cmpUser = Ext.getCmp('registerDialogFormUsername');
		var cmpPw = Ext.getCmp('registerDialogFormPassword');
		var cmpPwRepeat = Ext.getCmp('registerDialogFormPasswordRepeat');
		if (cmpUser && cmpUser.isValid() && 
		    cmpPw && cmpPw.isValid() && 
		    cmpPwRepeat && cmpPwRepeat.isValid()) {
			var user = cmpUser.getValue();
			var pw = cmpPw.getValue();
			var pwRepeat = cmpPwRepeat.getValue();
			if (pw == pwRepeat) {
				pw = MD5(pw);
				Ext.MessageBox.wait("Registriere.", "Wird geladen...");
				
				this.Username = user;

				this.sendMessage(Lseed.MessageCommands.RPC, { func: 'Register', user: user, pw: pw });
				this.hideRegisterDialog();
			} else {
				this.showMessage("Passwörter stimmen nicht überein. Bitte korrigieren.", "error");
			}
		} else {
			this.showMessage("Ungültige Eingabe. Bitte überprüfen.", "error");
		}
	};
	
	this.RegisterCallback = function (data) {
		if (data.success) {
			Ext.MessageBox.wait("Navigation wird geladen.", "Wird geladen...");
			
			this.sendMessage(Lseed.MessageCommands.ContentRequest, {content: 'nav'});
		} else {
			this.showRegisterDialog();
			this.showMessage(
				data.msg, 
				"error");
		}
	};
	
	// === Plant Managerment ===
	
	this.GetPlantList = function() {
		Ext.MessageBox.wait("Pflanzen werden geladen.", "Wird geladen...");
		
		this.sendMessage(Lseed.MessageCommands.RPC, { func: 'GetPlantList' });
	};
	
	this.GetPlantListCallback = function(data) {
		var grid = Ext.getCmp("plantlistgrid");
		if (grid) {
			grid.store.loadData(data.list);
		} else {
			console.error("Lseed.Communication.GetPlantList: 'plantlistgrid' could not be found.");
		}
	}
	
	this.loadNavigation = function(content) {
		var cmp = Ext.getCmp('navTree');
		if (cmp) {
			cmp.setRootNode(content);
		} else {
			console.error("Lseed.Communication.loadNavigation: 'navTree' does not exist.");
		}
	};
	
	this.clearNavigation = function() {
		var cmp = Ext.getCmp('navTree');
		if (cmp) {
			cmp.setRootNode(new Ext.tree.AsyncTreeNode({
				expanded: true
				,children: [{
					text: 'Start'
					,leaf: true
				}]
			}));
		} else {
			console.error("Lseed.Communication.clearNavigation: 'navTree' does not exist.");
		}
	};
	
	this.closeAllTabs = function() {
		var cmp = Ext.getCmp('contentTabPanel');
		if (cmp) {
			var elem = cmp.get(0);
			cmp.removeAll();
			cmp.add(elem);
		} else {
			console.error("Lseed.Communication.closeAllTabs: 'contentTabPanel' does not exist.");
		}
	};
	
	this.closeTab = function(tabname) {
		var cmp = Ext.getCmp('contentTabPanel');
		if (cmp) {
			var tab = this.getTab(tabname);
			if (!tab) {
				console.error("Lseed.Communication.closeTab: '"+tabname+"' does not exist.");
			}
			cmp.remove(tab);
		} else {
			console.error("Lseed.Communication.closeTab: 'contentTabPanel' does not exist.");
		}
	};
	
	this.loadTab = function (content, name) {
		var cmp = Ext.getCmp("contentTabPanel");
		if (cmp) {
			cmp.add(content);
			this.activateTab(name);
		} else {
			console.error("Lseed.Communication.showTab: 'contentTabPanel' does not exist.");
		}
	};
	
	this.getTab = function (tabname) {
		var result = null;
		
		var cmp = Ext.getCmp("contentTabPanel");
		if (cmp) {
			result = cmp.findById("ContentPanel_"+tabname);
		} else {
			console.error("Lseed.Communication.activateTab: 'contentTabPanel' does not exist.");
		}
		
		return result;
	};
	
	this.activateTab = function (tabname) {
		var done = false;
		
		var tab = this.getTab(tabname);
		
		if (tab) {
			var cmp = Ext.getCmp("contentTabPanel");
			if (cmp) {
				cmp.setActiveTab(tab);
				done = true;
			} else {
				console.error("Lseed.Communication.activateTab: 'contentTabPanel' does not exist.");
			}
		}
		
		return done;
	};
	
	this.showTab = function (tabname) {
		var done = false;
		
		done = this.activateTab(tabname);
			
		if (!done) {
			this.waitForPage();
			
			this.sendMessage(Lseed.MessageCommands.ContentRequest, {content: tabname});
		}
	};
	
	this.waitForPage = function() {
		var cmp = Ext.getCmp("loadPageProgressbar");
		if (cmp) {
			cmp.show();
			cmp.wait({
				interval: 200,
				increment: 15,
				text: 'Lädt...'
			});
		} else {
			console.error("Lseed.Communication.waitForPage: loadPageProgressbar is not defined.");
		}
	};
	
	this.stopWaitingForPage = function() {
		var cmp = Ext.getCmp("loadPageProgressbar");
		if (cmp) {
			cmp.updateText("");
			cmp.reset();
		} else {
			console.error("Lseed.Communication.stopWaitingForPage: loadPageProgressbar is not defined.");
		}
	};
	
	// === Season Managerment ===
	
	this.GetSeasonList = function() {
		Ext.MessageBox.wait("Saisons werden geladen.", "Wird geladen...");
		
		this.sendMessage(Lseed.MessageCommands.RPC, { func: 'GetSeasonList' });
	};
	
	this.GetSeasonListCallback = function(data) {
		var grid = Ext.getCmp("seasonlistgrid");
		if (grid) {
			grid.store.loadData(data.list);
		} else {
			console.error("Lseed.Communication.GetSeasonList: 'seasonlistgrid' could not be found.");
		}
	}
	

	this.Init = function(editor) {
		Ext.MessageBox.wait("Programmeinstellungen werden geladen.", "Wird geladen...");

		this.AddCallback("IsLoggedIn", this.isLoggedInCallback.createDelegate(this));
		this.AddCallback("Auth", this.AuthCallback.createDelegate(this));
		this.AddCallback("Register", this.RegisterCallback.createDelegate(this));
		this.AddCallback("GetPlantList", this.GetPlantListCallback.createDelegate(this));
		this.AddCallback("GetSeasonList", this.GetSeasonListCallback.createDelegate(this));
		
		this.AddCallback("CheckSyntax", editor.HandleSyntaxCheckAnswerForEditor.createDelegate(editor));
		
		this.AddCallback("SavePlant", editor.SaveCallback.createDelegate(editor));
		this.AddCallback("DeletePlant", editor.DeleteCallback.createDelegate(editor));
		this.AddCallback("ActivatePlant", editor.ActivateCallback.createDelegate(editor));
		
		this.sendMessage(Lseed.MessageCommands.RPC, {func: 'IsLoggedIn'});
	};
};


Lseed.Editor = function() {
	this.CurrentPlant = {
		Name: ''
		,Definition: ''
	};
	
	this.Save = function(plant) {
		communication.sendMessage(Lseed.MessageCommands.RPC, { 
			func: 'SavePlant'
			,plant: plant.data.Name
			,code: plant.data.Code
		});
	};
	
	this.SaveCallback = function(data) {
		if (data.success) {
			communication.showMessage("Erfolgreich gespeichert.", "info");
		} else {
			communication.showMessage(data.msg, "error");
		}
	};
	
	this.Activate = function(plant) {
		console.log(arguments);
		communication.sendMessage(Lseed.MessageCommands.RPC, { 
			func: 'ActivatePlant'
			,plant: plant.data.Name
			,user: communication.Username
		});
	};
	
	this.ActivateCallback = function(data) {
		if (data.success) {
			communication.showMessage("Erfolgreich aktiviert.", "info");
			communication.GetPlantList();
		} else {
			communication.showMessage(data.msg, "error");
		}
	};
	
	this.Preview = function(plant) {
		communication.sendMessage(Lseed.MessageCommands.ContentRequest, {content: 'previewplant', plantid: plant.data.ID} );
	};
	this.PreviewCallback = function() {
	};
	
	this.Edit = function(plant) {
		this.LastEditPlant = plant;
		var cmp = Ext.getCmp('ContentPanel_editplant');
		if (cmp) {
			communication.activateTab('editplant');
			this.EditCallback();
		} else {
			communication.sendMessage(Lseed.MessageCommands.ContentRequest, {content: 'editplant'});
		}
	};
	this.EditCallback = function() {
		var cmp = Ext.getCmp("editplantdefinitionnamefield");
		if (cmp) {
			cmp.setValue(this.LastEditPlant.data.Name);
		}
		var cmp = Ext.getCmp("editplantdefinitioneditor");
		if (cmp) {
			cmp.setValue(this.LastEditPlant.data.Code);
		}
	};
	
	this.Test = function(plant, callback) {
		communication.showMessage("Diese Funktion ist leider momentan nicht verfügbar", "error");
	};
	
	this.TestCallback = function() {
	};
	
	this.CheckSyntax = function(plant) {
		communication.sendMessage(Lseed.MessageCommands.RPC, { 
			func: 'CheckSyntax'
			,plant: plant.data.Name
			,code: plant.data.Code
		});
	};
	
	this.HandleSyntaxCheckAnswerForEditor = function(data) {
		if (data.success || data.valid) {
			Ext.MessageBox.show({
				title:'Valide'
				,msg: "Alles Super."
				,buttons: Ext.Msg.OK
				,icon: Ext.MessageBox.Info
			});
		} else {
			// This is a hack for now:
			var pdEditor = Ext.getCmp("plantdefinitioneditor");
			if (!pdEditor) {
				var pdEditor = Ext.getCmp("editplantdefinitioneditor");
			}
			if (pdEditor) {
				communication.stopWaitingForPage();
				Ext.MessageBox.show({
					title:'Fehler'
					,msg: data.msg
					,buttons: Ext.Msg.OK
					,fn: function() {
						if (typeof data.line != "undefined") {
							var index = editor.GetStartFromField(pdEditor, data.line-1, data.column);
							if (index != -1) {
								pdEditor.selectText(index-1, index);
							}
						}
					}
					,icon: Ext.MessageBox.ERROR
				});
			} else {
				console.error("Lseed.Editor.CheckSyntaxCallback_Callback: 'plantdefinitioneditor' Could not be found.");
				alert("Es trat ein Fehler auf...sowas.");
			}
		}
	};
	
	this.Delete = function(plant) {
		communication.sendMessage(Lseed.MessageCommands.RPC, { 
			func: 'DeletePlant'
			,id: plant.data.ID
		});
	};
	
	this.DeleteCallback = function(data) {
		if (!data.success) {
			communication.showMessage("Diese Pflanze konnte leider nicht gelöscht werden.", "error");
		}
		communication.GetPlantList();
	};
	
	this.GetStartFromField = function(field, row, column) {
		var result = 0;
		var content = field.getValue();
		var lines = content.split("\n");
		if (lines.length >= row) {
			for (var i=0; i<row; i++) {
				result += lines[i].length + 1;
			}
		}
		result += column;
		
		console.info("error in row: " + row + " column: " + column + " at: " + result);
		
		return result;
	};
};

Lseed.Plant = Ext.data.Record.create([{
	name: 'ID'
	,type: 'int'
}, {
	name: 'Name'
	,type: 'string'
}, {
	name: 'Code'
	,type: 'string'
}, {
	name: 'IsValid'
	,type: 'boolean'
}, {
	name: 'IsActive'
	,type: 'boolean'
}]);

Lseed.Season = Ext.data.Record.create([{
	name: 'ID'
	,type: 'int'
}, {
	name: 'User'
	,type: 'string'
}, {
	name: 'IsRunning'
	,type: 'boolean'
}, {
	name: 'Score'
	,type: 'float'
}]);

