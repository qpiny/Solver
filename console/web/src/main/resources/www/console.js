

var GraphMgr = {
	graphList : {},

	addGraph : function(name, options) {
		this.graphList[name] = new Graph($('#graphs'), name, options);
	},

	addValue : function(name, timestamp, value) {
		var g = this.graphList[name];
		if (g === undefined) {
			this.addGraph(name);
			g = this.graphList[name];
		}
		g.addValue(timestamp, value);
	}
};

$(function() {
	var ws = $.websocket("ws://localhost:8888/websocket/", {
		open : function() {
			ws.send('MonitorSubscribe', [ '*' ]);
		},
		events : {
			MonitorData : function(e) {
				var timestamp = e.data.timestamp;
				$.each(e.data.counters, function(name, value) {
					GraphMgr.addValue(name, timestamp, value);
				});
				$.each(e.data.variables, function(name, data) {
					GraphMgr.addValue(name, timestamp, data)
				});
			}
		}

	});

	$('#start').click(function() {
		ws.send('StartComputation')
	});
});