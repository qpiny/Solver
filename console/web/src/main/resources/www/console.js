

var GraphMgr = {
	graphList : {},

	addCounterGraph : function(name, options) {
		this.graphList[name] = new CounterGraph($('#graphs'), name, options);
	},

	addVariableGraph : function(name, options) {
		this.graphList[name] = new VariableGraph($('#graphs'), name, options);
	},

	addValue : function(name, type, timestamp, value, min, max) {
		var g = this.graphList[name];
		if (g === undefined) {
			if (type === 'counter')
				this.addCounterGraph(name);
			else
				this.addVariableGraph(name);
			g = this.graphList[name];
		}
		g.addValue(timestamp, value, min, max);
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
					GraphMgr.addValue(name, 'counter', timestamp, value);
				});
				$.each(e.data.variables, function(name, data) {
					GraphMgr.addValue(name, 'variable', timestamp, data.current, data.min, data.max)
				});
			}
		}

	});

	$('#start').click(function() {
		ws.send('StartComputation')
	});
});