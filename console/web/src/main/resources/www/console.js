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
	var memoryData = [ {
		label : "Free memory",
		data : []
	}, {
		label : "Total memory",
		data : []
	} ];
	updateMemoryGraph = function(timestamp, mem) {
		memoryData[0].data.push([ timestamp, mem[0] ]);
		memoryData[1].data.push([ timestamp, mem[1] ]);

		$.plot($('#memory'), memoryData, {
			series : {
				lines : {
					show : true,
					fill : false
				}
			},
			xaxis : {
				mode : "time",
				position : "bottom",
				timeformat : "%H:%M:%S",
				minTickSize : [ 5, "second" ]
			}
		});
	};
	var ws = $.websocket("ws://localhost:8888/websocket/", {
		open : function() {
			ws.send('MonitorSubscribe', [ '*' ]);
		},
		events : {
			MonitorData : function(e) {
				var timestamp = e.data.timestamp;
				updateMemoryGraph(timestamp, e.data.memory)
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