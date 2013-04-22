var GraphMgr = {
	graphList : {},

	addGraph : function(name, options) {
		this.graphList[name] = new Graph($("#graphs"), name, options);
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
		label : "Used memory",
		data : []
	}, {
		label : "Free memory",
		data : []
	} ];

	var cpuData = [ {
		label : "CPU load",
		data : []
	}, {
		label : "GC load",
		data : []
	} ];

	var previousGC = {
		ts : 0,
		load : 0
	};

	updateSystemGraph = function(_timestamp, system) {
		var timestamp = parseInt(_timestamp);
		var totalMemory = parseInt(system.totalMemory);
		var freeMemory = parseInt(system.freeMemory);

		memoryData[0].data.push([ timestamp, totalMemory - freeMemory ]);
		memoryData[1].data.push([ timestamp, freeMemory ]);

		$.plot($("#memory"), memoryData, {
			series : {
				stack: true,
				lines : {
					show : true,
					fill : true,
				},
				shadowSize : 0
			},
			xaxis : {
				mode : "time",
				position : "bottom",
				timeformat : "%H:%M:%S",
				minTickSize : [ 5, "second" ]
			},
			yaxis : {
				min : 0,
				tickFormatter: function(val, axis) {
					if (val >= 1000000000)
						return (val / 1000000000).toFixed(axis.tickDecimals) + " GB";
					else if (val >= 1000000)
				        return (val / 1000000).toFixed(axis.tickDecimals) + " MB";
				    else if (val >= 1000)
				        return (val / 1000).toFixed(axis.tickDecimals) + " kB";
				    else
				        return val.toFixed(axis.tickDecimals) + " B";
				}
			},
			legend : {
				position : "nw"
			}
		});

		var cpuLoad = parseInt(system.cpuLoad);
		var cpuCount = parseInt(system.cpuCount);
		var gcTime = parseInt(system.gcTime);

		if (cpuLoad >= 0)
			cpuData[0].data.push([ timestamp, cpuLoad / cpuCount ]);
		cpuData[1].data.push([
				timestamp,
				(gcTime - previousGC.load) / (timestamp - previousGC.ts) ])
		previousGC.ts = timestamp;
		previousGC.load = gcTime;
		$.plot($("#cpu"), cpuData, {
			series : {
				shadowSize : 0
			},
			xaxis : {
				mode : "time",
				position : "bottom",
				timeformat : "%H:%M:%S",
				minTickSize : [ 5, "second" ]
			},
			yaxis : {
				min : 0,
				max: 1,
				tickFormatter: function(val, axis) {
					return (100 * val) + "%";
				}
			},
			legend : {
				position : "nw"
			}
		});
	};

	var port = document.location.port;
	port = typeof port !== "undefined" && port !== null ? ":" + port : "";
	var ws = $.websocket("ws://" + document.location.host + port + "/websocket/", {
		open : function() {
			ws.send("MonitorSubscribe", [ "*" ]);
		},
		events : {
			MonitorData : function(e) {
				var timestamp = e.data.timestamp;
				updateSystemGraph(timestamp, e.data.system)
				$.each(e.data.counters, function(name, value) {
					GraphMgr.addValue(name, timestamp, value);
				});
				$.each(e.data.variables, function(name, data) {
					GraphMgr.addValue(name, timestamp, data)
				});
			}
		}

	});

	$("#start").click(function() {
		ws.send("StartComputation")
	});
});
