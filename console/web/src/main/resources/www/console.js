var CounterGraph = function(placeholder, name, type, options) {
	this.name = name;
	this.type = type;
	this.options = options;
	this.data = [];
	this.derive = [];
	this.graphPlaceholder = $(document.createElement('div')).addClass('graph')
	this.accordion = $(document.createElement('div')).append(
			'<h3>' + name + '</h3>').append(this.graphPlaceholder);
	placeholder.append(this.accordion);
	this.accordion.accordion({
		active : 0,
		animate : 'easeInOutQuint',
		collapsible : true,
		heightStyle : 'content'
	});
	this.graph = $.plot(this.graphPlaceholder, {
		label : this.name,
		data : [ {
			data : this.data,
			label : "Data",
			yaxis : 1
		}, {
			data : this.derive,
			label : "Derive",
			yaxis : 2
		} ]
	}, this.options);
};

CounterGraph.prototype.draw = function() {
	// this.graph = $.plot(this.graphPlaceholder, {
	// label : this.name,
	// data : [ {
	// data : this.data,
	// label : "Data",
	// yaxis : 1
	// }, {
	// data : this.derive,
	// label : "Derive",
	// yaxis : 2
	// } ]
	// }, this.options);
	this.graph.setData([ {
		data : this.data,
		label : "Data",
		yaxis : 1
	}, {
		data : this.derive,
		label : "Derive",
		yaxis : 2
	} ]);
	this.graph.setupGrid();
	this.graph.draw();
};

CounterGraph.prototype.isActive = function() {
	return (this.accordion.accordion("option", "active") !== false)
};

CounterGraph.prototype.addValue = function(timestamp, value) {
	var ts = parseInt(timestamp);
	var v = parseInt(value);
	var l = this.data.length;
	if (l > 0)
		this.derive.push([ ts, v - this.data[l - 1][1] ]);
	this.data.push([ ts, v ]);
	this.draw();
	// this.graph.setupGrid();
	// this.graph.draw();
};

var GraphMgr = {
	graphList : {},

	defaultOptions : {
		series : {
			shadowSize : 0
		},
		yaxis : [ {
			min : 0
		}, {
			position : "right"
		} ],
		xaxis : {
			mode : "time",
			position : "bottom",
			timeformat : "%H:%M:%S",
			minTickSize : [ 5, "second" ]
		}
	},

	addCounterGraph : function(name, options) {
		this.graphList[name] = new CounterGraph($('#graphs'), name, type, $
				.extend({}, this.defaultOptions, options));
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
			}
		}

	});

	$('#start').click(function() {
		ws.send('StartComputation')
	});
});