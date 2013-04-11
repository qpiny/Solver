var Graph = function(placeholder, name, type, options) {
	this.name = name;
	this.type = type;
	this.data = []
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
		label : name,
		data : []
	}, options);
};

Graph.prototype.isActive = function() {
	return (this.accordion.accordion("option", "active") !== false)
};

Graph.prototype.addValue = function(timestamp, value) {
	this.data.push([parseInt(timestamp), parseInt(value)]);
	this.graph.setData([ this.data ]);
	this.graph.setupGrid();
	this.graph.draw();
};

var GraphMgr = {
	graphList: {},
	
	defaultOptions: {
		series : {
			shadowSize : 0
		},
		yaxis : {
			min : 0,
			max : 255
		},
		xaxis : {
			mode : "time",
			timeformat : "%H:%M:%S",
			minTickSize : [ 5, "second" ]
		}
	},
	
	addGraph: function(name, type, options) {
		this.graphList[name] = new Graph($('#graphs'), name, type, $.extend({}, this.defaultOptions, options));
	},

	addValue: function(name, timestamp, value) {
		var g = this.graphList[name];
		if (g === undefined) {
			this.addGraph(name);
			g = this.graphList[name];
		}
		g.addValue(timestamp, value);
	}
};

$(function() {

//	GraphMgr.addGraph("random", "no used yet", {
//		series : {
//			shadowSize : 0
//		},
//		yaxis : {
//			min : 0,
//			max : 255
//		},
//		xaxis : {
//			mode : "time",
//			timeformat : "%H:%M:%S",
//			minTickSize : [ 5, "second" ]
//		}
//	});
	
	
	/*
	function update() {
		GraphMgr.addValue("random", new Date().getTime(), Math.floor(Math.random() * 100));
		setTimeout(update, 1000);
	};
	update();
	*/

	var ws = $.websocket("ws://localhost:8888/websocket/", {
		open : function() {
			$('#log').append('\nsubscribe');
			ws.send('subscribe');
		},
		events : {
			data : function(e) {
				var timestamp = e.data.timestamp;
				$.each(e.data, function(name, value) {
					if (name != 'timestamp')
						GraphMgr.addValue(name, timestamp, value);
				});
				//$('#log').append('\n' + e.data.random1);
				// data.push([ parseInt(e.data.timestamp),
				// parseInt(e.data.random1) ]);
				// plot.setData([ data ]);
				// plot.setupGrid();
				// plot.draw();
				
			}
		}

	});
});