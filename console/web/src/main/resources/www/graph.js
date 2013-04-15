var CounterGraph = function(placeholder, name, options) {
	this.init(placeholder, name, options);
}
$.extend(CounterGraph.prototype, {
	isActive : function() {
		return (this.accordion.accordion("option", "active") !== false)
	},

	draw : function() {
		if (this.isActive()) {
			this.graph.setData([ {
				data : this.data,
				label : "Data",
				yaxis : 1
			}, {
				data : this.delta,
				label : "Delta (/s)",
				yaxis : 2
			} ]);
			this.graph.setupGrid();
			this.graph.draw();
		}
	},

	addValue : function(_timestamp, _value) {
		var timestamp = parseInt(_timestamp);
		var value = parseInt(_value);
		var l = this.data.length;
		if (l > 0) {
			var previous = this.data[l - 1];
			var deltats = 1000 / (timestamp - previous[0])
			this.delta.push([ timestamp, (value - previous[1]) * deltats ]);
		}
		this.data.push([ timestamp, value ]);
		this.draw();
	},

	create : function() {
		this.graph = $.plot(this.graphPlaceholder, {
			label : this.name,
			data : [ {
				data : this.data,
				label : "Data",
				yaxis : 1
			}, {
				data : this.delta,
				label : "Delta (/s)",
				yaxis : 2
			} ]
		}, this.options);
	},

	defaultOptions : {
		series : {
			shadowSize : 0
		},
		yaxis : [ {
			//min : 0,
			position: "left"
		}, {
			//min : 0,
			position: "right"
		} ],
		xaxis : {
			mode : "time",
			position : "bottom",
			timeformat : "%H:%M:%S",
			minTickSize : [ 5, "second" ]
		},
		legend : {
			position : "nw"
		}
	},

	init : function(placeholder, name, options) {
		this.name = name;
		this.options = $.extend({}, this.defaultOptions, options);
		this.data = [];
		this.delta = [];
		this.graphPlaceholder = $(document.createElement('div')).addClass(
				'graph')
		this.accordion = $(document.createElement('div')).append(
				'<h3>' + name + '</h3>').append(this.graphPlaceholder);
		placeholder.append(this.accordion);
		this.accordion.accordion({
			active : 0,
			animate : 'easeInOutQuint',
			collapsible : true,
			heightStyle : 'content'
		});
		this.create()
	}
});

var VariableGraph = function(placeholder, name, options) {
	this.init(placeholder, name, options);
};

$.extend(VariableGraph.prototype, CounterGraph.prototype, {
	create : function() {
		this.graph = $.plot(this.graphPlaceholder, {
			label : this.name,
			data : [ {
				errorbars : 'y',
				yerr : {
					show : true,
					upperCap : "-",
					lowerCap : "-",
					radius : 5
				},
				data : this.data,
				label : "Data",
				yaxis : 1
			}, {
				errorbars : 'y',
				yerr : {
					show : true,
					upperCap : "-",
					lowerCap : "-",
					radius : 5
				},
				data : this.delta,
				label : "Delta (/s)",
				yaxis : 2
			} ]
		}, this.options);
	},

	addValue : function(_timestamp, _value, _min, _max) {
		var timestamp = parseInt(_timestamp);
		var min = parseInt(_min);
		var value = parseInt(_value);
		var max = parseInt(_max);
		var minerr = value - min;
		var maxerr = max - value;
		var l = this.data.length;
		if (l > 0) {
			var previous = this.data[l - 1];
			var mindelta = (previous[1] + previous[3]) - min;
			var maxdelta = max - (previous[1] - previous[2]);
			var deltats = 1000 / (timestamp - previous[0]);
			this.delta.push([ timestamp, (value - previous[1]) * deltats,
					mindelta * deltats, maxdelta * deltats ]);
		}
		this.data.push([ timestamp, value, minerr, maxerr ]);
		this.draw();
	}
});