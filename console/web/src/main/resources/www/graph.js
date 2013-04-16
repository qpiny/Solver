var Graph = function(placeholder, name, options) {
	this.isActive = function() {
		return (this.accordion.accordion("option", "active") !== false)
	};

	this.addValue = function(_timestamp, _value) {
		var l = this.data.length;
		var timestamp = parseInt(_timestamp);
		if (typeof _value == 'object' && 'current' in _value) {
			var value = parseInt(_value.current);
			var min = parseInt(_value.min);
			var max = parseInt(_value.max);
			if (l > 0) {
				var previous = this.data[l - 1];
				var deltats = 1000 / (timestamp - previous[0]);
				var mindelta = (min - previous[3]) * deltats;
				var maxdelta = (max - previous[2]) * deltats;
				var current = (value - previous[1]) * deltats;
				this.rate.push([ timestamp, current, mindelta, maxdelta ]);
			} else
				this.rate.push([ timestamp, 0, 0, 0 ]);
			this.data.push([ timestamp, value, min, max ]);
		} else {
			var value = parseInt(_value);
			if (l > 0) {
				var previous = this.data[l - 1];
				var deltats = 1000 / (timestamp - previous[0])
				var current = (value - previous[1]) * deltats;
				this.rate.push([ timestamp, current, current, current ]);
			} else
				this.rate.push([ timestamp, 0, 0, 0 ]);
			this.data.push([ timestamp, value, value, value ]);
		}
		this.draw();
	};

	this.draw = function() {
		if (this.isActive()) {
			this.graph = $.plot(this.graphPlaceholder, [ {
				fillArea : [ {
					representation : "asymmetric"
				} ],
				data : this.data,
				label : "Data",
				yaxis : 1
			}, {
				fillArea : [ {
					representation : "asymmetric"
				} ],
				data : this.rate,
				label : "Rate (/s)",
				yaxis : 2
			} ], this.options);
		}
	};

	this.name = name;
	this.options = $.extend({}, {
		series : {
			shadowSize : 0
		},
		yaxes : [ {
			min : 0,
			position : "left"
		}, {
			position : "right"
		} ],
		xaxes : [ {
			mode : "time",
			position : "bottom",
			timeformat : "%H:%M:%S",
			minTickSize : [ 5, "second" ]
		} ],
		legend : {
			position : "nw"
		}
	}, options);
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
	this.data = [];
	this.rate = [];
	this.draw();
}