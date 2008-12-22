/*
Script: Moo.js
	My Object Oriented javascript.

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.

Credits:
	- Class is slightly based on Base.js <http://dean.edwards.name/weblog/2006/03/base/> (c) 2006 Dean Edwards, License <http://creativecommons.org/licenses/LGPL/2.1/>
	- Some functions are based on those found in prototype.js <http://prototype.conio.net/> (c) 2005 Sam Stephenson sam [at] conio [dot] net, MIT-style license
	- Documentation by Aaron Newton (aaron.newton [at] cnet [dot] com) and Valerio Proietti.
*/

/*
Class: Class
	The base class object of the <http://mootools.net> framework.

Arguments:
	properties - the collection of properties that apply to the class. Creates a new class, its initialize method will fire upon class instantiation.

Example:
	(start code)
	var Cat = new Class({
		initialize: function(name){
			this.name = name;
		}
	});
	var myCat = new Cat('Micia');
	alert myCat.name; //alerts 'Micia'
	(end)
*/

var Class = function(properties){
	var klass = function(){
		if (this.initialize && arguments[0] != 'noinit') return this.initialize.apply(this, arguments);
		else return this;
	};
	for (var property in this) klass[property] = this[property];
	klass.prototype = properties;
	return klass;
};

/*
Property: empty
	Returns an empty function
*/

Class.empty = function(){};

Class.prototype = {

	/*
	Property: extend
		Returns the copy of the Class extended with the passed in properties.

	Arguments:
		properties - the properties to add to the base class in this new Class.

	Example:
		(start code)
		var Animal = new Class({
			initialize: function(age){
				this.age = age;
			}
		});
		var Cat = Animal.extend({
			initialize: function(name, age){
				this.parent(age); //will call the previous initialize;
				this.name = name;
			}
		});
		var myCat = new Cat('Micia', 20);
		alert myCat.name; //alerts 'Micia'
		alert myCat.age; //alerts 20
		(end)
	*/

	extend: function(properties){
		var pr0t0typ3 = new this('noinit');

		var parentize = function(previous, current){
			if (!previous.apply || !current.apply) return false;
			return function(){
				this.parent = previous;
				return current.apply(this, arguments);
			};
		};

		for (var property in properties){
			var previous = pr0t0typ3[property];
			var current = properties[property];
			if (previous && previous != current) current = parentize(previous, current) || current;
			pr0t0typ3[property] = current;
		}
		return new Class(pr0t0typ3);
	},

	/*
	Property: implement
		Implements the passed in properties to the base Class prototypes, altering the base class, unlike <Class.extend>.

	Arguments:
		properties - the properties to add to the base class.

	Example:
		(start code)
		var Animal = new Class({
			initialize: function(age){
				this.age = age;
			}
		});
		Animal.implement({
			setName: function(name){
				this.name = name
			}
		});
		var myAnimal = new Animal(20);
		myAnimal.setName('Micia');
		alert(myAnimal.name); //alerts 'Micia'
		(end)
	*/

	implement: function(properties){
		for (var property in properties) this.prototype[property] = properties[property];
	}

};

/* Section: Object related Functions */

/*
Function: Object.extend
	Copies all the properties from the second passed object to the first passed Object.
	If you do myWhatever.extend = Object.extend the first parameter will become myWhatever, and your extend function will only need one parameter.

Example:
	(start code)
	var firstOb = {
		'name': 'John',
		'lastName': 'Doe'
	};
	var secondOb = {
		'age': '20',
		'sex': 'male',
		'lastName': 'Dorian'
	};
	Object.extend(firstOb, secondOb);
	//firstOb will become: 
	{
		'name': 'John',
		'lastName': 'Dorian',
		'age': '20',
		'sex': 'male'
	};
	(end)

Returns:
	The first object, extended.
*/

Object.extend = function(){
	var args = arguments;
	args = (args[1]) ? [args[0], args[1]] : [this, args[0]];
	for (var property in args[1]) args[0][property] = args[1][property];
	return args[0];
};

/*
Function: Object.Native
	Will add a .extend method to the objects passed as a parameter, equivalent to <Class.implement>

Arguments:
	a number of classes/native javascript objects

*/

Object.Native = function(){
	for (var i = 0; i < arguments.length; i++) arguments[i].extend = Class.prototype.implement;
};

new Object.Native(Function, Array, String, Number, Class);

/*
Script: Utility.js
	Contains Utility functions

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

//htmlelement mapping

if (typeof HTMLElement == 'undefined'){
	var HTMLElement = Class.empty;
	HTMLElement.prototype = {};
}

/*
Function: $type
	Returns the type of object that matches the element passed in.

Arguments:
	obj - the object to inspect.

Example:
	>var myString = 'hello';
	>$type(myString); //returns "string"

Returns:
	'element' - if obj is a DOM element node
	'textnode' - if obj is a DOM text node
	'whitespace' - if obj is a DOM whitespace node
	'array' - if obj is an array
	'object' - if obj is an object
	'string' - if obj is a string
	'number' - if obj is a number
	'boolean' - if obj is a boolean
	'function' - if obj is a function
	false - (boolean) if the object is not defined or none of the above.
*/

function $type(obj){
	if (obj === null || obj === undefined) return false;
	var type = typeof obj;
	if (type == 'object'){
		if (obj instanceof HTMLElement) return 'element';
		if (obj instanceof Array) return 'array';
		if (obj.nodeName){
			switch (obj.nodeType){
				case 1: return 'element';
				case 3: return obj.nodeValue.test('\\S') ? 'textnode' : 'whitespace';
			}
		}
	}
	return type;
};

/*
Function: $chk
	Returns true if the passed in value/object exists or is 0, otherwise returns false.
	Useful to accept zeroes.
*/

function $chk(obj){
	return !!(obj || obj === 0);
};

/*
Function: $pick
	Returns the first object if defined, otherwise returns the second.
*/

function $pick(obj, picked){
	return ($type(obj)) ? obj : picked;
};

/*
Function: $random
	Returns a random integer number between the two passed in values.

Arguments:
	min - integer, the minimum value (inclusive).
	max - integer, the maximum value (inclusive).

Returns:
	a random integer between min and max.
*/

function $random(min, max){
	return Math.floor(Math.random() * (max - min + 1) + min);
};

/*
Function: $clear
	clears a timeout or an Interval.

Returns:
	null

Arguments:
	timer - the setInterval or setTimeout to clear.

Example:
	>var myTimer = myFunction.delay(5000); //wait 5 seconds and execute my function.
	>myTimer = $clear(myTimer); //nevermind

See also:
	<Function.delay>, <Function.periodical>
*/

function $clear(timer){
	clearTimeout(timer);
	clearInterval(timer);
	return null;
};

/* Section: Browser Detection */

/*
Properties:
	window.ie - will be set to true if the current browser is internet explorer (any).
	window.ie6 - will be set to true if the current browser is internet explorer 6.
	window.ie7 - will be set to true if the current browser is internet explorer 7.
	window.khtml - will be set to true if the current browser is Safari/Konqueror.
	window.gecko - will be set to true if the current browser is Mozilla/Gecko.
*/

if (window.ActiveXObject) window.ie = window[window.XMLHttpRequest ? 'ie7' : 'ie6'] = true;
else if (document.childNodes && !document.all && !navigator.taintEnabled) window.khtml = true;
else if (document.getBoxObjectFor != null) window.gecko = true;

/*
Script: Array.js
	Contains Array prototypes and the function <$A>;

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Array
	A collection of The Array Object prototype methods.
*/

//emulated methods

/*
Property: forEach
	Iterates through an array; This method is only available for browsers without native *forEach* support.
	For more info see <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Array:forEach>
*/

Array.prototype.forEach = Array.prototype.forEach || function(fn, bind){
	for (var i = 0; i < this.length; i++) fn.call(bind, this[i], i, this);
};

/*
Property: map
	This method is provided only for browsers without native *map* support.
	For more info see <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Array:map>
*/

Array.prototype.map = Array.prototype.map || function(fn, bind){
	var results = [];
	for (var i = 0; i < this.length; i++) results[i] = fn.call(bind, this[i], i, this);
	return results;
};

/*
Property: every
	This method is provided only for browsers without native *every* support.
	For more info see <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Array:every>
*/

Array.prototype.every = Array.prototype.every || function(fn, bind){
	for (var i = 0; i < this.length; i++){
		if (!fn.call(bind, this[i], i, this)) return false;
	}
	return true;
};

/*
Property: some
	This method is provided only for browsers without native *some* support.
	For more info see <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Array:some>
*/

Array.prototype.some = Array.prototype.some || function(fn, bind){
	for (var i = 0; i < this.length; i++){
		if (fn.call(bind, this[i], i, this)) return true;
	}
	return false;
};

/*
Property: indexOf
	This method is provided only for browsers without native *indexOf* support.
	For more info see <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Global_Objects:Array:indexOf>
*/

Array.prototype.indexOf = Array.prototype.indexOf || function(item, from){
	from = from || 0;
	if (from < 0) from = Math.max(0, this.length + from);
	while (from < this.length){
		if(this[from] === item) return from;
		from++;
	}
	return -1;
};

//custom methods

Array.extend({

	/*
	Property: each
		Same as <Array.forEach>.

	Arguments:
		fn - the function to execute with each item in the array
		bind - optional, the object that the "this" of the function will refer to.

	Example:
		>var Animals = ['Cat', 'Dog', 'Coala'];
		>Animals.forEach(function(animal){
		>	document.write(animal)
		>});
	*/

	each: Array.prototype.forEach,

	/*
	Property: copy
		Copy the array and returns it.

	Returns:
		an Array

	Example:
		>var letters = ["a","b","c"];
		>var copy = ["a","b","c"].copy();
	*/

	copy: function(){
		var newArray = [];
		for (var i = 0; i < this.length; i++) newArray[i] = this[i];
		return newArray;
	},

	/*
	Property: remove
		Removes all occurrences of an item from the array.

	Arguments:
		item - the item to remove

	Returns:
		the Array with all occurrences of the item removed.

	Example:
		>["1","2","3","2"].remove("2") // ["1","3"];
	*/

	remove: function(item){
		var i = 0;
		while (i < this.length){
			if (this[i] == item) this.splice(i, 1);
			else i++;
		}
		return this;
	},

	/*
	Property: test
		Tests an array for the presence of an item.

	Arguments:
		item - the item to search for in the array.
		from - optional, the index at which to begin the search, default is 0. If negative, it is taken as the offset from the end of the array.

	Returns:
		true - the item was found
		false - it wasn't

	Example:
		>["a","b","c"].test("a"); // true
		>["a","b","c"].test("d"); // false
	*/

	test: function(item, from){
		return this.indexOf(item, from) != -1;
	},

	/*
	Property: extend
		Extends an array with another

	Arguments:
		newArray - the array to extend ours with

	Example:
		>var Animals = ['Cat', 'Dog', 'Coala'];
		>Animals.extend(['Lizard']);
		>//Animals is now: ['Cat', 'Dog', 'Coala', 'Lizard'];
	*/

	extend: function(newArray){
		for (var i = 0; i < newArray.length; i++) this.push(newArray[i]);
		return this;
	},

	/*
	Property: associate
		Creates an object with key-value pairs based on the array of keywords passed in
		and the current content of the array.

	Arguments:
		keys - the array of keywords.

	Example:
		(start code)
		var Animals = ['Cat', 'Dog', 'Coala', 'Lizard'];
		var Speech = ['Miao', 'Bau', 'Fruuu', 'Mute'];
		var Speeches = Animals.associate(speech);
		//Speeches['Miao'] is now Cat.
		//Speeches['Bau'] is now Dog.
		//...
		(end)
	*/

	associate: function(keys){
		var obj = {}, length = Math.min(this.length, keys.length);
		for (var i = 0; i < length; i++) obj[keys[i]] = this[i];
		return obj;
	}

});

/* Section: Utility Functions */

/*
Function: $A()
	Same as <Array.copy>, but as function.
	Useful to apply Array prototypes to iterable objects, as a collection of DOM elements or the arguments object.

Example:
	(start code)
	function myFunction(){
		$A(arguments).each(argument, function(){
			alert(argument);
		});
	};
	//the above will alert all the arguments passed to the function myFunction.
	(end)
*/

function $A(array){
	return Array.prototype.copy.call(array);
};

/*
Function: $each
	use to iterate through iterables that are not regular arrays, such as builtin getElementsByTagName calls, or arguments of a function.

Arguments:
	iterable - an iterable element.
	function - function to apply to the iterable.
	bind - optional, the 'this' of the function will refer to this object.
*/

function $each(iterable, fn, bind){
	return Array.prototype.forEach.call(iterable, fn, bind);
};

/*
Script: String.js
	Contains String prototypes and Number prototypes.

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: String
	A collection of The String Object prototype methods.
*/

String.extend({

	/*
	Property: test
		Tests a string with a regular expression.

	Arguments:
		regex - the regular expression you want to match the string with
		params - optional, any parameters you want to pass to the regex ('g' has no effect)

	Returns:
		true if a match for the regular expression is found in the string, false if not.
		See <http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference:Objects:RegExp:test>

	Example:
		>"I like cookies".test("cookie"); // returns true
		>"I like cookies".test("COOKIE", "i") // ignore case, returns true
		>"I like cookies".test("cake"); // returns false
	*/

	test: function(regex, params){
		return new RegExp(regex, params).test(this);
	},

	/*
	Property: toInt
		parses a string to an integer.

	Returns:
		either an int or "NaN" if the string is not a number.

	Example:
		>var value = "10px".toInt(); // value is 10
	*/

	toInt: function(){
		return parseInt(this);
	},

	toFloat: function(){
		return parseFloat(this);
	},

	/*
	Property: camelCase
		Converts a hiphenated string to a camelcase string.

	Example:
		>"I-like-cookies".camelCase(); //"ILikeCookies"

	Returns:
		the camel cased string
	*/

	camelCase: function(){
		return this.replace(/-\D/g, function(match){
			return match.charAt(1).toUpperCase();
		});
	},

	/*
	Property: hyphenate
		Converts a camelCased string to a hyphen-ated string.

	Example:
		>"ILikeCookies".hyphenate(); //"I-like-cookies"
	*/

	hyphenate: function(){
		return this.replace(/\w[A-Z]/g, function(match){
			return (match.charAt(0)+'-'+match.charAt(1).toLowerCase());
		});
	},

	/*
	Property: capitalize
		Converts the first letter in each word of a string to Uppercase.

	Example:
		>"i like cookies".capitalize(); //"I Like Cookies"

	Returns:
		the capitalized string
	*/

	capitalize: function(){
		return this.toLowerCase().replace(/\b[a-z]/g, function(match){
			return match.toUpperCase();
		});
	},

	/*
	Property: trim
		Trims the leading and trailing spaces off a string.

	Example:
		>"    i like cookies     ".trim() //"i like cookies"

	Returns:
		the trimmed string
	*/

	trim: function(){
		return this.replace(/^\s+|\s+$/g, '');
	},

	/*
	Property: clean
		trims (<String.trim>) a string AND removes all the double spaces in a string.

	Returns:
		the cleaned string

	Example:
		>" i      like     cookies      \n\n".clean() //"i like cookies"
	*/

	clean: function(){
		return this.replace(/\s{2,}/g, ' ').trim();
	},

	/*
	Property: rgbToHex
		Converts an RGB value to hexidecimal. The string must be in the format of "rgb(255, 255, 255)" or "rgba(255, 255, 255, 1)";

	Arguments:
		array - boolean value, defaults to false. Use true if you want the array ['FF', '33', '00'] as output instead of #FF3300

	Returns:
		hex string or array. returns transparent if the fourth value of rgba in input string is 0,

	Example:
		>"rgb(17,34,51)".rgbToHex(); //"#112233"
		>"rgba(17,34,51,0)".rgbToHex(); //"transparent"
		>"rgb(17,34,51)".rgbToHex(true); //[11,22,33]
	*/

	rgbToHex: function(array){
		var rgb = this.match(/\d{1,3}/g);
		return (rgb) ? rgb.rgbToHex(array) : false;
	},

	/*
	Property: hexToRgb
		Converts a hexidecimal color value to RGB. Input string must be the hex color value (with or without the hash). Also accepts triplets ('333');

	Arguments:
		array - boolean value, defaults to false. Use true if you want the array ['255', '255', '255'] as output instead of "rgb(255,255,255)";

	Returns:
		rgb string or array.

	Example:
		>"#112233".hexToRgb(); //"rgb(17,34,51)"
		>"#112233".hexToRgb(true); //[17,34,51]
	*/

	hexToRgb: function(array){
		var hex = this.match('^#?(\\w{1,2})(\\w{1,2})(\\w{1,2})$');
		return (hex) ? hex.hexToRgb(array) : false;
	}

});

Array.extend({
	
	rgbToHex: function(array){
		if (this.length < 3) return false;
		if (this[3] && this[3] == 0) return 'transparent';
		var hex = [];
		for (var i = 0; i < 3; i++){
			var bit = (this[i]-0).toString(16);
			hex.push(bit.length == 1 ? '0'+bit : bit);
		}
		return array ? hex : '#'+hex.join('');
	},
	
	hexToRgb: function(array){
		if (this.length != 4) return false;
		var rgb = [];
		for (var i = 1; i < 4; i++){
			if (this[i].length == 1) this[i] += this[i];
			rgb.push(parseInt(this[i], 16));
		}
		return array ? rgb : 'rgb('+rgb.join(',')+')';
	}

});

/*
Class: Number
	contains the internal method toInt.
*/

Number.extend({

	/*
	Property: toInt
		Returns this number; useful because toInt must work on both Strings and Numbers.
	*/

	toInt: function(){
		return parseInt(this);
	},

	toFloat: function(){
		return parseFloat(this);
	}

});

/* 
Script: Function.js
	Contains Function prototypes and utility functions .

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.

Credits:
	- Some functions are inspired by those found in prototype.js <http://prototype.conio.net/> (c) 2005 Sam Stephenson sam [at] conio [dot] net, MIT-style license
*/

/*
Class: Function
	A collection of The Function Object prototype methods.
*/

Function.extend({

	create: function(options){
		var fn = this;
		options = Object.extend({
			'bind': fn,
			'event': false,
			'arguments': null,
			'delay': false,
			'periodical': false,
			'attempt': false
		}, options || {});
		if (options.arguments != null && typeof options.arguments != 'undefined' && !(options.arguments instanceof Array))
			options.arguments = [options.arguments];
		return function(event){
			var args = options.arguments || arguments;
			if (options.event){
				event = (options.event === true) ? event || window.event : new options.event(event);
				args = [event].concat(args);
			}
			var returns = function(){
				return fn.apply(options.bind, args);
			};
			if (options.delay) return setTimeout(returns, options.delay);
			if (options.periodical) return setInterval(returns, options.periodical);
			if (options.attempt){
				try {
					var result = returns();
				} catch(err){
					result = err;
				} finally {
					return result;
				}
			} else return returns();
		};
	},

	/*
	Property: pass
		Shortcut to create closures with arguments and bind.

	Returns:
		a function.

	Arguments:
		args - the arguments passed. must be an array if arguments > 1
		bind - optional, the object that the "this" of the function will refer to.

	Example:
		>myFunction.pass([arg1, arg2], myElement);
	*/

	pass: function(args, bind){
		return this.create({'arguments': args, 'bind': bind});
	},
	
	/*
	Property: attempt
		Tries to execute the function, returns either the function results or the error.

	Arguments:
		args - the arguments passed. must be an array if arguments > 1
		bind - optional, the object that the "this" of the function will refer to.

	Example:
		>myFunction.attempt([arg1, arg2], myElement);
	*/

	attempt: function(args, bind){
		return this.create({'arguments': args, 'bind': bind, 'attempt': true})();
	},

	/*
	Property: bind
		method to easily create closures with "this" altered.

	Arguments:
		bind - optional, the object that the "this" of the function will refer to.
		args - optional, the arguments passed. must be an array if arguments > 1

	Returns:
		a function.

	Example:
		>function myFunction(){
		>	this.setStyle('color', 'red');
		>	// note that 'this' here refers to myFunction, not an element
		>	// we'll need to bind this function to the element we want to alter
		>};
		>var myBoundFunction = myFunction.bind(myElement);
		>myBoundFunction(); // this will make the element myElement red.
	*/

	bind: function(bind, args){
		return this.create({'bind': bind, 'arguments': args});
	},

	/*
	Property: bindAsEventListener
		cross browser method to pass event firer

	Arguments:
		bind - optional, the object that the "this" of the function will refer to.
		args - optional, the arguments passed. must be an array if arguments > 1

	Returns:
		a function with the parameter bind as its "this" and as a pre-passed argument event or window.event, depending on the browser.

	Example:
		>function myFunction(event){
		>	alert(event.clientx) //returns the coordinates of the mouse..
		>};
		>myElement.onclick = myFunction.bindAsEventListener(myElement);
	*/

	bindAsEventListener: function(bind, args){
		return this.create({'bind': bind, 'event': true, 'arguments': args});
	},

	/*
	Property: delay
		Delays the execution of a function by a specified duration.

	Arguments:
		ms - the duration to wait in milliseconds
		bind - optional, the object that the "this" of the function will refer to.
		args - optional, the arguments passed. must be an array if arguments > 1

	Example:
		>myFunction.delay(50, myElement) //wait 50 milliseconds, then call myFunction and bind myElement to it
		>(function(){alert('one second later...')}).delay(1000); //wait a second and alert
	*/

	delay: function(ms, bind, args){
		return this.create({'delay': ms, 'bind': bind, 'arguments': args})();
	},

	/*
	Property: periodical
		Executes a function in the specified intervals of time

	Arguments:
		ms - the duration of the intervals between executions.
		bind - optional, the object that the "this" of the function will refer to.
		args - optional, the arguments passed. must be an array if arguments > 1
	*/

	periodical: function(ms, bind, args){
		return this.create({'periodical': ms, 'bind': bind, 'arguments': args})();
	}

});

/*
Script: Element.js
	Contains useful Element prototypes, to be used with the dollar function <$>.

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.

Credits:
	- Some functions are inspired by those found in prototype.js <http://prototype.conio.net/> (c) 2005 Sam Stephenson sam [at] conio [dot] net, MIT-style license
*/

/*
Class: Element
	Custom class to allow all of its methods to be used with any DOM element via the dollar function <$>.
*/

var Element = new Class({

	/*
	Property: initialize
		Creates a new element of the type passed in.

	Arguments:
		el - the tag name for the element you wish to create.

	Example:
		>var div = new Element('div');
	*/

	initialize: function(el){
		if ($type(el) == 'string') el = document.createElement(el);
		return $(el);
	}

});

/*
Function: $()
	returns the element passed in with all the Element prototypes applied.

Arguments:
	el - a reference to an actual element or a string representing the id of an element

Example:
	>$('myElement') // gets a DOM element by id with all the Element prototypes applied.
	>var div = document.getElementById('myElement');
	>$(div) //returns an Element also with all the mootools extentions applied.

	You'll use this when you aren't sure if a variable is an actual element or an id, as
	well as just shorthand for document.getElementById().

Returns:
	a DOM element or false (if no id was found).

Note:
	you need to call $ on an element only once to get all the prototypes.
	But its no harm to call it multiple times, as it will detect if it has been already extended.
*/

function $(el){
	if (!el) return false;
	if (el._element_extended_ || [window, document].test(el)) return el;
	if ($type(el) == 'string') el = document.getElementById(el);
	if ($type(el) != 'element') return false;
	if (['object', 'embed'].test(el.tagName.toLowerCase()) || el.extend) return el;
	el._element_extended_ = true;
	Garbage.collect(el);
	el.extend = Object.extend;
	if (!(el instanceof HTMLElement)) el.extend(Element.prototype);
	return el;
};

//elements class

var Elements = new Class({});

new Object.Native(Elements);

document.getElementsBySelector = document.getElementsByTagName;

/*
Function: $$()
	Selects, and extends DOM elements.

Arguments:
	HTMLCollection(document.getElementsByTagName, element.childNodes), an array of elements, a string.

Note:
	if you loaded <Dom.js>, $$ will also accept CSS Selectors.

Example:
	>$$('a') //an array of all anchor tags on the page
	>$$('a', 'b') //an array of all anchor and bold tags on the page
	>$$('#myElement') //array containing only the element with id = myElement. (only with <Dom.js>)
	>$$('#myElement a.myClass') //an array of all anchor tags with the class "myClass" within the DOM element with id "myElement" (only with <Dom.js>)

Returns:
	array - array of all the dom elements matched
*/

function $$(){
	if (!arguments) return false;
	if (arguments.length == 1){
		if (!arguments[0]) return false;
		if (arguments[0]._elements_extended_) return arguments[0];
	}
	var elements = [];
	$each(arguments, function(selector){
		switch ($type(selector)){
			case 'element': elements.push($(selector)); break;
			case 'string': selector = document.getElementsBySelector(selector);
			default:
			if (selector.length){
				$each(selector, function(el){
					if ($(el)) elements.push(el);
				});
			}
		}
	});
	elements._elements_extended_ = true;
	return Object.extend(elements, new Elements);
};

Elements.Multi = function(property){
	return function(){
		var args = arguments;
		var items = [];
		var elements = true;
		$each(this, function(el){
			var returns = el[property].apply(el, args);
			if ($type(returns) != 'element') elements = false;
			items.push(returns);
		});
		if (elements) items = $$(items);
		return items;
	};
};

Element.extend = function(properties){
	for (var property in properties){
		HTMLElement.prototype[property] = properties[property];
		Element.prototype[property] = properties[property];
		Elements.prototype[property] = Elements.Multi(property);
	}
};

Element.extend({

	inject: function(el, where){
		el = $(el) || new Element(el);
		switch (where){
			case "before": $(el.parentNode).insertBefore(this, el); break;
			case "after":
				if (!el.getNext()) $(el.parentNode).appendChild(this);
				else $(el.parentNode).insertBefore(this, el.getNext());
				break;
			case "inside": el.appendChild(this);
		}
		return this;
	},

	/*
	Property: injectBefore
		Inserts the Element before the passed element.

	Parameteres:
		el - a string representing the element to be injected in (myElementId, or div), or an element reference.
		If you pass div or another tag, the element will be created.

	Example:
		>html:
		><div id="myElement"></div>
		><div id="mySecondElement"></div>
		>js:
		>$('mySecondElement').injectBefore('myElement');
		>resulting html:
		><div id="mySecondElement"></div>
		><div id="myElement"></div>

	*/

	injectBefore: function(el){
		return this.inject(el, 'before');
	},

	/*
	Property: injectAfter
		Same as <Element.injectBefore>, but inserts the element after.
	*/

	injectAfter: function(el){
		return this.inject(el, 'after');
	},

	/*
	Property: injectInside
		Same as <Element.injectBefore>, but inserts the element inside.
	*/

	injectInside: function(el){
		return this.inject(el, 'inside');
	},

	/*
	Property: adopt
		Inserts the passed element inside the Element. Works as <Element.injectInside> but in reverse.

	Parameteres:
		el - a string representing the element to be injected in (myElementId, or div), or an element reference.
		If you pass div or another tag, the element will be created.
	*/

	adopt: function(el){
		this.appendChild($(el) || new Element(el));
		return this;
	},

	/*
	Property: remove
		Removes the Element from the DOM.

	Example:
		>$('myElement').remove() //bye bye
	*/

	remove: function(){
		this.parentNode.removeChild(this);
		return this;
	},

	/*
	Property: clone
		Clones the Element and returns the cloned one.

	Returns: 
		the cloned element

	Example:
		>var clone = $('myElement').clone().injectAfter('myElement');
		>//clones the Element and append the clone after the Element.
	*/

	clone: function(contents){
		var el = this.cloneNode(contents !== false);
		return $(el);
	},

	/*
	Property: replaceWith
		Replaces the Element with an element passed.

	Parameteres:
		el - a string representing the element to be injected in (myElementId, or div), or an element reference.
		If you pass div or another tag, the element will be created.

	Returns:
		the passed in element

	Example:
		>$('myOldElement').replaceWith($('myNewElement')); //$('myOldElement') is gone, and $('myNewElement') is in its place.
	*/

	replaceWith: function(el){
		el = $(el) || new Element(el);
		this.parentNode.replaceChild(el, this);
		return el;
	},

	/*
	Property: appendText
		Appends text node to a DOM element.

	Arguments:
		text - the text to append.

	Example:
		><div id="myElement">hey</div>
		>$('myElement').appendText(' howdy'); //myElement innerHTML is now "hey howdy"
	*/

	appendText: function(text){
		if (window.ie){
			switch(this.getTag()){
				case 'style': this.styleSheet.cssText = text; return this;
				case 'script': this.setProperty('text', text); return this;
			}
		}
		this.appendChild(document.createTextNode(text));
		return this;
	},

	/*
	Property: hasClass
		Tests the Element to see if it has the passed in className.

	Returns:
	 	true - the Element has the class
	 	false - it doesn't
	 
	Arguments:
		className - the class name to test.
	 
	Example:
		><div id="myElement" class="testClass"></div>
		>$('myElement').hasClass('testClass'); //returns true
	*/

	hasClass: function(className){
		return this.className.test('(?:^|\\s+)' + className + '(?:\\s+|$)');
	},

	/*
	Property: addClass
		Adds the passed in class to the Element, if the element doesnt already have it.

	Arguments:
		className - the class name to add

	Example: 
		><div id="myElement" class="testClass"></div>
		>$('myElement').addClass('newClass'); //<div id="myElement" class="testClass newClass"></div>
	*/

	addClass: function(className){
		if (!this.hasClass(className)) this.className = (this.className+' '+className).clean();
		return this;
	},

	/*
	Property: removeClass
		works like <Element.addClass>, but removes the class from the element.
	*/

	removeClass: function(className){
		if (this.hasClass(className)) this.className = this.className.replace(className, '').clean();
		return this;
	},

	/*
	Property: toggleClass
		Adds or removes the passed in class name to the element, depending on if it's present or not.

	Arguments:
		className - the class to add or remove

	Example:
		><div id="myElement" class="myClass"></div>
		>$('myElement').toggleClass('myClass');
		><div id="myElement" class=""></div>
		>$('myElement').toggleClass('myClass');
		><div id="myElement" class="myClass"></div>
	*/

	toggleClass: function(className){
		return this.hasClass(className) ? this.removeClass(className) : this.addClass(className);
	},

	/*
	Property: setStyle
		Sets a css property to the Element.

		Arguments:
			property - the property to set
			value - the value to which to set it

		Example:
			>$('myElement').setStyle('width', '300px'); //the width is now 300px
	*/

	setStyle: function(property, value){
		if (property == 'opacity') this.setOpacity(parseFloat(value));
		else this.style[property.camelCase()] = (value.push) ? value.rgbToHex() : value;
		return this;
	},

	/*
	Property: setStyles
		Applies a collection of styles to the Element.

	Arguments:
		source - an object or string containing all the styles to apply

	Examples:
		>$('myElement').setStyles({
		>	border: '1px solid #000',
		>	width: '300px',
		>	height: '400px'
		>});

		OR

		>$('myElement').setStyle('border: 1px solid #000; width: 300px; height: 400px;');
	*/

	setStyles: function(source){
		switch ($type(source)){
			case 'object':
				for (var property in source) this.setStyle(property, source[property]);
				break;
			case 'string':
				if (window.ie) this.cssText = source;
				else this.setAttribute('style', source);
		}
		return this;
	},

	/*
	Property: setOpacity
		Sets the opacity of the Element, and sets also visibility == "hidden" if opacity == 0, and visibility = "visible" if opacity == 1.

	Arguments:
		opacity - Accepts numbers from 0 to 1.

	Example:
		>$('myElement').setOpacity(0.5) //make it 50% transparent
	*/

	setOpacity: function(opacity){
		if (opacity == 0){
			if(this.style.visibility != "hidden") this.style.visibility = "hidden";
		} else {
			if(this.style.visibility != "visible") this.style.visibility = "visible";
		}
		if (!this.currentStyle || !this.currentStyle.hasLayout) this.style.zoom = 1;
		if (window.ie) this.style.filter = "alpha(opacity=" + opacity*100 + ")";
		this.style.opacity = this.opacity = opacity;
		return this;
	},

	/*
	Property: getStyle
		Returns the style of the Element given the property passed in.

	Arguments:
		property - the css style property you want to retrieve

	Example:
		>$('myElement').getStyle('width'); //returns "400px"
		>//but you can also use
		>$('myElement').getStyle('width').toInt(); //returns "400"

	Returns:
		the style as a string
	*/

	getStyle: function(property){
		property = property.camelCase();
		var style = this.style[property] || false;
		if (!$chk(style)){
			if (property == 'opacity') return $chk(this.opacity) ? this.opacity : 1;
			if (['margin', 'padding'].test(property)){
				return [this.getStyle(property+'-top') || 0, this.getStyle(property+'-right') || 0,
						this.getStyle(property+'-bottom') || 0, this.getStyle(property+'-left') || 0].join(' ');
			}
			if (document.defaultView) style = document.defaultView.getComputedStyle(this, null).getPropertyValue(property.hyphenate());
			else if (this.currentStyle) style = this.currentStyle[property];
		}
		return (style && property.test('color', 'i') && style.test('rgb')) ? style.rgbToHex() : style;
	},

	/*
	Property: addEvent
		Attaches an event listener to a DOM element.

	Arguments:
		type - the event to monitor ('click', 'load', etc) without the prefix 'on'.
		fn - the function to execute

	Example:
		>$('myElement').addEvent('click', function(){alert('clicked!')});
	*/

	addEvent: function(type, fn){
		this.events = this.events || {};
		this.events[type] = this.events[type] || {'keys': [], 'values': []};
		if (!this.events[type].keys.test(fn)){
			this.events[type].keys.push(fn);
			if (this.addEventListener){
				this.addEventListener((type == 'mousewheel' && window.gecko) ? 'DOMMouseScroll' : type, fn, false);
			} else {
				fn = fn.bind(this);
				this.attachEvent('on'+type, fn);
				this.events[type].values.push(fn);
			}
		}
		return this;
	},

	addEvents: function(source){
		if (source){
			for (var type in source) this.addEvent(type, source[type]);
		}
		return this;
	},

	/*
	Property: removeEvent
		Works as Element.addEvent, but instead removes the previously added event listener.
	*/

	removeEvent: function(type, fn){
		if (this.events && this.events[type]){
			var pos = this.events[type].keys.indexOf(fn);
			if (pos == -1) return this;
			var key = this.events[type].keys.splice(pos,1)[0];
			if (this.removeEventListener){
				this.removeEventListener((type == 'mousewheel' && window.gecko) ? 'DOMMouseScroll' : type, key, false);
			} else {
				this.detachEvent('on'+type, this.events[type].values.splice(pos,1)[0]);
			}
		}
		return this;
	},

	/*
	Property: removeEvents
		removes all events of a certain type from an element. if no argument is passed in, removes all events.
	*/

	removeEvents: function(type){
		if (this.events){
			if (type){
				if (this.events[type]){
					this.events[type].keys.each(function(fn){
						this.removeEvent(type, fn);
					}, this);
					this.events[type] = null;
				}
			} else {
				for (var evType in this.events) this.removeEvents(evType);
				this.events = null;
			}
		}
		return this;
	},

	/*
	Property: fireEvent
		executes all events of the specified type present in the element.
	*/

	fireEvent: function(type, args){
		if (this.events && this.events[type]){
			args = args || [];
			if ($type(args) != 'array') args = [args];
			this.events[type].keys.each(function(fn){
				fn.apply(this, args);
			}, this);
		}
	},

	getBrother: function(what){
		var el = this[what+'Sibling'];
		while ($type(el) == 'whitespace') el = el[what+'Sibling'];
		return $(el);
	},

	/*
	Property: getPrevious
		Returns the previousSibling of the Element, excluding text nodes.

	Example:
		>$('myElement').getPrevious(); //get the previous DOM element from myElement

	Returns:
		the sibling element or undefined if none found.
	*/

	getPrevious: function(){
		return this.getBrother('previous');
	},

	/*
	Property: getNext
		Works as Element.getPrevious, but tries to find the nextSibling.
	*/

	getNext: function(){
		return this.getBrother('next');
	},

	/*
	Property: getFirst
		Works as <Element.getPrevious>, but tries to find the firstChild.
	*/

	getFirst: function(){
		var el = this.firstChild;
		while ($type(el) == 'whitespace') el = el.nextSibling;
		return $(el);
	},

	/*
	Property: getLast
		Works as <Element.getPrevious>, but tries to find the lastChild.
	*/

	getLast: function(){
		var el = this.lastChild;
		while ($type(el) == 'whitespace') el = el.previousSibling;
		return $(el);
	},
	
	/*
	Property: getParent
		returns the $(element.parentNode)
	*/

	getParent: function(){
		return $(this.parentNode);
	},
	
	/*
	Property: getChildren
		returns all the $(element.childNodes), excluding text nodes. Returns as <Elements>.
	*/

	getChildren: function(){
		return $$(this.childNodes);
	},

	/*
	Property: setProperty
		Sets an attribute for the Element.

	Arguments:
		property - the property to assign the value passed in
		value - the value to assign to the property passed in

	Example:
		>$('myImage').setProperty('src', 'whatever.gif'); //myImage now points to whatever.gif for its source
	*/

	setProperty: function(property, value){
		switch (property){
			case 'class': this.className = value; break;
			case 'style': this.setStyles(value); break;
			case 'name': if (window.ie6){
				var el = $(document.createElement('<'+this.getTag()+' name="'+value+'" />'));
				$each(this.attributes, function(attribute){
					if (attribute.name != 'name') el.setProperty(attribute.name, attribute.value);
				});
				if (this.parentNode) this.replaceWith(el);
				return el;
			}
			default: this.setAttribute(property, value);
		}
		return this;
	},

	/*
	Property: setProperties
		Sets numerous attributes for the Element.

	Arguments:
		source - an object with key/value pairs.

	Example:
		>$('myElement').setProperties({
		>	src: 'whatever.gif',
		>	alt: 'whatever dude'
		>});
		><img src="whatever.gif" alt="whatever dude">
	*/

	setProperties: function(source){
		for (var property in source) this.setProperty(property, source[property]);
		return this;
	},

	/*
	Property: setHTML
		Sets the innerHTML of the Element.

	Arguments:
		html - the new innerHTML for the element.

	Example:
		>$('myElement').setHTML(newHTML) //the innerHTML of myElement is now = newHTML
	*/

	setHTML: function(html){
		this.innerHTML = html;
		return this;
	},

	/*
	Property: getProperty
		Gets the an attribute of the Element.

	Arguments:
		property - the attribute to retrieve

	Example:
		>$('myImage').getProperty('src') // returns whatever.gif

	Returns:
		the value, or an empty string
	*/

	getProperty: function(property){
		return (property == 'class') ? this.className : this.getAttribute(property);
	},

	/*
	Property: getTag
		Returns the tagName of the element in lower case.

	Example:
		>$('myImage').getTag() // returns 'img'

	Returns:
		The tag name in lower case
	*/

	getTag: function(){
		return this.tagName.toLowerCase();
	},

	getOffsets: function(){
		var el = this, offsetLeft = 0, offsetTop = 0;
		do {
			offsetLeft += el.offsetLeft || 0;
			offsetTop += el.offsetTop || 0;
			el = el.offsetParent;
		} while (el);
		return {'x': offsetLeft, 'y': offsetTop};
	},

	/*
	Property: scrollTo
		scrolls the element to the specified coordinated (if the element has an overflow)

	Arguments:
		x - the x coordinate
		y - the y coordinate

	Example:
		>$('myElement').scrollTo(0, 100)
	*/

	scrollTo: function(x, y){
		this.scrollLeft = x;
		this.scrollTop = y;
	},

	/*
	Property: getSize
		return an Object representing the size/scroll values of the element.

	Example:
		(start code)
		$('myElement').getSize();
		(end)

	Returns:
		(start code)
		{
			'scroll': {'x': 100, 'y': 100},
			'size': {'x': 200, 'y': 400},
			'scrollSize': {'x': 300, 'y': 500}
		}
		(end)
	*/

	getSize: function(){
		return {
			'scroll': {'x': this.scrollLeft, 'y': this.scrollTop},
			'size': {'x': this.offsetWidth, 'y': this.offsetHeight},
			'scrollSize': {'x': this.scrollWidth, 'y': this.scrollHeight}
		};
	},

	/*
	Property: getTop
		Returns the distance from the top of the window to the Element.
	*/

	getTop: function(){
		return this.getOffsets().y;
	},

	/*
	Property: getLeft
		Returns the distance from the left of the window to the Element.
	*/

	getLeft: function(){
		return this.getOffsets().x;
	},

	/*
	Property: getPosition
		Returns an object with width, height, left, right, top, and bottom, representing the values of the Element

	Example:
		(start code)
		var myValues = $('myElement').getPosition();
		(end)

	Returns:
		(start code)
		{
			width: 200,
			height: 300,
			left: 100,
			top: 50,
			right: 300,
			bottom: 350
		}
		(end)
	*/

	getPosition: function(){
		var offs = this.getOffsets();
		var obj = {
			'width': this.offsetWidth,
			'height': this.offsetHeight,
			'left': offs.x,
			'top': offs.y
		};
		obj.right = obj.left + obj.width;
		obj.bottom = obj.top + obj.height;
		return obj;
	},

	/*
	Property: getValue
		Returns the value of the Element, if its tag is textarea, select or input. no multiple select support.
	*/

	getValue: function(){
		switch (this.getTag()){
			case 'select': if (this.selectedIndex != -1) return this.options[this.selectedIndex].value; break;
			case 'input': if (!(this.checked && ['checkbox', 'radio'].test(this.type)) && !['hidden', 'text', 'password'].test(this.type)) break;
			case 'textarea': return this.value;
		}
		return false;
	}

});

var Window = window;

window.addEvent = document.addEvent = Element.prototype.addEvent;
window.removeEvent = document.removeEvent = Element.prototype.removeEvent;

var Garbage = {

	elements: [],

	collect: function(element){
		Garbage.elements.push(element);
	},

	trash: function(){
		window.removeEvent('unload', Garbage.trash);
		Garbage.elements.each(function(el){
			el.removeEvents();
			for (var p in Element.prototype) HTMLElement[p] = window[p] = document[p] = el[p] = null;
			el.extend = null;
		});
	}

};

window.addEvent('unload', Garbage.trash);

/*
Script: Event.js
	Event class

Author:
	Valerio Proietti, <http://mad4milk.net>, Michael Jackson, <http://ajaxon.com/michael>

License:
	MIT-style license.
*/

/*
Class: Event
	Cross browser methods to manage events.

Arguments:
	event - the event

Properties:
	shift - true if the user pressed the shift
	control - true if the user pressed the control 
	alt - true if the user pressed the alt
	meta - true if the user pressed the meta key
	code - the keycode of the key pressed
	page.x - the x position of the mouse, relative to the full window
	page.y - the y position of the mouse, relative to the full window
	client.x - the x position of the mouse, relative to the viewport
	client.y - the y position of the mouse, relative to the viewport
	key - the key pressed as a lowercase string. key also returns 'enter', 'up', 'down', 'left', 'right', 'space', 'backspace', 'delete', 'esc'. Handy for these special keys.
	target - the event target
	relatedTarget - the event related target

Example:
	(start code)
	$('myLink').onkeydown = function(event){
		var event = new Event(event);
		//event is now the Event class.
		alert(event.key); //returns the lowercase letter pressed
		alert(event.shift); //returns true if the key pressed is shift
		if (event.key == 's' && event.control) alert('document saved');
	};
	(end)
*/

var Event = new Class({

	initialize: function(event){
		this.event = event || window.event;
		this.type = this.event.type;
		this.target = this.event.target || this.event.srcElement;
		if (this.target.nodeType == 3) this.target = this.target.parentNode; // Safari
		this.shift = this.event.shiftKey;
		this.control = this.event.ctrlKey;
		this.alt = this.event.altKey;
		this.meta = this.event.metaKey;
		if (['DOMMouseScroll', 'mousewheel'].test(this.type)){
			this.wheel = this.event.wheelDelta ? (this.event.wheelDelta / (window.opera ? -120 : 120)) : -(this.event.detail || 0) / 3;
		} else if (this.type.test('key')){
			this.code = this.event.which || this.event.keyCode;
			for (var name in Event.keys){
				if (Event.keys[name] == this.code) var special = name;
			}
			this.key = special || String.fromCharCode(this.code).toLowerCase();

		} else if (this.type.test('mouse') || this.type == 'click'){
			this.page = {
				'x': this.event.pageX || this.event.clientX + document.documentElement.scrollLeft,
				'y': this.event.pageY || this.event.clientY + document.documentElement.scrollTop
			};
			this.client = {
				'x': this.event.pageX ? this.event.pageX - window.pageXOffset : this.event.clientX,
				'y': this.event.pageY ? this.event.pageY - window.pageYOffset : this.event.clientY
			};
			this.rightClick = (this.event.which == 3) || (this.event.button == 2);
			switch (this.type){
				case 'mouseover': this.relatedTarget = this.event.relatedTarget || this.event.fromElement; break;
				case 'mouseout': this.relatedTarget = this.event.relatedTarget || this.event.toElement;
			}
		}
	},

	/*
	Property: stop
		cross browser method to stop an event
	*/

	stop: function() {
		this.stopPropagation();
		this.preventDefault();
		return this;
	},

	/*
	Property: stopPropagation
		cross browser method to stop the propagation of an event
	*/

	stopPropagation: function(){
		if (this.event.stopPropagation) this.event.stopPropagation();
		else this.event.cancelBubble = true;
		return this;
    },

	/*
	Property: preventDefault
		cross browser method to prevent the default action of the event
	*/

	preventDefault: function(){
		if (this.event.preventDefault) this.event.preventDefault();
		else this.event.returnValue = false;
		return this;
	}

});

Event.keys = {
	'enter': 13,
	'up': 38,
	'down': 40,
	'left': 37,
	'right': 39,
	'esc': 27,
	'space': 32,
	'backspace': 8,
	'delete': 46
};

Function.extend({

	/*
	Property: bindWithEvent
		automatically passes mootools Event Class.

	Arguments:
		bind - optional, the object that the "this" of the function will refer to.

	Returns:
		a function with the parameter bind as its "this" and as a pre-passed argument event or window.event, depending on the browser.

	Example:
		>function myFunction(event){
		>	alert(event.clientx) //returns the coordinates of the mouse..
		>};
		>myElement.onclick = myFunction.bindWithEvent(myElement);
	*/

	bindWithEvent: function(bind, args){
		return this.create({'bind': bind, 'arguments': args, 'event': Event});
	}

});


/*
Script: Common.js
	Contains common implementations for custom classes. In Mootools is implemented in <Ajax> and <Fx>.

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Chain
	An "Utility" Class. Its methods can be implemented with <Class.implement> into any <Class>.
	Currently implemented in <Fx> and <Ajax>. In <Fx> for example, is used to execute a list of function, one after another, once the effect is completed.
	The functions will not be fired all togheter, but one every completion, to create custom complex animations.

Example:
	(start code)
	var myFx = new Fx.Style('element', 'opacity');

	myFx.start(1,0).chain(function(){
		myFx.start(0,1);
	}).chain(function(){
		myFx.start(1,0);
	}).chain(function(){
		myFx.start(0,1);
	});
	//the element will appear and disappear three times
	(end)
*/

var Chain = new Class({

	/*
	Property: chain
		adds a function to the Chain instance stack.

	Arguments:
		fn - the function to append.
	*/

	chain: function(fn){
		this.chains = this.chains || [];
		this.chains.push(fn);
		return this;
	},

	/*
	Property: callChain
		Executes the first function of the Chain instance stack, then removes it. The first function will then become the second.
	*/

	callChain: function(){
		if (this.chains && this.chains.length) this.chains.splice(0, 1)[0].delay(10, this);
	},

	/*
	Property: clearChain
		Clears the stack of a Chain instance.
	*/

	clearChain: function(){
		this.chains = [];
	}

});

/*
Class: Events
	An "Utility" Class. Its methods can be implemented with <Class.implement> into any <Class>.
	In <Fx> Class, for example, is used to give the possibility add any number of functions to the Effects events, like onComplete, onStart, onCancel

Example:
	(start code)
	var myFx = new Fx.Style('element', 'opacity').addEvent('onComplete', function(){
		alert('the effect is completed');
	}).addEvent('onComplete', function(){
		alert('I told you the effect is completed');
	});

	myFx.start(0,1);
	//upon completion it will display the 2 alerts, in order.
	(end)
*/

var Events = new Class({

	/*
	Property: addEvent
		adds an event to the stack of events of the Class instance.
	*/

	addEvent: function(type, fn){
		if (fn != Class.empty){
			this.events = this.events || {};
			this.events[type] = this.events[type] || [];
			if (!this.events[type].test(fn)) this.events[type].push(fn);
		}
		return this;
	},

	/*
	Property: fireEvent
		fires all events of the specified type in the Class instance.
	*/

	fireEvent: function(type, args, delay){
		if (this.events && this.events[type]){
			this.events[type].each(function(fn){
				fn.create({'bind': this, 'delay': delay, 'arguments': args})();
			}, this);
		}
		return this;
	},

	/*
	Property: removeEvent
		removes an event from the stack of events of the Class instance.
	*/

	removeEvent: function(type, fn){
		if (this.events && this.events[type]) this.events[type].remove(fn);
		return this;
	}

});

/*
Class: Options
	An "Utility" Class. Its methods can be implemented with <Class.implement> into any <Class>.
	Used to automate the options settings, also adding Class <Events> when the option begins with on.
*/

var Options = new Class({

	/*
	Property: setOptions
		sets this.options

	Arguments:
		defaults - the default set of options
		options - the user entered options. can be empty too.

	Note:
		if your Class has <Events> implemented, every option beginning with on, followed by a capital letter (onComplete) becomes an Class instance event.
	*/

	setOptions: function(defaults, options){
		this.options = Object.extend(defaults, options);
		if (this.addEvent){
			for (var option in this.options){
				if (($type(this.options[option]) == 'function') && option.test('^on[A-Z]')) this.addEvent(option, this.options[option]);
			}
		}
		return this;
	}

});

/*
Script: Dom.js
	Css Query related function and <Element> extensions

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/* Section: Utility Functions */

/* 
Function: $E 
	Selects a single (i.e. the first found) Element based on the selector passed in and an optional filter element.

Arguments:
	selector - the css selector to match
	filter - optional; a DOM element to limit the scope of the selector match; defaults to document.

Example:
	>$E('a', 'myElement') //find the first anchor tag inside the DOM element with id 'myElement'

Returns:
	a DOM element - the first element that matches the selector
*/

function $E(selector, filter){
	return ($(filter) || document).getElement(selector);
};

/*
Function: $ES
	Returns a collection of Elements that match the selector passed in limited to the scope of the optional filter.
	See Also: <Element.getElements> for an alternate syntax.

Returns:
	an array of dom elements that match the selector within the filter

Arguments:
	selector - css selector to match
	filter - optional; a DOM element to limit the scope of the selector match; defaults to document.

Examples:
	>$ES("a") //gets all the anchor tags; synonymous with $$("a")
	>$ES('a','myElement') //get all the anchor tags within $('myElement')
*/

function $ES(selector, filter){
	return ($(filter) || document).getElementsBySelector(selector);
};

/*
Class: Element
	Custom class to allow all of its methods to be used with any DOM element via the dollar function <$>.
*/

Element.extend({

	/*
	Property: getElements 
		Gets all the elements within an element that match the given (single) selector.

	Arguments:
		selector - the css selector to match

	Example:
		>$('myElement').getElements('a'); // get all anchors within myElement

	Credits:
		Say thanks to Christophe Beyls <http://digitalia.be> for the new regular expression that rules getElements, a big step forward in terms of speed.
	*/

	getElements: function(selector){
		var filters = [];
		selector.clean().split(' ').each(function(sel, i){
			var param = sel.match('^(\\w*|\\*)(?:#([\\w_-]+)|\\.([\\w_-]+))?(?:\\[["\']?(\\w+)["\']?(?:([\\*\\^\\$]?=)["\']?(\\w*)["\']?)?\\])?$');
			//PARAM ARRAY: 0 = full string: 1 = tag; 2 = id; 3 = class; 4 = attribute; 5 = operator; 6 = value;
			if (!param) return;
			param[1] = param[1] || '*';
			if (i == 0){
				if (param[2]){
					var el = this.getElementById(param[2]);
					if (!el || ((param[1] != '*') && (Element.prototype.getTag.call(el) != param[1]))) return;
					filters = [el];
				} else {
					filters = $A(this.getElementsByTagName(param[1]));
				}
			} else {
				filters = Elements.prototype.filterByTagName.call(filters, param[1]);
				if (param[2]) filters = Elements.prototype.filterById.call(filters, param[2]);
			}
			if (param[3]) filters = Elements.prototype.filterByClassName.call(filters, param[3]);
			if (param[4]) filters = Elements.prototype.filterByAttribute.call(filters, param[4], param[6], param[5]);
		}, this);
		return $$(filters);
	},

	/*
	Property: getElementById
		Targets an element with the specified id found inside the Element. Does not overwrite document.getElementById.

	Arguments:
		id - the id of the element to find.
	*/

	getElementById: function(id){
		var el = document.getElementById(id);
		if (!el) return false;
		for (var parent = el.parentNode; parent != this; parent = parent.parentNode){
			if (!parent) return false;
		}
		return el;
	},

	/*
	Property: getElement
		Same as <Element.getElements>, but returns only the first. Alternate syntax for <$E>, where filter is the Element.
	*/

	getElement: function(selector){
		return this.getElementsBySelector(selector)[0];
	},

	/*
	Property: getElementsBySelector
		Same as <Element.getElements>, but allows for comma separated selectors, as in css. Alternate syntax for <$$>, where filter is the Element.

	*/

	getElementsBySelector: function(selector){
		var els = [];
		selector.split(',').each(function(sel){
			els.extend(this.getElements(sel));
		}, this);
		return $$(els);
	}

});

document.extend = Object.extend;

/* Section: document related functions */

document.extend({
	/*
	Function: document.getElementsByClassName 
		Returns all the elements that match a specific class name. 
		Here for compatibility purposes. can also be written: document.getElements('.className'), or $$('.className')
	*/

	getElementsByClassName: function(className){
		return document.getElements('.'+className);
	},
	getElement: Element.prototype.getElement,
	getElements: Element.prototype.getElements,
	getElementsBySelector: Element.prototype.getElementsBySelector

});

/*
Class: Elements
	Methods for dom queries arrays, as <$$>.
*/

Elements.extend({

	//internal methods

	filterById: function(id, tag){
		var found = [];
		this.each(function(el){
			if (el.id == id) found.push(el);
		});
		return found;
	},

	filterByClassName: function(className){
		var found = [];
		this.each(function(el){
			if (Element.prototype.hasClass.call(el, className)) found.push(el);
		});
		return found;
	},

	filterByTagName: function(tagName){
		var found = [];
		this.each(function(el){
			found.extend(el.getElementsByTagName(tagName));
		});
		return found;
	},

	filterByAttribute: function(name, value, operator){
		var found = [];
		this.each(function(el){
			var att = el.getAttribute(name);
			if (!att) return found;
			if (!operator) return found.push(el);

			switch (operator){
				case '*=': if (att.test(value)) found.push(el); break;
				case '=': if (att == value) found.push(el); break;
				case '^=': if (att.test('^'+value)) found.push(el); break;
				case '$=': if (att.test(value+'$')) found.push(el);
			}
			return found;
		});
		return found;
	}

});

/*
Script: Hash.js
	Contains the class Hash.

Author:
	Christophe Beyls <http://digitalia.be>

License:
	MIT-style license.
*/

/*
Class: Hash
	It wraps an object that it uses internally as a map. The user must use put(), get(), and remove() to add/change, retrieve and remove values, it must not access the internal object directly. With this implementation, null values are not allowed.

Example:
	(start code)
	var hash = new Hash({a: 'hi', b: 'world', c: 'howdy'});
	hash.remove('b'); // b is removed.
	hash.set('c', 'hello');
	hash.get('c'); // returns 'hello'
	hash.length // returns 2 (a and b)
	(end)
*/

var Hash = new Class({

	length: 0,

	initialize: function(obj) {
		this.obj = {};
		for (var property in obj) {
			this.obj[property] = obj[property];
			this.length++;
		}
	},

	get: function(key) {
		return this.obj[key];
	},

	set: function(key, value) {
		if (value == null) return false;
		if (this.obj[key] == undefined) this.length++;
		this.obj[key] = value;
		return this;
	},

	remove: function(key) {
		if (this.obj[key] == undefined) return false;
		var obj = {};
		this.length--;
		for (var property in this.obj){
			if (property != key) obj[property] = this.obj[property];
		}
		this.obj = obj;
		return this;
	},

	each: function(fn, bind) {
		for (var property in this.obj) fn.call(bind || this, property, this.obj[property]);
	},
	
	extend: function(obj){
		this.initialize(Object.extend(this.obj, obj));
		return this;
	},

	empty: function() {
		return (this.length == 0);
	},

	keys: function() {
		var keys = [];
		for (var property in this.obj) keys.push(property);
		return keys;
	},

	values: function() {
		var values = [];
		for (var property in this.obj) values.push(this.obj[property]);
		return values;
	}

});

/*
Function: $H
	Shortcut to create an Hash from an Object.
*/

function $H(obj) {
	return new Hash(obj);
};

/*
Script: Color.js
	Contains the Color class.

Author:
	Michael Jackson <http://ajaxon.com/michael>

License:
	MIT-style license.
*/

/*
Class: Color
	Creates a new Color Object, which is an array with some color specific methods.

Example:
	(start code)
	var black = new Color('#000');
	var purple = new Color([255,0,255]);
	// mix black with white and purple, each time at 10% of the new color
	var darkpurple = black.mix('#fff', purple, 10);
	$('myDiv').setStyle('background-color', darkpurple);
	(end)
*/

var Color = new Class({

	initialize: function(color){
		if (color.mix && color.invert) return color;
		var rgb = (color.push) ? color : color.hexToRgb(true);
		return Object.extend(rgb, Color.prototype);
	},
	
	mix: function(){
		var colors = $A(arguments);
		var alpha = 50;
		if ($type(colors[colors.length-1]) == 'number') alpha = colors.pop();
		var rgb = this.copy();
		colors.each(function(color){
			color = new Color(color);
			for (var i = 0; i < 3; i++) rgb[i] = Math.round((rgb[i] / 100 * (100 - alpha)) + (color[i] / 100 * alpha));
		});
		return new Color(rgb);
	},

	invert: function(){
		var rgb = [];
		for (var i = 0; i < 3; i++) rgb.push(255 - this[i]);
		return new Color(rgb);
	}

});

function $C(color){
	return new Color(color);
};

/*
Script: Window.Base.js
	Contains Window.onDomReady and Window.disableImageCache

License:
	MIT-style license.
*/

/*
Class: Window
	Cross browser methods to get the window size, onDomReady method.
*/

window.extend = Object.extend;

window.extend({

	/*
	Function: window.disableImageCache
		Disables background image chache for internex explorer, to prevent flickering. 
		To be called if you have effects with background images, and they flicker.

	Example:
		Window.disableImageCache();
	*/

	disableImageCache: function(){
		if (this.ie6) try {document.execCommand("BackgroundImageCache", false, true);} catch (e){};
	},

	addEvent: function(type, fn){
		if (type == 'domready'){
			if (this.loaded) fn();
			else if (!this.events || !this.events.domready){
				var domReady = function(){
					if (this.loaded) return;
					this.loaded = true;
					if (this.timer) this.timer = $clear(this.timer);
					Element.prototype.fireEvent.call(this, 'domready');
					this.events.domready = null;
				}.bind(this);
				if (document.readyState && this.khtml){ //safari and konqueror
					this.timer = function(){
						if (['loaded','complete'].test(document.readyState)) domReady();
					}.periodical(50);
				}
				else if (document.readyState && this.ie){ //ie
					document.write("<script id=ie_ready defer src=javascript:void(0)><\/script>");
					$('ie_ready').onreadystatechange = function(){
						if (this.readyState == 'complete') domReady();
					};
				} else { //others
					this.addEvent("load", domReady);
					document.addEvent("DOMContentLoaded", domReady);
				}
			}
		}
		Element.prototype.addEvent.call(this, type, fn);
		return this;
	},

	/*
	Function: window.onDomReady
		Executes the passed in function when the DOM is ready (when the document tree has loaded, not waiting for images).
		Same as window.addEvent('domready', init);

	Credits:
		(c) Dean Edwards/Matthias Miller/John Resig, remastered for mootools. Later touched up by Christophe Beyls <http://digitalia.be>.

	Arguments:
		init - the function to execute when the DOM is ready

	Example:
		> window.addEvent('domready', function(){alert('the dom is ready')});
	*/

	onDomReady: function(init){
		return this.addEvent('domready', init);
	}

});

/*
Script: Window.Size.js
	Window cross-browser dimensions methods.

License:
	MIT-style license.
*/

/*
Class: window
	Cross browser methods to get the window size, onDomReady method.
*/

window.extend({

	/*
	Property: getWidth
		Returns an integer representing the width of the browser.
	*/

	getWidth: function(){
		if (this.khtml || this.opera) return this.innerWidth;
		else return document.documentElement.clientWidth || document.body.clientWidth;
	},

	/*
	Property: getHeight
		Returns an integer representing the height of the browser.
	*/

	getHeight: function(){
		if (this.khtml || this.opera) return this.innerHeight;
		return document.documentElement.clientHeight || document.body.clientHeight;
	},

	/*
	Property: getScrollHeight
		Returns an integer representing the scrollHeight of the window.

	See Also:
		<http://developer.mozilla.org/en/docs/DOM:element.scrollHeight>
	*/

	getScrollHeight: function(){
		return document.documentElement.scrollHeight;
	},

	/*
	Property: getScrollWidth
		Returns an integer representing the scrollWidth of the window.

	See Also:
		<http://developer.mozilla.org/en/docs/DOM:element.scrollWidth>
	*/

	getScrollWidth: function(){
		return document.documentElement.scrollWidth;
	},

	/*
	Property: getScrollTop
		Returns an integer representing the scrollTop of the window (the number of pixels the window has scrolled from the top).

	See Also:
		<http://developer.mozilla.org/en/docs/DOM:element.scrollTop>
	*/

	getScrollTop: function(){
		return this.pageYOffset || document.documentElement.scrollTop;
	},

	/*
	Property: getScrollLeft
		Returns an integer representing the scrollLeft of the window (the number of pixels the window has scrolled from the left).

	See Also:
		<http://developer.mozilla.org/en/docs/DOM:element.scrollLeft>
	*/

	getScrollLeft: function(){
		return this.pageXOffset || document.documentElement.scrollLeft;
	},

	/*
	Property: getSize
		Same as <Element.getSize>
	*/

	getSize: function(){
		return {
			'scroll': {'x': this.getScrollLeft(), 'y': this.getScrollTop()},
			'size': {'x': this.getWidth(), 'y': this.getHeight()},
			'scrollSize': {'x': this.getScrollWidth(), 'y': this.getScrollHeight()}
		};
	},

	//ignore
	getOffsets: function(){return {'x': 0, 'y': 0}}

});

/*
Script: Fx.Base.js
	Contains <Fx.Base> and two Transitions.

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

var Fx = {};

/*
Class: Fx.Base
	Base class for the Mootools Effects (Moo.Fx) library.

Options:
	onStart - the function to execute as the effect begins; nothing (<Class.empty>) by default.
	onComplete - the function to execute after the effect has processed; nothing (<Class.empty>) by default.
	transition - the equation to use for the effect see <Fx.Transitions>; default is <Fx.Transitions.sineInOut>
	duration - the duration of the effect in ms; 500 is the default.
	unit - the unit is 'px' by default (other values include things like 'em' for fonts or '%').
	wait - boolean: to wait or not to wait for a current transition to end before running another of the same instance. defaults to true.
	fps - the frames per second for the transition; default is 30
*/

Fx.Base = new Class({

	getOptions: function(){
		return {
			onStart: Class.empty,
			onComplete: Class.empty,
			onCancel: Class.empty,
			transition: Fx.Transitions.sineInOut,
			duration: 500,
			unit: 'px',
			wait: true,
			fps: 50
		};
	},

	initialize: function(options){
		this.element = this.element || null;
		this.setOptions(this.getOptions(), options);
		if (this.options.initialize) this.options.initialize.call(this);
	},

	step: function(){
		var time = new Date().getTime();
		if (time < this.time + this.options.duration){
			this.cTime = time - this.time;
			this.setNow();
			this.increase();
		} else {
			this.stop(true);
			this.now = this.to;
			this.increase();
			this.fireEvent('onComplete', this.element, 10);
			this.callChain();
		}
	},

	/*
	Property: set
		Immediately sets the value with no transition.

	Arguments:
		to - the point to jump to

	Example:
		>var myFx = new Fx.Style('myElement', 'opacity').set(0); //will make it immediately transparent
	*/

	set: function(to){
		this.now = to;
		this.increase();
		return this;
	},

	setNow: function(){
		this.now = this.compute(this.from, this.to);
	},

	compute: function(from, to){
		return this.options.transition(this.cTime, from, (to - from), this.options.duration);
	},

	/*
	Property: start
		Executes an effect from one position to the other.

	Arguments:
		from - integer: staring value
		to - integer: the ending value

	Examples:
		>var myFx = new Fx.Style('myElement', 'opacity').start(0,1); //display a transition from transparent to opaque.
	*/

	start: function(from, to){
		if (!this.options.wait) this.stop();
		else if (this.timer) return this;
		this.from = from;
		this.to = to;
		this.time = new Date().getTime();
		this.timer = this.step.periodical(Math.round(1000/this.options.fps), this);
		this.fireEvent('onStart', this.element);
		return this;
	},

	/*
	Property: stop
		Stops the transition.
	*/

	stop: function(end){
		if (!this.timer) return this;
		this.timer = $clear(this.timer);
		if (!end) this.fireEvent('onCancel', this.element);
		return this;
	},

	//compat
	custom: function(from, to){return this.start(from, to)},
	clearTimer: function(end){return this.stop(end)}

});

Fx.Base.implement(new Chain);
Fx.Base.implement(new Events);
Fx.Base.implement(new Options);

/*
Class: Fx.Transitions
	A collection of transition equations for use with the <Fx> Class.

See Also:
	<Fxtransitions.js> for a whole bunch of transitions.

Credits:
	Easing Equations, (c) 2003 Robert Penner (http://www.robertpenner.com/easing/), Open Source BSD License.
*/

Fx.Transitions = {

	/* Property: linear */
	linear: function(t, b, c, d){
		return c*t/d + b;
	},

	/* Property: sineInOut */
	sineInOut: function(t, b, c, d){
		return -c/2 * (Math.cos(Math.PI*t/d) - 1) + b;
	}

};

/*
Script: Fx.CSS.js
	Css parsing class for effects. Required by <Fx.Style>, <Fx.Styles>, <Fx.Elements>. No documentation needed, as its used internally.

Author:
	Christophe Beyls, <http://www.digitalia.be>,
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

Fx.CSS = {

	select: function(property, to){
		if (property.test('color', 'i')) return this.Color;
		if (to.test && to.test(' ')) return this.Multi;
		return this.Single;
	},

	parse: function(el, property, fromTo){
		if (!fromTo.push) fromTo = [fromTo];
		var from = fromTo[0], to = fromTo[1];
		if (!to && to != 0){
			to = from;
			from = el.getStyle(property);
		}
		var css = this.select(property, to);
		return {from: css.parse(from), to: css.parse(to), css: css};
	}

};

Fx.CSS.Single = {

	parse: function(value){
		return parseFloat(value);
	},

	getNow: function(from, to, fx){
		return fx.compute(from, to);
	},

	getValue: function(value, unit){
		return value+unit;
	}

};

Fx.CSS.Multi = {

	parse: function(value){
		return value.push ? value : value.split(' ').map(function(v){
			return parseFloat(v);
		});
	},

	getNow: function(from, to, fx){
		var now = [];
		for (var i = 0; i < from.length; i++) now[i] = fx.compute(from[i], to[i]);
		return now;
	},

	getValue: function(value, unit){
		return value.join(unit+' ')+unit;
	}

};

Fx.CSS.Color = {

	parse: function(value){
		return value.push ? value : value.hexToRgb(true);
	},

	getNow: function(from, to, fx){
		var now = [];
		for (var i = 0; i < from.length; i++) now[i] = Math.round(fx.compute(from[i], to[i]));
		return now;
	},

	getValue: function(value){
		return 'rgb('+value.join(',')+')';
	}

};

/*
Script: Fx.Style.js
	Contains <Fx.Style>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Fx.Style
	The Style effect; Extends <Fx.Base>, inherits all its properties. Used to transition any css property from one value to another. Includes colors.
	Colors must be in hex format.

Arguments:
	el - the $(element) to apply the style transition to
	property - the property to transition
	options - the Fx.Base options (see: <Fx.Base>)

Example:
	>var marginChange = new Fx.Style('myElement', 'margin-top', {duration:500});
	>marginChange.start(10, 100);
*/

Fx.Style = Fx.Base.extend({

	initialize: function(el, property, options){
		this.element = $(el);
		this.property = property;
		this.parent(options);
	},

	/*
	Property: hide
		Same as <Fx.Base.set>(0)
	*/

	hide: function(){
		return this.set(0);
	},

	setNow: function(){
		this.now = this.css.getNow(this.from, this.to, this);
	},

	set: function(to){
		this.css = Fx.CSS.select(this.property, to);
		return this.parent(this.css.parse(to));
	},

	/*
	Property: start
		displays the transition to the value/values passed in

	Example:
		(start code)
		var var marginChange = new Fx.Style('myElement', 'margin-top', {duration:500});
		marginChange.start(10); //tries to read current margin top value and goes from current to 10
		(end)
	*/

	start: function(from, to){
		if (this.timer && this.options.wait) return this;
		var parsed = Fx.CSS.parse(this.element, this.property, [from, to]);
		this.css = parsed.css;
		return this.parent(parsed.from, parsed.to);
	},

	increase: function(){
		this.element.setStyle(this.property, this.css.getValue(this.now, this.options.unit));
	}

});

/*
Class: Element
	Custom class to allow all of its methods to be used with any DOM element via the dollar function <$>.
*/

Element.extend({

	/*
	Property: effect
		Applies an <Fx.Style> to the Element; This a shortcut for <Fx.Style>.

	Example:
		>var myEffect = $('myElement').effect('height', {duration: 1000, transition: Fx.Transitions.linear});
		>myEffect.start(10, 100);
	*/

	effect: function(property, options){
		return new Fx.Style(this, property, options);
	}

});

/*
Script: Fx.Styles.js
	Contains <Fx.Styles>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Fx.Styles
	Allows you to animate multiple css properties at once; Extends <Fx.Base>, inherits all its properties. Includes colors.
	Colors must be in hex format.

Arguments:
	el - the $(element) to apply the styles transition to
	options - the fx options (see: <Fx.Base>)

Example:
	(start code)
	var myEffects = new Fx.Styles('myElement', {duration: 1000, transition: Fx.Transitions.linear});

	//height from 10 to 100 and width from 900 to 300
	myEffects.start({
		'height': [10, 100],
		'width': [900, 300]
	});

	//or height from current height to 100 and width from current width to 300
	myEffects.start({
		'height': 100,
		'width': 300
	});
	(end)
*/

Fx.Styles = Fx.Base.extend({

	initialize: function(el, options){
		this.element = $(el);
		this.parent(options);
	},

	setNow: function(){
		for (var p in this.from) this.now[p] = this.css[p].getNow(this.from[p], this.to[p], this);
	},

	set: function(to){
		var parsed = {};
		this.css = {};
		for (var p in to){
			this.css[p] = Fx.CSS.select(p, to[p]);
			parsed[p] = this.css[p].parse(to[p]);
		}
		return this.parent(parsed);
	},

	/*
	Property: start
		The function you'll actually use to execute a transition.

	Arguments:
		an object

	Example:
		see <Fx.Styles>
	*/

	start: function(obj){
		if (this.timer && this.options.wait) return this;
		this.now = {};
		this.css = {};
		var from = {}, to = {};
		for (var p in obj){
			var parsed = Fx.CSS.parse(this.element, p, obj[p]);
			from[p] = parsed.from;
			to[p] = parsed.to;
			this.css[p] = parsed.css;
		}
		return this.parent(from, to);
	},

	increase: function(){
		for (var p in this.now) this.element.setStyle(p, this.css[p].getValue(this.now[p], this.options.unit));
	}

});

/*
Class: Element
	Custom class to allow all of its methods to be used with any DOM element via the dollar function <$>.
*/

Element.extend({

	/*
	Property: effects
		Applies an <Fx.Styles> to the Element; This a shortcut for <Fx.Styles>.

	Example:
		>var myEffects = $(myElement).effects({duration: 1000, transition: Fx.Transitions.sineInOut});
 		>myEffects.start({'height': [10, 100], 'width': [900, 300]});
	*/

	effects: function(options){
		return new Fx.Styles(this, options);
	}

});

/*
Script: Fx.Elements.js
	Contains <Fx.Elements>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Fx.Elements
	Fx.Elements allows you to apply any number of styles transitions to a selection of elements. Includes colors (must be in hex format).

Arguments:
	elements - a collection of elements the effects will be applied to.
	options - same as <Fx.Base> options.
*/

Fx.Elements = Fx.Base.extend({

	initialize: function(elements, options){
		this.elements = $$(elements);
		this.parent(options);
	},

	setNow: function(){
		for (var i in this.from){
			var iFrom = this.from[i], iTo = this.to[i], iCss = this.css[i], iNow = this.now[i] = {};
			for (var p in iFrom) iNow[p] = iCss[p].getNow(iFrom[p], iTo[p], this);
		}
	},

	set: function(to){
		var parsed = {};
		this.css = {};
		for (var i in to){
			var iTo = to[i], iCss = this.css[i] = {}, iParsed = parsed[i] = {};
			for (var p in iTo){
				iCss[p] = Fx.CSS.select(p, iTo[p]);
				iParsed[p] = iCss[p].parse(iTo[p]);
			}
		}
		return this.parent(parsed);
	},

	/*
	Property: start
		Applies the passed in style transitions to each object named (see example). Each item in the collection is refered to as a numerical string ("1" for instance). The first item is "0", the second "1", etc.

	Example:
		(start code)
		var myElementsEffects = new Fx.Elements($$('a'));
		myElementsEffects.start({
			'0': { //let's change the first element's opacity and width
				'opacity': [0,1],
				'width': [100,200]
			},
			'1': { //and the second one's opacity
				'opacity': [0.2, 0.5]
			}
		});
		(end)
	*/

	start: function(obj){
		if (this.timer && this.options.wait) return this;
		this.now = {};
		this.css = {};
		var from = {}, to = {};
		for (var i in obj){
			var iProps = obj[i], iFrom = from[i] = {}, iTo = to[i] = {}, iCss = this.css[i] = {};
			for (var p in iProps){
				var parsed = Fx.CSS.parse(this.elements[i], p, iProps[p]);
				iFrom[p] = parsed.from;
				iTo[p] = parsed.to;
				iCss[p] = parsed.css;
			}
		}
		return this.parent(from, to);
	},

	increase: function(){
		for (var i in this.now){
			var iNow = this.now[i], iCss = this.css[i];
			for (var p in iNow) this.elements[i].setStyle(p, iCss[p].getValue(iNow[p], this.options.unit));
		}
	}

});

/*
Script: Fx.Scroll.js
	Contains <Fx.Scroll>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Fx.Scroll
	Scroll any element with an overflow, including the window element.

Arguments:
	element - the element to scroll
	options - same as <Fx.Base> options.
*/

Fx.Scroll = Fx.Base.extend({

	initialize: function(element, options){
		this.now = [];
		this.element = $(element);
		this.addEvent('onStart', function(){
			this.element.addEvent('mousewheel', this.stop.bind(this, false));
		}.bind(this));
		this.removeEvent('onComplete', function(){
			this.element.removeEvent('mousewheel', this.stop.bind(this, false));
		}.bind(this));
		this.parent(options);
	},

	setNow: function(){
		for (var i = 0; i < 2; i++) this.now[i] = this.compute(this.from[i], this.to[i]);
	},

	/*
	Property: scrollTo
		Scrolls the chosen element to the x/y coordinates.

	Arguments:
		x - the x coordinate to scroll the element to
		y - the y coordinate to scroll the element to
	*/

	scrollTo: function(x, y){
		if (this.timer && this.options.wait) return this;
		var el = this.element.getSize();
		var values = {'x': x, 'y': y};
		for (var z in el.size){
			var max = el.scrollSize[z] - el.size[z];
			if ($chk(values[z])) values[z] = ($type(values[z]) == 'number') ? Math.max(Math.min(values[z], max), 0) : max;
			else values[z] = el.scroll[z];
		}
		return this.start([el.scroll.x, el.scroll.y], [values.x, values.y]);
	},

	/*
	Property: toTop
		Scrolls the chosen element to its maximum top.
	*/

	toTop: function(){
		return this.scrollTo(false, 0);
	},

	/*
	Property: toBottom
		Scrolls the chosen element to its maximum bottom.
	*/

	toBottom: function(){
		return this.scrollTo(false, 'full');
	},

	/*
	Property: toLeft
		Scrolls the chosen element to its maximum left.
	*/

	toLeft: function(){
		return this.scrollTo(0, false);
	},

	/*
	Property: toRight
		Scrolls the chosen element to its maximum right.
	*/

	toRight: function(){
		return this.scrollTo('full', false);
	},

	/*
	Property: toElement
		Scrolls the specified element to the position the passed in element is found. Only usable if the chosen element is == window.

	Arguments:
		el - the $(element) to scroll the window to
	*/

	toElement: function(el){
		return this.scrollTo($(el).getLeft(), $(el).getTop());
	},

	increase: function(){
		this.element.scrollTo(this.now[0], this.now[1]);
	}

});

/*
Script: Fx.Slide.js
	Contains <Fx.Slide>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Fx.Slide
	The slide effect; slides an element in horizontally or vertically, the contents will fold inside. Extends <Fx.Base>, inherits all its properties.

Note:
	This effect works on any block element, but the element *cannot be positioned*; no margins or absolute positions. To position the element, put it inside another element (a wrapper div, for instance) and position that instead.

Options:
	mode - set it to vertical or horizontal. Defaults to vertical.
	and all the <Fx.Base> options

Example:
	(start code)
	var mySlider = new Fx.Slide('myElement', {duration: 500});
	mySlider.toggle() //toggle the slider up and down.
	(end)
*/

Fx.Slide = Fx.Base.extend({

	initialize: function(el, options){
		this.element = $(el).setStyle('margin', 0);
		this.wrapper = new Element('div').injectAfter(this.element).setStyle('overflow', 'hidden').adopt(this.element);
		this.setOptions({'mode': 'vertical'}, options);
		this.now = [];
		this.parent(this.options);
	},

	setNow: function(){
		for (var i = 0; i < 2; i++) this.now[i] = this.compute(this.from[i], this.to[i]);
	},

	vertical: function(){
		this.margin = 'top';
		this.layout = 'height';
		this.offset = this.element.offsetHeight;
		return [this.element.getStyle('margin-top').toInt(), this.wrapper.getStyle('height').toInt()];
	},

	horizontal: function(){
		this.margin = 'left';
		this.layout = 'width';
		this.offset = this.element.offsetWidth;
		return [this.element.getStyle('margin-left').toInt(), this.wrapper.getStyle('width').toInt()];
	},

	/*
	Property: slideIn
		slides the elements in view horizontally or vertically, depending on the mode parameter or options.mode.
	*/

	slideIn: function(mode){
		return this.start(this[mode || this.options.mode](), [0, this.offset]);
	},

	/*
	Property: slideOut
		slides the elements out of the view horizontally or vertically, depending on the mode parameter or options.mode.
	*/

	slideOut: function(mode){
		return this.start(this[mode || this.options.mode](), [-this.offset, 0]);
	},

	/*
	Property: hide
		Hides the element without a transition.
	*/

	hide: function(mode){
		this[mode || this.options.mode]();
		return this.set([-this.offset, 0]);
	},

	/*
	Property: show
		Shows the element without a transition.
	*/

	show: function(mode){
		this[mode || this.options.mode]();
		return this.set([0, this.offset]);
	},

	/*
	Property: toggle
		Slides in or Out the element, depending on its state
	*/

	toggle: function(mode){
		if (this.wrapper.offsetHeight == 0 || this.wrapper.offsetWidth == 0) return this.slideIn(mode);
		else return this.slideOut(mode);
	},

	increase: function(){
		this.element.setStyle('margin-'+this.margin, this.now[0]+this.options.unit);
		this.wrapper.setStyle(this.layout, this.now[1]+this.options.unit);
	}

});

/*
Script: Fx.Transitions.js
	Cool transitions, to be used with all the effects.

Author:
	Robert Penner, <http://www.robertpenner.com/easing/>, modified to be used with mootools.

License:
	Easing Equations v1.5, (c) 2003 Robert Penner, all rights reserved. Open Source BSD License.
*/

/*
Class: Fx.Transitions
	A collection of tweaning transitions for use with the <Fx.Base> classes.
*/

Fx.Transitions = {

	/* Property: linear */
	linear: function(t, b, c, d){
		return c*t/d + b;
	},

	/* Property: quadIn */
	quadIn: function(t, b, c, d){
		return c*(t/=d)*t + b;
	},

	/* Property: quatOut */
	quadOut: function(t, b, c, d){
		return -c *(t/=d)*(t-2) + b;
	},

	/* Property: quadInOut */
	quadInOut: function(t, b, c, d){
		if ((t/=d/2) < 1) return c/2*t*t + b;
		return -c/2 * ((--t)*(t-2) - 1) + b;
	},

	/* Property: cubicIn */
	cubicIn: function(t, b, c, d){
		return c*(t/=d)*t*t + b;
	},

	/* Property: cubicOut */
	cubicOut: function(t, b, c, d){
		return c*((t=t/d-1)*t*t + 1) + b;
	},

	/* Property: cubicInOut */
	cubicInOut: function(t, b, c, d){
		if ((t/=d/2) < 1) return c/2*t*t*t + b;
		return c/2*((t-=2)*t*t + 2) + b;
	},

	/* Property: quartIn */
	quartIn: function(t, b, c, d){
		return c*(t/=d)*t*t*t + b;
	},

	/* Property: quartOut */
	quartOut: function(t, b, c, d){
		return -c * ((t=t/d-1)*t*t*t - 1) + b;
	},

	/* Property: quartInOut */
	quartInOut: function(t, b, c, d){
		if ((t/=d/2) < 1) return c/2*t*t*t*t + b;
		return -c/2 * ((t-=2)*t*t*t - 2) + b;
	},

	/* Property: quintIn */
	quintIn: function(t, b, c, d){
		return c*(t/=d)*t*t*t*t + b;
	},

	/* Property: quintOut */
	quintOut: function(t, b, c, d){
		return c*((t=t/d-1)*t*t*t*t + 1) + b;
	},

	/* Property: quintInOut */
	quintInOut: function(t, b, c, d){
		if ((t/=d/2) < 1) return c/2*t*t*t*t*t + b;
		return c/2*((t-=2)*t*t*t*t + 2) + b;
	},

	/* Property: sineIn */
	sineIn: function(t, b, c, d){
		return -c * Math.cos(t/d * (Math.PI/2)) + c + b;
	},

	/* Property: sineOut */
	sineOut: function(t, b, c, d){
		return c * Math.sin(t/d * (Math.PI/2)) + b;
	},

	/* Property: sineInOut */
	sineInOut: function(t, b, c, d){
		return -c/2 * (Math.cos(Math.PI*t/d) - 1) + b;
	},

	/* Property: expoIn */
	expoIn: function(t, b, c, d){
		return (t==0) ? b : c * Math.pow(2, 10 * (t/d - 1)) + b;
	},

	/* Property: expoOut */
	expoOut: function(t, b, c, d){
		return (t==d) ? b+c : c * (-Math.pow(2, -10 * t/d) + 1) + b;
	},

	/* Property: expoInOut */
	expoInOut: function(t, b, c, d){
		if (t==0) return b;
		if (t==d) return b+c;
		if ((t/=d/2) < 1) return c/2 * Math.pow(2, 10 * (t - 1)) + b;
		return c/2 * (-Math.pow(2, -10 * --t) + 2) + b;
	},

	/* Property: circIn */
	circIn: function(t, b, c, d){
		return -c * (Math.sqrt(1 - (t/=d)*t) - 1) + b;
	},

	/* Property: circOut */
	circOut: function(t, b, c, d){
		return c * Math.sqrt(1 - (t=t/d-1)*t) + b;
	},

	/* Property: circInOut */
	circInOut: function(t, b, c, d){
		if ((t/=d/2) < 1) return -c/2 * (Math.sqrt(1 - t*t) - 1) + b;
		return c/2 * (Math.sqrt(1 - (t-=2)*t) + 1) + b;
	},

	/* Property: elasticIn */
	elasticIn: function(t, b, c, d, a, p){
		if (t==0) return b; if ((t/=d)==1) return b+c; if (!p) p=d*.3; if (!a) a = 1;
		if (a < Math.abs(c)){ a=c; var s=p/4; }
		else var s = p/(2*Math.PI) * Math.asin(c/a);
		return -(a*Math.pow(2,10*(t-=1)) * Math.sin( (t*d-s)*(2*Math.PI)/p )) + b;
	},

	/* Property: elasticOut */
	elasticOut: function(t, b, c, d, a, p){
		if (t==0) return b; if ((t/=d)==1) return b+c; if (!p) p=d*.3; if (!a) a = 1;
		if (a < Math.abs(c)){ a=c; var s=p/4; }
		else var s = p/(2*Math.PI) * Math.asin(c/a);
		return a*Math.pow(2,-10*t) * Math.sin( (t*d-s)*(2*Math.PI)/p ) + c + b;
	},

	/* Property: elasticInOut */
	elasticInOut: function(t, b, c, d, a, p){
		if (t==0) return b; if ((t/=d/2)==2) return b+c; if (!p) p=d*(.3*1.5); if (!a) a = 1;
		if (a < Math.abs(c)){ a=c; var s=p/4; }
		else var s = p/(2*Math.PI) * Math.asin(c/a);
		if (t < 1) return -.5*(a*Math.pow(2,10*(t-=1)) * Math.sin( (t*d-s)*(2*Math.PI)/p )) + b;
		return a*Math.pow(2,-10*(t-=1)) * Math.sin( (t*d-s)*(2*Math.PI)/p )*.5 + c + b;
	},

	/* Property: backIn */
	backIn: function(t, b, c, d, s){
		if (!s) s = 1.70158;
		return c*(t/=d)*t*((s+1)*t - s) + b;
	},

	/* Property: backOut */
	backOut: function(t, b, c, d, s){
		if (!s) s = 1.70158;
		return c*((t=t/d-1)*t*((s+1)*t + s) + 1) + b;
	},

	/* Property: backInOut */
	backInOut: function(t, b, c, d, s){
		if (!s) s = 1.70158;
		if ((t/=d/2) < 1) return c/2*(t*t*(((s*=(1.525))+1)*t - s)) + b;
		return c/2*((t-=2)*t*(((s*=(1.525))+1)*t + s) + 2) + b;
	},

	/* Property: bounceIn */
	bounceIn: function(t, b, c, d){
		return c - Fx.Transitions.bounceOut (d-t, 0, c, d) + b;
	},

	/* Property: bounceOut */
	bounceOut: function(t, b, c, d){
		if ((t/=d) < (1/2.75)){
			return c*(7.5625*t*t) + b;
		} else if (t < (2/2.75)){
			return c*(7.5625*(t-=(1.5/2.75))*t + .75) + b;
		} else if (t < (2.5/2.75)){
			return c*(7.5625*(t-=(2.25/2.75))*t + .9375) + b;
		} else {
			return c*(7.5625*(t-=(2.625/2.75))*t + .984375) + b;
		}
	},

	/* Property: bounceInOut */
	bounceInOut: function(t, b, c, d){
		if (t < d/2) return Fx.Transitions.bounceIn(t*2, 0, c, d) * .5 + b;
		return Fx.Transitions.bounceOut(t*2-d, 0, c, d) * .5 + c*.5 + b;
	}

};

/*
Script: Drag.Base.js
	Contains <Drag.Base>, <Element.makeResizable>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

var Drag = {};

/*
Class: Drag.Base
	Modify two css properties of an element based on the position of the mouse.

Arguments:
	el - the $(element) to apply the transformations to.
	options - optional. The options object.

Options:
	handle - the $(element) to act as the handle for the draggable element. defaults to the $(element) itself.
	modifiers - an object. see Modifiers Below.
	onStart - optional, function to execute when the user starts to drag (on mousedown);
	onComplete - optional, function to execute when the user completes the drag.
	onDrag - optional, function to execute at every step of the drag
	limit - an object, see Limit below.
	snap - optional, the distance you have to drag before the element starts to respond to the drag. defaults to false

	modifiers:
		x - string, the style you want to modify when the mouse moves in an horizontal direction. defaults to 'left'
		y - string, the style you want to modify when the mouse moves in a vertical direction. defaults to 'top'

	limit:
		x - array with start and end limit relative to modifiers.x
		y - array with start and end limit relative to modifiers.y
*/

Drag.Base = new Class({

	getOptions: function(){
		return {
			handle: false,
			unit: 'px',
			onStart: Class.empty, 
			onComplete: Class.empty,
			onSnap: Class.empty,
			onDrag: Class.empty,
			limit: false,
			modifiers: {x: 'left', y: 'top'},
			snap: 6
		};
	},

	initialize: function(el, options){
		this.setOptions(this.getOptions(), options);
		this.element = $(el);
		this.handle = $(this.options.handle) || this.element;
		this.mouse = {'now': {}, 'pos': {}};
		this.value = {'start': {}, 'now': {}};
		this.bound = {'start': this.start.bindWithEvent(this)};
		this.handle.addEvent('mousedown', this.bound.start);
		if (this.options.initialize) this.options.initialize.call(this);
	},

	start: function(event){
		this.mouse.start = event.page;
		var limit = this.options.limit;
		this.limit = {'x': [], 'y': []};
		for (var z in this.options.modifiers){
			this.value.now[z] = this.element.getStyle(this.options.modifiers[z]).toInt();
			this.mouse.pos[z] = event.page[z] - this.value.now[z];
			if (limit && limit[z]){
				for (var i = 0; i < 2; i++){
					if ($chk(limit[z][i])) this.limit[z][i] = limit[z][i].apply ? limit[z][i].call(this) : limit[z][i];
				}
			}
		}
		this.bound.drag = this.drag.bindWithEvent(this);
		this.bound.checkAndDrag = this.checkAndDrag.bindWithEvent(this);
		this.bound.stop = this.stop.bind(this);
		document.addEvent('mousemove', this.options.snap ? this.bound.checkAndDrag : this.bound.drag);
		document.addEvent('mouseup', this.bound.stop);
		this.fireEvent('onStart', this.element);
		event.stop();
	},

	checkAndDrag: function(event){
		var distance = Math.round(Math.sqrt(Math.pow(event.page.x - this.mouse.start.x, 2) + Math.pow(event.page.y - this.mouse.start.y, 2)));
		if (distance > this.options.snap){
			document.removeEvent('mousemove', this.bound.checkAndDrag);
			document.addEvent('mousemove', this.bound.drag);
			this.drag(event);
			this.fireEvent('onSnap', this.element);
		}
		event.stop();
	},

	drag: function(event){
		this.out = false;
		this.mouse.now = event.page;
		for (var z in this.options.modifiers){
			this.value.now[z] = event.page[z] - this.mouse.pos[z];
			if (this.limit[z]){
				if ($chk(this.limit[z][1]) && (this.value.now[z] > this.limit[z][1])){
					this.value.now[z] = this.limit[z][1];
					this.out = true;
				} else if ($chk(this.limit[z][0]) && (this.value.now[z] < this.limit[z][0])){
					this.value.now[z] = this.limit[z][0];
					this.out = true;
				}
			}
			this.element.setStyle(this.options.modifiers[z], this.value.now[z] + this.options.unit);
		}
		this.fireEvent('onDrag', this.element);
		event.stop();
	},
	
	detach: function(){
		this.handle.removeEvent('mousedown', this.bound.start);
	},

	stop: function(){
		document.removeEvent('mousemove', this.bound.drag);
		document.removeEvent('mouseup', this.bound.stop);
		this.fireEvent('onComplete', this.element);
	}

});

Drag.Base.implement(new Events);
Drag.Base.implement(new Options);

/*
Class: Element
	Custom class to allow all of its methods to be used with any DOM element via the dollar function <$>.
*/

Element.extend({

	/*
	Property: makeResizable
		Makes an element resizable (by dragging) with the supplied options.

	Arguments:
		options - see <Drag.Base> for acceptable options.
	*/

	makeResizable: function(options){
		return new Drag.Base(this, Object.extend(options || {}, {modifiers: {x: 'width', y: 'height'}}));
	}

});

/*
Script: Scroller.js
	Contains the <Scroller>.

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Scroller
	The Scroller is a class to scroll any element with an overflow (including the window) when the mouse cursor reaches certain buondaries of that element.
	You must call its start method to start listening to mouse movements.

Arguments:
	element - required, the element to scroll.
	options - optional, see options below, and <Fx.Base> options.

Options:
	area - integer, the necessary boundaries to make the element scroll.
	velocity - integer, velocity ratio, the modifier for the window scrolling speed.
	onChange - optionally, when the mouse reaches some boundaries, you can choose to alter some other values, instead of the scrolling offsets.
		Automatically passes as parameters x and y values.
*/

var Scroller = new Class({

	getOptions: function(){
		return {
			area: 20,
			velocity: 1,
			onChange: function(x, y){
				this.element.scrollTo(x, y);
			}
		};
	},

	initialize: function(element, options){
		this.setOptions(this.getOptions(), options);
		this.element = $(element);
		this.mousemover = ([window, document].test(element)) ? $(document.body) : this.element;
	},

	/*
	Property: start
		The scroller starts listening to mouse movements.
	*/

	start: function(){
		this.coord = this.getCoords.bindWithEvent(this);
		this.mousemover.addEvent('mousemove', this.coord);
	},

	/*
	Property: stop
		The scroller stops listening to mouse movements.
	*/

	stop: function(){
		this.mousemover.removeEvent('mousemove', this.coord);
		this.timer = $clear(this.timer);
	},

	getCoords: function(event){
		this.page = (this.element == window) ? event.client : event.page;
		if (!this.timer) this.timer = this.scroll.periodical(50, this);
	},

	scroll: function(){
		var el = this.element.getSize();
		var pos = this.element.getOffsets();

		var change = {'x': 0, 'y': 0};
		for (var z in this.page){
			if (this.page[z] < (this.options.area + pos[z]) && el.scroll[z] != 0)
				change[z] = (this.page[z] - this.options.area - pos[z]) * this.options.velocity;
			else if (this.page[z] + this.options.area > (el.size[z] + pos[z]) && el.scroll[z] + el.size[z] != el.scrollSize[z])
				change[z] = (this.page[z] - el.size[z] + this.options.area - pos[z]) * this.options.velocity;
		}
		if (change.y || change.x) this.fireEvent('onChange', [el.scroll.x + change.x, el.scroll.y + change.y]);
	}

});

Scroller.implement(new Events);
Scroller.implement(new Options);

/*
Script: Slider.js
	Contains <Slider>

Author:
	Valerio Proietti, <http://mad4milk.net>

License:
	MIT-style license.
*/

/*
Class: Slider
	Creates a slider with two elements: a knob and a container. Returns the values.

Arguments:
	element - the knob container
	knob - the handle
	options - see Options below

Options:
	onChange - a function to fire when the value changes.
	onComplete - a function to fire when you're done dragging.
	onTick - optionally, you can alter the onTick behavior, for example displaying an effect of the knob moving to the desired position. 
		Passes as parameter the new position.
	steps - the number of steps for your slider.
	mode - either 'horizontal' or 'vertical'. defaults to horizontal.
	wheel - experimental! Also use the mouse wheel to control the slider. defaults to false.
*/

var Slider = new Class({

	getOptions: function(){
		return {
			onChange: Class.empty,
			onComplete: Class.empty,
			onTick: function(pos){
				this.knob.setStyle(this.p, pos+'px');
			},
			steps: 100,
			mode: 'horizontal',
			wheel: false
		};
	},

	initialize: function(el, knob, options){
		this.element = $(el);
		this.knob = $(knob);
		this.setOptions(this.getOptions(), options);

		this.previousChange = -1;
		this.previousEnd = -1;
		this.step = -1;

		this.element.addEvent('mousedown', this.clickedElement.bindWithEvent(this));

		if (this.options.wheel) this.element.addEvent('mousewheel', this.scrolledElement.bindWithEvent(this));

		if (this.options.mode == 'horizontal'){
			this.z = 'x'; this.p = 'left';
			this.max = this.element.offsetWidth-this.knob.offsetWidth;
			this.half = this.knob.offsetWidth/2;
			this.getPos = this.element.getLeft.bind(this.element);
		} else if (this.options.mode == 'vertical'){
			this.z = 'y'; this.p = 'top';
			this.max = this.element.offsetHeight-this.knob.offsetHeight;
			this.half = this.knob.offsetHeight/2;
			this.getPos = this.element.getTop.bind(this.element);
		}

		this.knob.setStyle('position', 'relative').setStyle(this.p, 0);

		var modSlide = {}, limSlide = {};

		limSlide[this.z] = [0, this.max];
		modSlide[this.z] = this.p;

		this.drag = new Drag.Base(this.knob, {
			limit: limSlide,
			snap: 0,
			modifiers: modSlide,
			onStart: function(){
				this.draggedKnob();
			}.bind(this),
			onDrag: function(){
				this.draggedKnob();
			}.bind(this),
			onComplete: function(){
				this.draggedKnob();
				this.end();
			}.bind(this)
		});
		if (this.options.initialize) this.options.initialize.call(this);
	},

	/*
	Property: set
		The slider will get the step you pass.

	Arguments:
		step - one integer
	*/

	set: function(step){
		if (step > this.options.steps) step = this.options.steps;
		else if (step < 0) step = 0;
		this.step = step;
		this.checkStep();
		this.end();
		this.fireEvent('onTick', this.toPosition(this.step)+'');
		return this;
	},

	scrolledElement: function(event){
		if (event.wheel < 0) this.set(this.step + 1);
		else if (event.wheel > 0) this.set(this.step - 1);
		event.stop();
	},

	clickedElement: function(event){
		var position = event.page[this.z] - this.getPos() - this.half;
		if (position > this.max) position = this.max;
		else if (position < 0) position = 0;
		this.step = this.toStep(position);
		this.checkStep();
		this.end();
		this.fireEvent('onTick', position+'');
	},

	draggedKnob: function(){
		this.step = this.toStep(this.drag.value.now[this.z]);
		this.checkStep();
	},

	checkStep: function(){
		if (this.previousChange != this.step){
			this.previousChange = this.step;
			this.fireEvent('onChange', this.step);
		}
	},

	end: function(){
		if (this.previousEnd !== this.step){
			this.previousEnd = this.step;
			this.fireEvent('onComplete', this.step+'');
		}
	},

	toStep: function(position){
		return Math.round(position/this.max*this.options.steps);
	},

	toPosition: function(step){
		return (this.max)*step/this.options.steps;
	}

});

Slider.implement(new Events);
Slider.implement(new Options);