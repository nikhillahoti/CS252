
var map = function (f, array) {
	if (array.length > 0){
		var x = array.shift();
		return [f(x)].concat(map(f, array));	
	}
	return [];
}

var makeListofAdders = function(x){
	if(x.length > 0){
		var val = x.shift();
		return function(y){
			console.log(x);
			return val + y;
		}.concat(makeListofAdders(x));
	}
	else{
		return;
	}
}

a = makeListofAdders([1,5]);
console.log(a[0](42));