
var foldl = function (f, acc, array) {
	if (array.length > 0){
		return foldl(f, f(acc,array.pop()), array)	
	}
	return acc;
}

console.log(foldl(function(x,y){return x+y}, 0, [1,2,3]));

var foldr = function (f, acc, array) {
	if (array.length > 0){
		return foldr(f, f(acc,array.shift()), array);	
	}
	return acc;
}

console.log(foldr(function(x,y){return x/y}, 1, [2,4,8]));

var map = function (f, array) {
	if (array.length > 0){
		var x = array.shift();
		return [f(x)].concat(map(f, array));	
	}
	return [];
}

console.log(map(function(x){return x+x}, [1,2,3,5,7,9,11,13]));

// Write a curry function as we discussed in class.
// Create a `double` method using the curry function
// and the following `mult` function.
function mult(x,y) {
  return x * y;
}

Function.prototype.curry = function() {
  var slice = Array.prototype.slice,
      args = slice.apply(arguments),
      that = this;
  return function () {
    return that.apply(null, args.concat(slice.apply(arguments)));
  };
};

var mulOne = mult.curry(15);
console.log(mulOne(3));