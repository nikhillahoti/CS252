var contracts = require('contracts');

var contract = contracts.contract;
var isNumber = contracts.isNumber;


//
// ***YOUR SWEET.JS MACROS HERE***
//

$contract {
  pre: [isNumber, isNumber],
  post: isNumber
}
function add (x, y) {
  return x + y;
}

console.log("First test");
console.log(add(3,4));

console.log("Second test");
try {
  console.log(add(3,"4"));
} catch (e) {
  console.log(e.message);
}


