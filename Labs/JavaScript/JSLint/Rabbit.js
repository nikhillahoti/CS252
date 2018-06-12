
// JSLINT Code
var name = "Monty";
function Rabbit(name) {
  "use strict"; 
  this.name = name;
}
var r = new Rabbit("Python");

console.log(r.name);  // ERROR!!!
console.log(name);    // Prints "Python"

