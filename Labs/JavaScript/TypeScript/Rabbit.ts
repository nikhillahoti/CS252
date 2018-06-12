
// TypeScript Code
var name: string = "Monty";
function Rabbit(name: string) :void {
  "use strict"; 
  this.name = name;
}
var r = new Rabbit("Python");

console.log(r.name);  // ERROR!!!
console.log(name);    // Prints "Python"

