
// JSLINT Code
var name = "Monty";
function Rabbit(name) {
  "use strict"; 
  this.name = name;
}
var r = new Rabbit("Python");

console.log(r.name);  // ERROR!!!
console.log(name);    // Prints "Python"

function swap(arr,i,j) {
  "use strict";
  var tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
}
function sortAndGetLargest (arr) {
  "use strict";
  var tmp = arr[0]; // largest elem
  var i;
  var j;
  for(i=0; i<arr.length; i+=1) {
    if (arr[i] > tmp){
      tmp = arr[i];
    }
    for(j=i+1; j<arr.length; j+=1)
    {
      if (arr[i] < arr[j]){
         swap(arr,i,j);
      }
    }
  }
  return tmp;
}
var largest = sortAndGetLargest([99,2,43,8,0,21,12]);
console.log(largest); // should be 99, but prints 0

// TypeScript code
var name: string = "Monty";
function Rabbit(name: string) :void {
  "use strict"; 
  this.name = name;
}
var r = new Rabbit("Python");

console.log(r.name);  // ERROR!!!
console.log(name);    // Prints "Python"


function swap(arr,i,j) {
  "use strict";
  var tmp :number = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
}
function sortAndGetLargest (arr :number[]) :number {
  "use strict";
  var tmp :number = arr[0]; // largest elem
  var i :number;
  var j :number;
  for(i=0; i<arr.length; i+=1) {
    if (arr[i] > tmp){
      tmp = arr[i];
    }
    for(j=i+1; j<arr.length; j+=1)
    {
      if (arr[i] < arr[j]){
         swap(arr,i,j);
      }
    }
  }
  return tmp;
}
var largest :number = sortAndGetLargest([99,2,43,8,0,21,12]);
console.log(largest); // should be 99, but prints 0
