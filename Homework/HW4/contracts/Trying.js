
function contract (preList, post, f) {
  let cont = function(){
    let args = Array.prototype.slice.call(arguments);
    for (let i = 0 ; i < preList.length ; i++) {
      if (!preList[i].call(this, args[i])) {
        return "Contract violation in position " + i + ". Expected number but received " + args[i] + ". Blame -> Top-level code";
      }
    }

    let result = f.apply(this,args);
    if(!post.call(this, result)){
      return "Contract violation. Expected number but returned " + result + ". Blame -> " + f.name;
    }
    else {
      return result;
    }
  }
  return cont;
}

function and() {
  let args = Array.prototype.slice.call(arguments);
  let cont = function(v) {
    for (let i in args) {
      if (!args[i].call(this, v)) {
        return false;
      }
    }
    return true;
  }
  cont.expected = expect(args[0]);
  for (let i=1; i<args.length; i++) {
    cont.expected += " and " + expect(args[i]);
  }
  return cont;
}  

function isNumber(v) {
  return !Number.isNaN(v) && typeof v === 'number';
}
isNumber.expected = "number";

// Utility function that returns what a given contract expects.
function expect(f) {
  // For any contract function f, return the "expected" property
  // if it is specified.  (This allows developers to specify what
  // the expected property should be in a more readable form.)
  if (f.expected) {
    return f.expected;
  }
  // If the function name is available, use that.
  if (f.name) {
    return f.name;
  }
  // In case an anonymous contract is specified.
  return "ANONYMOUS CONTRACT";
}

var res = contract(
  [isNumber, isNumber],
  isNumber,
  function mult (x, y) {
    return x+y;
  })(3,"four");
console.log(res);