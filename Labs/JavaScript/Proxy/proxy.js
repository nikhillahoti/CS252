var handler = {
    get: function(obj, prop){
        console.log("Property " + prop + " was retrieved.");
        return prop in obj ? obj[prop] : "Not found";
    },
    set: function(obj, prop, value){
        if(prop in obj) {
            console.log("Property " + prop + " was changed");
            obj[prop] = value;
        }
        else{
            console.log("Property " + prop + " created ");
            obj[prop] = value;
        }
        return true;
    }
}

var pxy = new Proxy({}, handler);

pxy.a = 5;
console.log(pxy.a);
