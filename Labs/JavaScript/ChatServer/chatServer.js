var net = require('net');
var eol = require('os').EOL;

var srvr = net.createServer();
var clientList = [];

srvr.on('connection', function(client) {
  client.name = client.remoteAddress + ':' + client.remotePort;
  client.write('Welcome, ' + client.name + eol);
  clientList.push(client);

  client.on('data', function(data) {
    
    var myData = data.toString('utf8').trim();
    console.log(myData);
    if(myData.length > 5 && myData.substring(0, 5) === "\\list"){
      var strclientlist = "";
      for (var i in clientList) {
        if (client !== clientList[i]) {
          strclientlist = clientList[i].name + eol;
        }
      }
      console.log(" List Sent");
      client.write(strclientlist);
    }
    else{
      if(myData.length > 7 && myData.substring(0, 7) === "\\rename"){
        for (var i in clientList) {
          if (client === clientList[i]) {
            var dataName = myData.split(" ");
            client.name = dataName[1];
          }
        }  
        console.log(" Name renamed");
      }
      else{
        if(myData.length > 8 && myData.substring(0, 8) === "\\private"){
          var dataName = myData.split(" ");
          for (var i in clientList) {
            console.log(" " + dataName[1].toString());
            if (dataName[1] === clientList[i].name) {
              dataName.shift();
              dataName.shift();
              var message = dataName.toString().split(",").join(" ");
              clientList[i].write(client.name.trim() + ": " + message + eol);
            }
          }  
          console.log(" Privately messaged");
        }
        else{
          broadcast(myData, client);
        }
      }
    }
  });
});

function broadcast(data, client) {
  for (var i in clientList) {
    if (client !== clientList[i]) {
      clientList[i].write(client.name.trim() + ": " + data + eol);
    }
  }
}

srvr.listen(9000);

