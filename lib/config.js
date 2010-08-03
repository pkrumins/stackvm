var fs = require('fs');

var config = JSON.parse(fs.readFileSync(__dirname + '/../config/server.json', 'ascii'));
module.exports = config;

