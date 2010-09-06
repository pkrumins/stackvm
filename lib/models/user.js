var sys = require('sys');
var fs = require('fs');
var crypto = require('crypto');

module.exports = User;

function User () { }

User.hash = function (phrase) {
    return new crypto.Hash('sha512').update(phrase).digest('hex');
};

