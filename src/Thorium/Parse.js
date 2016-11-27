'use strict';

var Data_Maybe = require('../Data.Maybe');
var grammar = require('../Thorium.Parse.Grammar/foreign');

function parser(rule) {
    return function(text) {
        try {
            return new Data_Maybe.Just(grammar.parse(text, {startRule: rule}));
        } catch (e) {
            return Data_Maybe.Nothing.value;
        }
    };
}

exports.parseStatements = parser('statements');
exports.parseStatement = parser('statement');
