'use strict';

var Data_Either = require('../Data.Either');
var grammar = require('../Thorium.Parse.Grammar/foreign');

function parser(rule) {
    return function(text) {
        try {
            return new Data_Either.Right(grammar.parse(text, {startRule: rule}));
        } catch (e) {
            return new Data_Either.Left('' + e);
        }
    };
}

exports.parseStatements = parser('statements');
exports.parseStatement = parser('statement');
