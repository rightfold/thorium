'use strict';

var Data_Maybe = require('../Data.Maybe');
var grammar = require('../Thorium.Grammar/foreign');

exports.parseStatement = function(text) {
    try {
        return new Data_Maybe.Just(grammar.parse(text));
    } catch (e) {
        return Data_Maybe.Nothing.value;
    }
};
