{
var S = require('../Thorium.Syntax');
}

statement
    = createInputStream
    / createOutputStream

createInputStream
    = createKeyword inputKeyword streamKeyword name:identifier type:type semicolon
        { return new S.CreateInputStream(name, type); }

createOutputStream
    = createKeyword outputKeyword streamKeyword name:identifier type:type semicolon
        { return new S.CreateOutputStream(name, type); }

type
    = singleKeyword precisionKeyword
        { return S.SinglePrecision.value; }
    / doubleKeyword precisionKeyword
        { return S.DoublePrecision.value; }

identifier = _ name:$([a-zA-Z_][a-zA-Z0-9_]*) _ { return name.toLowerCase(); }

createKeyword    = _ "CREATE"i _
doubleKeyword    = _ "DOUBLE"i _
inputKeyword     = _ "INPUT"i _
outputKeyword    = _ "OUTPUT"i _
precisionKeyword = _ "PRECISION"i _
singleKeyword    = _ "SINGLE"i _
streamKeyword    = _ "STREAM"i _

semicolon = _ ";" _

_ = [ \t\r\n]*
