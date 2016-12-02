{
var S = require('../Thorium.Syntax');
var Data_List = require('../Data.List');
var Data_Maybe = require('../Data.Maybe');
var Data_Foldable = require('../Data.Foldable');
}

statements = statement*

statement
    = createInputStream
    / createOutputStream
    / createReactor

createInputStream
    = createKeyword inputKeyword streamKeyword name:identifier type:type semicolon
        { return new S.CreateInputStream(name, type); }

createOutputStream
    = createKeyword outputKeyword streamKeyword name:identifier type:type semicolon
        { return new S.CreateOutputStream(name, type); }

createReactor
    = createKeyword reactorKeyword name:identifier asKeyword clauses:clause* semicolon
        { return new S.CreateReactor(name, Data_List.fromFoldable(Data_Foldable.foldableArray)(clauses)); }

expression
    = name:identifier
        { return new S.Variable(name); }
    / accumulatorKeyword
        { return S.Accumulator.value; }

clause
    = fromKeyword name:identifier as:(asKeyword as:identifier { return as; })?
        { return new S.From(name, as === null ? name : as); }
    / distinctKeyword value:expression !withinKeyword
        { return new S.Distinct(value, S.Infinity.value); }
    / distinctKeyword value:expression withinKeyword n:expression elementsKeyword
        { return new S.Distinct(value, new S.Elements(n)); }
    / distinctKeyword value:expression withinKeyword periodKeyword n:expression
        { return new S.Distinct(value, new S.Period(n)); }
    / whereKeyword condition:expression
        { return new S.Where(condition); }
    / selectKeyword value:expression intoKeyword inputKeyword streamKeyword name:identifier
        { return new S.SelectIntoInputStream(value, name) }
    / selectKeyword value:expression intoKeyword outputKeyword streamKeyword name:identifier
        { return new S.SelectIntoOutputStream(value, name) }
    / scanKeyword initialKeyword initial:expression subsequentKeyword subsequent:expression intoKeyword inputKeyword streamKeyword name:identifier
        { return new S.ScanIntoInputStream(initial, subsequent, name) }
    / scanKeyword initialKeyword initial:expression subsequentKeyword subsequent:expression intoKeyword outputKeyword streamKeyword name:identifier
        { return new S.ScanIntoOutputStream(initial, subsequent, name) }

type
    = singleKeyword precisionKeyword
        { return S.SinglePrecision.value; }
    / doubleKeyword precisionKeyword
        { return S.DoublePrecision.value; }

identifier = _ !reserved name:$([a-zA-Z_][a-zA-Z0-9_]*) _ { return name.toLowerCase(); }

reserved
    = "ACCUMULATOR"i
    / "AS"i
    / "CREATE"i
    / "DISTINCT"i
    / "DOUBLE"i
    / "ELEMENTS"i
    / "FROM"i
    / "INITIAL"i
    / "INPUT"i
    / "INTO"i
    / "OUTPUT"i
    / "PERIOD"i
    / "PRECISION"i
    / "REACTOR"i
    / "SCAN"i
    / "SELECT"i
    / "SINGLE"i
    / "STREAM"i
    / "SUBSEQUENT"i
    / "WHERE"i
    / "WITHIN"i

accumulatorKeyword = _ "ACCUMULATOR"i_
asKeyword          = _ "AS"i _
createKeyword      = _ "CREATE"i _
distinctKeyword    = _ "DISTINCT"i _
doubleKeyword      = _ "DOUBLE"i _
elementsKeyword    = _ "ELEMENTS"i _
fromKeyword        = _ "FROM"i _
initialKeyword     = _ "INITIAL"i _
inputKeyword       = _ "INPUT"i _
intoKeyword        = _ "INTO"i _
outputKeyword      = _ "OUTPUT"i _
periodKeyword      = _ "PERIOD"i _
precisionKeyword   = _ "PRECISION"i _
reactorKeyword     = _ "REACTOR"i _
scanKeyword        = _ "SCAN"i _
selectKeyword      = _ "SELECT"i _
singleKeyword      = _ "SINGLE"i _
streamKeyword      = _ "STREAM"i _
subsequentKeyword  = _ "SUBSEQUENT"i _
whereKeyword       = _ "WHERE"i _
withinKeyword      = _ "WITHIN"i _

semicolon = _ ";" _

_ = [ \t\r\n]*
