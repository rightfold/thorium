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

clause
    = fromKeyword name:identifier as:(asKeyword as:identifier { return as; })?
        { return new S.From(name, as === null ? name : as); }
    / distinctKeyword value:expression? within:(withinKeyword w:( n:expression elementsKeyword { return ['e', n]; }
                                                                / periodKeyword n:expression { return ['p', n]; }
                                                                ) { return w; })?
        {
            var withinNode;
            if (within === null) {
                withinNode = S.Infinity.value;
            } else if (within[0] === 'e') {
                withinNode = new S.Elements(within[1]);
            } else if (within[0] === 'p') {
                withinNode = new S.Period(within[1]);
            }
            return new S.Distinct(value === null ? Data_Maybe.Nothing.value : new Data_Maybe.Just(value), withinNode);
        }
    / whereKeyword condition:expression
        { return new S.Where(condition); }
    / selectKeyword value:expression? intoKeyword inputKeyword streamKeyword name:identifier
        { return new S.SelectIntoInputStream(value === null ? Data_Maybe.Nothing.value : new Data_Maybe.Just(value), name) }
    / selectKeyword value:expression? intoKeyword outputKeyword streamKeyword name:identifier
        { return new S.SelectIntoOutputStream(value === null ? Data_Maybe.Nothing.value : new Data_Maybe.Just(value), name) }

type
    = singleKeyword precisionKeyword
        { return S.SinglePrecision.value; }
    / doubleKeyword precisionKeyword
        { return S.DoublePrecision.value; }

identifier = _ !reserved name:$([a-zA-Z_][a-zA-Z0-9_]*) _ { return name.toLowerCase(); }

reserved
    = "AS"i
    / "CREATE"i
    / "DISTINCT"i
    / "DOUBLE"i
    / "ELEMENTS"i
    / "FROM"i
    / "INPUT"i
    / "INTO"i
    / "OUTPUT"i
    / "PERIOD"i
    / "PRECISION"i
    / "REACTOR"i
    / "SELECT"i
    / "SINGLE"i
    / "STREAM"i
    / "WHERE"i
    / "WITHIN"i

asKeyword        = _ "AS"i _
createKeyword    = _ "CREATE"i _
distinctKeyword  = _ "DISTINCT"i _
doubleKeyword    = _ "DOUBLE"i _
elementsKeyword  = _ "ELEMENTS"i _
fromKeyword      = _ "FROM"i _
inputKeyword     = _ "INPUT"i _
intoKeyword      = _ "INTO"i _
outputKeyword    = _ "OUTPUT"i _
periodKeyword    = _ "PERIOD"i _
precisionKeyword = _ "PRECISION"i _
reactorKeyword   = _ "REACTOR"i _
selectKeyword    = _ "SELECT"i _
singleKeyword    = _ "SINGLE"i _
streamKeyword    = _ "STREAM"i _
whereKeyword     = _ "WHERE"i _
withinKeyword    = _ "WITHIN"i _

semicolon = _ ";" _

_ = [ \t\r\n]*
