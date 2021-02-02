# Generated from /Users/lorenzobacchiani/Desktop/session-subtyping-tool/SessionType.g4 by ANTLR 4.9
from antlr4 import *
from io import StringIO
from typing.io import TextIO
import sys



def serializedATN():
    with StringIO() as buf:
        buf.write("\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\21")
        buf.write("M\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7")
        buf.write("\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4\f\t\f\4\r\t\r\4\16")
        buf.write("\t\16\4\17\t\17\4\20\t\20\3\2\3\2\3\3\3\3\3\4\3\4\3\5")
        buf.write("\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13")
        buf.write("\3\13\3\13\3\f\3\f\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17")
        buf.write("\7\17B\n\17\f\17\16\17E\13\17\3\20\6\20H\n\20\r\20\16")
        buf.write("\20I\3\20\3\20\2\2\21\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21")
        buf.write("\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21\3\2\5\4\2C")
        buf.write("\\c|\5\2\62;C\\c|\5\2\13\f\17\17\"\"\2N\2\3\3\2\2\2\2")
        buf.write("\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3")
        buf.write("\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2")
        buf.write("\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2")
        buf.write("\2\2\37\3\2\2\2\3!\3\2\2\2\5#\3\2\2\2\7%\3\2\2\2\t\'\3")
        buf.write("\2\2\2\13)\3\2\2\2\r+\3\2\2\2\17-\3\2\2\2\21/\3\2\2\2")
        buf.write("\23\61\3\2\2\2\25\63\3\2\2\2\27\67\3\2\2\2\31;\3\2\2\2")
        buf.write("\33=\3\2\2\2\35?\3\2\2\2\37G\3\2\2\2!\"\7-\2\2\"\4\3\2")
        buf.write("\2\2#$\7}\2\2$\6\3\2\2\2%&\7\177\2\2&\b\3\2\2\2\'(\7]")
        buf.write("\2\2(\n\3\2\2\2)*\7_\2\2*\f\3\2\2\2+,\7=\2\2,\16\3\2\2")
        buf.write("\2-.\7.\2\2.\20\3\2\2\2/\60\7\60\2\2\60\22\3\2\2\2\61")
        buf.write("\62\7(\2\2\62\24\3\2\2\2\63\64\7t\2\2\64\65\7g\2\2\65")
        buf.write("\66\7e\2\2\66\26\3\2\2\2\678\7g\2\289\7p\2\29:\7f\2\2")
        buf.write(":\30\3\2\2\2;<\7#\2\2<\32\3\2\2\2=>\7A\2\2>\34\3\2\2\2")
        buf.write("?C\t\2\2\2@B\t\3\2\2A@\3\2\2\2BE\3\2\2\2CA\3\2\2\2CD\3")
        buf.write("\2\2\2D\36\3\2\2\2EC\3\2\2\2FH\t\4\2\2GF\3\2\2\2HI\3\2")
        buf.write("\2\2IG\3\2\2\2IJ\3\2\2\2JK\3\2\2\2KL\b\20\2\2L \3\2\2")
        buf.write("\2\5\2CI\3\2\3\2")
        return buf.getvalue()


class SessionTypeLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    PLUS = 1
    CLPAR = 2
    CRPAR = 3
    SLPAR = 4
    SRPAR = 5
    SEMIC = 6
    COMMA = 7
    DOT = 8
    AND = 9
    REC = 10
    END = 11
    OUT = 12
    IN = 13
    ID = 14
    WHITESP = 15

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "'+'", "'{'", "'}'", "'['", "']'", "';'", "','", "'.'", "'&'", 
            "'rec'", "'end'", "'!'", "'?'" ]

    symbolicNames = [ "<INVALID>",
            "PLUS", "CLPAR", "CRPAR", "SLPAR", "SRPAR", "SEMIC", "COMMA", 
            "DOT", "AND", "REC", "END", "OUT", "IN", "ID", "WHITESP" ]

    ruleNames = [ "PLUS", "CLPAR", "CRPAR", "SLPAR", "SRPAR", "SEMIC", "COMMA", 
                  "DOT", "AND", "REC", "END", "OUT", "IN", "ID", "WHITESP" ]

    grammarFileName = "SessionType.g4"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.9")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None


