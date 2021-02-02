from antlr4.error.ErrorListener import ErrorListener


class ParserErrorListener(ErrorListener):
    def __init__(self):
        self.error = 0
        self.message = ""

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        self.error += 1
        self.message += "Unexpected " + msg + " at " + str(line) + ":" + str(column + 1) + "\n\n"