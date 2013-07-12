#!/usr/bin/env python
# simpatico.py
""" This is a complete rewrite of the old simpatico.
Hopefully it's good. """

import sys

import headers


DEBUG = False
STOP_ON_MISSED_WHITESPACE = DEBUG and False
STOP_ON_DUPLICATED_WHITESPACE_CHECK = DEBUG and True

INDENT_SIZE = 4
LINE_CONTINUATION_SIZE = 8
ALLOW_ZERO = True
(NO_NEWLINE, MAY_NEWLINE, MUST_NEWLINE) = range(3)
IS_TYPEDEF = True
MISSING_TYPE = False

def d(elements):
    if DEBUG:
        print "D: " + " ".join([str(x) for x in elements])

TYPE_SPECIFIERS = ['void', 'char', 'short', 'int', 'long', 'float', 'double',
                 'signed', 'unsigned', '_Bool', '_Imaginary', '_Complex']
DEFINED_TYPES = ['__UINTMAX_TYPE__', '__SIZE_TYPE__', '__CHAR16_Type__',
        '__WCHAR_TYPE__', '__WINT_TYPE__', '__CHAR32_TYPE__',
        '__INTMAX_TYPE__', '__PTRDIFF_TYPE__'] #default defines with gcc
STRUCT_UNION = ["struct", "union"]
STORAGE_CLASS = ["register", "static", "extern", "auto", "typedef"]
TYPE_QUALIFIERS = ["const", "restrict", "volatile"]

class Terminals(object):
    KW_AUTO = "auto"
    KW_BREAK = "break"
    KW_CASE = "case"
    KW_CHAR = "char"
    KW_CONST = "const"
    KW_CONTINUE = "continue"
    KW_DEFAULT = "default"
    KW_DO = "do"
    KW_DOUBLE = "double"
    KW_ELSE = "else"
    KW_ENUM = "enum"
    KW_EXTERN = "extern"
    KW_FLOAT = "float"
    KW_FOR = "for"
    KW_GOTO = "goto"
    KW_IF = "if"
    KW_INLINE = "inline"
    KW_INT = "int"
    KW_LONG = "long"
    KW_REGISTER = "register"
    KW_RESTRICT = "restrict"
    KW_RETURN = "return"
    KW_SHORT = "short"
    KW_SIGNED = "signed"
    KW_SIZEOF = "sizeof"
    KW_STATIC = "static"
    KW_STRUCT = "struct"
    KW_SWITCH = "switch"
    KW_TYPEDEF = "typedef"
    KW_UNION = "union"
    KW_UNSIGNED = "unsigned"
    KW_VOID = "void"
    KW_VOLATILE = "volatile"
    KW_WHILE = "while"
    KW_BOOL = "_Bool"
    KW_COMPLEX = "_Complex"
    KW_IMAGINARY = "_Imaginary"

BINARY_OPERATORS = ["+", "/", "%", ">>", "<<", "|", "^", "->", ".", "?", ":"]
UNARY_OPERATORS = ["--", "++", "!"]
LOGICAL_OPERATORS = ["&&", "||", "<", ">", "<=", ">=", "==", "!="]
ASSIGNMENTS = ["=", "%=", "+=", "-=", "*=", "/=", "|=", "&=", "<<=", ">>=",
        "^="]
ALL_OPS = BINARY_OPERATORS + UNARY_OPERATORS + ASSIGNMENTS + LOGICAL_OPERATORS
#by the time we use this one, there's no natural \t chars left
COMMENT = '\t'

class Type(object):
    """ Yes, this could be an Enum, but I'm being kind to older versions of
    Python """
    ANY = -1
    (   ERROR_TYPE, DEFINE, INCLUDE, COMMENT, NEWLINE, COMMA, LBRACE, RBRACE,
        LPAREN, RPAREN, MINUS, BINARY_OP, LOGICAL_OP, STAR,
        AMPERSAND, TYPE, CREMENT, IGNORE, EXTERN, BREAK, FOR, SWITCH, CASE,
        STRUCT, CONTINUE, TYPEDEF, RETURN, UNKNOWN, CONSTANT, WHILE, DO,
        SEMICOLON, COLON, TERNARY, ASSIGNMENT, IF, ELSE, LSQUARE, RSQUARE,
        LINE_CONT, DEFAULT, NOT, SIZEOF, PRECOMPILER, ATTRIBUTE, HASH
    ) = range(46)

class Word(object):
    """ Keeps track of contextual details about the word """
    def __init__(self):
        self.space = -1
        self.line_number = -1
        self.line = []
        self.start = -1
        self._type = Type.ERROR_TYPE
        self.whitespace_checked = 0
        self.inner_tokens = []
        self.inner_position = 0
        
    def get_type(self):
        if self.inner_tokens:
            return self.inner_tokens[self.inner_position].get_type()
        else:
            return self._type

    def get_string(self):
        return "".join(self.line)

    def get_position(self):
        return self.start

    def get_spacing_left(self):
        return self.space

    def append(self, char, space_left, line_number, char_location):
        if self.line_number == -1:
            self.line_number = line_number
            self.space = space_left
            self.start = char_location
        self.line.append(char)

    def empty(self):
        return len(self.line) == 0

    def finalise(self):
        """ here's where we work out what type of thing this word is """
        self.line = "".join(self.line)
        line = self.line
        #prepare thyself for many, many elifs
        if line.lower() == "define":
            self._type = Type.DEFINE
        elif line in ["ifdef", "ifndef", "endif", "undef"]:
            self._type = Type.PRECOMPILER
        elif line == "include":
            self._type = Type.INCLUDE
        elif line == "#":
            self._type = Type.HASH
        elif line == Terminals.KW_IF:
            self._type = Type.IF
        elif line == Terminals.KW_ELSE:
            self._type = Type.ELSE
        elif line == "\t":
            self._type = Type.COMMENT
        elif line == ";":
            self._type = Type.SEMICOLON
        elif line == "!":
            self._type = Type.NOT
        elif line in ASSIGNMENTS:
            self._type = Type.ASSIGNMENT
        elif line == "\n":
            self._type = Type.NEWLINE
        elif line == ",":
            self._type = Type.COMMA
        elif line == "{":
            self._type = Type.LBRACE
        elif line == "?":
            self._type = Type.TERNARY
        elif line == ":":
            self._type = Type.COLON
        elif line == "}":
            self._type = Type.RBRACE
        elif line == "(":
            self._type = Type.LPAREN
        elif line == ")":
            self._type = Type.RPAREN
        elif line == "-":
            self._type = Type.MINUS
        elif line in BINARY_OPERATORS + LOGICAL_OPERATORS:
            self._type = Type.BINARY_OP
        elif line == "*":
            self._type = Type.STAR
        elif line == "&":
            self._type = Type.AMPERSAND
        elif line in TYPE_SPECIFIERS + DEFINED_TYPES:
            self._type = Type.TYPE
        elif line in ["--", "++"]:
            self._type = Type.CREMENT
        elif line == Terminals.KW_EXTERN:
            self._type = Type.EXTERN
        elif line == Terminals.KW_BREAK:
            self._type = Type.BREAK
        elif line == Terminals.KW_FOR:
            self._type = Type.FOR
        elif line == Terminals.KW_DO:
            self._type = Type.DO
        elif line == Terminals.KW_WHILE:
            self._type = Type.WHILE
        elif line == Terminals.KW_SWITCH:
            self._type = Type.SWITCH
        elif line == Terminals.KW_CASE:
            self._type = Type.CASE
        elif line == Terminals.KW_DEFAULT:
            self._type = Type.DEFAULT
        elif line in STRUCT_UNION:
            self._type = Type.STRUCT
        elif line == Terminals.KW_CONTINUE:
            self._type = Type.BREAK #since they're equivalent for us
        elif line == "typedef":
            self._type = Type.TYPEDEF
        elif line in TYPE_QUALIFIERS + STORAGE_CLASS:
            self._type = Type.IGNORE        
        elif line == Terminals.KW_RETURN:
            self._type = Type.RETURN
        elif line[0] == '"' or line[0] == "'" or line.isdigit():
            self._type = Type.CONSTANT
        elif line == "[":
            self._type = Type.LSQUARE
        elif line == "]":
            self._type = Type.RSQUARE
        elif line == "\\":
            self._type = Type.LINE_CONT
        elif line == Terminals.KW_SIZEOF:
            self._type = Type.SIZEOF
        elif line == "__attribute__":
            self._type = Type.ATTRIBUTE
        else:
            #d(["finalise() could not match type for", self])
            self._type = Type.UNKNOWN #variables and externally defined types

    def __str__(self):
        return self.__repr__()

    def __repr__(self):
        rep = "%d:%d  i:%d '\033[1m%s\033[0m'" % (self.line_number,
                self.start, self.space, "".join(self.line))
        if len(self.inner_tokens) != 0:
            rep += "-> defined as \033[1m" + \
                    "".join([x.line for x in self.inner_tokens]) + "\033[0m" \
                    + " current:" + str(self.inner_tokens[self.inner_position])
        return rep

class Tokeniser(object):
    """ The thing that turns a gigantic file of hopefully not terrible code
    into tokens that we can then deal with """
    DUPLICATE_OPS = ['|', '&', '<', '>', '+', '-', '=']
    def __init__(self, filename):
        self.tokens = []
        self.line_number = 1
        self.line_start = 0
        self.in_operator = False
        self.in_string = False
        self.in_char = False
        self.multi_char_op = False
        self.multiline_comment = 0
        self.in_singleline_comment = False
        self.deal_breakers = [' ', '.', '-', '+', '/', '*', '>', '<', '&',
                '|', '!', '~', '%', '^', '(', ')', '{', '}', ';', ',', ':',
                '?', '[', ']', '#', '=']
        self.current_word = Word()
        self.space_left = 0
        self.current_word_start = 1
        #well that was fun, now we should do some real work
        f = open(filename, "r")
        allllll_of_it = f.read().expandtabs(8).replace('\r', ' ')
        f.close()
        self.tokenise(allllll_of_it)

    def end_word(self):
        if self.current_word.empty():
            return
        self.current_word.finalise()
        self.tokens.append(self.current_word)
        self.current_word = Word()
        self.in_operator = False
        self.in_string = False
        self.in_char = False
        self.multi_char_op = False

    def add_to_word(self, char, n):
        self.current_word.append(char, self.space_left, self.line_number, n)
        self.space_left = 0

    def tokenise(self, megastring):
        """ Why yes, this is a big function. Be glad it's not the usual parser
        switch statement that's 1000 lines long. """
        for n, c in enumerate(megastring):
            #step 0: if we were waiting for the second char in a "==" or
            # similar, grab it and move on already
            if self.multi_char_op:
                self.add_to_word(c, n - self.line_start)
                #catch dem silly >>= and <<= ops
                if self.current_word.get_string() + "=" in ASSIGNMENTS:
                    continue
                self.end_word()
                continue
            #step 1: deal with the case of being in a //comment
            if self.in_singleline_comment:
                if c == '\n':
                    self.in_singleline_comment = False
                    self.add_to_word(COMMENT, n - self.line_start)
                    self.end_word()
                    #then add the newline
                    self.add_to_word(c, n - self.line_start)
                    self.end_word()
                    self.line_number += 1
                    self.line_start = n + 1

            #step 2: continue on while inside a multiline comment
            elif self.multiline_comment:
                #if we've reached the end of the comment
                if self.multiline_comment == n:
                    self.multiline_comment = 0
                    self.add_to_word(COMMENT, n - self.line_start)
                    self.end_word()
                #but update line numbers if it's a newline
                if c == '\n':
                    self.line_number += 1
                    self.line_start = n + 1
            #step 3: deal with newlines, ends the current word
            elif c == '\n':
                #out with the old
                self.end_word()
                #in with the new..
                self.line_number += 1
                self.line_start = n + 1
                #...line AHYUK, AHYUK
                self.add_to_word(c, n - self.line_start)
                self.end_word()

            #don't want to get caught interpreting chars in strings as real
            elif self.in_string:
                self.add_to_word(c, n - self.line_start)
                #string ending
                if c == '"':
                    #but not if it's escaped
                    if megastring[n-1] == '\\':
                        #make sure the slash wasn't itself escaped
                        if megastring[n-2] == '\\':
                            self.end_word()
                    else:
                        #eeennnd it, and escape this if tree
                        self.end_word()
            #that was fuuun, but it repeats with chars
            elif self.in_char:
                self.add_to_word(c, n - self.line_start)
                #first: is it a '; second: are sneaky people involved
                if c == "'" and megastring[n-1] != '\\':
                    self.end_word()
            #catch dem spaces
            elif c == ' ':
                self.end_word()
                self.space_left += 1

            #catch the start of a string
            elif c == '"':
                self.in_string = not self.in_string
                self.add_to_word(c, n - self.line_start)
            #or, for that matter, the start of a char
            elif c == "'":
                self.in_char = not self.in_char
                self.add_to_word(c, n - self.line_start)
            #now we just have to catch the possible word seperators
            elif c in self.deal_breakers:
                if c == "}":
                    self.end_word()
                    self.add_to_word(c, n - self.line_start)
                    self.end_word()
                elif c == "/" and megastring[n+1] == "*":
                    self.multiline_comment = megastring.find("*/", n) + 1
                elif c == "/" and megastring[n+1] == "/":
                    self.in_singleline_comment = True
                elif c + megastring[n+1] in ALL_OPS:
                    self.end_word()
                    self.multi_char_op = True
                    self.add_to_word(c, n - self.line_start)
                #ennnnd of ze word
                else:
                    self.end_word()
                    #only single character constructs remain, so add them and
                    #include "bad_jokes.h"
                    #... END THEM
                    self.add_to_word(c, n - self.line_start)
                    self.end_word()
                    
            else:
                self.add_to_word(c, n - self.line_start)

    def get_tokens(self):
        return self.tokens

class Errors(object):
    """Everyone's favourite"""
    (IF, ELSE, ELSEIF, RUNON, FUNCTION, GLOBALS, VARIABLE, TYPE,
            DEFINE, MISSING, CLOSING, FILES) = range(12)
    def __init__(self):
        self.naming_d = {}
        self.whitespace_d = {}
        self.comments_d = {}
        self.braces_d = {}
        self.line_length_d = {}
        self.overall_d = {}
        self.total = 0

    def naming(self, token, name_type):
        self.total += 1
        msg = "WHOOPS"
        line_no = 1
        name = ""
        if name_type == Errors.FILES:
            msg = " misnamed, files should be namedLikeThis.c"
            name = token
        else:
            if name_type == Errors.TYPE:
                msg = " misnamed, types should be NamedLikeThis"
            elif name_type == Errors.FUNCTION:
                msg = " misnamed, functions should be named_like_this"
            elif name_type == Errors.DEFINE:
                msg = " misnamed, #defines should be NAMED_LIKE_THIS"
            elif name_type == Errors.VARIABLE:
                msg = " misnamed, variables should be namedLikeThis"
            name = token.get_string()
            line_no = token.line_number
        self.naming_d[line_no] = "[NAMING] " + name + msg

    def whitespace(self, token, expected):
        self.total += 1
        assert token.get_spacing_left() != expected
        if self.whitespace_d.get(token.line_number):
            return
        self.whitespace_d[token.line_number] = "".join([
                "[WHITESPACE] '", token.line, "' at ",
                "position %d: expected %d whitespace, found %d " % \
                (token.get_position(), expected, token.get_spacing_left())])
        
    def line_length(self, line_number, length):
        self.total += 1
        self.line_length_d[line_number] = "[LINE-LENGTH] line is " + \
                "%d chars long" % length

    def func_length(self, line_number, length):
        self.total += 1
        if not self.overall_d.get(line_number):
            self.overall_d[line_number] = []
        self.overall_d[line_number].append("[OVERALL] Function length of" \
                + " %d is over the maximum of 50" % length)

    def overall(self, line_number, message):
        self.total += 1
        if not self.overall_d.get(line_number):
            self.overall_d[line_number] = []
        self.overall_d[line_number].append("[OVERALL] %s" % message)        

    def braces(self, token, error_type):
        self.total += 1
        msg = "WHOOPS"
        if error_type == Errors.IF:
            msg = ", if braces should look like: if (cond) {"
        elif error_type == Errors.ELSE:
            msg = ", else braces should look like: } else {"
        elif error_type == Errors.ELSEIF:
            msg = ", else if braces should look like: } else if (cond) {"
        elif error_type == Errors.RUNON:
            msg = ", an opening brace should be the last character on the line"
        elif error_type == Errors.MISSING:
            msg = ", braces are required, even for single line blocks"
        self.braces_d[token.line_number] = \
                "[BRACES] at position %d%s" % (token.get_position(), msg)

    def comments(self, line_number, error_type):
        self.total += 1
        msg = "WHOOPS"
        if error_type == Errors.FUNCTION:
            msg = "Functions should be preceeded by explanatory comments"
        elif error_type == Errors.GLOBALS:
            msg = "Global variables should be commented"
        self.comments[line_number] = msg

    def get(self, line_number):
        result = []
        for error_type in [self.braces_d, self.whitespace_d,
                self.line_length_d, self.naming_d, self.overall,
                self.comments_d]:
            result.extend(error_type.get(line_number, []))
        return result

    def print_lines(self):
        for error_type in [self.braces_d, self.whitespace_d,
                self.line_length_d, self.naming_d, self.comments_d]:
            for key in sorted(error_type.keys()):
                print "line", key, ":", error_type[key]
        for key in sorted(self.overall_d.keys()):
            for line in self.overall_d[key]:
                print "line", key, ":", line

    def __repr__(self):
        if not self.total:
            return "No errors found"
        counts = [len(error_type.keys()) for error_type in [
                self.braces_d, self.whitespace_d, self.comments_d,
                self.naming_d, self.overall_d, self.line_length_d]]
        #cap the violations to 5 per category
        for i in range(len(counts)):
            if counts[i] > 5:
                counts[i] = 5
        return " ".join(["%d total errors found, capped at " % self.total,
                "B:%d W:%d C:%d N:%d O:%d L:%d" % tuple(counts)])

class Styler(object):
    MAX = False
    """ Where style violations are born """
    def __init__(self, filename, verbose = True, output_file = False):
        #some setup
        self.errors = Errors()
        self.found_types = []
        self.included_files = []
        self.filename = filename
        self.path = ""
        if "/" in filename:
            self.path = filename[:filename.rfind("/") + 1]
        elif "\\" in filename:
            self.path = filename[:filename.rfind("\\") + 1]
        #quick run for line lengths
        line_number = 0
        self.infile = open(self.filename, "r")
        for line in self.infile:
            line_number += 1
            if len(line) > 80: #79 + \n
                self.errors.line_length(line_number, len(line) - 1)
        self.infile.close()
        self.position = 0
        self.depth = 0
        self.comments = {}
        self.line_continuation = False
        #then the guts of it all
        tokeniser = Tokeniser(filename)
        self.tokens = tokeniser.get_tokens()
        self.current_token = self.tokens[self.position]
        self.last_real_token = None
        try:
            while self.current_type() in [Type.NEWLINE, Type.COMMENT]:
                d(["pre-process: skipping newline/comment", self.current_token])
                if self.current_type() == Type.COMMENT:
                    self.check_whitespace()
                    self.comments[self.current_token.line_number] = True
                self.position += 1
                self.current_token = self.tokens[self.position]
            self.process_globals()
        except IndexError:
            #that'd be us finished
            pass
        #before we're done with the file, check the filename style
        if "/" in filename:
            filename = filename[filename.rfind("/") + 1:]
        elif "\\" in filename:
            filename = filename[filename.rfind("\\") + 2:]
        self.check_naming(filename, Errors.FILES)
        
        #make sure no changes skip whitespace
        if DEBUG:
            for token in self.tokens:
                if token.get_type() not in [Type.NEWLINE, Type.LINE_CONT,
                        Type.COMMENT]:
                    if token.whitespace_checked == 0:
                        print "whitespace check missed:", token
                    elif token.whitespace_checked > 1:
                        print "whitespace check duplicated:", token
                    
        if output_file:
            self.write_output_file(filename)

        if verbose:
            self.errors.print_lines()

    def current_type(self):
        return self.current_token.get_type()    

    def previous_token(self):
        if self.current_token.inner_tokens:
            return self.current_token.inner_token[ \
                    self.current_token.inner_position - 1]
        return self.tokens[self.position - 1]

    def peek(self):
        i = self.position + 1
        while self.tokens[i].get_type() in [Type.COMMENT, Type.NEWLINE]:
            i += 1
        return self.tokens[i]

    def match(self, req_type = Type.ANY, post_newline = NO_NEWLINE,
            pre_newline = NO_NEWLINE):
        #store interesting parts
        old = self.current_token
        self.last_real_token = self.current_token
        if STOP_ON_MISSED_WHITESPACE and old.get_type() not in [Type.NEWLINE,
                Type.COMMENT]:
            assert old.whitespace_checked != 0
        d(["matching", old])
        if old.inner_tokens:
            if req_type != Type.ANY and old.get_type() != req_type:
                print "match fail:", old.get_type(), req_type
                assert old.get_type() == req_type
            if old.inner_position < len(old.inner_tokens) - 1:
                old.inner_position += 1
                old = old.inner_tokens[old.inner_position - 1]
            else:
                self.position += 1
                self.current_token = self.tokens[self.position]
                while self.current_token.get_type() in [Type.NEWLINE,
                        Type.COMMENT, Type.LINE_CONT]:
                    self.position += 1
                    self.current_token = self.tokens[self.position]
            return

        # ensure we're matching what's expected
        if req_type != Type.ANY and old.get_type() != req_type:
            print "match fail:", self.current_token, old.get_type(), req_type
            assert old.get_type() == req_type
        # check pre-token newlines if {}
        elif old.get_type() in [Type.LBRACE, Type.RBRACE]:
            # previous was a newline but shouldn't have been
            if self.previous_token().get_type() in [Type.NEWLINE,
                    Type.COMMENT]:
                if pre_newline == NO_NEWLINE:
                    err = Errors.IF
                    print self.last_real_token, self.peek()
                    if Type.ELSE in [self.last_real_token.get_type(),
                            self.peek().get_type()]:
                        err = Errors.ELSE
                    self.errors.braces(self.current_token, err)
            else: #previous wasn't a newline but should've been
                if pre_newline == MUST_NEWLINE:
                    self.errors.braces(self.current_token, Errors.RUNON)
        #update
        self.position += 1
        self.current_token = self.tokens[self.position] #deliberately unsafe
        
        # clear the trash
        while self.current_type() in [Type.COMMENT, Type.NEWLINE,
                Type.LINE_CONT]:
            if self.current_type() == Type.COMMENT:
                self.comments[self.current_token.line_number] = True
            self.position += 1
            self.current_token = self.tokens[self.position]
       
        # check for extra post-token newlines
        if post_newline == NO_NEWLINE and self.last_real_token.line_number \
                != self.current_token.line_number:
            if old.get_type() == Type.RBRACE:
                self.errors.braces(old, Errors.ELSE)
            if old.get_type() not in [Type.SEMICOLON, Type.LBRACE]:
                self.line_continuation = True
        # check for missing post-token newlines
        if post_newline == MUST_NEWLINE and self.last_real_token.line_number \
                == self.current_token.line_number:
            if old.get_type() not in [Type.LBRACE, Type.RBRACE]:
                pass #TODO for now, might have to add semicolon checks
            elif self.tokens[self.position-2].get_type() == Type.ELSE:
                self.errors.braces(self.last_real_token, Errors.ELSE)
            else:
                self.errors.braces(self.last_real_token, Errors.RUNON)

    def check_whitespace(self, expected = -1, one_or_zero = not ALLOW_ZERO):
        token = self.current_token
        #skip checks for tokens that are precompiler definitions 
        #(provided they aren't the first)
        if token.inner_tokens and token.inner_position != 0:
            return
        if expected == -1:
            expected = self.depth * INDENT_SIZE
        if self.line_continuation and token.get_type() != Type.RBRACE:
            expected = self.depth * INDENT_SIZE + LINE_CONTINUATION_SIZE
        if token.whitespace_checked and DEBUG:
            print "whitespace check duplicated:", token
            token.whitespace_checked += 1
            assert not STOP_ON_DUPLICATED_WHITESPACE_CHECK
            return
        token.whitespace_checked += 1
        if one_or_zero:
            if expected <= 1 and token.get_spacing_left() > 1:
                d(["whitespace \033[1merror\033[0m:", "expected", "1 or 0",
                        "with token", token, "but had",
                        token.get_spacing_left()])
                self.errors.whitespace(token, expected)
        elif token.get_spacing_left() != expected:
            d(["whitespace \033[1merror\033[0m:", "expected", expected,
                    "with token", token, "but had", token.get_spacing_left()])
            self.errors.whitespace(token, expected)
        if self.line_continuation:
            self.line_continuation = False

    def has_matching_else(self):
        d(["has matching_else: starting at ", self.current_token])
        i = self.position
        depth = 0
        while i < (self.tokens) and depth >= 0:
            i += 1
            if self.tokens[i].get_type() == Type.RBRACE:
                depth -= 1
                i += 1
                if depth == 0:
                    while self.tokens[i] in [Type.COMMENT, Type.NEWLINE]:
                        i += 1
                    d(["has matching_else: ending at ", self.tokens[i]])
                    return self.tokens[i].get_type() == Type.ELSE
            elif self.tokens[i].get_type() == Type.LBRACE:
                depth += 1
        d(["has matching_else: ending at ", self.tokens[i]])
        return False

    def write_output_file(self, filename):
        """go over the file and insert messages when appropriate"""
        line_number = 1
        outf = open(filename+".style", "w")
        infile = open(filename, "r")
        for line in infile:
            outf.writelines(self.errors.get(line_number) + [line])
        infile.close()
        outf.close()

    def consume_line(self):
        while self.current_type() != Type.NEWLINE:
            d(["consume_line(): consuming:", self.current_token, 
                    self.current_type() == Type.NEWLINE])
            self.position += 1
            self.current_token = self.tokens[self.position]
        d(["consume_line(): consuming:", self.current_token])
        self.position += 1
        self.current_token = self.tokens[self.position]

    def match_type(self):
        d(["match_type(): entered", self.current_token])
        if self.current_type() == Type.UNKNOWN:
            print "iunno about this type you're giving me, it's unknown"
            assert False
        assert self.current_type() in [Type.TYPE, Type.IGNORE, Type.STRUCT,
                Type.LPAREN]
        if self.current_type() in [Type.TYPE, Type.IGNORE]:
            self.match()
            while self.current_type() in [Type.TYPE, Type.IGNORE]:           
                self.check_whitespace(1)
                self.match()
        elif self.current_type() == Type.STRUCT:
            #match the struct keyword first
            self.match(Type.STRUCT)
            self.check_whitespace(1)
            #then let it go on to the name of the struct type
            self.match(Type.UNKNOWN)
        # check if function pointer (preceeded by type, that's why not elif)
        if self.current_type() == Type.LPAREN:
            d(["this type is a function pointer"])
            if self.last_real_token.get_type() == Type.TYPE:
                self.check_whitespace(1)
            self.match(Type.LPAREN) #(
            self.check_whitespace(0)
            if self.current_type() == Type.STAR:
                self.match(Type.STAR) #(*
                self.check_whitespace(0)
            #allow for non-declaration
            if self.current_type() == Type.UNKNOWN:
                name = self.current_token
                d(["found identifier", name])
                self.match(Type.UNKNOWN) #(*id
                self.check_whitespace(0)
                #this could very well be an array type, so check for indicies
                while self.current_type() == Type.LSQUARE:
                    self.match(Type.LSQUARE)
                    self.check_whitespace(0)
                    self.check_expression() #static size
                    self.check_whitespace(0)
                    self.match(Type.RSQUARE)
                #is this a function returning a func pointer?
                if self.current_type() == Type.LPAREN:
                    self.check_naming(name, Errors.FUNCTION)
                else:
                    self.check_naming(name, Errors.VARIABLE)
            #now, is this a function itself
            if self.current_type() == Type.LPAREN:
                self.match(Type.LPAREN) #(id(
                if self.current_type() != Type.RPAREN:
                    self.check_whitespace(0)
                    self.match_type() #(id(types
                    if self.current_type() == Type.UNKNOWN:
                        self.check_whitespace(1, ALLOW_ZERO)
                        self.match(Type.UNKNOWN)
                    while self.current_type() == Type.COMMA:
                        self.check_whitespace(0)
                        self.match(Type.COMMA)
                        self.check_whitespace(1)
                        self.match_type() #(id(types,types
                        if self.current_type() == Type.UNKNOWN:
                            self.check_whitespace(1, ALLOW_ZERO)
                            self.match(Type.UNKNOWN)
                self.check_whitespace(0)
                self.match(Type.RPAREN) #(id(types,types)
            elif self.current_type() == Type.RPAREN: #(id)
                self.check_whitespace(0)
                self.match(Type.RPAREN)
                self.check_whitespace(0)
                self.match(Type.LPAREN) #type (id)(
                if self.current_type() != Type.RPAREN:
                    self.check_whitespace(0)
                    self.match_type()
                    if self.current_type() == Type.UNKNOWN:
                        self.check_whitespace(1, ALLOW_ZERO)
                        self.match(Type.UNKNOWN)
                    while self.current_type() == Type.COMMA:
                        self.check_whitespace(0)
                        self.match(Type.COMMA)
                        self.check_whitespace(1)
                        self.match_type()
                        if self.current_type() == Type.UNKNOWN:
                            self.check_whitespace(1, ALLOW_ZERO)
                            self.match(Type.UNKNOWN)
            self.check_whitespace(0)
            self.match(Type.RPAREN) #(id(types,types))
        # strip the pointers if they're there
        if self.current_type() == Type.STAR:
            self.check_whitespace(1, ALLOW_ZERO)
            self.match(Type.STAR)
            while self.current_type() == Type.STAR:
                self.check_whitespace(0)
                self.match(Type.STAR)
            while self.current_type() == Type.LSQUARE:
                self.check_whitespace(0)
                self.match(Type.LSQUARE)
                self.check_expression()
                self.check_whitespace(0)
                self.match(Type.RSQUARE)
        d(["match_type(): exited", self.current_token])

    def process_globals(self):
        """ There's an assumption here that the code compiles to start with.
        Only checking the types of tokens that can start lines in this
        context (compiler directives, prototypes, declarations, definitions).
        """
        while True:
            d(["global space: ", self.current_token])
            self.line_continuation = False
            self.check_whitespace(0)
            #check for compiler directives that aren't #define
            if self.current_type() == Type.HASH:
                self.check_precompile()
            #declaration
            elif self.current_type() in [Type.TYPE, Type.LPAREN, Type.IGNORE]:
                self.check_declaration()
            #declaration missing a leading type
            elif self.current_type() == Type.UNKNOWN:
                self.check_declaration(MISSING_TYPE)
            elif self.current_type() == Type.EXTERN:
                self.match(Type.EXTERN)
                self.check_whitespace(1)
                self.check_declaration(self.current_type() != Type.UNKNOWN, \
                        Type.EXTERN)
            #struct definition/declaration
            elif self.current_type() == Type.STRUCT:
                self.match()
                self.check_struct()
                self.check_whitespace(0)
                self.match(Type.SEMICOLON, MUST_NEWLINE)
            #typedef
            elif self.current_type() == Type.TYPEDEF:
                self.match()
                self.check_typedef()
            #ruh roh
            else:
                print "found an awkward type in global space:", \
                        self.current_token
                assert False #crash this thing so we can trace it

    def check_precompile(self):
        self.match(Type.HASH)
        self.check_whitespace(0)
        if self.current_type() == Type.INCLUDE:
            self.match(Type.INCLUDE)
            include_std = False
            include_name = []
            #include "stuff.h"
            if self.current_type() == Type.CONSTANT:
                self.check_whitespace(1)
                include_name.append(self.current_token.line)
                self.match(Type.CONSTANT, MUST_NEWLINE)
            #include <std_stuff.h>
            else:
                include_std = True
                self.check_whitespace(1)
                self.match() #<
                while self.current_token.line != ">":
                    include_name.append(self.current_token.line)
                    self.check_whitespace(0)
                    self.match()
                self.check_whitespace(0)
                self.match(Type.ANY, MUST_NEWLINE) #>
            include_name = "".join(include_name)
            new_types = []
            if include_std:
                new_types = headers.standard_header_types.get(include_name, -1)
                if new_types == -1:
                    print "".join([
                        "\nThe header <", include_name,
                        "> was not found in the preprocessed list.\n"
                        "Please report this to the maintainer so it ",
                        "can be fixed.\n",
                        "Since the parsing will likely break terribly due to ",
                        "unknown types\n(C is not a context free language), ",
                        "simpatico will end parsing now."])
                    exit(2)
            #custom header
            else:
                #strip the " from beginning and end, prepend with path
                fun_with_recursion = Styler(self.path + include_name[1:-1])
                new_types = fun_with_recursion.found_types
            d(["including", len(new_types), "types from", include_name])
            #add the types
            self.update_types(new_types)
            self.included_files.append(include_name)
        #define
        elif self.current_type() == Type.DEFINE:
            self.match(Type.DEFINE, MAY_NEWLINE)
            #was it just an include guard?
            if self.last_real_token.get_type() != Type.NEWLINE:
                self.check_define()
#TODO undefine
        elif self.current_type() == Type.PRECOMPILER:
            self.match(Type.PRECOMPILER)
            self.check_whitespace(1)
            self.consume_line()

    def update_types(self, new_types):
        self.found_types.extend(new_types)
        count = 0
        for token in self.tokens:
            #we use the actual token type here and not defined ones
            #that's because #defined overrides it and the inner ones will
            #update anyway
            if token._type == Type.UNKNOWN and token.line in new_types:
                token._type = Type.TYPE
                count += 1
        d(["updated token type for", count, "tokens"])

    def check_naming(self, token, name_type = Errors.VARIABLE):
        if name_type == Errors.FILES:
            name = token
            if "_" in name:
                self.errors.naming(token, name_type)
            return
        name = token.line
        if name_type == Errors.VARIABLE:
            if "_" in name or len(name) == 1 and name.isupper():
                self.errors.naming(token, name_type)
        elif name_type == Errors.FUNCTION:
            #if any uppercase char in the name, it's bad
            for c in name:
                if c.isupper():
                    self.errors.naming(token, name_type)
                    break
        elif name_type == Errors.TYPE:
            if "_" in name or not name[0].isupper():
                self.errors.naming(token, name_type)
        elif name_type == Errors.DEFINE:
            if not name.isupper():
                self.errors.naming(token, name_type)
        else:
            print "check_naming(): unknown naming type given: token=", token

    def check_struct(self, isTypedef = False):
        d(["check_struct() entered"])
        #skip the type name
        if self.current_type() == Type.UNKNOWN:
            self.check_whitespace(1)
            self.match(Type.UNKNOWN) # identifier
        #ensure it's the block, then start it
        assert self.current_type() == Type.LBRACE
        self.check_whitespace(1)
        self.match(Type.LBRACE, MAY_NEWLINE, MAY_NEWLINE)
        self.check_block()
        self.check_whitespace()
        self.match(Type.RBRACE, MAY_NEWLINE, MAY_NEWLINE)
        if self.current_type() == Type.ATTRIBUTE:
            #ruh roh
            #TODO better manual checking required
            print "manual checking of __attribute__ tag required on line", \
                    self.current_token.line_number
            self.check_whitespace(1)
            self.match(Type.ATTRIBUTE)
            self.check_whitespace(1, ALLOW_ZERO)
            self.match(Type.LPAREN)
            depth = 1
            while depth != 0:
                if self.current_type() == Type.LPAREN:
                    depth += 1
                elif self.current_type() == Type.RPAREN:
                    depth -= 1
                self.check_whitespace(1, ALLOW_ZERO)
                self.match()
        d(["check_struct() exited", self.current_token])

    def check_typedef(self):
        d(["check_typedef() entered", self.current_token])
        self.check_whitespace(1)
        if self.current_type() == Type.STRUCT:
            self.match(Type.STRUCT)
            self.check_struct(IS_TYPEDEF)
        else:
            self.match_type()
        self.check_whitespace(1)
        assert self.current_type() == Type.UNKNOWN #wasn't a type
        d(["check_typedef() adding type:", self.current_token.line])
        self.update_types([self.current_token.line])
        self.check_naming(self.current_token, Errors.TYPE)
        self.match(Type.TYPE) #but now it is
        self.check_whitespace(0)
        self.match(Type.SEMICOLON, MUST_NEWLINE)
        d(["check_typedef() exited", self.current_token])

    def check_for(self):
        d(["check_for() entered", self.current_token])
        self.match(Type.LPAREN)
        self.check_whitespace(0)
        d(["checking for init", self.current_token])
        if self.current_type() != Type.SEMICOLON:
            if self.current_type() == Type.TYPE:
                self.check_statement(True) #for (type thing;
            else:
                self.check_expression() #for (thing;
                self.check_whitespace(0)
                self.match(Type.SEMICOLON)
        else:
            self.check_whitespace(0)
            self.match(Type.SEMICOLON)
        d(["checking for conditional", self.current_token])
        self.check_whitespace(1)
        if self.current_type() != Type.SEMICOLON:
            self.check_expression() #for (thing; thing
            self.check_whitespace(0)
        self.match(Type.SEMICOLON)
        self.check_whitespace(1)
        if self.current_type() != Type.RPAREN:
            d(["checking for post-loop", self.current_token])
            self.check_expression() #for (thing; thing; thing
        while self.current_type() == Type.COMMA:
            self.check_whitespace(0)
            self.match(Type.COMMA)
            self.check_whitespace(1)
            self.check_expression() #for (thing; thing; thing, ...)
        self.check_whitespace(0)
        self.match(Type.RPAREN)
        self.should_have_block()
        d(["check_for() exited", self.current_token])

    def should_have_block(self, is_chained = False):
        if self.current_type() == Type.LBRACE:
            self.check_whitespace(1)
            self.match(Type.LBRACE, MUST_NEWLINE) # {\n regardless
            self.check_block()
            self.check_whitespace() #based on current depth
            if is_chained:
                self.match(Type.RBRACE, NO_NEWLINE, MUST_NEWLINE) #\n}
            else:
                self.match(Type.RBRACE, MUST_NEWLINE, MUST_NEWLINE) #\n}\n
        elif self.current_type() == Type.SEMICOLON:
            pass # while(a);
        else:
            self.check_whitespace((self.depth + 1) * INDENT_SIZE)
            self.errors.braces(self.current_token, Errors.MISSING)
            self.check_statement()

    def check_condition(self):
        # check spacing on the parenthesis
        self.check_whitespace(1, ALLOW_ZERO) # if/while (
        self.match(Type.LPAREN)
        self.check_whitespace(0) # (exp
        self.check_expression()
        while self.current_type() == Type.COMMA:
            self.check_whitespace(0)
            self.match(Type.COMMA)
            self.check_whitespace(1)
            self.check_expression()
        self.check_whitespace(0) # exp)
        self.match(Type.RPAREN)

    def check_do(self):
        self.should_have_block(Type.DO)
        self.check_whitespace(1)
        self.match(Type.WHILE)
        self.check_condition()
        self.check_whitespace(0)
        self.match(Type.SEMICOLON, MUST_NEWLINE)

    def check_switch(self):
        d(["check_switch(): entered", self.current_token])
        self.match(Type.LPAREN)
        self.check_whitespace(0)
        self.check_expression()
        self.check_whitespace(0)
        self.match(Type.RPAREN)
        self.check_whitespace(1)
        self.match(Type.LBRACE, MUST_NEWLINE)
        self.depth += 1
        while self.current_type() == Type.CASE:
            self.check_whitespace(self.depth * INDENT_SIZE)
            self.match(Type.CASE)
            self.check_whitespace(1)
            self.match(Type.CONSTANT)
            self.check_whitespace(0)
            self.match(Type.COLON, MUST_NEWLINE)
            self.check_case()
        if self.current_type() == Type.DEFAULT:
            self.check_whitespace(self.depth * INDENT_SIZE)
            self.match(Type.DEFAULT)
            self.check_whitespace(0)
            self.match(Type.COLON, MUST_NEWLINE)
            self.check_case()
        self.depth -= 1
        self.check_whitespace(self.depth * INDENT_SIZE)
        self.match(Type.RBRACE, MUST_NEWLINE, MUST_NEWLINE)
        d(["check_switch(): exited", self.current_token])

    def check_case(self):
        d(["check_case(): entered", self.current_token])
        while self.current_type() not in [Type.CASE, Type.DEFAULT]:
            if self.current_type() == Type.RBRACE:
                break #catches end of block
            self.check_whitespace((self.depth + 1) * INDENT_SIZE)
            self.check_statement()
        d(["check_case(): exited", self.current_token])

    def check_statement(self, in_for = False):
        d(["check_statement(): entered", self.current_token])
        if self.current_type() == Type.TYPE:
            self.check_declaration()
            self.check_whitespace(0)
            self.match(Type.SEMICOLON, MUST_NEWLINE)
        elif self.current_type() == Type.BREAK:
            self.match(Type.BREAK)
        elif self.current_type() == Type.RETURN:
            self.match(Type.RETURN)
            #return value?
            if self.current_type() != Type.SEMICOLON:
                self.check_whitespace(1)
                self.check_expression()
        elif self.current_type() in [Type.LPAREN, Type.STAR, Type.UNKNOWN]:
            self.check_expression()
        if not in_for:
            self.check_whitespace(0)
            self.match(Type.SEMICOLON, MUST_NEWLINE)
        d(["check_statement(): exited", self.current_token])

    def check_sizeof(self):
        d(["check_sizeof(): entered", self.current_token])
        #size of type
        if self.current_type() == Type.LPAREN:
            self.check_whitespace(0)
            self.match(Type.LPAREN)
            self.check_whitespace(0)
            if self.current_type() == Type.TYPE: #sizeof(type)
                self.match_type()
            else:
                self.match(Type.UNKNOWN) #sizeof(var)
            self.check_whitespace(0)
            self.match(Type.RPAREN)
        #sizeof var
        elif self.current_type() == Type.UNKNOWN:
            self.check_whitespace(1)
            self.match(Type.UNKNOWN)
            self.check_post_identifier()
        else:
            print "check_sizeof(): unexpected token:", self.current_token
            assert False #crash this thing so we can trace it
        
        d(["check_sizeof(): exited", self.current_token])

    def check_post_identifier(self):
        d(["check_post_identifier(): entered", self.current_token])
        # ++ or --
        if self.current_type() == Type.CREMENT:
            self.check_whitespace(0)
            self.match(Type.CREMENT)
        # func params
        elif self.current_type() == Type.LPAREN:
            self.check_whitespace(0)
            self.match(Type.LPAREN)
            self.check_whitespace(0)
            if self.current_type() != Type.RPAREN:
                self.check_expression()
                self.check_whitespace(0)
                #multiple
                while self.current_type() == Type.COMMA:
                    self.match(Type.COMMA)
                    self.check_whitespace(1)
                    self.check_expression()
                    self.check_whitespace(0)
            self.match(Type.RPAREN)
        #indexing
        elif self.current_type() == Type.LSQUARE:
            #can be chained
            while self.current_type() == Type.LSQUARE:
                self.check_whitespace(0)
                self.match(Type.LSQUARE)
                self.check_whitespace(0)
                if self.current_type() != Type.RSQUARE:
                    self.check_expression()
                    self.check_whitespace(0)
                self.match(Type.RSQUARE)
        d(["check_post_identifier(): exited", self.current_token])

    def check_expression(self):
        d(["check_exp(): entered", self.current_token])
        #the empty string/expression
        if self.current_type() in [Type.RPAREN, Type.RSQUARE, Type.COMMA,
                Type.SEMICOLON]:
            d(["check_exp(): exited", self.current_token])
            return
        #repeatable unary ops
        if self.current_type() in [Type.STAR, Type.NOT]:
            while self.current_type() in [Type.STAR, Type.NOT]:
                self.match()
                self.check_whitespace(0)
        #single instance unary ops
        elif self.current_type() in [Type.CREMENT, Type.AMPERSAND, Type.MINUS]:
            self.match()
            self.check_whitespace(0)

        #array initialisers are special
        if self.current_type() in [Type.LBRACE]:
            self.check_array_assignment()
        #but if not array, then
        #only identifiers, sizeof, constants and subexpressions should remain
        elif self.current_type() not in [Type.UNKNOWN, Type.CONSTANT,
                Type.SIZEOF, Type.LPAREN]:
            d(["check_exp(): unexpected token:", self.current_token])
            assert False #crash this thing so we can trace it

        #grab a value of some form
        #Type.LPAREN (subexp, typecast)
        if self.current_type() == Type.LPAREN:
            self.match(Type.LPAREN)
            self.check_whitespace(0)
            #typecast
            if self.current_type() in [Type.TYPE, Type.STRUCT, Type.IGNORE]:
                #first clear the typecast
                self.match_type()
                self.check_whitespace(0)
                self.match(Type.RPAREN)
                #then get what's being cast
                self.check_whitespace(1, ALLOW_ZERO)
                self.check_expression()
            #subexpression
            else:
                self.check_expression()
                self.check_whitespace(0)
                self.match(Type.RPAREN)
        #const
        elif self.current_type() == Type.CONSTANT:
            self.match(Type.CONSTANT)
            #possible for following constants (e.g. printf("aaa" "bbb")
            while self.current_type() == Type.CONSTANT:
                self.check_whitespace(1)
                self.match(Type.CONSTANT)
        #identifier (with optional following crement, index or params)
        elif self.current_type() == Type.UNKNOWN:
            self.match(Type.UNKNOWN)
            self.check_post_identifier()
        #sizeof
        elif self.current_type() == Type.SIZEOF:
            self.match(Type.SIZEOF)
            self.check_sizeof()
            
        #now test for a following operator
        if self.current_type() in [Type.BINARY_OP, Type.MINUS, Type.STAR,
                Type.TERNARY, Type.COLON]:
            self.check_whitespace(1, ALLOW_ZERO)
            self.match()
            self.check_whitespace(1, ALLOW_ZERO)
            self.check_expression()
        #or possibly assignments with their special case of whitespace
        elif self.current_type() == Type.ASSIGNMENT:
            self.check_whitespace(1)
            self.match(Type.ASSIGNMENT)
            self.check_whitespace(1)
            self.check_expression()
        #and done
        d(["check_exp(): exited", self.current_token])

    def check_block(self):
        d(["\nD: check_block(): entered", self.current_token])
        self.depth += 1
        #block ends if we hit the matching brace
        while self.current_type() != Type.RBRACE:
            d(["in block while: ", self.current_token])
            if self.current_type() == Type.HASH:
                self.check_whitespace(0)
                self.check_precompile()
            self.check_whitespace()
            if self.current_type() in [Type.TYPE, Type.IGNORE]:
                self.check_declaration()
            elif self.current_type() == Type.STRUCT:
                self.match(Type.STRUCT)
                self.check_whitespace(1)
                self.match() #the struct type
                first = True
                while first or self.current_type() == Type.COMMA:
                    if first:
                        first = False
                    else:
                        self.match(Type.COMMA)
                    if self.current_type() == Type.STAR: #them pointers again
                        self.check_whitespace(1, ALLOW_ZERO)
                        self.match(Type.STAR)
                        while self.current_type() == Type.STAR:
                            self.check_whitespace(0)
                            self.match(Type.STAR)
                        self.check_whitespace(1, ALLOW_ZERO)               
                    else:
                        self.check_whitespace(1)
                    self.check_naming(self.current_token, Errors.VARIABLE)
                    self.match(Type.UNKNOWN)
                    if self.current_type() == Type.ASSIGNMENT:
                        self.check_whitespace(1)
                        self.match(Type.ASSIGNMENT)
                        self.check_whitespace(1)
                        #awkward types of struct initialisers to deal with
                        #if it's {.member = } style
                        if self.current_type() == Type.LBRACE:
                            d(["struct assignment {.x = ...} style"])
                            self.match(Type.LBRACE)
                            self.check_whitespace(0)
                            while self.current_type() == Type.BINARY_OP: # .
                                self.match(Type.BINARY_OP)
                                d(["next member", self.current_token])
                                self.check_whitespace(0)
                                self.match(Type.UNKNOWN)
                                self.check_whitespace(1)
                                self.match(Type.ASSIGNMENT)
                                self.check_whitespace(1)
                                self.check_expression()
                                self.check_whitespace(0)
                                if self.current_type() == Type.COMMA:
                                    self.match(Type.COMMA)
                                    self.check_whitespace(1)
                            self.match(Type.RBRACE, NO_NEWLINE)
                        #otherwise it's just a normal expression
                        else:
                            self.check_expression()
            elif self.current_type() == Type.RETURN:
                self.match(Type.RETURN)
                #if returning a value
                if self.current_type() != Type.SEMICOLON:
                    self.check_whitespace(1)
                    self.check_expression()
            elif self.current_type() in [Type.STAR, Type.CREMENT,
                    Type.CONSTANT, Type.SIZEOF, Type.LPAREN]:
                self.check_expression()
            elif self.current_type() == Type.BREAK:
                self.match(Type.BREAK)
            elif self.current_type() == Type.FOR:
                self.match(Type.FOR)
                self.check_whitespace(1, ALLOW_ZERO)
                self.check_for()
                continue
            elif self.current_type() == Type.WHILE:
                self.match(Type.WHILE)
                self.check_condition()
                self.should_have_block()
                continue
            elif self.current_type() == Type.DO:
                self.match(Type.DO)
                self.check_do()
                continue
            elif self.current_type() == Type.SWITCH:
                self.match(Type.SWITCH)
                self.check_whitespace(1, ALLOW_ZERO)
                self.check_switch()
                continue
            elif self.current_type() == Type.IF:
                self.match(Type.IF)
                has_else = self.has_matching_else()
                d(["check_block(): ", self.last_real_token, 
                        " has else:", has_else])
                self.check_condition()
                self.should_have_block(has_else)
                while self.current_type() == Type.ELSE:
                    self.check_whitespace(1)
                    self.match(Type.ELSE)
                    if self.current_type() == Type.IF:
                        self.check_whitespace(1, ALLOW_ZERO)
                        self.match(Type.IF)
                        has_else = self.has_matching_else()
                        d(["check_block(): ", self.last_real_token, 
                                " has else:", has_else])
                        self.check_condition()
                        self.should_have_block(has_else)
                    else:
                        self.should_have_block() #else already
                continue
            elif self.current_type() == Type.UNKNOWN:
                self.check_expression()
            elif self.current_type() == Type.LBRACE:
                d(["found a block inside a block"])
                self.match(Type.LBRACE, MUST_NEWLINE, MUST_NEWLINE)
                self.check_block()
                self.check_whitespace()
                self.match(Type.RBRACE, MUST_NEWLINE, MUST_NEWLINE)
                continue
            elif self.current_type() == Type.SEMICOLON: #no statement, just ;
                self.match(Type.SEMICOLON, MUST_NEWLINE)
                continue
            self.check_whitespace(0)
            self.match(Type.SEMICOLON, MUST_NEWLINE)
        self.depth -= 1
        d(["check_block(): exited", self.current_token, "\n"])

    def check_define(self):
#TODO mark it to be manually checked
# since we can't tell here what they're doing
        self.check_whitespace(1)
        first = self.current_token
        #before we match, we just want to know what's coming next, since match
        #moves to the next meaningful token (but in this case we need to know
        #if it's a newline)
        next = self.tokens[self.position+1]
        self.match() #the identifier (can't rely on it being Type.UNKNOWN)
        #just defining, no other values, nuffin'
        if next.get_type() in [Type.NEWLINE, Type.COMMENT]:
            return
        #is it a macro
        if self.current_type() == Type.LPAREN and \
                self.current_token.get_spacing_left() == 0:
            self.check_whitespace(0)
            #consume until newline for now
            while self.current_type() != Type.NEWLINE:
                self.position += 1
                self.current_token = self.tokens[self.position]
                self.check_whitespace(1, ALLOW_ZERO)
            self.position += 1
            self.current_token = self.tokens[self.position]
        #just a plain identifier swap
        else:
            self.check_whitespace(1)
            tokens = []
            while self.current_type() not in [Type.NEWLINE,
                    Type.COMMENT]:
                tokens.append(self.current_token)
                self.position += 1
                self.current_token = self.tokens[self.position]
                self.check_whitespace(1, ALLOW_ZERO)
            self.match()
            if first._type == Type.UNKNOWN: #direct access deliberate
                self.check_naming(first, Errors.DEFINE)
                #complicated
                if len(tokens) > 1:
                    for token in self.tokens:
                        if token.line == first.line:
                            token.inner_tokens = tokens
                #simple one-to-one
                else:
                    if tokens[0] != Type.UNKNOWN:
                        for n in xrange(self.position + 1, len(self.tokens)):
                            if self.tokens[n].line == first.line:
                                self.tokens[n]._type = tokens[0]._type
            #oh my, they #defined an existing symbol/keyword
            else:
                if len(tokens) > 1:
                    print "this is terrible, why do this to me"
#TODO violate them
                    for token in self.tokens:
                        if token._type == first._type:
                            token.inner_tokens = tokens
                else:
                    for n in xrange(self.position + 1, len(self.tokens)):
                        if self.tokens[n]._type == first._type:
                            self.tokens[n]._type = tokens[0]._type
        
    def check_array_assignment(self):
        if self.current_type() in [Type.UNKNOWN, Type.CONSTANT]:
            #assignment is to another variable or to a string
            self.check_expression()
            return
        self.match(Type.LBRACE, MAY_NEWLINE)
        self.check_whitespace(0)
        #partial init
        if self.current_type() == Type.LSQUARE:
            self.match(Type.LSQUARE)
            self.check_whitespace(0)
            self.check_expression()
            self.check_whitespace(0)
            self.match(Type.RSQUARE)
            self.check_whitespace(1)
            self.match(Type.ASSIGNMENT)
            self.check_whitespace(1)
            self.check_expression()
            while self.current_type() == Type.COMMA:
                self.check_whitespace(0)
                self.match(Type.COMMA)
                self.check_whitespace(1)
                self.match(Type.LSQUARE)
                self.check_whitespace(0)
                self.check_expression()
                self.check_whitespace(0)
                self.match(Type.RSQUARE)
                self.check_whitespace(1)
                self.match(Type.ASSIGNMENT)
                self.check_whitespace(1)
                self.check_expression()
        #complete init
        else:
            self.check_expression()
            while self.current_type() == Type.COMMA:
                self.check_whitespace(0)
                self.match(Type.COMMA)
                self.check_whitespace(1)
                self.check_expression()
        self.check_whitespace(0)
        self.match(Type.RBRACE, MAY_NEWLINE, MAY_NEWLINE)
        assert self.current_type() == Type.SEMICOLON

    def check_declaration(self, match_types = True, external = False):
        d(["check_declaration() entered", self.current_token])
        if match_types:
            self.match_type()
            #ALLOW_ZERO here because if it's not a pointer, zero spaces will
            #actually break C anyway
            self.check_whitespace(1, ALLOW_ZERO)
        else:
            d(["skipping types, match_types = False"])
        array = False
        name = None
        #if we're dealing with a function pointer, no following identifer
        if self.current_type() == Type.UNKNOWN:
            name = self.current_token
            self.match(Type.UNKNOWN)
        #is this a function?
        if self.current_type() == Type.LPAREN:
            if not name:
                d(["decl is a func returning func pointer"])
            else:
                d(["decl is a func", name])
                self.check_whitespace(0)
            param_names = []
            self.match(Type.LPAREN)
            #arg matching time
            while self.current_type() != Type.RPAREN:
                self.check_whitespace(0)
                if self.current_type() == Type.COMMA:
                    self.match(Type.COMMA)
                    self.check_whitespace(1)
                #types can be omitted if prototyped
                if self.current_type() in [Type.TYPE, Type.STRUCT, Type.IGNORE]: 
                    self.match_type() #type
                    if self.current_type() == Type.UNKNOWN:
                        self.check_whitespace(1, ALLOW_ZERO)
                #identifiers can be ommitted in a prototype
                if self.current_type() == Type.UNKNOWN:
                    param_names.append(self.current_token)
                    self.match(Type.UNKNOWN)
                    if self.current_type() == Type.STAR:
                        #turns out it was a type after all and was relying on
                        #a typedef never mentioned here.. bad bad people
                        #TODO violate them maybe? iunno
                        param_names.pop()
                        self.check_whitespace(1, ALLOW_ZERO)
                        self.match(Type.STAR)
                        while self.current_type() == Type.STAR:
                            self.check_whitespace(0)
                            self.match(Type.STAR)
                        if self.current_type() == Type.UNKNOWN:
                            param_names.append(self.current_token)
                            self.check_whitespace(1, ALLOW_ZERO)
                            self.match(Type.UNKNOWN)
                    #strip array type indicators
                    self.check_post_identifier()
            self.check_whitespace(0)
            self.match(Type.RPAREN, MAY_NEWLINE)
            if self.current_type() == Type.LBRACE:
                #check the name, now that we're in the definition
                if name:
                    self.check_naming(name, Errors.FUNCTION)
                for param in param_names:
                    self.check_naming(param, Errors.VARIABLE)
                start_line = self.current_token.line_number
                if self.last_real_token.line_number != \
                        self.current_token.line_number:
                    self.check_whitespace()
                else:
                    self.check_whitespace(1)
                self.match(Type.LBRACE, MUST_NEWLINE, MAY_NEWLINE)
                self.check_block()
                self.check_whitespace()
                self.match(Type.RBRACE, MUST_NEWLINE, MUST_NEWLINE)
                func_length = self.current_token.line_number - start_line
                if func_length >= 50:
                    self.errors.func_length(start_line, func_length)
            elif self.current_type() == Type.ASSIGNMENT:
                self.check_whitespace(1)
                self.match(Type.ASSIGNMENT)
                self.check_whitespace(1)
                self.check_expression()
                self.check_whitespace(0)
                self.match(Type.SEMICOLON, MUST_NEWLINE)
            else:
                self.check_whitespace(0)
                self.match(Type.SEMICOLON, MUST_NEWLINE)
            d(["check_declaration() exited a func", self.current_token])
            return
        d(["decl is a var", self.last_real_token])
        #well, it's a non-func then
        if not external and name:
            self.check_naming(name, Errors.VARIABLE)
        #is it an array?
        if self.current_type() == Type.LSQUARE:
            array = True
            self.check_post_identifier()            
        #token will now be , or = or ;
        if self.current_type() == Type.ASSIGNMENT:
            self.check_whitespace(1)
            self.match(Type.ASSIGNMENT)
            self.check_whitespace(1)
            if array:
                self.check_array_assignment()
                assert self.current_type() == Type.SEMICOLON
            else:
                self.check_expression()
        
        #is it a multi-var declaration?
        while self.current_type() == Type.COMMA:
            self.check_whitespace(0)
            self.match(Type.COMMA)
            self.check_whitespace(1)
            #in case this is pointers to the type
            while self.current_type() == Type.STAR:
                self.match(Type.STAR)
                if self.current_type() == Type.STAR:
                    self.check_whitespace(0)
                else:
                    self.check_whitespace(1, ALLOW_ZERO)
            self.match(Type.UNKNOWN) #identifier
            if self.current_type() == Type.ASSIGNMENT:
                self.check_whitespace(1)
                self.match(Type.ASSIGNMENT)
                self.check_whitespace(1)
                self.check_expression() #match out the expression
                continue
        if self.depth == 0: #since parent can't tell if it was func or not
            self.check_whitespace(0)
            self.match(Type.SEMICOLON, MUST_NEWLINE)
        d(["check_declaration() exited", self.current_token])
            
if __name__ == '__main__':
    if (len(sys.argv)) == 1:
        print "no arguments given"
    for f in range(1, len(sys.argv)):
        if sys.argv[f].strip():
            print 'Parsing %s...' % sys.argv[f]
            style = Styler(sys.argv[f])
            print style.errors
            print "THIS IS NOT A GUARANTEE OF CORRECTNESS"

