# Bem-vindo ao compilador do ꟼHꟼ
# A ideia é que a linguagem seja toda ao contrário!!!!!
# Então reconhecer um if o token representado será um token = Else e assim seguindo a mesma ideia
# para os demais e.g.(1=STRING, "texto"=INT, etc.)

################################################### Lexer do ꟼHꟼ ######################################A################

from typing import NamedTuple 
import re

class Token(NamedTuple):
    type: str
    value: str

class Lexer(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0

    def tokenize(self):
        keywords = {
        'IF': 'ELSE',     # IF = [ELSE]
        'ELSE': 'IF',     # ELSE = [IF]
        'IFELSE': 'ELIF',
        'PRINT': 'TNIRP'} # ELIF: [IFELSE]

        token_specification = [
            ('STRING',      r'\d+'),              
            ('INT',       r'"[A-Za-z]+[0-9]*[\s]*"'),          
            ('EQUAL',      r'UNEQUAL'),           
            ('DIFFERENT',       r'='),            
            ('MATCH',       r'!='),               
            ('MENOR',           r'>'),            
            ('MAIOR',           r'<'),            
            ('LPARENTHESIS', r'RPARENTHESIS'),    
            ('RPARENTHESIS', r'LPARENTHESIS'),    
            ('RBRACE',       r'[{]'),             # RBRACK = [{]
            ('LBRACE',       r'[}]'),             # LBRACK = [}]
            ('ID',           r'[A-Za-z]+[0-9]*'), # ID = [a-z A-Z]*[0-9]^+
            ('SUB',          r'[+]'),             # SUB = [+]
            ('SUM',          r'[\-]'),            # SUM = [\-]
            ('DIV',          r'[*]'),             # DIV = [*]
            ('MULT',          r'[/]'),            # MULT = [/]
            ('PREVIOUSLINE',      r'\n'),         # PREVIOUSLINE = [\n] 
            ('BAT',      r'\t'),         # PREVIOUSLINE = [\n]     
            ('FULL',        r'[\s]+'),            # FULL = [\s]+
        ]

        tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
        token_regex = re.compile(tok_regex)
        for mo in re.finditer(token_regex, self.text):
            kind = mo.lastgroup
            value = mo.group()
            if kind == 'STRING':
                value = int(value)
            elif kind == 'ID' and value in keywords and value=='ELSE':
                kind = 'IF'
            elif kind == 'ID' and value in keywords and value=='IF':
                kind = 'ELSE'
            elif kind == 'ID' and value in keywords and value=='IFELSE':
                kind = 'ELIF'
            elif kind == 'ID' and value in keywords and value=='PRINT':
                kind = 'TNIRP'
            elif kind == 'FULL':
                continue
            elif kind == 'MISMATCH':
                raise RuntimeError(f'{value!r} invalid sintax')
            yield Token(kind, value)

############################################## Parser e AST do ꟼHꟼ ####################################################

class AST(object):
    pass

class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class IdOperation(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right

class Identifier(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class String(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value

class NewLine(AST):
    def __init__(self, pline, op, nline):
        self.pline = pline
        self.token = "\n"
        self.nline = nline

class Executelines(AST):
    def __init__(self, pline, op, nline):
        self.tab = "\t"
        self.pline = pline
        self.token = "\n"
        self.nline = nline

class Tnirp(AST):
    def __init__(self, token):
        self.token = token
        self.value = "print"

class Lparenthesis(AST):
    def __init__(self, token):
        self.token = token
        self.value = "("

class Rparenthesis(AST):
    def __init__(self, token):
        self.token = token
        self.value = ")"

class Lbrace(AST):
    def __init__(self, token):
        self.token = token
        self.value = ""

class Rbrace(AST):
    def __init__(self, token):
        self.token = token
        self.value = ":"

class Imprime(AST):
    def __init__(self, left, lparenthesis, item, rparenthesis):
        self.left = left
        self.lparenthesis = lparenthesis
        self.item = item
        self.rparenthesis = rparenthesis

class Else(AST):
    def __init__(self, token):
        self.token = token
        self.value = "if"

class If(AST):
    def __init__(self, token):
        self.token = token
        self.value = "else"

class Elif(AST):
    def __init__(self, token):
        self.token = token
        self.value = "elif"

class Ifcondition(AST):
    def __init__(self, op, lparenthesis, condition, rparenthesis, rbrace, do, lbrace):
        self.op = op
        self.lparenthesis = lparenthesis        
        self.condition = condition
        self.rparenthesis = rparenthesis
        self.rbrace = rbrace
        self.do = do
        self.lbrace = lbrace

class Elsecondition(AST):
    def __init__(self, op, rbrace, do, lbrace):
        self.op = op
        self.rbrace = rbrace
        self.do = do
        self.lbrace = lbrace

class Elifcondition(AST):
    def __init__(self, op, lparenthesis, condition, rparenthesis, rbrace, do, lbrace):
        self.op = op
        self.lparenthesis = lparenthesis        
        self.condition = condition
        self.rparenthesis = rparenthesis
        self.rbrace = rbrace
        self.do = do
        self.lbrace = lbrace

class Parser(object):
    def __init__(self, lexer):
        self.lexer = lexer
        self.token_generator = self.lexer.tokenize()
        self.current_token = next(self.token_generator)
    
    def error(self):
        raise Exception('Invalid Syntax')

    def eat(self, token_type):
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            try:
                self.current_token = next(self.token_generator)
            except StopIteration:
                pass
        else:
            self.error()
        
    def factor(self):
        """factor : INTEGER | LPAREN expr RPAREN"""
        token = self.current_token
        if token.type == 'STRING':
            self.eat('STRING')
            return Num(token)
        elif token.type == 'RPARENTHESIS':
            self.eat('RPARENTHESIS')
            node = self.expr()
            self.eat('LPARENTHESIS')
            return node
        elif token.type == 'ID':
            self.eat('ID')
            return Identifier(token)
        elif token.type == 'INT':
            self.eat('INT')
            return String(token)
        elif token.type == 'TNIRP':
            self.eat('TNIRP')
            return Tnirp(token)

    def term(self):
        """term : factor ((MUL | DIV) factor)*"""
        node = self.factor()

        while self.current_token.type in ('MULT', 'DIV'):
            token = self.current_token
            if token.type == 'MULT':
                self.eat('MULT')
            elif token.type == 'DIV':
                self.eat('DIV')

            node = BinOp(left=node, op=token, right=self.factor())
        
        while self.current_token.type in ('SUB', 'SUM'):
            token = self.current_token
            if token.type == 'SUB':
                self.eat('SUB')
            elif token.type == 'SUM':
                self.eat('SUM')

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def expr(self):
        """
        expr   : term ((PLUS | MINUS) term)*
        term   : factor ((MUL | DIV) factor)*
        factor : INTEGER | LPAREN expr RPAREN
        """
        node = self.term()
        
        while self.current_token.type in ('DIFFERENT', 'SUM', 'SUB', 'DIV', 'MULT', 'EQUAL', 'MAIOR', 'MENOR') and node.token.type == 'ID':
            token = self.current_token
            if token.type == 'DIFFERENT':
                self.eat('DIFFERENT')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'SUM':
                self.eat('SUM')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'SUB':
                self.eat('SUB')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'DIV':
                self.eat('DIV')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'MULT':
                self.eat('MULT')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'EQUAL':
                self.eat('EQUAL')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'MAIOR':
                self.eat('MAIOR')
                node = IdOperation(left=node, op=token, right=self.term())
            elif token.type == 'MENOR':
                self.eat('MENOR')
                node = IdOperation(left=node, op=token, right=self.term())
            
        while self.current_token.type in ('LPARENTHESIS') and node.token.type == 'TNIRP':
            token = self.current_token
            if token.type == 'LPARENTHESIS':
                self.eat('LPARENTHESIS')
                lp = Lparenthesis(token)
                value = self.term()
                self.eat('RPARENTHESIS')
                rp = Rparenthesis(token)
                node = Imprime(left=node, lparenthesis=lp, item=value, rparenthesis=rp)
        

        return node

    def conditions(self):
        node = self.expr()

        while self.current_token.type in ('IF', 'ELSE', 'ELIF'):
            token = self.current_token
            if token.type == 'IF':
                self.eat('IF')
                op = Else(token)
                self.eat('LPARENTHESIS')
                lparen = Lparenthesis(token)
                condition = self.multline()
                self.eat('RPARENTHESIS')
                rparen = Rparenthesis(token)
                self.eat('RBRACE')
                rbrace = Rbrace(token)
                do = self.execute()
                self.eat('LBRACE')
                lbrace = Lbrace(token)
                node = Ifcondition(op, lparen, condition, rparen, rbrace, do, lbrace)
            elif token.type == 'ELSE':
                self.eat('ELSE')
                op = If(token)
                self.eat('RBRACE')
                rbrace = Rbrace(token)
                do = self.execute()
                self.eat('LBRACE')
                lbrace = Lbrace(token)
                node = Elsecondition(op, rbrace, do, lbrace)
            elif token.type == 'ELIF':
                self.eat('ELIF')
                op = Elif(token)
                self.eat('LPARENTHESIS')
                lparen = Lparenthesis(token)
                condition = self.multline()
                self.eat('RPARENTHESIS')
                rparen = Rparenthesis(token)
                self.eat('RBRACE')
                rbrace = Rbrace(token)
                do = self.execute()
                self.eat('LBRACE')
                lbrace = Lbrace(token)
                node = Elifcondition(op, lparen, condition, rparen, rbrace, do, lbrace)
        
        return node

    def multline(self):
        node = self.conditions()
        
        while self.current_token.type in ('PREVIOUSLINE'):
            token = self.current_token
            if token.type == 'PREVIOUSLINE':
                self.eat('PREVIOUSLINE')
                node = NewLine(pline=node, op=token, nline=self.conditions())

        return node

    def execute(self):
        node = self.conditions()
        
        while self.current_token.type in ('PREVIOUSLINE'):
            token = self.current_token
            if token.type == 'PREVIOUSLINE':
                self.eat('PREVIOUSLINE')
                node = Executelines(pline=node, op=token, nline=self.conditions())

        return node

    def parse(self):
        return self.multline()

############################################ Gerador de código do ꟼHꟼ ################################################

class NodeVisitor(object):
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))
    
class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

    def visit_Num(self, node):
        return node.value

    def visit_BinOp(self, node):
        if node.op.type == 'SUB':
            return "{}{}{}".format(self.visit(node.left), " - ", self.visit(node.right))
        elif node.op.type == 'SUM':
            return "{}{}{}".format(self.visit(node.left), " + ", self.visit(node.right))
        elif node.op.type == 'DIV':
            return "{}{}{}".format(self.visit(node.left), " / ", self.visit(node.right))
        elif node.op.type == 'MULT':
            return "{}{}{}".format(self.visit(node.left), " * ", self.visit(node.right))

    def visit_Identifier(self, node):
        return node.value

    def visit_String(self, node):
        return node.value

    def visit_IdOperation(self, node):
        if node.op.type == 'DIFFERENT':
            return "{}{}{}".format(self.visit(node.left), " = ", self.visit(node.right))
        elif node.op.type == 'SUM':
            return "{}{}{}".format(self.visit(node.left), " + ", self.visit(node.right))
        elif node.op.type == 'SUB':
            return "{}{}{}".format(self.visit(node.left), " - ", self.visit(node.right))
        elif node.op.type == 'MULT':
            return "{}{}{}".format(self.visit(node.left), " * ", self.visit(node.right))
        elif node.op.type == 'DIV':
            return "{}{}{}".format(self.visit(node.left), " / ", self.visit(node.right))
        elif node.op.type == 'EQUAL':
            return "{}{}{}".format(self.visit(node.left), " == ", self.visit(node.right))
        elif node.op.type == 'MAIOR':
            return "{}{}{}".format(self.visit(node.left), " > ", self.visit(node.right))
        elif node.op.type == 'MENOR':
            return "{}{}{}".format(self.visit(node.left), " < ", self.visit(node.right))

    def visit_Tnirp(self, node):
        return node.value

    def visit_Lparenthesis(self, node):
        return node.value

    def visit_Rparenthesis(self, node):
        return node.value   

    def visit_Imprime(self, node):
        return "{} {}{}{}".format(self.visit(node.left), self.visit(node.lparenthesis), self.visit(node.item), self.visit(node.rparenthesis))

    def visit_NoneType(self, node):
        return ""

    def visit_NewLine(self, node):
        return "{}{}{}".format(self.visit(node.pline), node.token, self.visit(node.nline))

    def visit_Lbrace(self, node):
        return node.value

    def visit_Rbrace(self, node):
        return node.value

    def visit_Else(self, node):
        return node.value
    
    def visit_If(self, node):
        return node.value

    def visit_Elif(self, node):
        return node.value

    def visit_Executelines(self, node):
        return "{}{}{}{}".format(self.visit(node.pline), node.token, node.tab, self.visit(node.nline))

    def visit_Ifcondition(self, node):
        return "{} {}{}{}{}".format(self.visit(node.op), self.visit(node.condition), self.visit(node.rbrace),  self.visit(node.do),  self.visit(node.lbrace))

    def visit_Elsecondition(self, node):
        return "{}{}{}{}".format(self.visit(node.op), self.visit(node.rbrace),  self.visit(node.do),  self.visit(node.lbrace))

    def visit_Elifcondition(self, node):
        return "{} {}{}{}{}".format(self.visit(node.op), self.visit(node.condition), self.visit(node.rbrace),  self.visit(node.do),  self.visit(node.lbrace))

    def interpret(self):
        tree = self.parser.parse()
        codigo = self.visit(tree)
        return codigo

########################################## Chamada das funções ###################################################

def main():
    file = open("ide.txt","r")
    text = file.read()
    file.close()
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    arquivo = open("codigo.py", "w")
    result = interpreter.interpret()
    arquivo.write(result)
    print("Código compilado com sucesso!")
    
main()