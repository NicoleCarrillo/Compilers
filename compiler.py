import ply.yacc as yacc
import ply.lex as lex

reserved = { 
    'int': 'INTDEC', 
    'float': 'FLOATDEC', 
    'boolean': 'BOOLDEC', 
    'string': 'STRINGDEC',
    'if': 'IF', 
    'else': 'ELSE', 
    'while': 'WHILE', 
    'print': 'PRINT',
    'and': 'AND', 
    'or': 'OR', 
    'not': 'NOT', 
 }

tokens = [
    'ID',
    'EQUAL',
    'DIFFERENT',
    'INUMBER',
    'FNUMBER',
    'STR',
    'GREATERTHAN',
    'LESSTHAN'
] + list(reserved.values()) 

literals = ['(', ')', '{', '}', '-', '+', '*', '/', '^', '=', '>', '<', ';']

t_INUMBER = r'\d+'

t_FNUMBER = r'\d+\.\d+' 

t_STR = r'".*"'

t_ignore = ' \t'

t_EQUAL = r'=='

t_DIFFERENT = r'!='

t_GREATERTHAN = r'>='

t_LESSTHAN = r'<='

# Token
def t_ID(t):
    r'[A-Za-z_][\w_]*'
    t.type = reserved.get(t.value, "ID")
    return t

def t_newline(t):
	r'\n+'
	t.lexer.lineno += t.value.count("\n")

def t_error(t):
	print("ILLEGAL CHARACTER '%s' AT T_ERROR FED LINE 82" % t.value[0])
	t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules

precedence = (
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('right', 'UMINUS'),
)

# dictionary of names
names = {}
abstractTree = []

def p_statement_declare_int(p):
    '''statement : INTDEC NAME is_assing
    '''
    if type(p[3]) == 'float':
        print('No puedes asignar flotantes a enteros')
    else:
        names[p[2]] = { "type": "INT", "value": p[3]}

def p_is_assing(p):
    '''is_assing : "=" expression 
                | '''
    p[0] = 0
    if len(p) > 2:
        p[0] = p[2]

def p_statement_declare_float(p):
    'statement : FLOATDEC NAME is_assing'
    names[p[2]] = { "type": "FLOAT", "value": p[3]}

def p_statement_declare_bool(p):
    'statement : BOOLDEC NAME is_assing'
    names[p[2]] = { "type": "BOOL", "value": p[3]}

def p_statement_print(p):
    '''statement : PRINT '(' expression ')' '''
    print(p[3])

def p_statement_assign(p):
    'statement : NAME "=" expression'
    if p[1] not in names:
        print ( "You must declare a variable before using it")
    names[p[1]]["value"] = p[3]


def p_statement_expr(p):
    'statement : expression'
    # print(p[1])

def p_expression_binop(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]

def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]

def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_expression_inumber(p):
    "expression : INUMBER"
    p[0] = p[1]

def p_expression_fnumber(p):
    "expression : FNUMBER"
    p[0] = p[1]

def p_expression_boolean(p):
    "expression : BOOL"
    p[0] = p[1]

def p_expression_name(p):
    "expression : NAME"
    try:
        p[0] = names[p[1]]["value"]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0

def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' character '%s'" % (p.lexpos, p.lineno) )
    else:
        print("Syntax error at EOF")

parser = yacc.yacc()

#File
inputData = []
with open('data.txt') as file:
    inputData = file.readlines()

for data in inputData:
    yacc.parse(data)