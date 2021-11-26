# ---------------------------------------------------------------------------------------------
# comp.py
# A simple compiler. 
# This is based on O'Reilly's "Lex and Yacc", calculator

import ply.yacc as yacc
import ply.lex as lex
from Nodo import Nodo

reserved = {
    'float' : 'FLOAT',
    'int' : 'INT',
    'string' : 'STRING',
    'and' : 'AND',
    'or' : 'OR',
    'if' : 'IF',
    'elif' : 'ELIF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'boolean' : 'BOOLEAN',
    'for' : 'FOR',
    'true' : 'TRUE',
    'false' : 'FALSE',
    'print' : 'PRINT'
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
lx = lex.lex()

def parseTree(varInto, controlFlag):
    
    controller = []
    result = []
    counter = 0
    flagWhile = controlFlag

    while(len(varInto) > 0):
        character = varInto.pop(0)
        counter = counter + 1
        # * IF THERE IS A  (, INIT OF CONDITION ... THEN PUT INTO STACK
        if(character == "("):
            controller.append(character)
            counter = counter + 1
            flagWhile = True
        elif(character == ")"):
            while(len(controller) > 0 and controller[len(controller)-1] != "("):
                # * THE END OF THE ) SO POP WHAT WAS INSIDE AND CONTINUE
                result.append(controller.pop())
                counter = counter + 1
                flagWhile = True
            if(controller[len(controller)-1] == "("):
                controller.pop()
                counter = counter + 1
                flagWhile = True
            else:
                # * IF ERROR
                counter = 0
                flagWhile = False
                print("HEY SOMETHING IS WRONG IN THE ( ) CONDITIONS! REVIEW ELSE 119")
        elif(character in "+-/*^"):
            counter = counter + 1
            flagWhile = True
            # * CHECKS THE CORRECT PRECEDENCE OF THE OPERATORS 
            while(len(controller) > 0 and (controller[len(controller)-1] in "^" or (controller[len(controller)-1] in "*/" and character in "*/+-") or (controller[len(controller)-1] in "+-" and character in "+-"))):
                result.append(controller.pop())
            controller.append(character)
        else:
            counter = counter + 1
            flagWhile = True
            result.append(character)
    while(len(controller) > 0):
        result.append(controller.pop())
        flagWhile = True
        counter = counter + 1
    if(not flagWhile):
        print("HEY SOMETHING IS NOT WORKING AT PARSE TREE METHOD LINE 100")

    controllerFinal = []
    resultFinal = result
    counterFinal = 1
    errorWord = ""

    # * CHECKS NUMERIC EXPRESSIONS
    while(isLenghtValid(resultFinal)):
        character = resultFinal.pop(0)
        if(isinstance(character, Nodo)):
            controllerFinal.append(character)
            counterFinal = counterFinal + 1
        elif(character == '('):
            counterFinal = 0
            errorWord = character + errorWord
        elif(character in "+-*/^"):
            if(not len(controllerFinal) < 2):
                a2 = controllerFinal.pop()
                counterFinal = counterFinal + 1
                if(not(isinstance(a2, Nodo))):
                    a2 = Nodo(a2)
                    errorWord = character + errorWord
                a1 = controllerFinal.pop()
                if(not(isinstance(a1, Nodo))):
                    a1 = Nodo(a1)
                    errorWord = character + errorWord
                newNode = Nodo(character,children=[a1, a2])
                Nodo.setParent(newNode)
                controllerFinal.append(newNode)
        else:
            controllerFinal.append(character)
    # * ERROR VALIDATION
    if(counterFinal == 0):
        print("IT CAN BE ( AT CHARACTER 152")
        print("ERROR TILL" + errorWord)
    if(len(controllerFinal) != 1):
        print("ERROR AT NUMBER EXPRESSION WHILE 148")
    else:
        character = controllerFinal.pop()
        if(not(isinstance(character, Nodo))):
            character = Nodo(character)
        return character

def isLenghtValid(variable):
    return len(variable) > 0

def p_block(p):
    '''
    block : statement
        | statement block
    '''
    if(len(p) > 2):
        p[0] = Nodo('block', children=[p[1], p[2]])
        Nodo.setParent(p[0])
    elif(len(p) < 0):
        print("INVALID BLOCK GENERATION")
    else:
        p[0] = p[1]

def p_statement(p):
    '''
    statement : prodstatement ';'
        | control
    '''
    p[0] = p[1]

def p_prodstatement_assdec_num_int(p):
    '''
    prodstatement : INT ID '=' numericexp
    '''
    declarationDefinition = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(declarationDefinition)
    p[0] = Nodo('assignment', [declarationDefinition, parseTree(p[4], False)])
    Nodo.setParent(p[0])

def p_prodstatement_assdec_num_float(p):
    '''
    prodstatement : FLOAT ID '=' numericexp
    '''
    declarationDefinition = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(declarationDefinition)
    p[0] = Nodo('assignment', [declarationDefinition, parseTree(p[4], False)])
    Nodo.setParent(p[0])

def p_prodstatement_assdec_strings(p):
    '''
    prodstatement : STRING ID '=' stringexp
    '''
    declarationDefinition = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(declarationDefinition)
    p[0] = Nodo('assignment', [declarationDefinition, p[4]])
    Nodo.setParent(p[0])

def p_prodstatement_assdec_boolean(p):
    '''
    prodstatement : BOOLEAN ID '=' booleanexp
    '''
    declarationDefinition = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(declarationDefinition)
    p[0] = Nodo('assignment', [declarationDefinition, p[4]])
    Nodo.setParent(p[0])

def p_expr_num(p):
    '''
    expr : numericexp
    '''
    p[0] = parseTree(p[1], False)

def p_expr_bool(p):
    '''
    expr : booleanexp
    '''
    p[0] = p[1]

def p_expr_str(p):
    '''
    expr : stringexp
    '''
    p[0] = p[1]

def p_control_if(p):
    '''
    control : IF '(' booleanexp ')' '{' block '}' elif else
    '''
    if(not len(p) > 2):
        print("THERE IS AN ERROR AT P_CONTROL_IF!")
    else:
        p[0] = 0
        ch = [p[3], p[6]]
        if(p[8]):
            ch.append(p[8])
        if(p[9]):
            ch.append(p[9])
        p[0] = Nodo('if', children=ch)
        Nodo.setParent(p[0])

def p_control_elif(p): 
    '''
    elif : ELIF '(' booleanexp ')' '{' block '}' elif
        | epsilon
    '''
    if(len(p) < 2):
        print("THERE IS AN ERROR AT P_CONTROL_ELIF!")
    if(len(p) > 2):
        ch = [p[3], p[6]]
        p[0] = 0
        if(p[8]):
            ch.append(p[8])
        p[0] = Nodo('elif', children=ch)
        Nodo.setParent(p[0])

def p_control_else(p):
    '''
    else : ELSE '{' block '}'
        | epsilon
    '''
    if(not len(p) > 2):
        print("THERE IS AN ERROR AT P_CONTROL_ELSE!")
    else:
        p[0] = 0
        elseNode = Nodo('else', [p[3]])
        p[0] = elseNode
        Nodo.setParent(elseNode)
        Nodo.setParent(p[0])

def p_control_while(p):
    '''
    control : WHILE '(' booleanexp ')' '{' block '}'
    '''
    p[0] = Nodo('while', [p[3], p[6]])
    Nodo.setParent(p[0])

def p_prodstatement_assigment(p):
    '''
    prodstatement : ID '=' expr
    '''
    p[0] = Nodo('assignment', [Nodo(p[1]), p[3]])
    Nodo.setParent(p[0])

def p_prodstatement_declaration_float(p):
    '''
    prodstatement : FLOAT ID 
    '''
    p[0] = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(p[0])

def p_prodstatement_declaration_int(p):
    '''
    prodstatement : INT ID
    '''
    p[0] = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(p[0])

def p_prodstatement_declaration_string(p):
    '''
    prodstatement : STRING ID
    '''
    p[0] = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(p[0])

def p_prodstatement_declaration_boolean(p):
    '''
    prodstatement : BOOLEAN ID
    '''
    p[0] = Nodo('declaration', [Nodo(p[2]), Nodo(p[1])])
    Nodo.setParent(p[0])

def p_numericexp_id(p):
    '''
    numericexp : ID '+' numericexp
        | ID '-' numericexp
        | ID '/' numericexp
        | ID '*' numericexp 
        | ID '^' numericexp
    '''
    p[0] = []

    p[0].append(p[1])
    p[0].append(p[2])

    for i in p[3]:
        p[0].append(i)

def p_numericexp_num(p):
    '''
    numericexp : num
    '''
    p[0] = [p[1]]

def p_numericexp_operator(p):
    '''
    numericexp : numericexp operator numericexp
    '''
    p[0] = []

    for i in p[1]:
        p[0].append(i)

    p[0].append(p[2])

    for i in p[3]:
        p[0].append(i)

def p_numericexp_group(p):
    '''numericexp : '(' numericexp ')' '''
    p[0] = p[2]

def p_num(p):
    '''
    num : INUMBER
        | FNUMBER
    '''
    p[0] = p[1]

def p_operator(p):
    '''
    operator : '+'
        | '-'
        | '*'
        | '/'
        | '^'
    '''
    p[0] = p[1]

def p_concat(p):
    '''
    concat : ID
        | STR
    '''
    p[0] = 0
    p[0] = Nodo(p[1])

def p_stringexp_one(p):
    '''
    stringexp : concat
    '''
    p[0] = p[1]
    
def p_stringexp_concat_id(p):
    '''
    stringexp : ID '+' stringexp
    '''
    p[0] = Nodo('concat', [Nodo(p[1]), p[3]])
    Nodo.setParent(p[0])

def p_stringexp_concat_id_id(p):
    '''
    stringexp : ID '+' ID
    '''
    p[0] = Nodo('concat', [Nodo(p[1]), p[3]])
    Nodo.setParent(p[0])

def p_stringexp_concat_string(p):
    '''
    stringexp : stringexp '+' stringexp
    '''
    p[0] = Nodo('concat', [p[1], p[3]])
    Nodo.setParent(p[0])
    
def p_bool(p):
    '''
    bool : TRUE
        | FALSE
        | ID
    '''
    p[0] = Nodo(p[1])

def p_booleanexp(p):
    '''
    booleanexp : booleanexp AND booleanexp
        | booleanexp OR booleanexp
        | booleanexp EQUAL booleanexp
        | booleanexp DIFFERENT booleanexp
    '''
    p[0] = Nodo(p[2], [p[1], p[3]])
    Nodo.setParent(p[0])

def p_booleanexp_equal_dif(p):
    '''
    booleanexp : ID EQUAL stringexp
        | ID DIFFERENT stringexp
    '''
    p[0] = Nodo(p[2], [Nodo(p[1]), p[3]])
    Nodo.setParent(p[0])

def p_booleanexp_num(p):
    '''
    booleanexp : ID EQUAL numericexp
        | ID DIFFERENT numericexp
        | ID GREATERTHAN numericexp
        | ID LESSTHAN numericexp
        | ID '<' numericexp
        | ID '>' numericexp
    '''
    p[0] = Nodo(p[2], [Nodo(p[1]), parseTree(p[3], False)])
    Nodo.setParent(p[0])

def p_boolop(p):
    '''
    boolop : strcomp
        | numcomp
        | bool
    '''
    p[0] = p[1]

def p_comparation(p):
    '''
    comp : EQUAL
        | DIFFERENT
        | GREATERTHAN
        | LESSTHAN
        | '<'
        | '>'
    '''
    p[0] = p[1]

def p_str_comparation(p):
    '''
    strcomp : stringexp DIFFERENT stringexp
        | stringexp  EQUAL stringexp
    '''
    p[0] = Nodo(p[2], [p[1], p[3]])
    Nodo.setParent(p[0])

def p_num_comparation(p):
    '''
    numcomp : numericexp comp numericexp
    '''
    p[0] = Nodo(p[2], [parseTree(p[1], False), parseTree(p[3], False)])
    Nodo.setParent(p[0])
        
def p_epsilon(p):
    'epsilon :'
    pass

def p_error(p):
    print("Syntax error.")
    pass

def p_statement_print(p): 
    '''statement : PRINT '(' ID ')' '''
    p[0] = Nodo('print', [Nodo(p[3]), Nodo(p[3])])
    Nodo.setParent(p[0])

parser = yacc.yacc()
root = parser.parse(lexer=lx, input=open("/Users/nicolecarrillo/Desktop/ply-compiler-master/input.txt").read())

variable = { }

def StrCheck(node):
    node.ptype = "string"

def numCheck(node):
    countInt = 0
    countFlt = 0
    recursion = True
    if node.children:
        if node.type in ["+", "-", "/", "*", "^"]:
            for child in node.children:
                recursion = True
                numCheck(child)
            if node.children[0].ptype == node.children[1].ptype:
                node.ptype = node.children[0].ptype
            else:
                for i in range(len(node.children)):
                    if node.children[i].ptype == "int":
                        countInt = countInt + 1;
                        parseNode = Nodo('int2float', ptype="float")
                        node.children[i].parent = parseNode
                        parseNode.children = [node.children[i]]
                        node.children[i] = parseNode
                countFlt = countFlt + 1;
                node.ptype = "float"
        else:
            print("THERE IS AN ERROR AT THE NUMBERS DECLARATION, numCheck 548")
    else:
        if(re.fullmatch(r'\d+?', node.type)):
            node.ptype = "int"
            countInt = countInt + 1;
        elif(re.fullmatch(r'((\d+)(\.\d+))', node.type)):
            node.ptype = "float"
            countFlt = countFlt + 1;
        else:
            if((node.type[0] == "-" and not Var.isInScope(Var, node, node.type[1:], variable))):
                print("VAR" + node.type + " NEEDS TO BE DECLARED FIRST")
            varType = Var.getVarType(Var, node, node.type, variable)
            if varType == "string" or varType == "boolean":
                print("CAN'T CONVERT A STRING TO NUMBER PLIS CHECK")
            node.ptype = varType
    if(not recursion):
        print("THERE IS AN ERROR AT NUMCHECK, NOT ENTERING RECURSION")

def BoolCheck(node):
    if not node.children:
        if node.type != "true" and node.type != "false":
            if not Var.isInScope(Var, node, node.type, variable):
                print("Variable " + node.type + " NEEDS TO BE DECLARED BEFORE USING IT")
    elif node.type in ["==", "!="]:
        if(node.children[0].type in ["+", "-", "/", "*", "^"]):
            numCheck(node.children[0])
            numCheck(node.children[1])
        elif(node.children[0].type in ["==", "!=", "<", ">", ">=", "<=", "and", "or", "true", "false"]):
            BoolCheck(node.children[0])
    elif node.type in [">", "<", ">=", "<="]:
        numCheck(node.children[0])
        numCheck(node.children[1])
    elif node.type in ["and", "or"]:
            BoolCheck(node.children[0])
    node.ptype = "boolean"

#File
inputData = []
with open('data.txt') as file:
    inputData = file.readlines()

for data in inputData:
    yacc.parse(data)