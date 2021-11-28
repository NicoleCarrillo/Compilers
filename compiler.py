# ---------------------------------------------------------------------------------------------
# comp.py
# A simple compiler. 
# This is based on O'Reilly's "Lex and Yacc", calculator

import ply.yacc as yacc
import ply.lex as lex
from Utils import Nodo
from Utils import Var
import re

# ! --------------------------------------- LEXER ---------------------------------------------

# ? -------------------------------------------------------------------------------------------
# ? RESERVED VARIABLES, TOKENS, CONSTANTS DECLARATIONS

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
    'for' : 'FOR',
    'boolean' : 'BOOLEAN',
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

# ? -------------------------------------------------------------------------------------------
# & -------------------------------------------------------------------------------------------
# & VALUE OF EACH VARIABLE DEFINITION

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

# * THIS IS CALLED AT THE SEMATIC PART 
lx = lex.lex()

# & -------------------------------------------------------------------------------------------


# ! ------------------------------------ END LEXER --------------------------------------------

# ! ------------------------------------- PARSER ----------------------------------------------

# ? -------------------------------------------------------------------------------------------
# ? PARSE TREE GENERATION FOR ERRORS DETECTION 

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

# ? -------------------------------------------------------------------------------------------
# ^ -------------------------------------------------------------------------------------------
# ^ VARIBLE FUNCTION DEFINITION

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

def p_control_for(p):
    '''
    control : FOR '(' prodstatement ';' booleanexp ';' prodstatement ')' '{' block '}'
    '''
    p[0] = Nodo('for', [p[3], p[5], p[7], p[10]])
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

# ^ -------------------------------------------------------------------------------------------

# ! ------------------------------------- END PARSER ------------------------------------------

parser = yacc.yacc()
root = parser.parse(lexer=lx, input=open("input.txt").read())

variable = { }

# ! ------------------------------------- SEMANTICS ------------------------------------------

# & -------------------------------------------------------------------------------------------
# & TYPE VARIABLE CHECK

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
                        countInt = countInt + 1
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
    
# & -------------------------------------------------------------------------------------------

# * -------------------------------------------------------------------------------------------
# * VARIABLES INITIALIZATION, ERROR CHECKING 

def initVariables(root):
    counter = 0
    if(root.type == "declaration"):
        scopeNode = Nodo.findScopeNode(Nodo,root)
        if Var.isInScope(Var,root, root.children[0].type, variable):
            counter = counter + 1;
            print("VAR " + root.children[0].type + " IS ALREADY DEFINED, errors" + counter)
        if scopeNode in variable.keys():
            variable[scopeNode].append(Var(root.children[0].type, root.children[1].type))
        else:
            variable[scopeNode] = [Var(root.children[0].type, root.children[1].type)]
    if root.children:
        for child in root.children:
            initVariables(child)

# * -------------------------------------------------------------------------------------------
# ? -------------------------------------------------------------------------------------------
# *? ORQUESTER, GENERAL COMANDER FOR THE SEMANTIC ANALYSIS

def semantic(root):
    if(root.type == "assignment"):
        if root.children[0].type == "declaration":
            correctType = root.children[0].children[1].type
            varName = root.children[0].children[0].type
            Var.isVarInTree(Var, root.children[1], varName)
        elif (not Var.isInScope(Var,root, root.children[0].type, variable)):
            print("VARIABLE " + root.children[0].type + "NEEDS TO BE DECLARED")
        else:
            correctType = Var.getVarType(Var, root, root.children[0].type, variable)
        if correctType == "int" or correctType == "float":
            numCheck(root.children[1])
            if correctType == "float" and root.children[1].ptype == "int":
                parseNode = Nodo('int2float', ptype="float")
                root.children[1].parent = parseNode
                parseNode.children = [root.children[1]]
                root.children[1] = parseNode
        elif correctType == "boolean":
            BoolCheck(root.children[1])
        elif correctType == "string":
            StrCheck(root.children[1])
    elif(root.type in ["==", "!=", "<", ">", ">=", "<=", "and", "or", "true", "false"]):
        BoolCheck(root)
    elif(root.type in ["concat", "num2string"]):
        StrCheck(root)
    elif(root.type in ["+", "-", "/", "*", "^"]):
        numCheck(root)
    elif root.children:
        for child in root.children:
            semantic(child)

initVariables(root)
semantic(root)

# ? -------------------------------------------------------------------------------------------

# ! ---------------------------------- END SEMANTICS ------------------------------------------

# ! ----------------------------------------- TAC ---------------------------------------------

tokenNodes = {}
extraNodes = {}
spaceNodes = {}
endNodes = {}

tokenCounter = 1
destinyCounter = 1
gotoCounter = 1

finalFile = open("output.txt", "w")

def space_gen():
    finalFile.write('\n')
    finalFile.write('\n')

def block_else_code(node):
    for child in node.children:
        threeAddressCode(child)

def declaration_code(node, tokenNodes):
    varType = node.children[1].type
    finalFile.write(varType + " declaration( " + node.children[0].type + " )\n")
    tokenNodes[node] = node.children[0].type

def assigment_code(node, tokenNodes):
    # * preorder visit
    threeAddressCode(node.children[0])
    threeAddressCode(node.children[1])
    finalFile.write(tokenNodes[node.children[0]] + " := " + tokenNodes[node.children[1]] + "\n")

def numconvertion_code(node, tokenCounter, tokenNodes):
    threeAddressCode(node.children[0])
    finalFile.write("t" + str(tokenCounter) + " := " + node.type + "(" + tokenNodes[node.children[0]] + ")" + "\n")
    tokenNodes[node] = "t" + str(tokenCounter)

def numoperators_code(node, tokenCounter, tokenNodes):
    threeAddressCode(node.children[0])
    threeAddressCode(node.children[1])
    finalFile.write("t" + str(tokenCounter) + " := " + tokenNodes[node.children[0]] + " " + node.type + " " + tokenNodes[node.children[1]] + "\n")
    tokenNodes[node] = "t" + str(tokenCounter)

def logicaloperators_code(node, tokenCounter, tokenNodes, destinyCounter):
    threeAddressCode(node.children[0])
    threeAddressCode(node.children[1])
    finalFile.write("if (" + tokenNodes[node.children[0]] + " " + node.type + " " + tokenNodes[node.children[1]] + ") GOTO COSO" + str(destinyCounter) + "\n")
    finalFile.write("t " + str(tokenCounter) + " := false" + "\n")
    finalFile.write("GOTO COSO " + str(destinyCounter + 1) + "\n")
    space_gen()
    finalFile.write("COSO -> " + str(destinyCounter) + "\n")
    finalFile.write("t " + str(tokenCounter) + " := true" + "\n")
    space_gen()
    finalFile.write("COSO ->" + str(destinyCounter + 1) + "\n")
    tokenNodes[node] = "t" + str(tokenCounter)

def and_code(node, tokenCounter, tokenNodes, destinyCounter):
    threeAddressCode(node.children[0])
    threeAddressCode(node.children[1])
    finalFile.write("if (" + tokenNodes[node.children[0]] + ") GOTO COSO" + str(destinyCounter) + "\n")
    finalFile.write("t" + str(tokenCounter) + " := false" + "\n")
    finalFile.write("GOTO COSO" + str(destinyCounter + 2) + "\n")
    space_gen()
    finalFile.write("COSO -> " + str(destinyCounter) + "\n")
    finalFile.write("if (" + tokenNodes[node.children[1]] + ") GOTO COSO" + str(destinyCounter + 1) + "\n")
    finalFile.write("t" + str(tokenCounter) + " := false" + "\n")
    finalFile.write("GOTO COSO" + str(destinyCounter + 2) + "\n")
    space_gen()
    finalFile.write("COSO -> " + str(destinyCounter + 1) + "\n")
    finalFile.write("t" + str(tokenCounter) + " := true" + "\n")
    space_gen()
    finalFile.write("COSO -> " + str(destinyCounter + 2) + "\n")
    tokenNodes[node] = "t" + str(tokenCounter)

def or_code(node, tokenCounter, tokenNodes, destinyCounter):
    threeAddressCode(node.children[0])
    threeAddressCode(node.children[1])
    finalFile.write("if (" + tokenNodes[node.children[0]] + ") GOTO COSO" + str(destinyCounter) + "\n")
    finalFile.write("if (" + tokenNodes[node.children[1]] + ") GOTO COSO" + str(destinyCounter) + "\n")
    finalFile.write("t" + str(tokenCounter) + " := false" + "\n")
    finalFile.write("GOTO COSO" + str(destinyCounter + 1) + "\n")
    space_gen()
    finalFile.write("COSO -> " + str(destinyCounter) + "\n")
    finalFile.write("t" + str(tokenCounter) + " := true" + "\n")
    space_gen()
    finalFile.write("COSO -> " + str(destinyCounter + 1) + "\n")
    tokenNodes[node] = "t" + str(tokenCounter)

def threeAddressCode(node):
    global tokenCounter
    global destinyCounter
    global finalFile
    if node.type in ["block", "else"]:
        block_else_code(node)
    elif node.type == "declaration":
        declaration_code(node, tokenNodes)
    elif node.type == "assignment":
        assigment_code(node, tokenNodes)
    elif node.type == "int2float" :
        numconvertion_code(node, tokenCounter, tokenNodes)
        tokenCounter += 1
    elif node.type in ["+", "-", "/", "*", "^", "concat"]:
        numoperators_code(node, tokenCounter, tokenNodes)
        tokenCounter += 1
    elif node.type in ["!=", "==", "<", ">", ">=", "<="]:
        logicaloperators_code(node, tokenCounter, tokenNodes, destinyCounter)
        tokenCounter += 1
        destinyCounter += 2
    elif node.type == "and":
        and_code(node, tokenCounter, tokenNodes, destinyCounter)
        tokenCounter += 1
        destinyCounter += 3
    elif node.type == "or":
        or_code(node, tokenCounter, tokenNodes, destinyCounter)
        tokenCounter += 1
        destinyCounter += 2
    elif node.type in ["if", "elif"]: 
        threeAddressCode(node.children[0])
        finalFile.write("if (" + tokenNodes[node.children[0]] + ") GOTO COSO " + str(destinyCounter) + "\n")
        finalFile.write("GOTO COSO " + str(destinyCounter + 1) + "\n")
        space_gen()
        finalFile.write("COSO -> " + str(destinyCounter) + "\n")
        saveLCount = destinyCounter
        destinyCounter += 2
        threeAddressCode(node.children[1])
        saveLCount2 = destinyCounter
        if node.type == "if":
            finalFile.write("GOTO COSO " + str(destinyCounter) + "\n")
            extraNodes[node] = str(destinyCounter)
            destinyCounter += 1
        else:
            extraNodes[node] = extraNodes[node.parent]
            finalFile.write("GOTO COSO " + extraNodes[node.parent] + "\n")
        space_gen()
        finalFile.write("COSO -> " + str(saveLCount + 1) + "\n")
        if(len(node.children) > 2):
            threeAddressCode(node.children[2])
        if(len(node.children) > 3):
            threeAddressCode(node.children[3])
        if node.type == "if":
            space_gen()
            finalFile.write("COSO -> " + str(saveLCount2) + "\n")
    elif node.type == "while":
        threeAddressCode(node.children[0])
    elif node.type == "for":
        threeAddressCode(node.children[0])
        finalFile.write("COSO -> " + str(destinyCounter) + "\n")
        forJumps = destinyCounter
        destinyCounter += 1
        finalFile.write("COSO -> " + "FOR DECLARATION" + "\n")
        threeAddressCode(node.children[1])
        finalFile.write("COSO -> " + str(destinyCounter) + "\n")
        finalFile.write("if (" + tokenNodes[node.children[1]] + ") GOTO COSO -> " + str(destinyCounter + 1) + "\n")
        finalFile.write("GOTO COSO -> " + str(destinyCounter + 2) + "\n")
        finalFile.write("COSO -> " + str(destinyCounter + 1) + "\n")
        forJumps2 = destinyCounter
        destinyCounter += 3
        threeAddressCode(node.children[3])
        threeAddressCode(node.children[2])
        finalFile.write("GOTO COSO -> " + str(forJumps) + "\n")
        finalFile.write("COSO -> " + str(forJumps2 + 2) + "\n")
    elif node.type == 'print':
        finalFile.write("print( " + node.children[0].type + " )\n")
    elif not node.children:
        tokenNodes[node] = node.type

threeAddressCode(root)
print("IT COMPILES!!!!")