
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = "AND BOOLEAN DIFFERENT ELIF ELSE EQUAL FALSE FLOAT FNUMBER FOR GREATERTHAN ID IF INT INUMBER LESSTHAN OR PRINT STR STRING TRUE WHILE\n    block : statement\n        | statement block\n    \n    statement : prodstatement ';'\n        | control\n    \n    prodstatement : INT ID '=' numericexp\n    \n    prodstatement : FLOAT ID '=' numericexp\n    \n    prodstatement : STRING ID '=' stringexp\n    \n    prodstatement : BOOLEAN ID '=' booleanexp\n    \n    expr : numericexp\n    \n    expr : booleanexp\n    \n    expr : stringexp\n    \n    control : IF '(' booleanexp ')' '{' block '}' elif else\n    \n    elif : ELIF '(' booleanexp ')' '{' block '}' elif\n        | epsilon\n    \n    else : ELSE '{' block '}'\n        | epsilon\n    \n    control : WHILE '(' booleanexp ')' '{' block '}'\n    \n    control : FOR '(' prodstatement ';' booleanexp ';' prodstatement ')' '{' block '}'\n    \n    prodstatement : ID '=' expr\n    \n    prodstatement : FLOAT ID \n    \n    prodstatement : INT ID\n    \n    prodstatement : STRING ID\n    \n    prodstatement : BOOLEAN ID\n    \n    numericexp : ID '+' numericexp\n        | ID '-' numericexp\n        | ID '/' numericexp\n        | ID '*' numericexp \n        | ID '^' numericexp\n    \n    numericexp : num\n    \n    numericexp : numericexp operator numericexp\n    numericexp : '(' numericexp ')' \n    num : INUMBER\n        | FNUMBER\n    \n    operator : '+'\n        | '-'\n        | '*'\n        | '/'\n        | '^'\n    \n    concat : ID\n        | STR\n    \n    stringexp : concat\n    \n    stringexp : ID '+' stringexp\n    \n    stringexp : ID '+' ID\n    \n    stringexp : stringexp '+' stringexp\n    \n    bool : TRUE\n        | FALSE\n        | ID\n    \n    booleanexp : booleanexp AND booleanexp\n        | booleanexp OR booleanexp\n        | booleanexp EQUAL booleanexp\n        | booleanexp DIFFERENT booleanexp\n    \n    booleanexp : ID EQUAL stringexp\n        | ID DIFFERENT stringexp\n    \n    booleanexp : ID EQUAL numericexp\n        | ID DIFFERENT numericexp\n        | ID GREATERTHAN numericexp\n        | ID LESSTHAN numericexp\n        | ID '<' numericexp\n        | ID '>' numericexp\n    \n    boolop : strcomp\n        | numcomp\n        | bool\n    \n    comp : EQUAL\n        | DIFFERENT\n        | GREATERTHAN\n        | LESSTHAN\n        | '<'\n        | '>'\n    \n    strcomp : stringexp DIFFERENT stringexp\n        | stringexp  EQUAL stringexp\n    \n    numcomp : numericexp comp numericexp\n    epsilon :statement : PRINT '(' ID ')' "
    
_lr_action_items = {'PRINT':([0,2,4,15,45,103,104,112,113,115,117,119,121,123,124,129,130,131,133,134,],[5,5,-4,-3,-73,5,5,-72,-17,-72,-14,-12,-16,5,5,-18,-15,5,-72,-13,]),'INT':([0,2,4,15,24,45,103,104,110,112,113,115,117,119,121,123,124,129,130,131,133,134,],[7,7,-4,-3,7,-73,7,7,7,-72,-17,-72,-14,-12,-16,7,7,-18,-15,7,-72,-13,]),'FLOAT':([0,2,4,15,24,45,103,104,110,112,113,115,117,119,121,123,124,129,130,131,133,134,],[8,8,-4,-3,8,-73,8,8,8,-72,-17,-72,-14,-12,-16,8,8,-18,-15,8,-72,-13,]),'STRING':([0,2,4,15,24,45,103,104,110,112,113,115,117,119,121,123,124,129,130,131,133,134,],[9,9,-4,-3,9,-73,9,9,9,-72,-17,-72,-14,-12,-16,9,9,-18,-15,9,-72,-13,]),'BOOLEAN':([0,2,4,15,24,45,103,104,110,112,113,115,117,119,121,123,124,129,130,131,133,134,],[10,10,-4,-3,10,-73,10,10,10,-72,-17,-72,-14,-12,-16,10,10,-18,-15,10,-72,-13,]),'ID':([0,2,4,7,8,9,10,15,16,17,22,23,24,32,37,38,39,40,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,77,101,102,103,104,106,110,112,113,115,117,119,121,122,123,124,129,130,131,133,134,],[6,6,-4,18,19,20,21,-3,25,26,42,42,6,69,69,69,72,42,-73,78,69,69,69,69,85,85,69,69,69,69,69,-34,-35,-36,-37,-38,42,42,42,42,72,42,69,107,6,6,111,6,-72,-17,-72,-14,-12,-16,42,6,6,-18,-15,6,-72,-13,]),'IF':([0,2,4,15,45,103,104,112,113,115,117,119,121,123,124,129,130,131,133,134,],[11,11,-4,-3,-73,11,11,-72,-17,-72,-14,-12,-16,11,11,-18,-15,11,-72,-13,]),'WHILE':([0,2,4,15,45,103,104,112,113,115,117,119,121,123,124,129,130,131,133,134,],[12,12,-4,-3,-73,12,12,-72,-17,-72,-14,-12,-16,12,12,-18,-15,12,-72,-13,]),'FOR':([0,2,4,15,45,103,104,112,113,115,117,119,121,123,124,129,130,131,133,134,],[13,13,-4,-3,-73,13,13,-72,-17,-72,-14,-12,-16,13,13,-18,-15,13,-72,-13,]),'$end':([1,2,4,14,15,45,112,113,115,117,119,121,129,130,133,134,],[0,-1,-4,-2,-3,-73,-72,-17,-72,-14,-12,-16,-18,-15,-72,-13,]),'}':([2,4,14,15,45,108,109,112,113,115,117,119,121,126,127,129,130,132,133,134,],[-1,-4,-2,-3,-73,112,113,-72,-17,-72,-14,-12,-16,129,130,-18,-15,133,-72,-13,]),';':([3,18,19,20,21,26,27,28,29,30,31,33,34,35,36,44,70,71,72,73,74,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,105,107,111,],[15,-21,-20,-22,-23,-39,-19,-9,-10,-11,-29,-41,-32,-33,-40,77,-5,-6,-39,-7,-8,-39,-24,-42,-25,-26,-27,-28,-39,-52,-54,-53,-55,-56,-57,-58,-59,-30,-48,-49,-50,-51,-44,-31,110,-39,-39,]),'(':([5,11,12,13,17,32,37,38,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,101,106,116,],[16,22,23,24,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,-34,-35,-36,-37,-38,32,32,122,]),'=':([6,18,19,20,21,],[17,37,38,39,40,]),'INUMBER':([17,32,37,38,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,101,106,],[34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,-34,-35,-36,-37,-38,34,34,]),'FNUMBER':([17,32,37,38,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,101,106,],[35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,-34,-35,-36,-37,-38,35,35,]),'STR':([17,39,46,51,52,67,102,106,],[36,36,36,36,36,36,36,36,]),')':([18,19,20,21,25,26,27,28,29,30,31,33,34,35,36,41,43,68,70,71,72,73,74,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,107,111,114,125,],[-21,-20,-22,-23,45,-39,-19,-9,-10,-11,-29,-41,-32,-33,-40,75,76,100,-5,-6,-39,-7,-8,-39,-24,-42,-25,-26,-27,-28,-39,-52,-54,-53,-55,-56,-57,-58,-59,-30,-48,-49,-50,-51,-44,-31,-39,-39,118,128,]),'+':([26,28,30,31,33,34,35,36,68,69,70,71,72,73,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,99,100,107,111,],[46,58,67,-29,-41,-32,-33,-40,58,101,58,58,102,67,46,58,67,58,58,58,58,106,67,58,67,58,58,58,58,58,58,67,-31,102,106,]),'-':([26,28,31,34,35,68,69,70,71,78,79,81,82,83,84,85,87,89,90,91,92,93,94,100,111,],[47,59,-29,-32,-33,59,47,59,59,47,59,59,59,59,59,47,59,59,59,59,59,59,59,-31,47,]),'/':([26,28,31,34,35,68,69,70,71,78,79,81,82,83,84,85,87,89,90,91,92,93,94,100,111,],[48,61,-29,-32,-33,61,48,61,61,48,61,61,61,61,61,48,61,61,61,61,61,61,61,-31,48,]),'*':([26,28,31,34,35,68,69,70,71,78,79,81,82,83,84,85,87,89,90,91,92,93,94,100,111,],[49,60,-29,-32,-33,60,49,60,60,49,60,60,60,60,60,49,60,60,60,60,60,60,60,-31,49,]),'^':([26,28,31,34,35,68,69,70,71,78,79,81,82,83,84,85,87,89,90,91,92,93,94,100,111,],[50,62,-29,-32,-33,62,50,62,62,50,62,62,62,62,62,50,62,62,62,62,62,62,62,-31,50,]),'EQUAL':([26,29,31,33,34,35,36,41,42,43,72,74,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,105,107,111,125,],[51,65,-29,-41,-32,-33,-40,65,51,65,-39,65,-24,-42,-25,-26,-27,-28,-39,-52,-54,-53,-55,-56,-57,-58,-59,-30,65,65,65,65,-44,-31,65,-39,-39,65,]),'DIFFERENT':([26,29,31,33,34,35,36,41,42,43,72,74,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,105,107,111,125,],[52,66,-29,-41,-32,-33,-40,66,52,66,-39,66,-24,-42,-25,-26,-27,-28,-39,-52,-54,-53,-55,-56,-57,-58,-59,-30,66,66,66,66,-44,-31,66,-39,-39,66,]),'GREATERTHAN':([26,42,],[53,53,]),'LESSTHAN':([26,42,],[54,54,]),'<':([26,42,],[55,55,]),'>':([26,42,],[56,56,]),'AND':([29,31,33,34,35,36,41,43,72,74,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,105,107,111,125,],[63,-29,-41,-32,-33,-40,63,63,-39,63,-24,-42,-25,-26,-27,-28,-39,-52,-54,-53,-55,-56,-57,-58,-59,-30,63,63,63,63,-44,-31,63,-39,-39,63,]),'OR':([29,31,33,34,35,36,41,43,72,74,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,105,107,111,125,],[64,-29,-41,-32,-33,-40,64,64,-39,64,-24,-42,-25,-26,-27,-28,-39,-52,-54,-53,-55,-56,-57,-58,-59,-30,64,64,64,64,-44,-31,64,-39,-39,64,]),'{':([75,76,118,120,128,],[103,104,123,124,131,]),'ELIF':([112,133,],[116,116,]),'ELSE':([112,115,117,133,134,],[-72,120,-14,-72,-13,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'block':([0,2,103,104,123,124,131,],[1,14,108,109,126,127,132,]),'statement':([0,2,103,104,123,124,131,],[2,2,2,2,2,2,2,]),'prodstatement':([0,2,24,103,104,110,123,124,131,],[3,3,44,3,3,114,3,3,3,]),'control':([0,2,103,104,123,124,131,],[4,4,4,4,4,4,4,]),'expr':([17,],[27,]),'numericexp':([17,32,37,38,46,47,48,49,50,51,52,53,54,55,56,57,101,106,],[28,68,70,71,79,81,82,83,84,87,89,90,91,92,93,94,79,79,]),'booleanexp':([17,22,23,40,63,64,65,66,77,122,],[29,41,43,74,95,96,97,98,105,125,]),'stringexp':([17,39,46,51,52,67,102,106,],[30,73,80,86,88,99,80,80,]),'num':([17,32,37,38,46,47,48,49,50,51,52,53,54,55,56,57,101,106,],[31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,]),'concat':([17,39,46,51,52,67,102,106,],[33,33,33,33,33,33,33,33,]),'operator':([28,68,70,71,79,81,82,83,84,87,89,90,91,92,93,94,],[57,57,57,57,57,57,57,57,57,57,57,57,57,57,57,57,]),'elif':([112,133,],[115,134,]),'epsilon':([112,115,133,],[117,121,117,]),'else':([115,],[119,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> block","S'",1,None,None,None),
  ('block -> statement','block',1,'p_block','compiler.py',192),
  ('block -> statement block','block',2,'p_block','compiler.py',193),
  ('statement -> prodstatement ;','statement',2,'p_statement','compiler.py',205),
  ('statement -> control','statement',1,'p_statement','compiler.py',206),
  ('prodstatement -> INT ID = numericexp','prodstatement',4,'p_prodstatement_assdec_num_int','compiler.py',212),
  ('prodstatement -> FLOAT ID = numericexp','prodstatement',4,'p_prodstatement_assdec_num_float','compiler.py',221),
  ('prodstatement -> STRING ID = stringexp','prodstatement',4,'p_prodstatement_assdec_strings','compiler.py',230),
  ('prodstatement -> BOOLEAN ID = booleanexp','prodstatement',4,'p_prodstatement_assdec_boolean','compiler.py',239),
  ('expr -> numericexp','expr',1,'p_expr_num','compiler.py',248),
  ('expr -> booleanexp','expr',1,'p_expr_bool','compiler.py',254),
  ('expr -> stringexp','expr',1,'p_expr_str','compiler.py',260),
  ('control -> IF ( booleanexp ) { block } elif else','control',9,'p_control_if','compiler.py',266),
  ('elif -> ELIF ( booleanexp ) { block } elif','elif',8,'p_control_elif','compiler.py',282),
  ('elif -> epsilon','elif',1,'p_control_elif','compiler.py',283),
  ('else -> ELSE { block }','else',4,'p_control_else','compiler.py',297),
  ('else -> epsilon','else',1,'p_control_else','compiler.py',298),
  ('control -> WHILE ( booleanexp ) { block }','control',7,'p_control_while','compiler.py',311),
  ('control -> FOR ( prodstatement ; booleanexp ; prodstatement ) { block }','control',11,'p_control_for','compiler.py',318),
  ('prodstatement -> ID = expr','prodstatement',3,'p_prodstatement_assigment','compiler.py',325),
  ('prodstatement -> FLOAT ID','prodstatement',2,'p_prodstatement_declaration_float','compiler.py',332),
  ('prodstatement -> INT ID','prodstatement',2,'p_prodstatement_declaration_int','compiler.py',339),
  ('prodstatement -> STRING ID','prodstatement',2,'p_prodstatement_declaration_string','compiler.py',346),
  ('prodstatement -> BOOLEAN ID','prodstatement',2,'p_prodstatement_declaration_boolean','compiler.py',353),
  ('numericexp -> ID + numericexp','numericexp',3,'p_numericexp_id','compiler.py',360),
  ('numericexp -> ID - numericexp','numericexp',3,'p_numericexp_id','compiler.py',361),
  ('numericexp -> ID / numericexp','numericexp',3,'p_numericexp_id','compiler.py',362),
  ('numericexp -> ID * numericexp','numericexp',3,'p_numericexp_id','compiler.py',363),
  ('numericexp -> ID ^ numericexp','numericexp',3,'p_numericexp_id','compiler.py',364),
  ('numericexp -> num','numericexp',1,'p_numericexp_num','compiler.py',376),
  ('numericexp -> numericexp operator numericexp','numericexp',3,'p_numericexp_operator','compiler.py',382),
  ('numericexp -> ( numericexp )','numericexp',3,'p_numericexp_group','compiler.py',395),
  ('num -> INUMBER','num',1,'p_num','compiler.py',400),
  ('num -> FNUMBER','num',1,'p_num','compiler.py',401),
  ('operator -> +','operator',1,'p_operator','compiler.py',407),
  ('operator -> -','operator',1,'p_operator','compiler.py',408),
  ('operator -> *','operator',1,'p_operator','compiler.py',409),
  ('operator -> /','operator',1,'p_operator','compiler.py',410),
  ('operator -> ^','operator',1,'p_operator','compiler.py',411),
  ('concat -> ID','concat',1,'p_concat','compiler.py',417),
  ('concat -> STR','concat',1,'p_concat','compiler.py',418),
  ('stringexp -> concat','stringexp',1,'p_stringexp_one','compiler.py',425),
  ('stringexp -> ID + stringexp','stringexp',3,'p_stringexp_concat_id','compiler.py',431),
  ('stringexp -> ID + ID','stringexp',3,'p_stringexp_concat_id_id','compiler.py',438),
  ('stringexp -> stringexp + stringexp','stringexp',3,'p_stringexp_concat_string','compiler.py',445),
  ('bool -> TRUE','bool',1,'p_bool','compiler.py',452),
  ('bool -> FALSE','bool',1,'p_bool','compiler.py',453),
  ('bool -> ID','bool',1,'p_bool','compiler.py',454),
  ('booleanexp -> booleanexp AND booleanexp','booleanexp',3,'p_booleanexp','compiler.py',460),
  ('booleanexp -> booleanexp OR booleanexp','booleanexp',3,'p_booleanexp','compiler.py',461),
  ('booleanexp -> booleanexp EQUAL booleanexp','booleanexp',3,'p_booleanexp','compiler.py',462),
  ('booleanexp -> booleanexp DIFFERENT booleanexp','booleanexp',3,'p_booleanexp','compiler.py',463),
  ('booleanexp -> ID EQUAL stringexp','booleanexp',3,'p_booleanexp_equal_dif','compiler.py',470),
  ('booleanexp -> ID DIFFERENT stringexp','booleanexp',3,'p_booleanexp_equal_dif','compiler.py',471),
  ('booleanexp -> ID EQUAL numericexp','booleanexp',3,'p_booleanexp_num','compiler.py',478),
  ('booleanexp -> ID DIFFERENT numericexp','booleanexp',3,'p_booleanexp_num','compiler.py',479),
  ('booleanexp -> ID GREATERTHAN numericexp','booleanexp',3,'p_booleanexp_num','compiler.py',480),
  ('booleanexp -> ID LESSTHAN numericexp','booleanexp',3,'p_booleanexp_num','compiler.py',481),
  ('booleanexp -> ID < numericexp','booleanexp',3,'p_booleanexp_num','compiler.py',482),
  ('booleanexp -> ID > numericexp','booleanexp',3,'p_booleanexp_num','compiler.py',483),
  ('boolop -> strcomp','boolop',1,'p_boolop','compiler.py',490),
  ('boolop -> numcomp','boolop',1,'p_boolop','compiler.py',491),
  ('boolop -> bool','boolop',1,'p_boolop','compiler.py',492),
  ('comp -> EQUAL','comp',1,'p_comparation','compiler.py',498),
  ('comp -> DIFFERENT','comp',1,'p_comparation','compiler.py',499),
  ('comp -> GREATERTHAN','comp',1,'p_comparation','compiler.py',500),
  ('comp -> LESSTHAN','comp',1,'p_comparation','compiler.py',501),
  ('comp -> <','comp',1,'p_comparation','compiler.py',502),
  ('comp -> >','comp',1,'p_comparation','compiler.py',503),
  ('strcomp -> stringexp DIFFERENT stringexp','strcomp',3,'p_str_comparation','compiler.py',509),
  ('strcomp -> stringexp EQUAL stringexp','strcomp',3,'p_str_comparation','compiler.py',510),
  ('numcomp -> numericexp comp numericexp','numcomp',3,'p_num_comparation','compiler.py',517),
  ('epsilon -> <empty>','epsilon',0,'p_epsilon','compiler.py',523),
  ('statement -> PRINT ( ID )','statement',4,'p_statement_print','compiler.py',531),
]
