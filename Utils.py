class Nodo:

    counter = 0

    def __init__(self,type,children=None):
        parent = None
        ptype=None
        global counter 
        counter = 0
        self.type = type
        if children:
            self.children = children
            counter = counter + 1
        else:
            self.children = [ ]
            counter = 1
        if parent:
            self.parent = parent
        else:
            self.parent = None
        if ptype:
            self.ptype = ptype
        else:
            self.ptype = None

    def setParent(node):
        for child in node.children:
            child.parent = node
    
    def findScopeNode(self, node):
        if(node.type in ["block", "if", "elif", "else", "while"]): 
            return node
        if(node.parent):
            return self.findScopeNode(self, node.parent)
        return Nodo('Self')
    
    def getTotalNodes():
        global counter
        print("TOTAL NODES COUNTER: " + counter)

class Var:
    def __init__(self,value,typ):
        self.typ = typ
        self.value = value

    
