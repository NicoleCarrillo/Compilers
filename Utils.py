# CLASS FOR TREE CONSTRUCTION

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
        if(node.type in ["block","for", "if", "elif", "else", "while"]): 
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

    def getVarType(self, node, varName, variables):
        if node in variables.keys() and varName in (o.value for o in variables[node]):
            return [x for x in variables[node] if x.value == varName][0].typ
        if node.type == "else" or node.type == "elif": 
            currentNode = node.parent
            while((currentNode.type == "if" or currentNode.type == "elif") and currentNode.parent): 
                currentNode = currentNode.parent
            return self.getVarType(self, currentNode, varName, variables)
        if node.parent:
            return self.getVarType(self, node.parent, varName, variables)
        return None

    def isVarInTree(self, node, varName):
        if node.children:
            for child in node.children:
                self.isVarInTree(self,child, varName)

    def isInScope(self, node, varName, variables):
        if node in variables.keys():
            if(varName in (o.value for o in variables[node])):
                return True
        if node.type == "else" or node.type == "elif": 
            currentNode = node.parent
            while((currentNode.type == "if" or currentNode.type == "elif")): 
                currentNode = currentNode.parent
                if currentNode == None:
                    return False
            return self.isInScope(self, currentNode, varName, variables)
        if node.parent:
            return self.isInScope(self, node.parent, varName, variables)
        return False
