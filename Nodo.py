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