# -*- coding: utf-8 -*-
"""
Yan Wang
Binary Search Tree. BST is a binary tree. Each node has the feature that 
left child node has value < current node; right child node has value > current node.
The searching complexity is O(logN)
Implementation of BST in python3
"""
class Node:
    # Constructor of node
    def __init__(self, val, left = None, right = None, parent = None):
        self.val = val
        self.left = left
        self.right = right
        self.parent = parent
        
    def get(self):
        return(self.val)
        
    def set(self, val):
        self.val = val
        
    def hasLeft(self):
        return self.left
    
    def hasRight(self):
        return self.right
    
    def isLeft(self):
        return self.parent and self.parent.left == self
    
    def isRight(self):
        return self.parent and self.parent.right == self
    
    def getChildren(self):
        children = []
        if(self.left != None):
            children.append(self.left)
        if(self.right !=None):
            children.append(self.rights)
        return(children)
        
class BST:
    def __init__(self):
        self.root = None
    
    def setRoot(self, val):
        self.root = Node(val)
        
    def insert(self, val):
        if(self.root is None):
            self.setRoot(val)
        else:
            self.insertNode(self.root, val)
        
    def insertNode(self, curr, val):
        # If smaller than left, insert the Node(val) to the left
        if(val<=curr.val):
            if(curr.left != None):
                self.insertNode(curr.left, val)
            else:
                curr.left = Node(val)
        elif(val>curr.val):
            if(curr.right != None):
                self.insertNode(curr.right, val)
            else:
                curr.right = Node(val)
        
    def search(self, val):
        return(self.searchNode(self.root, val))
        
    def searchNode(self, curr, val):
        if(curr is None):
            return(False)
        elif(val == curr.val):
            return(True)
        elif(val < curr.val):
            return(self.searchNode(curr.left, val))
        else:
            return(self.searchNode(curr.right, val))
    

if __name__ == "__main__":
    