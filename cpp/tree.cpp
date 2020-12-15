#include <iostream>
#include "tree.h"
using namespace std;


struct NodeT {
int elem;
NodeT* left;
NodeT* right;
};
typedef NodeT* Tree;


Tree emptyT(){
    return NULL;
}

Tree nodeT(int el, Tree izq, Tree der){
    NodeT* tree = new NodeT;
    tree->elem = el;
    tree->left = izq;
    tree->right = der;

    return tree;
}

bool isEmptyT(Tree t){
    return t == NULL;
}

int rootT(Tree t){
    return t->elem;
}

Tree left(Tree t){
    return t->left;
}
Tree right(Tree t){
    return t->right;
}

