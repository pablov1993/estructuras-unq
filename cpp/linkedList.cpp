#include <iostream> 
#include "linkedList.h"
using namespace std;

struct NodoL {
int elem; // valor del nodo
NodoL* siguiente; // puntero al siguiente nodo
};
struct LinkedListSt {
int cantidad; // cantidad de elementos
NodoL* primero; // puntero al primer nodo
NodoL* actual; // puntero al nodo actual (para recorridos)
};
typedef LinkedListSt* LinkedList;

LinkedList nil(){
    LinkedListSt* list = new LinkedListSt;
    list->cantidad = 0;
    list->primero = NULL;
    list->actual = NULL;
    return list;
}

bool isEmpty(LinkedList xs){
    return xs->cantidad == 0;
}

int head(LinkedList xs){
    return xs->primero->elem;
}

int cantidad(LinkedList xs){
    return xs->cantidad;
} 

void cons(int x, LinkedList xs){
     
    NodoL* first = new NodoL;
    first->elem = x;
    first->siguiente = xs->primero;
    xs->cantidad = cantidad(xs) +1;
    xs->primero = first;    
}

void tail(LinkedList xs){

    NodoL* primeroViejo = xs->primero;
    xs->primero = xs->primero->siguiente;
    xs->cantidad = cantidad(xs) -1;
    delete primeroViejo;
}

int length(LinkedList xs){
    return cantidad(xs);
}

void snoc(int x, LinkedList xs){

    NodoL* nuevo = new NodoL;
    nuevo->elem  = x;
    nuevo->siguiente = NULL;
    if(xs->primero == NULL){
        xs->primero = nuevo;
    }
    else{
        NodoL* actual = xs->primero;
        while(actual->siguiente != NULL){
            actual = actual->siguiente;
        }
        actual->siguiente = nuevo;
        
    }

    xs->cantidad ++;    
    
}

void initialize(LinkedList xs){
    xs->actual = xs->primero;   
}

int current(LinkedList xs){
    return xs->actual->elem;
}

void setCurrent(int x, LinkedList xs){
    xs->actual->elem = x;
}

void next(LinkedList xs){
    xs->actual = xs->actual->siguiente;
}

bool finished(LinkedList xs){
    return xs->actual->siguiente == NULL;
}

void destroyL(LinkedList xs){
    
    while(!isEmpty(xs)){
        tail(xs);
    }

    delete xs;
}


