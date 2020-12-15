#include <iostream> 
#include "set.h"
using namespace std;

// LinkedList
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


// Set

struct NodoS {
int elem; // valor del nodo
NodoS* siguiente; // puntero al siguiente nodo
};
struct SetSt {
int cantidad; // cantidad de elementos diferentes
NodoS* primero; // puntero al primer nodo
};
typedef SetSt* Set;

Set emptyS(){
    Set set = new SetSt;
    set->cantidad = 0;
    set->primero = NULL;
    return set;
}

bool isEmptyS(Set s){
    return s->primero == NULL;
}

bool belongsS(int x, Set s){
    
    bool res = false;
    if(s->primero == NULL){
        res = false;
    }else{
        NodoS* actual = s->primero;
        while(actual->siguiente != NULL){
            if(actual->elem == x){
                res = true;
            }
            actual = actual->siguiente;
        }
        if(actual->elem == x){
                res = true;
            }
           
    }
    return res;
}

void addS(int x, Set s){
    
    NodoS* nuevo = new NodoS;
    nuevo->elem = x;
    nuevo->siguiente = s->primero;
    s->primero = nuevo;
    if (!belongsS   (x,s))
    {
        s->cantidad = s->cantidad +1;
    }
    
}

void removeS(int x, Set s){

    
    if(s->primero->elem == x){
        s->primero = s->primero->siguiente;
        delete s->primero;
    }else
        {
        NodoS* actual = s->primero;
        while (actual->siguiente->elem != x)
        {
            actual = actual->siguiente;
        }
        NodoS* eliminar = actual->siguiente;
        actual->siguiente = eliminar->siguiente;        
        delete eliminar;
    }
}

int sizeS(Set s){
    return s->cantidad;
}

LinkedList setToList(Set s){
    LinkedList llist = nil();
    NodoS* actual = s->primero;
   
    while(actual->siguiente != NULL){
        cons(actual->elem,llist);
        actual = actual->siguiente;
    }

    cons(actual->elem,llist);
    return llist;    
}

void destroyS(Set s){

    NodoS* actual = s->primero;
    while(actual->siguiente != NULL){
        removeS(actual->elem,s);
        actual = actual->siguiente;
    }
    removeS(actual->elem,s);
}


// Queue

struct NodoQ {
int elem; // valor del nodo
NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt {
int cantidad; // cantidad de elementos
NodoQ* primero; // puntero al primer nodo
NodoQ* ultimo; // puntero al ultimo nodo
};
typedef QueueSt* Queue;


Queue emptyQ(){
    Queue queue = new QueueSt;
    queue->cantidad = 0;
    queue->primero = NULL;
    queue->ultimo = NULL;
    return queue;
}

bool isEmptyQ(Queue q){
    return q->primero == NULL;
}

int firstQ(Queue q){
    return q->primero->elem;
}

void enqueue(int x, Queue q){

    NodoQ* nuevo = new NodoQ;
    nuevo->elem = x;
    nuevo->siguiente = NULL;

    if(q->primero == NULL){
        q->primero = nuevo;
        q->ultimo = nuevo;
    }else{
        q->primero->siguiente= nuevo;
        q->ultimo = nuevo;
    }

    q->cantidad++;
}

void dequeue(Queue q){
    
    NodoQ* eliminar = q->primero;
    q->primero = eliminar->siguiente;
    delete eliminar;
    if(q->primero == NULL){
        q->ultimo == NULL;
    }
    
    q->cantidad--;

}

int lastQ(Queue q){
    return q->ultimo->elem;
}

int lengthQ(Queue q){
    return q->cantidad;
}