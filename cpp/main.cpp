#include <iostream> 
#include "linkedList.h"
using namespace std;


int sumatoria(LinkedList xs){
    
    int resultado = 0;
    while(!finished(xs)){
        resultado = resultado + current(xs);
        next(xs);
    }

    return resultado + current(xs) ;
}

void sucesores(LinkedList xs){
    
    initialize(xs);
     while(!finished(xs)){
        setCurrent(current(xs) + 1,xs);
        next(xs);
    }

    setCurrent(current(xs) +1,xs);
}


bool pertenece(int x, LinkedList xs){
    bool res = false;
    initialize(xs);
    while(!finished(xs)){
        if(current(xs) == x){
            res = true;
        }
        next(xs);
    }
    return res || current(xs) == x;
}

int apariciones(int x, LinkedList xs){
    
    int res = 0;
    initialize(xs);
    while(!finished(xs)){
        if(current(xs) == x){
            res++;
        }
        next(xs);
    }
    if(current(xs) == x){
            res++;
        }
    return res;
}

int minimo(LinkedList xs){
    
    initialize(xs);
    int res =current(xs);
    while(!finished(xs)){
        if(current(xs)< res){
            res = current(xs);
        }
        next(xs);
    }
    if(current(xs)< res){
            res = current(xs);
        }
    return res;
}

LinkedList copy(LinkedList xs){
    LinkedList list = nil();
    initialize(xs);
    while (!finished(xs))
    {
        cons(current(xs), list);
        next(xs);
    }
    cons(current(xs), list);
    return list;    
}

void append(LinkedList xs, LinkedList ys){

    initialize(ys);
    while (!finished(ys))
    {
        snoc(current(ys),xs);
        next(ys);
    }
    snoc(current(ys),xs);    
}
int main(){
    
    LinkedList list = nil();

    cout << "Memoria de la list : "<<list<< endl;
    cout << "Lista vacia : " << isEmpty(list) << endl;
    cons(20,list);
    cons(30,list);
    cout << "Primer elemento: " << head(list) << endl;
    cout << "Cantidad: " << cantidad(list) << endl;
    tail(list);
    cout << "Cantidad: " << length(list) << endl;
    cout << "Primer elemento: " << head(list) << endl;
    snoc(40,list);
    cout << "Tamano: " << length(list) << endl;
    initialize(list);
    cout << "Actual : " << current(list) << endl;
 
    cout << "terminada? : " << finished(list) << endl;
   
    cout << "Sumatoria: " << sumatoria(list) << endl;
    cout <<"El primero: " << head(list) << endl;
    sucesores(list);
    cout << "Elemento sucesor: " <<  head(list) << endl;
    cout << "Actual : " << current(list) << endl;
    //setCurrent(10,list);
    cout << "Actual : " << current(list) << endl;
    cout << "Pertenece el nro 21: " << pertenece(21,list) << endl;
    cons(41,list);

    initialize(list);
    while(!finished(list)){
        cout << "Elemento: " << current(list) << endl;
        next(list);
    }
    cout << "Elemento: " << current(list) << endl;
    cout << "Apariciones del 41: " << apariciones(41,list) << endl;
    cout << "El minimo: " << minimo(list) << endl;
    LinkedList list2 = copy(list);
    initialize(list2);
    while(!finished(list2)){
        cout << "Elemento2: " << current(list2) << endl;
        next(list2);
    }
    cout << "Elemento: " << current(list2) << endl;
    append(list,list2);
    initialize(list);
    while(!finished(list)){
        cout << "Elemento: " << current(list) << endl;
        next(list);
    }
    cout << "Elemento: " << current(list) << endl;
    return 0;
    
}