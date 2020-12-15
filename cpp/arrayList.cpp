#include <iostream>
using namespace std;



struct ArrayListSt {
int cantidad; // cantidad de elementos
int* elementos; // array de elementos
int capacidad; // tamaño del array
};

typedef ArrayListSt* ArrayList;

ArrayList newArrayList(){

    ArrayListSt* array = new ArrayListSt;
    array->cantidad = 0;
    array->elementos = new int[16];
    array->capacidad = 16;
    return array;
}

ArrayList newArrayListWith(int capacidad){
    ArrayList array = newArrayList();
    array->capacidad = capacidad;
    return array;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

int capacidad(ArrayList xs){
    return xs->capacidad;
}

int get(int i, ArrayList xs){
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){

    xs->elementos[i] = x;
}

void resize(int cap, ArrayList xs){

    //ArrayList nuevo = newArrayListWith(cap);

    int* newArray = new int[cap];
    xs->capacidad = cap;
    //xs->cantidad = 0;
    for (int i = 0; i < lengthAL(xs);i++){
        if(xs->cantidad <= capacidad(xs)){
            newArray[i] = get(i,xs);
           // xs->cantidad = xs->cantidad +1;
        }
            
    }
    xs->elementos = newArray;

}

void add(int x, ArrayList xs){
    if(xs->cantidad < capacidad(xs)){
        
        set(xs->cantidad,x,xs);
        xs->cantidad = xs->cantidad +1;
    }
}


int cantidad(ArrayList xs){
    return xs->cantidad;
}

void remove(ArrayList xs){
    xs->cantidad = cantidad(xs) -1;
}

int sumatoria(ArrayList xs){


    int res = 0;
    for(int i=0;i<= lengthAL(xs);i++){
        res = res + xs->elementos[i];
    }

    return res;
}

void sucesores(ArrayList xs){

    for(int i=0;i<= lengthAL(xs);i++){

       xs->elementos[i] = get(i,xs) +1;        
    }
}

bool pertenece(int x, ArrayList xs){
    bool res = false;
    
    for(int i=0;i<= lengthAL(xs);i++){

       if(get(i,xs) == x){
           res = true;
       }        
    }
    
    return res;
}

int apariciones(int x, ArrayList xs){

    int cantidad = 0;
    for(int i=0;i<= lengthAL(xs);i++){
        if(get(i,xs) == x){
            cantidad++;
        }
    }

    return cantidad;
}