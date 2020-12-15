#include <iostream>
using namespace std;

struct ArrayListSt;
typedef ArrayListSt* ArrayList;


ArrayList newArrayList();
ArrayList newArrayListWith(int capacidad);
int lengthAL(ArrayList xs);
int get(int i, ArrayList xs);
int capacidad(ArrayList xs);
void set(int i, int x, ArrayList xs);
void resize(int capacidad, ArrayList xs);
void add(int x, ArrayList xs);
int cantidad(ArrayList xs);
void remove(ArrayList xs);
int sumatoria(ArrayList xs);
void sucesores(ArrayList xs);
bool pertenece(int x, ArrayList xs);
int apariciones(int x, ArrayList xs);