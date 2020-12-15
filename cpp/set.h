#include <iostream>
using namespace std;




// linkedlist

struct NodoL;
struct LinkedListSt;
typedef LinkedListSt* LinkedList;

LinkedList nil();
bool isEmpty(LinkedList xs);
int head(LinkedList xs);
int cantidad(LinkedList xs);
void cons(int x, LinkedList xs);
void tail(LinkedList xs);
int length(LinkedList xs);
void snoc(int x, LinkedList xs);
void initialize(LinkedList xs);
int current(LinkedList xs);
void setCurrent(int x, LinkedList xs);
void next(LinkedList xs);
bool finished(LinkedList xs);
void destroyL(LinkedList xs);

//Set

struct NodoS;
struct SetSt;
typedef SetSt* Set;

Set emptyS();
bool isEmptyS(Set s);
bool belongsS(int x, Set s);
void addS(int x, Set s);
void removeS(int x, Set s);
int sizeS(Set s);
LinkedList setToList(Set s);
void destroyS(Set s);


// Queue

struct NodoQ;
struct QueueSt;
typedef QueueSt* Queue;

Queue emptyQ();
bool isEmptyQ(Queue q);
int firstQ(Queue q);
void enqueue(int x, Queue q);
void dequeue(Queue q);
int lastQ(Queue q);
int lengthQ(Queue q);