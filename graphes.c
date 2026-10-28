#include <stdlib>
#include <stdbool>

struct list_s {
    int elem ; /* l'élément contenu dans le maillon */
    struct list_s* suivant ; /* le maillon suivant */
};

/* Une liste est un pointeur vers son premier maillon */
typedef struct list_s* list;

struct graph_s {
    int taille ; /* le nombre de sommets */
    list* contenu ; /* le tableau des listes d'adjacence*/
};
typedef struct graph_s* graphe ;

//R.7-1
void aux_prof(graphe g, int s, bool){

}
//R.7-2
//R.7-3
//R.7-4
//R.7-5

void main(){
    printf("hello world")
}