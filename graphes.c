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
void aux_prof(graphe g, int s, bool** bt){
    if ((*bt)[s]){
        (*bt)[s] = false;
        printf("%d ", s);
        list temp = g->contenu[s];
        while(temp != NULL){
            aux_prof(g, temp->elem, bt);
            temp = temp->suivant;
        }
    }
}

void prof_rec(graphe g){
    bool** bt = malloc(sizeof(bool*));
    *bt = malloc(g->taille * sizeof(bool));
    for(int i = 0; i < g->taille; i ++){(*bt)[i] = false;}
    for(int i = 0; i < g->taille; i ++){
        aux_prof(g, i, bt);
    }
}

//R.7-2
void prof(graphe g){
    bt = malloc(g->taille * sizeof(bool));
    int* p = malloc(g->taille * sizeof(int));
    int top = -1;
    for(int i = 0; i < g->taille; i ++){
        top ++;
        p[top] = i;
        while(top != -1){
            int cur = p[top];
            top --;
            printf("%d ", cur);
            list temp = g->contenu[cur];
            while(temp != NULL){
                top ++;
                p[top] = temp->elem;
                temp = temp->suivant;
            }
        }
    }
}

//R.7-3
//R.7-4
//R.7-5

void main(){
    printf("hello world")
}