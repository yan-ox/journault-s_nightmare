#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

//R.5-1
typedef struct simple_chain {
    int etiq;
    struct simple_chain* suiv;
}chain1;
typedef chain1* liste1;
//R.5-2
typedef struct double_chain {
    int etiq;
    struct double_chain* suiv;
    struct double_chain* av;
}chain2;
typedef struct liste2_s{
    chain2* debut;
    chain2* fin;
}liste2;

//Journault's list
typedef struct s_cell cell;
struct s_cell {
    int val ;
    cell* next ; /* adresse de la cell. suivante ou NULL en fin de liste */
};
/* adresse de la 1ere cellule ou NULL pr la liste vide */
typedef cell* liste_c;

//R.5-5
cell* create_cell(int x){
    cell* l = malloc(sizeof(cell));
    l->val = x;
    l->next = NULL;
    return l;
}
//R.5-6
void affiche_list(liste_c l){
    cell* cache = l;
    printf("[");
    while(cache != NULL){
        if(cache->next == NULL){
            printf("%d", cache->val);
        }else{
            printf("%d, ", cache->val);
        }
        cache = cache->next;
    }
    printf("]\n");
}
//R.5-7
liste_c n_premier(int n){
    if (n == 0){return NULL;}
    liste_c l = create_cell(0);
    cell* c = l;
    for(int i = 1; i < n; i++){
        c->next = create_cell(i);
        c = c->next;
    }
    return l;
}
//R.5-8
void free_list(liste_c l){
    cell* c = l;
    while (l != NULL){
        c = l->next;
        free(l);
        l = c;
    }
}
//R.5-9
//R.5-11

void main(){
    liste_c l = n_premier(7);
    affiche_list(l);
    free_list(l);
}