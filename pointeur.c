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
liste_c create_list(int x){
    liste_c* l = malloc(sizeof(liste_c));
    (*l)->val = x;
    (*l)->next = NULL;
    return *l;
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
//R.5-8
//R.5-9
//R.5-11