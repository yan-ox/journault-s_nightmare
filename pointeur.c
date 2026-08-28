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
liste_c list_from_tab(int* t, int n){
    if (n == 0){return NULL;}
    liste_c l = create_cell(t[0]);
    cell* c = l;
    for(int i = 1; i < n; i++){
        c->next = create_cell(t[i]);
        c = c->next;
    }
    return l;
}
//R.5-11
liste_c list_fus(liste_c l1, liste_c l2){
    cell* l3 = l2;
    cell* c1 = l1;
    while(c1 != NULL){
        cell* c2 = create_cell(c1->val);
        c2->next = l3;
        l3 = c2;
        c1 = c1->next;
    }
    return l3;
}

void main(){
    liste_c l = n_premier(7);
    affiche_list(l);
    int t[8] = {4, 5, 7, 86, 5, 3, 2, 98};
    liste_c l2 = list_from_tab(t, 8);
    affiche_list(l2);
    affiche_list(l);
    liste_c l3 = list_fus(l, l2);
    affiche_list(l);
    affiche_list(l2);
    affiche_list(l3);
    free_list(l);
    free_list(l2);
}