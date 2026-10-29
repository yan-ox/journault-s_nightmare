#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>

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

/******************************************/
/* Implémentation d'une structure de pile */
/******************************************/

struct pile_s {
    int* contenu;
    int taille_allouee;
    int nb_elems;
};
typedef struct pile_s* pile;

/* Crée un pile vide. */
pile pile_cree_pile() {
    pile p = malloc(sizeof(struct pile_s));
    p->contenu = malloc(sizeof(int));
    p->taille_allouee = 1;
    p->nb_elems = 0;
    return p;
}

/* Ajout l'élément x au sommet de la pile p. */
void pile_empile(pile p, int x) {
    if (p->nb_elems == p->taille_allouee) {
        int* n_contenu = malloc(2 * p->taille_allouee * sizeof(int));
        for (int i = 0; i < p->nb_elems; i ++){
            n_contenu[i] = p->contenu[i];
        }
        free(p->contenu);
        p->contenu = n_contenu;
        p->taille_allouee = 2 * p->taille_allouee;
    }
    p->contenu[p->nb_elems] = x;
    p->nb_elems ++;
}

/* Retire et renvoie l'élément en sommet de la pile p. */
int pile_depile(pile p) {
    if (p->nb_elems == 0) {printf("Dépile d'une pile vide\n"); exit(1);}
    p->nb_elems --;
    return p->contenu[p->nb_elems];
}

/* Test si la pile p est vide. */
bool pile_est_vide(pile p) {
    return (p->nb_elems == 0);
}

/* Libère l'espace occupé par une pile. */
void pile_libere(pile p) {
    free(p->contenu);
    free(p);
}

/* Transvase le contenu de la pile p1 dans la pile p2, en inversant l'ordre
 * des éléments */
void transvase(pile p1, pile p2) {
    while(!(pile_est_vide(p1))) {
        int x = pile_depile(p1);
        pile_empile(p2, x);
    }
}

/****************************************************************/
/* Implémentation d'une file dans un tableau de taille statique */
/****************************************************************/

struct file_s {
    int* contenu;
    int taille_allouee;
    int deb;
    int fin;
};
typedef struct file_s* file;

/* Crée une file vide dont la taille ne dépassera pas n. */
file file_cree_file(int n) {
    file f = malloc(sizeof(struct file_s));
    f->contenu = malloc(sizeof(int) * n);
    f->taille_allouee = n;
    f->deb = 0;
    f->fin = 0;
    return f;
}

/* Ajoute l'élément x à la fin de la file f. */
void file_enfile(file f, int x) {
    f->contenu[f->fin] = x;
    f->fin ++;
}

/* Retire et renvoie l'élément en début de file f. */
int file_defile(file f) {
    if (f->deb == f->fin) {printf("Défile une file vide\n"); exit(1);}
    f->deb ++;
    return f->contenu[f->deb - 1];
}

/* Teste si la file p est vide. */
bool file_est_vide(file f) {
    return (f->deb == f->fin);
}

/* Libère l'espace occupé par une file. */
void file_libere(file f) {
    free(f->contenu);
    free(f);
}

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
    bool* bt = malloc(g->taille * sizeof(bool));
    for(int i = 0; i < g->taille; i ++){bt[i] = false;}
    int* p = malloc(g->taille * sizeof(int));
    int top = -1;
    for(int i = 0; i < g->taille; i ++){
        top ++;
        p[top] = i;
        while(top != -1){
            int cur = p[top];
            top --;
            if(bt[cur]){
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
}

//R.7-3
//R.7-4
//R.7-5

void main(){
    printf("hello world");
}