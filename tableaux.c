#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>

// struct pour les tableaux d'entiers munis de leur taille
struct tableau_s {
    int taille ;
    int* tab ;
};
typedef struct tableau_s tableau;

// struct pour les tableaux de tableaux d'entiers munis de leur taille
struct tabtab_s {
    int taille ;
    tableau* tab ;
};
typedef struct tabtab_s tabtab;

// struct pour les tableaux de booléens munis de leur taille
struct btableau_s {
    int taille ;
    bool* tab ;
};
typedef struct btableau_s btableau;

//R.4-1
btableau create_btab (int n){
    btableau* tab = malloc(sizeof(btableau));
    tab->taille = n;
    tab->tab = malloc(n * sizeof(bool));
    int i = 0;
    while (i < n){
        tab->tab[i] = false;
        i ++;
    }
    return *tab;
}
//R.4-2
tableau create_tab (int n){
    tableau* tab = malloc(sizeof(tableau));
    tab->taille = n;
    tab->tab = malloc(n * sizeof(int));
    for (int i = 0; i < n; i++){
        tab->tab[i] = i + 1;
    }
    return *tab;
}
//R.4-4
bool present_0 (tableau t){
    int i = 0;
    while (i < t.taille && t.tab[i] != 0){
        i++;
    }

    return i< t.taille && t.tab[i] == 0;
}
//R.4-6
int premier_0 (tableau t){
    int i = 0;
    while (i < t.taille){
        if(t.tab[i] == 0){
            return i;
        }
        i++;
    }

    return -1;
}
int premier_02 (tableau t){
    for(int i = 0; i < t.taille; i++){
        if(t.tab[i] == 0){
            return i;
        }
    }

    return -1;
}
//R.4-7
//R.4-10
//R.4-12
//R.4-16
//R.4-32
//R.4-33

void main(){
    btableau btab = create_btab(7);
    for(int i = 0; i < btab.taille; i++){
        assert(!btab.tab[i]);
    }
    tableau tab = create_tab(7);
    for(int i = 0; i < btab.taille; i++){
        assert(tab.tab[i] == i + 1);
    }
    assert(!(present_0(tab)));
    assert(premier_0(tab) == -1);
    assert(premier_02(tab) == -1);
    tab.tab[3] = 0;
    assert(present_0(tab));
    assert(premier_0(tab) == 3);
    assert(premier_02(tab) == 3);
}