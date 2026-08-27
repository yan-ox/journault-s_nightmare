#include <stdbool.h>


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
//R.4-2
//R.4-4
//R.4-6
//R.4-7
//R.4-10
//R.4-12
//R.4-16
//R.4-32
//R.4-33