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
//R.4-7
int premier_02 (tableau t){
    for(int i = 0; i < t.taille; i++){
        if(t.tab[i] == 0){
            return i;
        }
    }

    return -1;
}
//R.4-10
int mini (tableau t){
    int imin = 0;
    int min = t.tab[0];
    for(int i = 0; i < t.taille; i++){
        if(t.tab[i] < min){
            min = t.tab[i];
            imin = i;
        }
    }
    return imin;
}
//R.4-12
int plus_occ (tableau t, int m){
    int* occ = malloc(sizeof(m));
    for(int i = 0; i < m; i++){
        occ[i] = 0;
    }
    for(int i = 0; i < t.taille; i++){
        occ[t.tab[i]] += 1;
    }
    int max = occ[0];
    int imax = 0;
    for(int i = 1; i < m; i++){
        if(occ[i] > max){
            max = occ[i];
            imax = i;
        }
    }
    free(occ);
    return imax;
}
//R.4-16
tabtab elargi(tableau t){
    tabtab* out = malloc(sizeof(tabtab));
    out->taille = t.taille;

    out->tab = malloc(out->taille * sizeof(tableau));
    for(int i = 0; i < out->taille; i++){
        out->tab[i].tab = malloc(t.tab[i] * sizeof(int));
        out->tab[i].taille = t.taille;
        for(int j = 0; j < t.tab[i]; j++){
            out->tab[i].tab[j] = 0;
        }
    }
    return *out;
}
//R.4-32
bool dichotrec(tableau t, int g, int d, int p){
    if(g >= d){
        return t.tab[g] == p;
    }
    int mid = (g + d)/2;
    if(t.tab[mid] == p){
        return true;
    }else if(t.tab[mid] > p){
        return dichotrec(t, g, mid-1, p);
    }else{
        return dichotrec(t, mid+1, d, p);
    }
}
bool dicho_(tableau t, int p){
    return dichotrec(t, 0, t.taille, p);
}
//R.4-33
bool dichot(tableau t, int p){
    int g = 0;
    int d = t.taille;
    bool found = false;
    while(g <= d && !found){
        int mid = (g + d)/2;
        if(t.tab[mid] == p){
            found = true;
        }else if(t.tab[mid] < p){
            d = mid - 1;
        }else{
            g = mid + 1;
        }
    }
    return found;
}
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
    assert(mini(tab) == 0);
    tab.tab[3] = 0;
    assert(present_0(tab));
    assert(premier_0(tab) == 3);
    assert(premier_02(tab) == 3);
    assert(mini(tab) == 3);
    int tibi[12] = {3, 4, 8, 3, 5, 4, 8, 4, 4, 3, 2, 9};
    tableau multi = {.taille = 12, .tab = tibi};
    assert(plus_occ(multi, 10) == 3);
    assert(dicho_(multi, 8));
    assert(!dicho_(multi, 1));
    assert(dichot(multi, 8));
    assert(!dichot(multi, 1));
}