type 'a btree =
    |E                              (*arbre vide*)
    |N of 'a * 'a btree * 'a btree  (*noeud*)

type 'a gtree =
    |GN of 'a * 'a gtree list