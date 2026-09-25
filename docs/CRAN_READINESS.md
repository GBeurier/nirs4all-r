# Préparation CRAN du paquet R `nirs4all`

État au 25 septembre 2026 : **pas prêt pour soumission**. Cette note sépare
le tarball R vérifié localement de la publication R-universe et d'une archive
de soumission CRAN. Le produit public R s'appelle `nirs4all` et sa source est
`GBeurier/nirs4all-r`.
Les textes préparatoires des formulaires pour `n4m` puis `nirs4all` figurent
dans [CRAN_FORM_DRAFT.md](CRAN_FORM_DRAFT.md) ; ils contiennent volontairement
des champs de preuve à compléter avant toute soumission.

## Vérification actuellement réalisée

- `R CMD build .` a produit `nirs4all_0.4.0.9012.tar.gz`.
- `R CMD check --as-cran --no-manual` sous R 4.6.0 (Linux) a exécuté les tests
  du paquet, y compris les exemples Python/n4m, les aller-retour N4MM
  formats 1 (PLS seul) et 2 (SNV→SG→PLS) R↔Python en processus Python distinct,
  les formats et le chemin DAG natif. Un test de CV groupée compare chaque
  fold à un fit R et Python indépendant, refuse le chevauchement des groupes
  et la modification du sidecar des IDs. Les contrôleurs optionnels `parsnip`
  et `mlr3` sont exercés en fit/prédiction, persistance et CV/OOF/refit/rejeu
  DAG-ML ; `mlr3` est aussi rechargé en processus R neuf. Le générateur borné
  `_or_`/`_cartesian_` + plage PLS a été comparé à `nirs4all` Python, puis ses
  huit variantes ont été évaluées par la sélection CV native et des fits R
  indépendants par fold. Six prétraitements n4m supplémentaires lus depuis
  JSON/YAML (`LSNV`, `RNV`, aire, detrend, MSC, EMSC) ont été comparés sur les
  matrices train/validation à un processus Python n4m indépendant ; leur
  parcours avec holdout Kennard–Stone est aussi testé. Le contrôleur `torch`
  à module personnalisé est testé sur une architecture à deux couches cachées,
  le rejeu RDS dans un processus R neuf et la CV/OOF/refit/replay DAG-ML avec
  ajustements indépendants par fold.
  Après installation des tarballs R-universe `dagmldata` et
  `nirs4alldatasets`, **tous les `Suggests` étaient disponibles** et le
  contrôle a été relancé sans `_R_CHECK_FORCE_SUGGESTS_=false` : 0 erreur,
  1 avertissement *CRAN incoming* sur la nouvelle soumission, la version de
  développement et les dépendances hors CRAN. Le CLI DAG-ML utilisé par les
  tests stricts provenait encore du build local, pas du tarball du paquet.
- Les résultats de Windows/macOS et la portabilité Archive V2/V3 Python↔R du
  pipeline entraîné complet restent à qualifier.
- Le tarball autonome `n4m_1.0.21.9002.tar.gz`, avec 238 unités natives
  vendorizées, a passé `R CMD check --as-cran --no-manual` sous Linux/R 4.6.0 :
  0 erreur, 0 avertissement, 2 notes (nouvelle soumission/version de
  développement et `-march=nocona` injecté par le R conda local). Les tests
  incluent le N4MM format 2 SNV→SG, son inspection et le refus des octets
  tronqués.

## Blocage de politique et ordre de soumission

`n4m (>= 1.0.21.9002)` est dans `Imports`, mais n'est pas dans CRAN ni dans le
dépôt logiciel Bioconductor. La [politique CRAN sur les dépendances](https://stat.ethz.ch/CRAN/web/packages/policies.html)
demande que les dépendances fortes (`Depends`, `Imports`, `LinkingTo`) soient
disponibles dans l'un de ces dépôts ; `Additional_repositories` couvre les
paquets de `Suggests`/`Enhances`, pas cette dépendance forte. Il faut donc
qualifier et soumettre `n4m` **avant** `nirs4all`, ou revoir l'architecture de
dépendance sans perdre l'exécution native n4m. La simple présence des deux
tarballs dans une archive ZIP destinée au mainteneur ne résout pas ce blocage
de dépôt.

Avant une soumission `nirs4all` :

1. terminer et qualifier les niveaux 1–2 annoncés par le produit ;
2. disposer d'une version stable de `n4m` admise sur CRAN et retirer le
   minimum requis à une version de développement ;
3. vérifier le tarball final avec R-devel sur Linux, Windows et macOS, avec
   toutes les dépendances installées et sans avertissement significatif ;
4. préparer une archive **de travail** distincte contenant les tarballs source
   épinglés, leur SHA-256, les notices/licences, les logs de vérification et les
   textes des formulaires. Seul le tarball propre du paquet est destiné au
   formulaire CRAN.

La [politique CRAN de soumission](https://stat.ethz.ch/CRAN/web/packages/policies.html)
demande un `R CMD check --as-cran` du tarball à envoyer et, en principe, aucun
avertissement ni note significative. Aucun formulaire de soumission ne doit
présenter la version `0.4.0.9012` comme prête tant que ces gates restent ouverts.
