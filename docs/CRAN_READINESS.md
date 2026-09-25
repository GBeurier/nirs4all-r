# Préparation CRAN du paquet R `nirs4all`

État au 25 septembre 2026 : **pas prêt pour soumission**. Cette note sépare
le tarball R vérifié localement de la publication R-universe et d'une archive
de soumission CRAN. Le produit public R s'appelle `nirs4all` et sa source est
`GBeurier/nirs4all-r`.
Les textes préparatoires des formulaires pour `n4m` puis `nirs4all` figurent
dans [CRAN_FORM_DRAFT.md](CRAN_FORM_DRAFT.md) ; ils contiennent volontairement
des champs de preuve à compléter avant toute soumission.

## Vérification actuellement réalisée

- `R CMD build .` a produit `nirs4all_0.4.0.9021.tar.gz`.
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
  ajustements indépendants par fold. Le mode `split_steps = TRUE` exécute chaque
  étape n4m dans un nœud DAG distinct : deux chaînes MSC/EMSC sont recoupées
  avec les fits R par fold, puis une sélection SNV/MSC et les cinq variantes
  PLS de l'exemple Python sont validées avec deux workers R.
  Une composition de deux branches parallèles (SNV→SG et MSC→detrend) est
  testée avec une concaténation native DAG-ML avant PLS. Les références MSC
  par fold, les matrices concaténées, les OOF et l'inférence externe sont
  recoupées avec des ajustements indépendants ; les matrices train/validation
  concordent aussi avec un processus Python n4m distinct.
  Le lecteur portable accepte maintenant les recettes Python
  `branch` → `merge: features` (branches nommées ou en listes), y compris
  MSC et un holdout Kennard–Stone. L'export R borné SNV/SG de cette structure
  est reconnu par l'analyseur de topologie Python ; son exécution numérique
  et son abaissement en DAG-ML sont testés. La lecture de cette structure par
  Core/WASM n'est pas encore qualifiée.
  La classification probabiliste `ranger`, `parsnip` et `mlr3` conserve les
  classes factorielles dans R ; la CV/OOF, la sélection et le refit DAG-ML
  utilisent un codage de classes attesté et sont recoupés avec des fits
  indépendants. Un CSV réel décodé par `nirs4all-formats` fournit des labels
  catégoriels demandés explicitement depuis `targets` ou les métadonnées.
  Les contrôleurs CPU `torch` de classification MLP et module personnalisé
  produisent des facteurs et probabilités `N×K`. Leur apprentissage local,
  leur répétabilité, la sérialisation/relecture dans un processus R neuf,
  et la CV/OOF/refit/inférence DAG-ML sont recoupés avec des fits R
  indépendants par fold. La perte `torch` R emploie les indices de classes
  `1..K`; le codage `0..K-1` du contrat DAG-ML reste confiné à l'adaptateur.
  Après installation des tarballs R-universe `dagmldata` et
  `nirs4alldatasets`, **tous les `Suggests` étaient disponibles** et le
  contrôle de 0.4.0.9016 avait été relancé sans
  `_R_CHECK_FORCE_SUGGESTS_=false` : 0 erreur, 1 avertissement *CRAN incoming*.
  Pour le tarball 0.4.0.9020, le contrôle Linux/R 4.6.0 avec `torch` CPU et
  `dagml` publics installés, CLI DAG local et `Suggests` manquants non forcés
  donne 0 erreur et 1 avertissement *CRAN incoming* sur la nouvelle
  soumission, la version de développement et `n4m` hors CRAN. Le test ciblé
  de classification `torch` a passé séparément avec le mode DAG strict.
  Ce résultat ne remplace pas un contrôle 0.4.0.9020 avec **tous** les
  `Suggests` installés. Le CLI DAG-ML provenait encore du build local.
- Le tarball 0.4.0.9021 ajoute le classifieur `n4m` sparse PLS-DA et son
  alias JSON/YAML. Les scores et labels prédits hors échantillon concordent
  avec le binding Python n4m à `1e-10` ; CV/OOF/refit/inférence DAG ont passé
  avec vérification par folds indépendants. `R CMD check --as-cran --no-manual`
  sous Linux/R 4.6.0 a terminé avec 0 erreur et 1 avertissement *CRAN incoming*
  en désactivant le contrôle obligatoire des `Suggests` absents de la machine.
  Le mode strict ciblé de ce classifieur a passé ; le premier contrôle strict
  global échouait faute de `ranger`, `glmnet`, `parsnip`, `mlr3` et `rpart`
  dans sa bibliothèque R. Les pseudo-probabilités softmax ne sont pas
  calibrées ; l'état appris de ce classifieur n'est pas portable entre langages.
- **Contrôle strict complémentaire** : la bibliothèque de test contenant tous
  les `Suggests` a été retrouvée. L'archive 0.4.0.9021 issue du commit
  `aa41e6a`, SHA-256
  `51cdffa7c5da68b4743c8f6fd33afbdfb69f0b41dc2869d2fc15c808347d65ff`,
  a passé `R CMD check --as-cran --no-manual` sur Linux/R 4.6.0 avec
  `NIRS4ALL_REQUIRE_DAG_PARITY=1`, l'oracle Python n4m, le runtime `torch` CPU
  et **tous les `Suggests` installés** : 0 erreur, 1 avertissement CRAN-incoming.
  Ce contrôle exerce les contrôleurs optionnels en DAG strict ; il n'efface
  pas le blocage CRAN de `n4m` ni l'absence de check Windows/macOS du même
  tarball. Le présent ajout documentaire est postérieur à ce tarball ; une
  archive de soumission finale devra être reconstruite et recontrôlée.
- `nirs4all` R 0.4.0.9022 ([PR #19](https://github.com/GBeurier/nirs4all-r/pull/19))
  exécute aussi les recettes JSON/YAML `n4m.SparsePLSDA` avec cibles facteurs,
  chaînes ou codes numériques, et sélectionne une variante par accuracy.
  Les classes numériques restent numériques dans les résultats. Sont testés
  la sélection Kennard–Stone, un jeu catégoriel de `nirs4all-formats`, la
  correspondance des prédictions avec un processus Python n4m distinct pour
  SNV→sparse PLS-DA et le parcours DAG strict. Le tarball issu de `f199749`,
  SHA-256
  `10a94ad79899e28a4e8ae1cfeaee10441fc21b68f717d91c34c57ea8c9f6be5a`,
  a passé `R CMD check --as-cran --no-manual` Linux/R 4.6.0 avec tous les
  `Suggests` : 0 erreur, 1 avertissement CRAN-incoming. Cet alias n'est pas
  encore qualifié dans les lecteurs Python/Core/WASM, et son état entraîné
  reste un RDS. Le présent ajout documentaire est postérieur au tarball ;
  une archive finale devra être rebâtie et contrôlée.
- Les résultats de Windows/macOS et la portabilité Archive V2/V3 Python↔R du
  pipeline entraîné complet restent à qualifier pour **ce tarball 0.4.0.9021**.
  R-universe publie déjà `nirs4all` 0.4.0.9018 depuis `nirs4all-r` : son
  rebuild a réussi sur Linux, Windows, macOS et WASM. Ses sources publiques
  et celles de `n4m` 1.0.21.9002 ont été installées dans une bibliothèque R
  vierge sous Linux/R 4.6.0 avec GCC système ; un pipeline
  SNV→Savitzky-Golay→PLS a ajusté et prédit sans checkout local. Cette preuve
  de distribution ne remplace ni le check multi-OS du tarball 0.4.0.9021 ni la
  publication de `dagml`, encore en attente de synchronisation.
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
présenter la version `0.4.0.9021` comme prête tant que ces gates restent ouverts.
