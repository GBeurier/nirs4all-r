# Pipelines n4m entraînés R ↔ Python

`nirs4all_export_trained_pipeline()` écrit un document JSON sans RDS, pickle ni
code exécutable. `nirs4all_import_trained_pipeline()` vérifie la recette,
l'état de prétraitement, les empreintes SHA-256, le descripteur N4MM et les
dimensions avant prédiction. Le même document est consommé par
`PortableN4MTrainedPipeline` dans `nirs4all` Python.

| Schéma | Modèle | Contenu portable | Réentraînement |
|---|---|---|---|
| `nirs4all.n4m.trained_pipeline.v1` | PLS SIMPLS, une cible numérique | Recette, références MSC/EMSC et N4MM PLS ; profil SNV→SG embarqué sous conditions | Recette ajustée à nouveau dans chaque hôte |
| `nirs4all.n4m.trained_pipeline.v2` | sparse PLS-DA, au moins deux classes texte | Recette, références MSC/EMSC, N4MM affine multiclasse et ordre des labels | Nouveau sparse PLS-DA ajusté sur les nouvelles lignes, sans réutiliser les poids |
| `nirs4all.n4m.trained_pipeline.v3` | PLS SIMPLS avec SPA externe | Indices SPA classés, zéro-based sur le fil, et N4MM PLS | SPA et PLS refaits sur les nouvelles lignes |
| `nirs4all.n4m.trained_pipeline.v4` | PLS SIMPLS avec sélecteur générique externe | Indices classés, zéro-based sur le fil, et N4MM PLS | Sélecteur et PLS refaits sur les nouvelles lignes |
| `nirs4all.n4m.trained_pipeline.v5` | Treize régressions affines MethodResult, dont N-PLS, une cible numérique | Recette, états externes MSC/EMSC/sélecteurs/branches, assertion de recette, N4MM affine PREDICT-only | Méthode réajustée à partir de la recette sur les nouvelles lignes |

Pour v2, la sortie N4MM est une matrice `échantillons × classes` de *scores*.
Le contrôleur choisit la première classe au score maximal et calcule, si
demandé, un softmax non calibré. La recette et la liste ordonnée des classes
doivent accompagner les octets ; un N4MM seul ne suffit pas pour décoder les
labels. Les deux hôtes utilisent n4m pour les calculs spectraux et les scores
du modèle, avec les lignes de validation exclues de l'ajustement MSC/EMSC.

Le descripteur N4MM affine v2 atteste le nombre de features, le nombre de
classes et l'équation de prédiction. Il n'atteste **pas** que cette équation
provient effectivement de sparse PLS-DA, ni le `n_components`, la pénalité ou
les noms des classes. Ces faits sont déclarés par le producteur dans le
manifeste. Les SHA-256 détectent une modification accidentelle du document,
mais ne constituent pas une signature ou une preuve d'origine ; un producteur
malveillant peut recalculer les empreintes. Ne charger que des artefacts d'une
source de confiance.

En v5, `fit_recipe_assertion` déclare la classe de la recette de
réentraînement ; le N4MM affine ne prouve pas quelle méthode a produit ses
coefficients. Les deux hôtes vérifient les empreintes, la classe de recette,
les dimensions transformées et le profil N4MM affine sans pipeline embarqué.
Les colonnes nommées doivent garder exactement leurs noms et leur ordre lors
de la prédiction et du réentraînement. Le descripteur exposé au binding R ne
contient pas `training_samples`, contrairement à l'inspection Python : R ne
peut donc pas en imposer une valeur positive à l'import, même si ses propres
exports portent toujours le nombre de lignes d'ajustement. Cette asymétrie
ne change pas la portée PREDICT-only du binaire.

Ce format borné n'est pas une archive DAG-ML V2/V3 : il ne transporte ni
FoldSet, OOF, sélection, identités d'échantillons ou lineage d'entraînement.
Il ne rend pas les modèles `ranger`, `glmnet` ou `torch` R portables. Les
recettes JSON/YAML de ces contrôleurs restent propres à R.
