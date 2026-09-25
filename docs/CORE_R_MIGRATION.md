# Migration du paquet R hors de nirs4all-core

Le paquet public R s'appelle toujours `nirs4all`, mais sa source et sa release
appartiennent maintenant à `GBeurier/nirs4all-r`. L'ancien code du core reste
récupérable dans l'historique Git antérieur au commit de retrait
`82e3e80` (`nirs4all-core/bindings/r`). Le worktree original du core, qui
contenait des modifications préexistantes, n'a pas été nettoyé ni écrasé.

| Élément de l'ancien paquet | Destination | Décision |
|---|---|---|
| `nirs4all_load_pipeline`, `nirs4all_portable_class_names`, `nirs4all_parse_execution_plan`, `nirs4all_run_portable_pipeline` | `R/portable.R` | API conservée, calcul délégué au pipeline R et à `n4m`. |
| Huit définitions JSON/YAML et `execution_contract_cases.json` | `inst/extdata/` | Copies exactes des fixtures canoniques `nirs4all-core/tests/parity/fixtures`, avec MD5 épinglés dans les tests du produit. |
| `nirs4all_upstreams`, `nirs4all_require`, six accesseurs de domaine et registre local DAG-ML | `R/upstreams.R` | Façades de découverte/délégation conservées, sans recopier les runtimes amont. |
| Manifeste de capacités, contrats d'artefacts et liste de runtimes de Core | Core Rust, `compat/capabilities.toml` et leur historique Git | Non recopiés tels quels : ils décrivaient cinq distributions possédées par Core et revendiqueraient faussement le paquet R après l'extraction. Un manifeste R produit devra être dérivé de tests locaux réels. |
| Licences et notices | `LICENSE` AGPL-3.0-or-later du produit R ; historique Git du Core | Les notices du vieux paquet Core restent récupérables, mais ne sont pas recopiées sans nécessité dans le nouveau paquet pur R. |

La compatibilité de recette actuellement vérifiée couvre les quatre exemples
Kennard-Stone, SNV, Savitzky-Golay et PLS, plus le corpus commun de cas valides
et invalides. `n4m.PLS` et quelques identifiants sémantiques sont résolus vers
les contrôleurs R natifs. Ce n'est pas encore l'ensemble du catalogue n4m ou
des primitives DAG-ML. Le RDS du produit n'est pas une archive interlangage ;
ses octets N4MM PLS seuls sont portables. Ne pas remplacer Archive V2/V3 par
un nouveau format non qualifié : raccorder un lecteur natif validé et tester
Python↔R avant de revendiquer la portabilité d'un pipeline entraîné complet.
