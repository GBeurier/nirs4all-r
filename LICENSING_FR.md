# Licence — nirs4all-r

`nirs4all-r` (le paquet R `nirs4all`) fait partie de l'**écosystème nirs4all** et est proposé sous une **double
licence open-source assortie d'une licence commerciale optionnelle** (modèle open-core).

## 1. Licence open-source (par défaut)

> **SPDX-License-Identifier :** `CeCILL-2.1 OR AGPL-3.0-or-later`

Vous pouvez utiliser, modifier et redistribuer ce logiciel selon les termes de **l'une
ou l'autre**, à votre choix :

1. le **Contrat de licence de logiciel libre CeCILL, version 2.1** — texte intégral dans
   [`inst/LICENSES/CeCILL-2.1.txt`](inst/LICENSES/CeCILL-2.1.txt) ; **ou**
2. la **GNU Affero General Public License, version 3.0 ou (à votre choix) toute version
   ultérieure** — texte intégral dans [`inst/LICENSES/AGPL-3.0-or-later.txt`](inst/LICENSES/AGPL-3.0-or-later.txt).

CeCILL-2.1 est la licence française (CIRAD) compatible GPL/AGPL ; AGPL-3.0-or-later en
est l'équivalent internationalement reconnu. Les deux sont équivalentes — choisissez
celle que vous préférez. Le fichier `LICENSE` à la racine contient le texte
AGPL-3.0-or-later afin que les plateformes affichent une licence SPDX reconnue ; cela ne
diminue en rien l'option CeCILL-2.1.

Sauf accord commercial explicite distinct, l'usage de ce code est régi par les termes
open-source ci-dessus.

## 2. Licence commerciale

Si les obligations de copyleft de CeCILL-2.1 / AGPL-3.0-or-later ne conviennent pas à
votre usage — par exemple intégrer `nirs4all-r` dans un **produit propriétaire fermé ou un
SaaS** sans publier votre propre code source — une **licence commerciale** est disponible.

- Conditions : [`inst/LICENSES/COMMERCIAL-LICENSE_FR.md`](inst/LICENSES/COMMERCIAL-LICENSE_FR.md).
- **Pour tout usage commercial, contactez : <nirs4all-admin@cirad.fr>.**

## 3. Composants tiers

`nirs4all-r` n'embarque aucun code extérieur à l'écosystème nirs4all. Il appelle les paquets R listés dans
`DESCRIPTION`, que les utilisateurs installent séparément sous leurs propres licences ; ils
sont résumés, avec l'analyse de compatibilité, dans
[`THIRD_PARTY_NOTICES.md`](THIRD_PARTY_NOTICES.md).


## En-tête SPDX recommandé dans les fichiers source

```
    # SPDX-License-Identifier: CeCILL-2.1 OR AGPL-3.0-or-later
    // SPDX-License-Identifier: CeCILL-2.1 OR AGPL-3.0-or-later
```

© 2025 CIRAD — écosystème nirs4all.
