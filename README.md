## `xlsx2tex.py` — Générer des `longtable` LaTeX depuis Excel

### Usage

```

# Plusieurs tables → génère <nom>.tex dans le répertoire de -o
uv run work/xlsx2tex.py combined.xlsx -o table

```

| Option | Description |
|--------|-------------|
| `-o` / `--output` | Sortie : fichier (table unique) ou répertoire de référence (tables multiples) |
| `--caption` | Légende par défaut (sinon : nom de la table) |
| `--label` | Label LaTeX (sinon : `tab:<nom>`) |
| `--landscape` | Enveloppe dans `landscape` (nécessite `pdflscape`) |
| `--fontsize` | Ex. `\footnotesize` |
| `--tabcolsep` | Ex. `3pt` |
| `--stdout` | Afficher dans le terminal |

### Structure du xlsx (lignes de définition de table)

| Ligne | Rôle |
|-------|------|
| 1 | Nom de la source (fusionné sur le groupe de colonnes) |
| 2 | Nom du groupe de colonnes |
| 3 | Unité |
| **4+** | **Lignes de définition de table** — une par table souhaitée |
| Première ligne dont col A = `Austria` | Début des données |

Chaque ligne de définition (row 4 et suivantes, avant `Austria`) dont au moins une cellule en colonnes B+ est non vide produit un fichier `.tex`.

- **Col A** : nom de la table = nom du fichier de sortie et racine de la macro caption. Peut être vide (nom auto `table1`, `table2`…).
- **Cols B+** : label LaTeX à utiliser comme en-tête de colonne. Cellule vide = colonne exclue.

### Tables multiples

Si le xlsx contient plusieurs lignes de définition, le script génère un fichier par ligne dans le répertoire de `-o` :

```
row 4 col A = "table"    →  work/report/table.tex
row 5 col A = "table_s"  →  work/report/table_s.tex
```

### Caption depuis le document parent

Chaque fichier généré expose une macro `\<nom>caption` (lettres seulement) via `\providecommand`. La valeur par défaut est le nom de la table. Pour overrider dans `report.tex` :

```latex
\renewcommand{\tablecaption}{Contributions par État membre}
\input{table}

\renewcommand{\tablescaption}{Contributions — scénario réduit}
\input{table_s}
```

### Ce que produit le script

- Première colonne fixe à 2,2 cm (suffit pour `Luxembourg` sur une ligne).
- Colonnes de données : largeur calculée automatiquement pour remplir la ligne.
- Ligne EU27 en **gras** avec `\midrule` de séparation ; ignorée si tous ses champs inclus sont vides.
- Nombres formatés avec séparateur de milliers (`29,800`).
- Caractères spéciaux LaTeX échappés (`_`, `%`, `&`, etc.).
- Guards `\ifdefined` sur `\firstcolw` et `\colw` : plusieurs tables peuvent coexister dans le même document sans conflit.

### Packages requis dans le document `.tex`

```latex
\usepackage{longtable, booktabs, colortbl, array}
```
