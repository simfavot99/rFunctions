# rFunctions

Personal R utility package — a growing collection of helper functions for data
exploration and analysis.

- GitHub: <https://github.com/simfavot99/rFunctions>
- Issues: <https://github.com/simfavot99/rFunctions/issues>

## Installation

```r
# install.packages("devtools")
devtools::install_github("simfavot99/rFunctions")
```

## Functions

### `eye_shiny(data, max_n = 50)`

Launches an interactive Shiny app for exploring the unique values and
distributions of every variable in a data frame.

```r
eye_shiny(df)
eye_shiny(df, max_n = 100)
```

**Arguments**

| Argument | Default | Description |
|----------|---------|-------------|
| `data`   | —       | A data frame to explore. |
| `max_n`  | `50`    | Max unique values shown in the Explorer table and bar chart. |

**Explorer tab** (three-column layout)

| Column | Content |
|--------|---------|
| Left   | Scrollable variable list. Toggle between *All* (flat) and *By category* (grouped by type: Integer, Numeric, Character, Factor, Logical, Date). |
| Middle | Frequency table — values ordered from most to least frequent, with percentage and a colour-bar indicator. Capped at `max_n`. |
| Right  | Interactive distribution plot (hover for exact values). Numeric/Integer → histogram with % y-axis. Categorical/Logical/Factor → horizontal bar chart. Date → daily count timeline. |

**Special behaviours**

- **Uniform distribution** — a yellow banner appears above both panels when all
  unique values share the same frequency.
- **Date variables** — the table panel shows a date-range badge (min → max),
  unique count, and first/last sample dates.
- **Integer vs Numeric** — `integer` columns are classified separately from
  `double` columns so they appear in their own group.

**Copy tab**

A read-only monospaced text area with the full summary for every variable:
top 30 most frequent values + percentage, grouped by variable type.
Press **Ctrl+A** then **Ctrl+C** to copy the entire output.

**Dependencies:** `shiny`, `bslib`, `ggiraph`, `ggplot2`, `DT`
(installed automatically via `pacman` if missing).