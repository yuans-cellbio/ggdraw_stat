# ggplot_draw_stat

Add GraphPad-style statistical annotation brackets to ggplot2 plots programmatically.

`draw_stat` takes a ggplot object and a dataframe of pairwise comparisons, then overlays significance brackets with labels — handling coordinate extraction, bracket stacking, and label positioning automatically.

## Features

- **Automatic coordinate extraction** via `ggplot_build()` — bracket endpoints are derived from the plot's internal geometry, not hard-coded.
- **Facet support** — works with `facet_wrap` and `facet_grid`, including **multiple facet variables** (e.g., `facet_wrap(~A + B)`, `facet_grid(A ~ B)`). Also supports subclasses like `ggh4x::facet_wrap2` and `facet_grid2`.
- **Per-panel y-range** — vertical padding is computed from actual built data per panel, not from shared axis scales.
- **Bracket stacking** — overlapping comparisons are detected and stacked vertically with configurable padding.
- **Label nudging** — midpoint labels are shifted to avoid crossing vertical bracket lines.
- **Flexible group mapping** — supports both simple (`x = Treatment`) and interaction-based (`x = Fct1`, `group = Fct2`) designs via a custom `comp_group` aesthetic.
- **Input validation** — checks for required columns, valid aesthetic mappings, and layer index range with informative error messages.
- **Customizable appearance** — `segment_linewidth`, `label_size`, and `label_color` arguments for quick styling.

## Installation

No package installation required. Source the script directly:

```r
source("ggplot_draw_stat.R")
```

### Dependencies

- `tidyverse` (dplyr, purrr, rlang, ggplot2)

## Full Vignette

Check ggplot_draw_statistics_testing.Rmd or the rendered ggplot_draw_statistics_testing.html. 

## Quick Start

### 1. Prepare a contrast dataframe

The contrast dataframe must have columns `Group1`, `Group2`, and `Label`. Group names must match the values of the aesthetic mapped to `comp_group` in your ggplot call.

```r
df_stat <- data.frame(
  Group1 = c("Control", "Control"),
  Group2 = c("Treatment_A", "Treatment_B"),
  Label  = c("*", "**")
)
```

### 2. Map `comp_group` in your ggplot

Pass a `comp_group` aesthetic (or any custom name via `contrast_group_name`) that identifies each comparison group:

```r
p <- ggplot(df, aes(x = Treatment, y = Value, comp_group = Treatment)) +
  stat_summary(geom = "col", fun = "mean", fill = "steelblue") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.3)
```

### 3. Draw annotations

```r
draw_stat(p, df_stat, contrast_group_name = "comp_group")
```

## Usage with Interaction Groups

When your design has two factors (e.g., Treatment × Genotype), use `interaction()` to create composite group labels:

```r
p <- ggplot(df, aes(
  x = Treatment, y = Value,
  comp_group = interaction(Treatment, Genotype, sep = " ")
)) +
  stat_summary(geom = "col", fun = "mean", aes(fill = Genotype),
               position = position_dodge(0.8), width = 0.6) +
  stat_summary(geom = "errorbar", fun.data = "mean_se",
               aes(group = Genotype),
               position = position_dodge(0.8), width = 0.4)

draw_stat(p, df_stat, contrast_group_name = "comp_group")
```

Ensure that `Group1` and `Group2` in your contrast dataframe use the same separator and level ordering as `interaction()`.

## Usage with Facets

For faceted plots, include the facet variable column(s) in your contrast dataframe. The function matches comparisons to panels automatically.

### Single facet variable

```r
# df_stat must contain a "Measurement" column matching the facet variable
p <- ggplot(df, aes(x = Treatment, y = Value,
                     comp_group = interaction(Treatment, Genotype, sep = " "))) +
  stat_summary(geom = "col", fun = "mean", aes(fill = Genotype),
               position = position_dodge(0.8), width = 0.6) +
  facet_wrap(~Measurement, scales = "free")

draw_stat(p, df_stat, contrast_group_name = "comp_group")
```

### Multiple facet variables

When using `facet_wrap(~A + B)` or `facet_grid(A ~ B)`, `contrast_df` must contain columns for **all** facet variables. The function creates a composite panel identifier internally to match annotations to the correct panels.

```r
# df_stat must contain both "Genotype" and "Measurement" columns
p <- ggplot(df, aes(x = Treatment, y = Value,
                     comp_group = interaction(Treatment, Genotype, sep = " "))) +
  stat_summary(geom = "col", fun = "mean", aes(fill = Treatment),
               position = position_dodge(0.8), width = 0.6) +
  facet_wrap(~Genotype + Measurement, scales = "free")

draw_stat(p, df_stat, contrast_group_name = "comp_group", ypad_ratio = 0.1)
```

## Customizing Appearance

`draw_stat` exposes styling arguments directly:

```r
draw_stat(p, df_stat, contrast_group_name = "comp_group",
          segment_linewidth = 0.3, label_size = 4, label_color = "red")
```

For full control over annotation layers (e.g., custom label formatting, `coord_cartesian(clip = "off")`), use `get_annotation_df()` directly — see [Advanced Usage](#advanced-usage) below.

## API Reference

### `draw_stat()`

Wrapper that computes annotation coordinates and adds bracket + label layers to a ggplot.

| Argument | Type | Default | Description |
|---|---|---|---|
| `p` | ggplot | — | A ggplot object |
| `contrast_df` | data.frame | — | Must have `Group1`, `Group2`, `Label`; faceted plots also require the facet variable column(s) |
| `main_geom_idx` | integer | `1` | Index of the geom/stat layer used to extract x-positions |
| `contrast_group_name` | character | — | Name of the aesthetic mapping that stores group identifiers |
| `ypad_ratio` | numeric | `0.05` | Vertical padding between brackets as a fraction of per-panel y-range |
| `tolerance` | numeric | `0.1` | Floating-point tolerance for coordinate matching |
| `segment_linewidth` | numeric | `0.4` | Line width of bracket segments |
| `label_size` | numeric | `5` | Text size of annotation labels |
| `label_color` | character | `"black"` | Color for both segments and labels |

### `get_annotation_df()`

Lower-level function that returns the annotation dataframe without drawing. Accepts the same arguments as `draw_stat()` except the three styling arguments.

## Advanced Usage

Use `get_annotation_df()` for custom rendering — for example, displaying p-values instead of asterisks, or rendering annotations above the panel boundary:

```r
df_annot <- get_annotation_df(p, df_stat, contrast_group_name = "comp_group",
                               ypad_ratio = 0.5)

segment_x_pad <- 0.02
p +
  geom_segment(data = df_annot,
    aes(x = Group1.x + segment_x_pad, xend = Group2.x - segment_x_pad,
        y = ymax_adj, yend = ymax_adj),
    linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = df_annot,
    aes(x = Group1.x + segment_x_pad, xend = Group1.x + segment_x_pad,
        y = Group1.y_adj, yend = ymax_adj),
    linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = df_annot,
    aes(x = Group2.x - segment_x_pad, xend = Group2.x - segment_x_pad,
        y = Group2.y_adj, yend = ymax_adj),
    linewidth = 0.5, inherit.aes = FALSE) +
  geom_text(data = df_annot,
    aes(x = mid.x_adj, y = ymax_adj + ypad * 0.6,
        label = paste0("p=", formatC(p.value, digits = 2, format = "e"))),
    size = 8, hjust = 0, size.unit = "pt", color = "red",
    inherit.aes = FALSE) +
  coord_cartesian(clip = "off")
```

### Key Columns in the Annotation Dataframe

| Column | Description |
|---|---|
| `Group1.x`, `Group2.x` | x-coordinates of the two compared groups |
| `Group1.y_adj`, `Group2.y_adj` | Adjusted y-coordinates for vertical bracket legs |
| `ymax_adj` | y-coordinate of the horizontal bracket bar (stacked) |
| `mid.x_adj` | Nudged midpoint x for label placement |
| `ypad` | Computed vertical padding for the panel |
| `panel_name` | Composite panel identifier (faceted plots only) |

## Known Limitations

- **Jittered layers as `main_geom_idx`** — If a jittered geom (e.g., `geom_jitter`, `geom_quasirandom`) is used as the main reference layer, x-coordinates are non-deterministic. Use a deterministic layer (e.g., `stat_summary` with `geom = "col"`) via `main_geom_idx`.
- **Jittered points exceeding ymax** — When jittered points extend above summary geoms, bracket heights may be too low. A workaround is to add an invisible anchor layer: `stat_summary(geom = "point", fun = "max", shape = ".", color = NA)`.
- **Many overlapping comparisons** — With many significant pairwise comparisons, annotations can stack high above the data. Consider filtering `contrast_df` to the most relevant comparisons.
- **`scales = "free_x"` with facets** — If x-axis scales differ across panels, the shared x-coordinate lookup may produce incorrect results. This is uncommon for typical grouped bar plots.

## License

MIT
