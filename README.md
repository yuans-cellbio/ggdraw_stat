# ggplot_draw_stat

Add GraphPad-style statistical annotation brackets to ggplot2 plots programmatically.

`draw_stat` takes a ggplot object and a dataframe of pairwise comparisons, then overlays significance brackets with labels — handling coordinate extraction, bracket stacking, and label positioning automatically.

## Features

- **No special aesthetic required** — group positions are resolved from `contrast_vars` (column names in the plot data), eliminating the need for a custom `comp_group` mapping in `aes()`.
- **Automatic coordinate extraction** via `ggplot_build()` — bracket endpoints are derived from the plot's internal geometry, not hard-coded.
- **Jitter-aware** — correctly handles jittered and beeswarm layers (e.g., `geom_jitter`, `ggbeeswarm::geom_quasirandom`) when computing bracket heights.
- **Facet support** — works with `facet_wrap` and `facet_grid`, including **multiple facet variables** (e.g., `facet_wrap(~A + B)`, `facet_grid(A ~ B)`). Also supports subclasses like `ggh4x::facet_wrap2` and `facet_grid2`.
- **Per-panel y-range** — vertical padding is computed from actual built data per panel, not from shared axis scales.
- **Bracket stacking** — overlapping comparisons are detected and stacked vertically with configurable padding.
- **Label nudging** — midpoint labels are shifted to avoid crossing vertical bracket lines.
- **Input validation** — checks for required columns, valid `contrast_vars`, and layer index range with informative error messages.
- **Customizable appearance** — `segment_linewidth`, `label_size`, `label_color`, `label_hjust`, and `label_vjust` arguments for quick styling.

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

The contrast dataframe must have columns `Group1`, `Group2`, and `Label`. Group names must match the values produced by `paste(contrast_vars, collapse = sep)` — i.e., the same labels that would result from `interaction(contrast_vars, sep = sep)`.

```r
df_stat <- data.frame(
  Group1 = c("Control", "Control"),
  Group2 = c("Treatment_A", "Treatment_B"),
  Label  = c("*", "**")
)
```

### 2. Build your ggplot (no special aesthetic needed)

```r
p <- ggplot(df, aes(x = Treatment, y = Value)) +
  stat_summary(geom = "col", fun = "mean", fill = "steelblue") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", width = 0.3)
```

### 3. Draw annotations

Pass `contrast_vars` — the column name(s) in the plot data that define the contrast groups:

```r
draw_stat(p, df_stat, contrast_vars = "Treatment")
```

## Usage with Interaction Groups

When your design has two factors (e.g., Treatment × Genotype), pass both column names as `contrast_vars`:

```r
p <- ggplot(df, aes(x = Treatment, y = Value)) +
  stat_summary(geom = "col", fun = "mean", aes(fill = Genotype),
               position = position_dodge(0.8), width = 0.6) +
  stat_summary(geom = "errorbar", fun.data = "mean_se",
               aes(group = Genotype),
               position = position_dodge(0.8), width = 0.4)

draw_stat(p, df_stat, contrast_vars = c("Treatment", "Genotype"))
```

Ensure that `Group1` and `Group2` in your contrast dataframe use the same separator (default `" "`) and level ordering as `paste(Treatment, Genotype)`.

## Usage with Facets

For faceted plots, include the facet variable column(s) in your contrast dataframe. The function matches comparisons to panels automatically.

### Single facet variable

```r
# df_stat must contain a "Measurement" column matching the facet variable
p <- ggplot(df, aes(x = Treatment, y = Value)) +
  stat_summary(geom = "col", fun = "mean", aes(fill = Genotype),
               position = position_dodge(0.8), width = 0.6) +
  facet_wrap(~Measurement, scales = "free")

draw_stat(p, df_stat, contrast_vars = c("Treatment", "Genotype"))
```

### Multiple facet variables

When using `facet_wrap(~A + B)` or `facet_grid(A ~ B)`, `contrast_df` must contain columns for **all** facet variables. The function creates a composite panel identifier internally to match annotations to the correct panels.

```r
# df_stat must contain both "Genotype" and "Measurement" columns
p <- ggplot(df, aes(x = Treatment, y = Value)) +
  stat_summary(geom = "col", fun = "mean", aes(fill = Treatment),
               position = position_dodge(0.8), width = 0.6) +
  facet_wrap(~Genotype + Measurement, scales = "free")

draw_stat(p, df_stat, contrast_vars = c("Treatment", "Genotype"), ypad_ratio = 0.1)
```

## Customizing Appearance

`draw_stat` exposes styling arguments directly:

```r
draw_stat(p, df_stat, contrast_vars = c("Treatment", "Genotype"),
          segment_linewidth = 0.3, label_size = 20, label_color = "red")
```

For full control over annotation layers (e.g., custom label formatting, `coord_cartesian(clip = "off")`), use `get_annotation_df()` directly — see [Advanced Usage](#advanced-usage) below.

## API Reference

### `draw_stat()`

Wrapper that computes annotation coordinates and adds bracket + label layers to a ggplot.

| Argument | Type | Default | Description |
|---|---|---|---|
| `p` | ggplot | — | A ggplot object |
| `contrast_df` | data.frame | — | Must have `Group1`, `Group2`, `Label`; faceted plots also require the facet variable column(s) |
| `contrast_vars` | character | — | Column name(s) in `p$data` that define the contrast groups |
| `sep` | character | `" "` | Separator used when pasting `contrast_vars` to build group labels |
| `main_geom_idx` | integer | `1` | Index of the geom/stat layer used to extract x-positions |
| `ypad_ratio` | numeric | `0.05` | Vertical padding between brackets as a fraction of per-panel y-range |
| `tolerance` | numeric | `0.1` | Floating-point tolerance for coordinate matching |
| `segment_linewidth` | numeric | `0.4` | Line width of bracket segments |
| `label_size` | numeric | `12` | Text size of annotation labels (in pt) |
| `label_color` | character | `"black"` | Color for both segments and labels |
| `label_hjust` | numeric | `0.5` | Horizontal justification of annotation labels |
| `label_vjust` | numeric | `0.5` | Vertical justification of annotation labels |

### `get_annotation_df()`

Lower-level function that returns the annotation dataframe without drawing. Accepts the same arguments as `draw_stat()` except the styling arguments (`segment_linewidth`, `label_size`, `label_color`, `label_hjust`, `label_vjust`).

## Advanced Usage

Use `get_annotation_df()` for custom rendering — for example, displaying p-values instead of asterisks, or rendering annotations above the panel boundary:

```r
df_annot <- get_annotation_df(p, df_stat, contrast_vars = c("Treatment", "Genotype"),
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

- **Many overlapping comparisons** — With many significant pairwise comparisons, annotations can stack high above the data. Consider filtering `contrast_df` to the most relevant comparisons.
- **`scales = "free_x"` with facets** — If x-axis scales differ across panels, the shared x-coordinate lookup may produce incorrect results. This is uncommon for typical grouped bar plots.

## License

MIT
