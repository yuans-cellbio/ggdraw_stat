require(tidyverse)

# Helper functions --------------------------------------------------------

# Assign group indices by whether line intervals (x1, x2) overlap.
get_overlapping_groups <- function(x1, x2) {
  if (length(x1) == 1) return(1)
  df_interval <- data.frame(
    index = 1:length(x1),
    starts = pmin(x1, x2),
    ends = pmax(x1, x2)
  )
  df_interval <- df_interval %>% arrange(starts, ends)
  df_interval$group <- 1
  overlapping_interval_max <- df_interval$ends[1]
  for (i in 2:nrow(df_interval)) {
    if (df_interval$starts[i] <= overlapping_interval_max) {
      overlapping_interval_max <- max(df_interval$ends[i], overlapping_interval_max)
      df_interval$group[i] <- df_interval$group[i - 1]
    } else {
      df_interval$group[i] <- df_interval$group[i - 1] + 1
    }
  }
  df_interval <- df_interval %>% arrange(index)
  return(df_interval$group)
}

# Adjust ymax values so stacked annotations don't overlap.
nudge_y <- function(x, pad) {
  if (length(x) == 1) return(x)
  if (any(!is.finite(x) | !is.finite(pad))) {
    warning("Non-finite values detected in nudge_y input. Returning x unchanged.")
    return(x)
  }
  for (i in 2:length(x)) {
    if (x[i] - x[i - 1] < pad[i]) x[i] <- x[i - 1] + pad[i]
  }
  return(x)
}

nudge_x <- function(x, a, b, z, panel_names, pb_data, main_geom_idx = 1, tolerance = 0.1) {
  if (length(x) == 1) return(x)

  .nudge_single_panel <- function(x, a, b, z, pb_data, main_geom_idx, tolerance, current_panel_name = NULL) {
    xs <- unique(c(a, b))
    for (i in seq_along(x)) {
      if (any(abs(xs - x[i]) < tolerance)) {
        passing_group_x <- xs[which(abs(xs - x[i]) < tolerance)][1]
        passing_group_ys <- z[which(abs(a - passing_group_x) < tolerance | abs(b - passing_group_x) < tolerance)]
        if (any(z[i] < passing_group_ys)) {
          adj_x <- find_adjacent_x(x[i], current_panel_name = current_panel_name,
                                   pb_data = pb_data, main_geom_idx = main_geom_idx,
                                   direction = "right", tolerance = tolerance)
          if (length(adj_x) > 0 && !is.na(adj_x)) {
            x[i] <- mean(c(x[i], adj_x))
          }
        }
      }
    }
    x
  }

  if (is.null(panel_names)) {
    x <- .nudge_single_panel(x, a, b, z, pb_data, main_geom_idx, tolerance)
  } else {
    for (panel in unique(panel_names)) {
      mask <- panel_names == panel
      idx <- which(mask)
      x[idx] <- .nudge_single_panel(
        x[idx], a[idx], b[idx], z[idx],
        pb_data, main_geom_idx, tolerance, current_panel_name = panel
      )
    }
  }
  return(x)
}

find_adjacent_x <- function(mid.x, current_panel_name = NULL, pb_data, main_geom_idx = 1, direction, tolerance = 0.1) {
  if (!is.null(current_panel_name)) {
    contrast_group_xs <- pb_data %>% filter(panel_name == current_panel_name, geom_idx == main_geom_idx)
  } else {
    contrast_group_xs <- pb_data %>% filter(geom_idx == main_geom_idx)
  }
  contrast_group_xs <- contrast_group_xs %>% pull(x) %>% unique() %>% as.numeric() %>% sort()
  match_idx <- which(abs(contrast_group_xs - mid.x) < tolerance)
  if (length(match_idx) == 0) return(NA_real_)
  target_idx <- if (direction == "left") match_idx[1] - 1 else match_idx[1] + 1
  if (target_idx < 1 || target_idx > length(contrast_group_xs)) return(NA_real_)
  return(contrast_group_xs[target_idx])
}

safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}


# Facet detection ---------------------------------------------------------

get_facet_var_names <- function(p) {
  facet <- p$facet
  if (inherits(facet, "FacetNull")) {
    return(NULL)
  } else if (inherits(facet, "FacetWrap")) {
    return(map_chr(facet$params$facets, rlang::as_label))
  } else if (inherits(facet, "FacetGrid")) {
    row_vars <- map_chr(facet$params$rows, rlang::as_label)
    col_vars <- map_chr(facet$params$cols, rlang::as_label)
    return(c(row_vars, col_vars))
  } else {
    warning("Unrecognized facet class: ", class(facet)[1], ". Proceeding without facet support.")
    return(NULL)
  }
}

build_panel_map <- function(pb, facet_var_names) {
  layout <- pb$layout$layout
  layout$panel_name <- apply(layout[, facet_var_names, drop = FALSE], 1, paste, collapse = " | ")
  return(layout[, c("PANEL", "panel_name")])
}


# Group-label <-> x-position lookup ---------------------------------------

#' Build a lookup table mapping contrast group labels to built x-positions.
#'
#' This replaces the old `comp_group` aesthetic approach.
#' The function computes `interaction(contrast_vars, sep)` over the original
#' data (`p$data`), then identifies the unique group labels. For each label
#' it finds the matching rows in the built layer data (by matching the
#' combination of discrete aesthetics / group index) and extracts the numeric
#' x position from the main reference layer.
#'
#' Strategy:
#'   1. In p$data, compute the contrast label for every row.
#'   2. Each unique label also maps to a unique combination of the *discrete*
#'      aesthetics that ggplot2 uses to compute `group`. So we also compute
#'      the ggplot-internal `group` integer for each row from the built data
#'      of layer `main_geom_idx`.
#'   3. Because stat_summary aggregates rows, we do the join at the
#'      *unique label* level: for each label we know its (x, group) pair
#'      in built data, which gives us the numeric x position.
build_group_position_lookup <- function(p, pb, contrast_vars, sep, main_geom_idx) {

  orig_data <- p$data

  # Validate that contrast_vars exist in the original data.
  missing_vars <- setdiff(contrast_vars, names(orig_data))
  if (length(missing_vars) > 0) {
    stop(
      "contrast_vars not found in plot data: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  # Compute the contrast label for every row of the original data.
  orig_data$.contrast_label <- apply(
    orig_data[, contrast_vars, drop = FALSE], 1,
    function(row) paste(row, collapse = sep)
  )

  # We need to figure out which built-data group index corresponds to which

  # contrast label. The built data for layer `main_geom_idx` has columns
  # `x` (numeric) and `group` (integer). The `group` integer is assigned by
  # ggplot2 as the interaction of all discrete aesthetics.
  #
  # The most reliable approach: for the reference layer, ggplot2 evaluates
  # the layer's aesthetics on the original data in the same row order, then
  # stat_summary collapses. We can recover the mapping by building a small
  # bridge: for each unique combination of contrast_vars in the original data,
  # determine its x factor level (from the x aesthetic) and its group integer.

  built_layer <- pb$data[[main_geom_idx]]

  # Extract the x aesthetic expression from the layer (or fall back to global).
  layer_mapping <- p$layers[[main_geom_idx]]$mapping
  global_mapping <- p$mapping
  x_expr <- layer_mapping$x %||% global_mapping$x
  if (is.null(x_expr)) stop("Cannot determine x aesthetic for layer ", main_geom_idx)

  # Evaluate the x expression on the original data to get factor labels.
  x_evaluated <- rlang::eval_tidy(x_expr, data = orig_data)
  orig_data$.x_label <- as.character(x_evaluated)

  # Get the x-scale breaks/labels from the built plot to map factor labels -> numeric x.
  # For discrete scales, panel_params stores the mapping.
  panel_params <- pb$layout$panel_params[[1]]

  # ggplot2 stores discrete x mapping in different places depending on version.
  # Common patterns: $x$limits, $x.sec$limits, or $x$get_labels().
  # The safest approach: the built data for the reference layer already has
  # numeric x, and the scale has labels. We build the map from unique (x, label) pairs.
  if (!is.null(panel_params$x$limits)) {
    x_labels <- panel_params$x$limits
  } else if (!is.null(panel_params$x.labels)) {
    x_labels <- panel_params$x.labels
  } else {
    # Fallback: get unique x-label pairs from evaluating on the built data.
    x_labels <- levels(as.factor(x_evaluated))
  }
  # Numeric positions are just 1, 2, 3, ... for discrete scales.
  x_label_to_num <- setNames(seq_along(x_labels), x_labels)

  # Now build the unique contrast_label -> numeric x position map.
  # For non-dodged plots (x == group), numeric x comes directly from x_label_to_num.
  # For dodged plots (x != group), we need the actual dodged position from built data.

  # Determine if dodging is involved by checking if multiple contrast labels
  # share the same x_label.
  label_x_map <- orig_data %>%
    select(.contrast_label, .x_label) %>%
    distinct()

  is_dodged <- any(duplicated(label_x_map$.x_label))

  if (!is_dodged) {
    # Simple case: each contrast label has a unique x position.
    lookup <- label_x_map %>%
      mutate(x_num = x_label_to_num[.x_label]) %>%
      select(.contrast_label, x_num)
  } else {
    # Dodged case: we need to extract the actual dodged x positions from built data.
    # The built data for stat_summary has one row per unique (x, group, PANEL).
    # We need to map the `group` integer back to our contrast labels.
    #
    # ggplot2 computes `group` as the interaction of all discrete aesthetics.
    # We replicate that: evaluate all discrete aesthetics on orig_data, compute
    # the interaction, and match against the integer group in built data.

    # Collect all discrete aesthetic expressions (x, fill, colour, group, etc.)
    all_aes <- modifyList(as.list(global_mapping), as.list(layer_mapping))
    # Remove non-grouping aesthetics
    non_grouping <- c("y", "ymin", "ymax", "label", "weight", "sample")
    all_aes <- all_aes[!names(all_aes) %in% non_grouping]

    # Evaluate each aesthetic on original data
    aes_evaluated <- lapply(all_aes, function(expr) {
      as.character(rlang::eval_tidy(expr, data = orig_data))
    })

    # Compute the interaction the same way ggplot2 does
    if (length(aes_evaluated) > 0) {
      orig_data$.gg_group_label <- do.call(interaction, c(aes_evaluated, list(drop = TRUE, sep = ".")))
    } else {
      orig_data$.gg_group_label <- factor(rep(1, nrow(orig_data)))
    }

    # Map: each unique .gg_group_label -> integer (in factor level order)
    gg_group_levels <- levels(orig_data$.gg_group_label)
    gg_group_to_int <- setNames(seq_along(gg_group_levels), gg_group_levels)
    orig_data$.gg_group_int <- gg_group_to_int[as.character(orig_data$.gg_group_label)]

    # Build unique mapping: contrast_label -> (gg_group_int, x_label)
    label_group_map <- orig_data %>%
      select(.contrast_label, .gg_group_int, .x_label) %>%
      distinct()

    # From the built layer, extract unique (group, x) pairs (take PANEL == 1 as reference).
    built_ref <- built_layer %>%
      filter(PANEL == 1) %>%
      select(x, group) %>%
      distinct()

    # Join to get numeric x for each contrast label.
    lookup <- label_group_map %>%
      left_join(built_ref, by = c(".gg_group_int" = "group")) %>%
      select(.contrast_label, x_num = x)
  }

  return(lookup)
}


# Input validation --------------------------------------------------------

validate_inputs <- function(p, contrast_df, contrast_vars, main_geom_idx) {
  required_cols <- c("Group1", "Group2", "Label")
  missing_cols <- setdiff(required_cols, names(contrast_df))
  if (length(missing_cols) > 0) {
    stop("contrast_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  n_layers <- length(p$layers)
  if (main_geom_idx < 1 || main_geom_idx > n_layers) {
    stop("main_geom_idx (", main_geom_idx, ") is out of range. Plot has ", n_layers, " layer(s).")
  }

  missing_vars <- setdiff(contrast_vars, names(p$data))
  if (length(missing_vars) > 0) {
    stop(
      "contrast_vars not found in plot data: ",
      paste(missing_vars, collapse = ", "),
      ". Available columns: ", paste(names(p$data), collapse = ", ")
    )
  }

  invisible(TRUE)
}


# Main function -----------------------------------------------------------

#' Calculate annotation coordinates for statistical comparisons.
#'
#' @param p A ggplot object. No special aesthetic (like `comp_group`) is needed.
#' @param contrast_df Data frame with columns `Group1`, `Group2`, `Label`.
#'   Group labels must match `interaction(contrast_vars, sep = sep)`.
#'   For faceted plots, must also contain facet variable columns.
#' @param contrast_vars Character vector of variable names in `p$data` that
#'   define the contrast groups (e.g., `c("Fct1", "Fct2")`).
#' @param sep Separator used in `interaction()` for building group labels.
#'   Must match how Group1/Group2 are encoded in `contrast_df`. Default `" "`.
#' @param main_geom_idx Index of the geom/stat layer to extract x positions from.
#' @param ypad_ratio Vertical padding ratio between stacked annotations.
#' @param tolerance Floating-point tolerance for position matching.
get_annotation_df <- function(p, contrast_df, contrast_vars, sep = " ",
                              main_geom_idx = 1,
                              ypad_ratio = 0.05, tolerance = 0.1) {

  validate_inputs(p, contrast_df, contrast_vars, main_geom_idx)

  # Retrieve facet variable names.
  facet_var_names <- get_facet_var_names(p)
  has_facet <- !is.null(facet_var_names)

  # Build the plot to extract computed coordinates.
  pb <- ggplot_build(p)

  # Build the group-label -> x-position lookup table.
  group_lookup <- build_group_position_lookup(p, pb, contrast_vars, sep, main_geom_idx)

  # Assemble built data across all layers.
  # Instead of selecting by contrast_group_name, we add the contrast label
  # by joining the original data's label back in.
  #
  # We need to tag each row of built data with its contrast_label.
  # For the reference layer this comes from the lookup. For other layers
  # we use a positional approach: match by (x, group, PANEL).
  #
  # Simpler approach: just tag via the x position, since the lookup gives us
  # the x -> label mapping. We build a reverse map x_num -> contrast_label.
  x_to_label <- setNames(group_lookup$.contrast_label, as.character(group_lookup$x_num))

  df_pb_data <- imap_dfr(pb$data, function(d, i) {
    d %>%
      mutate(geom_idx = i) %>%
      select(geom_idx, any_of(c("PANEL", "x", "y", "xmin", "xmax", "ymin", "ymax", "group")))
  })

  # Ensure y columns exist.
  if (!"y" %in% names(df_pb_data)) df_pb_data$y <- NA_real_
  if (!"ymin" %in% names(df_pb_data)) df_pb_data$ymin <- NA_real_
  if (!"ymax" %in% names(df_pb_data)) df_pb_data$ymax <- NA_real_
  df_pb_data <- df_pb_data %>%
    mutate(ymax = ifelse(is.na(ymax), y, ymax))

  # Tag each row with its contrast label by matching x position (within tolerance).
  # This handles both dodged and non-dodged layouts.
  lookup_x <- group_lookup$x_num
  lookup_label <- group_lookup$.contrast_label
  df_pb_data$.contrast_label <- vapply(df_pb_data$x, function(xi) {
    match_idx <- which(abs(lookup_x - xi) < tolerance)
    if (length(match_idx) == 0) return(NA_character_)
    lookup_label[match_idx[1]]
  }, character(1))

  # Map PANEL indices to panel names for faceted plots.
  if (has_facet) {
    panel_map <- build_panel_map(pb, facet_var_names)
    df_pb_data <- df_pb_data %>% left_join(panel_map, by = "PANEL")

    if (length(facet_var_names) == 1) {
      contrast_df$panel_name <- as.character(contrast_df[[facet_var_names]])
    } else {
      contrast_df$panel_name <- apply(
        contrast_df[, facet_var_names, drop = FALSE], 1, paste, collapse = " | "
      )
    }
  }

  # Validate Group1/Group2 against available labels.
  available_groups <- unique(na.omit(df_pb_data$.contrast_label))
  bad_groups <- unique(c(
    setdiff(unique(contrast_df$Group1), available_groups),
    setdiff(unique(contrast_df$Group2), available_groups)
  ))
  if (length(bad_groups) > 0) {
    warning(
      "The following Group1/Group2 values do not match any contrast group ",
      "labels (interaction of ", paste(contrast_vars, collapse = ", "),
      " with sep='", sep, "') and will be dropped: ",
      paste(bad_groups, collapse = ", ")
    )
    contrast_df <- contrast_df %>%
      filter(Group1 %in% available_groups, Group2 %in% available_groups)
  }
  if (nrow(contrast_df) == 0) {
    warning("No valid contrasts remain after filtering. Returning empty annotation dataframe.")
    return(tibble())
  }

  # Calculate x and y coordinates for each contrast.
  if (has_facet) {
    stat_rows <- vector("list", nrow(contrast_df))
    for (ri in seq_len(nrow(contrast_df))) {
      row <- contrast_df[ri, , drop = FALSE]
      cur_panel <- row$panel_name
      cur_g1 <- row$Group1
      cur_g2 <- row$Group2

      g1x <- df_pb_data %>% filter(geom_idx == main_geom_idx, .contrast_label == cur_g1) %>% pull(x) %>% mean(na.rm = TRUE)
      g2x <- df_pb_data %>% filter(geom_idx == main_geom_idx, .contrast_label == cur_g2) %>% pull(x) %>% mean(na.rm = TRUE)
      g1y <- df_pb_data %>% filter(panel_name == cur_panel, .contrast_label == cur_g1) %>% pull(ymax) %>% safe_max()
      g2y <- df_pb_data %>% filter(panel_name == cur_panel, .contrast_label == cur_g2) %>% pull(ymax) %>% safe_max()
      ym  <- df_pb_data %>%
        filter(panel_name == cur_panel,
               x >= min(g1x, g2x) - tolerance,
               x <= max(g1x, g2x) + tolerance) %>%
        pull(ymax) %>% safe_max()

      stat_rows[[ri]] <- row %>% mutate(
        Group1.x = g1x, Group2.x = g2x,
        Group1.y = g1y, Group2.y = g2y,
        ymax = ym, mid.x = mean(c(g1x, g2x))
      )
    }
    df_stat <- bind_rows(stat_rows)
  } else {
    df_stat <- contrast_df %>%
      rowwise() %>%
      mutate(
        Group1.x = df_pb_data %>% filter(geom_idx == main_geom_idx, .contrast_label == Group1) %>% pull(x) %>% mean(na.rm = TRUE),
        Group2.x = df_pb_data %>% filter(geom_idx == main_geom_idx, .contrast_label == Group2) %>% pull(x) %>% mean(na.rm = TRUE),
        Group1.y = df_pb_data %>% filter(.contrast_label == Group1) %>% pull(ymax) %>% safe_max(),
        Group2.y = df_pb_data %>% filter(.contrast_label == Group2) %>% pull(ymax) %>% safe_max(),
        ymax = df_pb_data %>%
          filter(x >= min(Group1.x, Group2.x) - tolerance,
                 x <= max(Group1.x, Group2.x) + tolerance) %>%
          pull(ymax) %>% safe_max(),
        mid.x = mean(c(Group1.x, Group2.x))
      ) %>%
      ungroup()
  }

  # Guard against non-finite coordinates.
  df_stat <- df_stat %>%
    filter(is.finite(Group1.x), is.finite(Group2.x),
           is.finite(Group1.y), is.finite(Group2.y),
           is.finite(ymax))

  if (nrow(df_stat) == 0) {
    warning("All contrasts produced non-finite coordinates. Returning empty annotation dataframe.")
    return(tibble())
  }

  # Compute per-panel y range for padding.
  if (has_facet) {
    panel_y_range <- df_pb_data %>%
      group_by(panel_name) %>%
      summarise(y_range = max(ymax, na.rm = TRUE) - min(c(y, ymin), na.rm = TRUE), .groups = "drop") %>%
      deframe()
    df_stat <- df_stat %>%
      group_by(panel_name) %>%
      mutate(
        overlapping_group = get_overlapping_groups(Group1.x, Group2.x),
        ypad = panel_y_range[first(panel_name)] * ypad_ratio
      ) %>%
      ungroup()
  } else {
    y_range_global <- max(df_pb_data$ymax, na.rm = TRUE) - min(c(df_pb_data$y, df_pb_data$ymin), na.rm = TRUE)
    df_stat <- df_stat %>%
      mutate(
        overlapping_group = get_overlapping_groups(Group1.x, Group2.x),
        ypad = y_range_global * ypad_ratio
      )
  }

  # Group for vertical stacking.
  if (has_facet) {
    df_stat <- df_stat %>% group_by(panel_name, overlapping_group)
  } else {
    df_stat <- df_stat %>% group_by(overlapping_group)
  }

  df_stat <- df_stat %>%
    arrange(ymax, abs(Group1.x - Group2.x), .by_group = TRUE) %>%
    mutate(
      ymax_adj = nudge_y(ymax, ypad) + ypad * 2,
      Group1.y_adj = Group1.y + ypad,
      Group2.y_adj = Group2.y + ypad
    ) %>%
    ungroup()

  # Nudge label x to avoid crossing vertical segments.
  if (has_facet) {
    df_stat <- df_stat %>%
      group_by(panel_name) %>%
      mutate(
        mid.x_adj = nudge_x(mid.x, Group1.x, Group2.x, ymax_adj,
                            panel_names = panel_name,
                            pb_data = df_pb_data, main_geom_idx = main_geom_idx,
                            tolerance = tolerance)
      ) %>%
      ungroup()
  } else {
    df_stat <- df_stat %>%
      mutate(
        mid.x_adj = nudge_x(mid.x, Group1.x, Group2.x, ymax_adj,
                            panel_names = NULL,
                            pb_data = df_pb_data, main_geom_idx = main_geom_idx,
                            tolerance = tolerance)
      )
  }

  return(df_stat)
}


# Wrapper function for drawing --------------------------------------------

#' Add statistical comparison brackets to a ggplot.
#'
#' @inheritParams get_annotation_df
#' @param segment_linewidth Line width for bracket segments.
#' @param label_size Text size for annotation labels.
#' @param label_color Color for segments and labels.
draw_stat <- function(p, contrast_df, contrast_vars, sep = " ",
                      main_geom_idx = 1,
                      ypad_ratio = 0.05, tolerance = 0.1,
                      segment_linewidth = 0.4, label_size = 5, label_color = "black") {
  df_stat_annot <- get_annotation_df(
    p, contrast_df,
    contrast_vars = contrast_vars,
    sep = sep,
    main_geom_idx = main_geom_idx,
    ypad_ratio = ypad_ratio,
    tolerance = tolerance
  )

  if (nrow(df_stat_annot) == 0) {
    message("No annotations to draw.")
    return(p)
  }

  p +
    geom_segment(
      data = df_stat_annot,
      aes(x = Group1.x, y = ymax_adj, xend = Group2.x, yend = ymax_adj),
      linewidth = segment_linewidth, color = label_color, inherit.aes = FALSE
    ) +
    geom_segment(
      data = df_stat_annot,
      aes(x = Group1.x, y = Group1.y_adj, xend = Group1.x, yend = ymax_adj),
      linewidth = segment_linewidth, color = label_color, inherit.aes = FALSE
    ) +
    geom_segment(
      data = df_stat_annot,
      aes(x = Group2.x, y = Group2.y_adj, xend = Group2.x, yend = ymax_adj),
      linewidth = segment_linewidth, color = label_color, inherit.aes = FALSE
    ) +
    geom_text(
      data = df_stat_annot,
      aes(x = mid.x_adj, y = ymax_adj, label = Label),
      size = label_size, color = label_color, hjust = 0.5, vjust = -0.5, inherit.aes = FALSE
    )
}
