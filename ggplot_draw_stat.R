require(tidyverse)

# Helper functions --------------------------------------------------------

# Make a function to determine assign group indices by whether line intervals overlap.
get_overlapping_groups <- function(x1, x2) {
  # x1 and x2 are two numeric vectors with equal lengths for two groups' x coordinates.
  
  if (length(x1) == 1) return(1)
  # x1 may not be smaller than x2. We need to arrange them.
  # We store starts and ends in a dataframe to keep track of their indices
  df_interval <- data.frame(
    index = 1:length(x1),
    starts = pmin(x1, x2),
    ends = pmax(x1, x2)
  )
  
  # We order by starts and ends so that starts[i] >= starts[i-1]
  df_interval <- df_interval %>%
    arrange(starts, ends)
  
  # Initialize group indices.
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
  
  df_interval <- df_interval %>%
    arrange(index)
  
  return(df_interval$group)
}

# Make a function to adjust `ymax`.
nudge_y <- function(x, pad) {
  # x is a numeric vector in ascending order.
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
  # `mid.x` is passed to x.
  # Group1.x and Group2.x are passed to a and b.
  # ymax_adj is passed to z.
  
  if (length(x) == 1) return(x)
  
  if (is.null(panel_names)) {
    xs <- unique(c(a, b))
    for (i in 1:length(x)) {
      # Only nudge if there is a vertical line passing through the mid point.
      if (any(abs(xs - x[i]) < tolerance)) {
        passing_group_x <- xs[which(abs(xs - x[i]) < tolerance)][1]
        passing_group_ys <- z[which(abs(a - passing_group_x) < tolerance | abs(b - passing_group_x) < tolerance)]
        if (any(z[i] < passing_group_ys)) {
          adj_x <- find_adjacent_x(x[i], pb_data = pb_data, main_geom_idx = main_geom_idx, direction = "right", tolerance = tolerance)
          if (length(adj_x) > 0 && !is.na(adj_x)) {
            x[i] <- mean(c(x[i], adj_x))
          }
        }
      }
    }
  } else {
    unique_panels <- unique(panel_names)
    for (panel in unique_panels) {
      panel_mask <- panel_names == panel
      xs <- unique(c(a[panel_mask], b[panel_mask]))
      panel_indices <- which(panel_mask)
      for (i in panel_indices) {
        if (any(abs(xs - x[i]) < tolerance)) {
          passing_group_x <- xs[which(abs(xs - x[i]) < tolerance)][1]
          passing_group_ys <- z[which((abs(a - passing_group_x) < tolerance | abs(b - passing_group_x) < tolerance) & panel_mask)]
          if (any(z[i] < passing_group_ys)) {
            adj_x <- find_adjacent_x(x[i], current_panel_name = panel, pb_data = pb_data, main_geom_idx = main_geom_idx, direction = "right", tolerance = tolerance)
            if (length(adj_x) > 0 && !is.na(adj_x)) {
              x[i] <- mean(c(x[i], adj_x))
            }
          }
        }
      }
    }
  }
  return(x)
}

# Get the adjacent contrast group x to use for nudging.
find_adjacent_x <- function(mid.x, current_panel_name = NULL, pb_data, main_geom_idx = 1, direction, tolerance = 0.1) {
  if (!is.null(current_panel_name)) {
    contrast_group_xs <- pb_data %>%
      filter(panel_name == current_panel_name, geom_idx == main_geom_idx)
  } else {
    contrast_group_xs <- pb_data %>%
      filter(geom_idx == main_geom_idx)
  }
  contrast_group_xs <- contrast_group_xs %>%
    pull(x) %>%
    unique() %>%
    as.numeric() %>%
    sort()
  match_idx <- which(abs(contrast_group_xs - mid.x) < tolerance)
  if (length(match_idx) == 0) return(NA_real_)
  if (direction == "left") {
    target_idx <- match_idx[1] - 1
  } else {
    target_idx <- match_idx[1] + 1
  }
  if (target_idx < 1 || target_idx > length(contrast_group_xs)) return(NA_real_)
  return(contrast_group_xs[target_idx])
}


# Facet detection ---------------------------------------------------------

# Retrieve facet variable names from a ggplot object.
# Supports facet_wrap (single or multiple variables) and facet_grid,
# including subclasses such as ggh4x::facet_wrap2 / facet_grid2.
get_facet_var_names <- function(p) {
  facet <- p$facet
  
  if (inherits(facet, "FacetNull")) {
    return(NULL)
  } else if (inherits(facet, "FacetWrap")) {
    # Covers FacetWrap and subclasses (e.g., ggh4x::FacetWrap2)
    return(map_chr(facet$params$facets, rlang::as_label))
  } else if (inherits(facet, "FacetGrid")) {
    # Covers FacetGrid and subclasses (e.g., ggh4x::FacetGrid2)
    row_vars <- map_chr(facet$params$rows, rlang::as_label)
    col_vars <- map_chr(facet$params$cols, rlang::as_label)
    return(c(row_vars, col_vars))
  } else {
    warning("Unrecognized facet class: ", class(facet)[1], ". Proceeding without facet support.")
    return(NULL)
  }
}

# Build a panel lookup table mapping PANEL index to composite facet label.
# Uses ggplot_build layout data for reliable mapping.
build_panel_map <- function(pb, facet_var_names) {
  layout <- pb$layout$layout
  # Create composite panel name by pasting facet variable values
  layout$panel_name <- apply(layout[, facet_var_names, drop = FALSE], 1, paste, collapse = " | ")
  return(layout[, c("PANEL", "panel_name")])
}


# Input validation --------------------------------------------------------

validate_inputs <- function(p, contrast_df, contrast_group_name, main_geom_idx) {
  # Check that contrast_df has required columns
  required_cols <- c("Group1", "Group2", "Label")
  missing_cols <- setdiff(required_cols, names(contrast_df))
  if (length(missing_cols) > 0) {
    stop("contrast_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check that contrast_group_name exists in ggplot aesthetics
  pb <- ggplot_build(p)
  if (!contrast_group_name %in% names(pb$data[[1]])) {
    stop(
      "contrast_group_name '", contrast_group_name,
      "' not found in built plot data. ",
      "Ensure it is mapped in aes() of the ggplot call."
    )
  }
  
  # Check main_geom_idx is within range
  n_layers <- length(p$layers)
  if (main_geom_idx < 1 || main_geom_idx > n_layers) {
    stop("main_geom_idx (", main_geom_idx, ") is out of range. Plot has ", n_layers, " layer(s).")
  }
  
  invisible(TRUE)
}


# Main function -----------------------------------------------------------

# Main function to calculate the statistical annotations.
get_annotation_df <- function(p, contrast_df, main_geom_idx = 1, contrast_group_name,
                              ypad_ratio = 0.05, tolerance = 0.1) {
  # `p` is a ggplot object.
  # `contrast_df` is a dataframe with columns `Group1`, `Group2`, and `Label`.
  #   Group1 and Group2 must match the variables mapped via `contrast_group_name`.
  #   For faceted plots, contrast_df must also contain the facet variable column(s).
  # `main_geom_idx` is the index of the geom/stat layer used to retrieve x positions.
  # `contrast_group_name` is the name of the aesthetic in aes() that stores contrast group names.
  # `ypad_ratio` controls vertical padding between annotation segments.
  # `tolerance` is used for floating-point comparisons and label nudging.
  
  validate_inputs(p, contrast_df, contrast_group_name, main_geom_idx)
  
  # Retrieve data passed to ggplot.
  df_p_data <- p$data
  
  # Retrieve facet variable names (supports multi-variable facets).
  facet_var_names <- get_facet_var_names(p)
  has_facet <- !is.null(facet_var_names)
  
  # Build the plot to extract computed coordinates.
  pb <- ggplot_build(p)
  
  # Assemble built data across all layers.
  # Use any_of() for safe column selection — not all layers have all columns.
  df_pb_data <- imap_dfr(pb$data, function(d, i) {
    d %>%
      mutate(geom_idx = i) %>%
      select(geom_idx, any_of(c(contrast_group_name, "PANEL", "x", "y", "xmin", "xmax", "ymin", "ymax")))
  })
  
  # Ensure y, ymin, ymax columns exist (some layers may lack them).
  if (!"y" %in% names(df_pb_data)) df_pb_data$y <- NA_real_
  if (!"ymin" %in% names(df_pb_data)) df_pb_data$ymin <- NA_real_
  if (!"ymax" %in% names(df_pb_data)) df_pb_data$ymax <- NA_real_
  
  # Coalesce ymax: use ymax where available, fall back to y.
  df_pb_data <- df_pb_data %>%
    mutate(ymax = ifelse(is.na(ymax), y, ymax))
  
  # Map PANEL indices to panel names for faceted plots.
  if (has_facet) {
    panel_map <- build_panel_map(pb, facet_var_names)
    df_pb_data <- df_pb_data %>%
      left_join(panel_map, by = "PANEL")
    
    # Build a composite facet column in contrast_df to match panel_name.
    if (length(facet_var_names) == 1) {
      contrast_df$panel_name <- as.character(contrast_df[[facet_var_names]])
    } else {
      contrast_df$panel_name <- apply(
        contrast_df[, facet_var_names, drop = FALSE], 1, paste, collapse = " | "
      )
    }
  }
  
  # Validate that Group1/Group2 values exist in the built data.
  available_groups <- unique(df_pb_data[[contrast_group_name]])
  bad_g1 <- setdiff(unique(contrast_df$Group1), available_groups)
  bad_g2 <- setdiff(unique(contrast_df$Group2), available_groups)
  bad_groups <- unique(c(bad_g1, bad_g2))
  if (length(bad_groups) > 0) {
    warning(
      "The following Group1/Group2 values in contrast_df do not match any '",
      contrast_group_name, "' values in the plot data and will be dropped: ",
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
  cgn <- contrast_group_name # shorthand for tidy eval
  
  # Helper: safe max that returns NA instead of -Inf for empty input.
  safe_max <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    max(x)
  }
  
  if (has_facet) {
    # Use a loop approach to avoid .data ambiguity inside rowwise + nested filter.
    stat_rows <- vector("list", nrow(contrast_df))
    for (ri in seq_len(nrow(contrast_df))) {
      row <- contrast_df[ri, , drop = FALSE]
      cur_panel <- row$panel_name
      cur_g1 <- row$Group1
      cur_g2 <- row$Group2
      
      g1x <- df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(cgn) == cur_g1) %>% pull(x) %>% mean(na.rm = TRUE)
      g2x <- df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(cgn) == cur_g2) %>% pull(x) %>% mean(na.rm = TRUE)
      g1y <- df_pb_data %>% filter(panel_name == cur_panel, !!sym(cgn) == cur_g1) %>% pull(ymax) %>% safe_max()
      g2y <- df_pb_data %>% filter(panel_name == cur_panel, !!sym(cgn) == cur_g2) %>% pull(ymax) %>% safe_max()
      ym  <- df_pb_data %>%
        filter(
          panel_name == cur_panel,
          x >= min(g1x, g2x) - tolerance,
          x <= max(g1x, g2x) + tolerance
        ) %>% pull(ymax) %>% safe_max()
      
      stat_rows[[ri]] <- row %>% mutate(
        Group1.x = g1x, Group2.x = g2x,
        Group1.y = g1y, Group2.y = g2y,
        ymax = ym,
        mid.x = mean(c(g1x, g2x))
      )
    }
    df_stat <- bind_rows(stat_rows)
  } else {
    df_stat <- contrast_df %>%
      rowwise() %>%
      mutate(
        Group1.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(cgn) == Group1) %>% pull(x) %>% mean(na.rm = TRUE),
        Group2.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(cgn) == Group2) %>% pull(x) %>% mean(na.rm = TRUE),
        Group1.y = df_pb_data %>% filter(!!sym(cgn) == Group1) %>% pull(ymax) %>% safe_max(),
        Group2.y = df_pb_data %>% filter(!!sym(cgn) == Group2) %>% pull(ymax) %>% safe_max(),
        ymax = df_pb_data %>%
          filter(
            x >= min(Group1.x, Group2.x) - tolerance,
            x <= max(Group1.x, Group2.x) + tolerance
          ) %>% pull(ymax) %>% safe_max(),
        mid.x = mean(c(Group1.x, Group2.x))
      ) %>%
      ungroup()
  }
  
  # Guard against NA/NaN/-Inf from empty filter results.
  df_stat <- df_stat %>%
    filter(
      is.finite(Group1.x), is.finite(Group2.x),
      is.finite(Group1.y), is.finite(Group2.y),
      is.finite(ymax)
    )
  
  if (nrow(df_stat) == 0) {
    warning("All contrasts produced non-finite coordinates. Returning empty annotation dataframe.")
    return(tibble())
  }
  
  # Retrieve y ranges per panel for padding calculation.
  # panel_scales_y reflects the *scale* range, which is shared across panels when
  # scales are not free (facet_grid default, facet_wrap without scales="free").
  # Instead, compute per-panel y-range from the actual built data for accurate padding.
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
        mid.x_adj = nudge_x(
          mid.x, Group1.x, Group2.x, ymax_adj,
          panel_names = panel_name,
          pb_data = df_pb_data, main_geom_idx = main_geom_idx, tolerance = tolerance
        )
      ) %>%
      ungroup()
  } else {
    df_stat <- df_stat %>%
      mutate(
        mid.x_adj = nudge_x(
          mid.x, Group1.x, Group2.x, ymax_adj,
          panel_names = NULL,
          pb_data = df_pb_data, main_geom_idx = main_geom_idx, tolerance = tolerance
        )
      )
  }
  
  return(df_stat)
}


# Wrapper function for drawing --------------------------------------------

draw_stat <- function(p, contrast_df, main_geom_idx = 1, contrast_group_name,
                      ypad_ratio = 0.05, tolerance = 0.1,
                      segment_linewidth = 0.4, label_size = 5, label_color = "black") {
  df_stat_annot <- get_annotation_df(
    p, contrast_df,
    main_geom_idx = main_geom_idx,
    contrast_group_name = contrast_group_name,
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
