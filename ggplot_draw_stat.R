require(emmeans)

# Helper functions and variables
# Direct comparison between flaoting point values may give unexpected results. 
# Evaluate difference instead. 
tolerance <- 0.1

# Make a function to determine assign group indices by whether lines intervals overlap. 
get_overlapping_groups <- function(x1, x2){
  # x1 and x2 are two numeric vectors with equal lengths for two groups' x coordinates. 
  
  if (length(x1) == 1) return(1)
  # x1 may not be smaller than x2. We need to arrange them. 
  # We store starts and ends in a dataframe to keep track of their indices
  df_interval <- data.frame(
    index = 1:length(x1), 
    starts = pmin(x1, x2), 
    ends = pmax(x1, x2)
  )
  
  # We order by starts and ends so that starts[i] >= start[i-1]
  # For each unique starts, ends[i] < ends[i-1]
  # There should be no cage where starts[i] == starts[i-1] & ends[i] == ends[i-1]
  df_interval <- df_interval %>%
    arrange(starts, ends)
  
  # Initialize group indices.
  df_interval$group <- 1
  overlapping_interval_max <- df_interval$ends[1]
  
  # Known issue: overlapping groups may not be identified correctly. Solved?
  for (i in 2:nrow(df_interval)){
    # if (df_interval$start[i] <= df_interval$ends[i-1]) {
    if (df_interval$start[i] <= overlapping_interval_max) {
      overlapping_interval_max <- max(df_interval$end[i], overlapping_interval_max) 
      df_interval$group[i] <- df_interval$group[i-1]
    } else {
      df_interval$group[i] <- df_interval$group[i-1] + 1
    }
  }
  
  df_interval <- df_interval %>%
    arrange(index)
  
  return(df_interval$group)
}

# Make a function to adjust `ymax`. 
nudge_y <- function(x, pad){
  # x is a numeric vector in ascending order. 
  if (length(x) == 1) return(x)
  
  for (i in 2:length(x)){
    if (x[i] - x[i-1] < pad[i]) x[i] <- x[i-1] + pad[i]
  }
  
  return(x)
}

nudge_x <- function(x, a, b, z, panel_names, pb_data = df_pb_data, main_geom_idx=1){
  # `mid.x` is passed to x.
  # Group1.x and Group2.x are passed to a and b.
  # ymax_adj is passed to z.


  if (length(x) == 1) return(x)
  
  if (is.null(panel_names)){
    xs <- unique(c(a, b))
    for (i in 1:length(x)){
      # Only nudge if there is a vertical line passing through the mid point.
      if (any(abs(xs - x[i]) < tolerance)){
        passing_group_x <- xs[which(abs(xs - x[i]) < tolerance)][1]
        passing_group_ys <- z[which(a == passing_group_x | b == passing_group_x)]
        if (any(z[i] < passing_group_ys)){
          # print(glue("x:{x[i]}"))
          # print("nudge to the right")
          x[i] <- mean(c(x[i], find_adjacent_x(x[i], pb_data = pb_data, main_geom_idx = main_geom_idx, direction = "right")))
        }
      }
    }
  } else{
    for (panel in panel_names){
      xs <- unique(c(a[panel_names == panel], b[panel_names == panel]))
      for (i in 1:length(x)){
        if (any(abs(xs - x[i]) < tolerance)){
          passing_group_x <- xs[which(abs(xs - x[i]) < tolerance)][1]
          passing_group_ys <- z[which((a == passing_group_x | b == passing_group_x) & panel_names == panel)]
          if (any(z[i] < passing_group_ys)){
            # print(glue("x:{x[i]}"))
            # print("nudge to the right")
            x[i] <- mean(c(x[i], find_adjacent_x(x[i], current_panel_name = panel, pb_data, main_geom_idx = main_geom_idx, direction = "right")))
          }
        }
      }
    }
  }
  return(x)
}

# Get the adjacent contrast group x to use for nudging. 
find_adjacent_x <- function(mid.x, current_panel_name=NULL, pb_data = df_pb_data, main_geom_idx=1, direction){
  if (!is.null(current_panel_name)){
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
  if (direction == "left") adjacent_x <- contrast_group_xs[which(abs(contrast_group_xs-mid.x)<tolerance) - 1]
  if (direction == "right") adjacent_x <- contrast_group_xs[which(abs(contrast_group_xs-mid.x)<tolerance) + 1]
  # print(glue("mid.x:{mid.x}; adjacent x:{adjacent_x}"))
  return(adjacent_x)
}


# Main function to calculate the statistical annotations. 
get_annotation_df <- function(p, contrast_df, main_geom_idx=1, contrast_group_name, ypad_ratio=0.05, tolerance=0.1){
  # `p` is a ggplot object. 
  # `contrast_df` is a dataframe with columns `Group1`, `Group1`, and `Label`. 
  # `Group1` and `Group2` must match the variables mapped to `x` or the interaction of `x` and `group.` 
  # `main_geom_idx` is the index of geom used to retrieve the `x` values of each group. 
  # `contrast_group_name` is the name of the variable passed to ggplot, which stores the contrast groups' names. 
  # `ypad_ratio` is used to calculate vertical padding between segments. 
  # `tolerance` is used to dodge label from crossing segments. 
  
  # Retrieve data passed to ggplot.
  df_p_data <- p$data
  
  # Retrieve facet variable. 
  # This returns a list if there is facet. 
  facet_var_name <- p$facet$params$facets
  
  # Known issue: Facet variable names are not retrieved correctly when there are multiple variables. 
  # A more difficult issue is that each facet variable needs to be evaluated to match the panel in data. 
  if (!is.null(facet_var_name)){
    facet_var_name <- map_chr(facet_var_name, as_label)
    fct_facet <- df_p_data[[facet_var_name]]
    if (!"factor" %in% class(fct_facet)) fct_facet <- factor(fct_facet, levels = unique(fct_facet))
    fct_facet_levels <- levels(fct_facet)
  }
  
  pb <- ggplot_build(p)
  
  df_pb_data <- imap_dfr(pb$data, function(d, i){
    if (!"ymin" %in% names(d)) d$ymin <- NA
    if (!"ymax" %in% names(d)) d$ymax <- NA
    df_sub <- d %>%
      mutate(
        geom_idx = i
      ) %>% 
      select(geom_idx, contrast_group_name, PANEL, x, y, xmin, xmax, ymin, ymax)
  })
  
  # if (!is.null(facet_var_name)) {
  #   df_pb_data$panel_name <- fct_facet_levels[df_pb_data$PANEL]
  #   # The column with facet and its column name variable must be preserved if the plot has facets.
  #   # Instead of replicating the column, another solution is to use !!sym().
  #   contrast_df$facet_var <- contrast_df[[facet_var_name]]
  #   df_stat <- contrast_df %>%
  #     rowwise() %>%
  #     mutate(
  #       Group1.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group1) %>% pull(x) %>% first(),
  #       Group2.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group2) %>% pull(x) %>% first(),
  #       Group1.y = df_pb_data %>% filter(panel_name == facet_var, !!sym(contrast_group_name) == Group1) %>% pull(y) %>% max(),
  #       Group2.y = df_pb_data %>% filter(panel_name == facet_var, !!sym(contrast_group_name) == Group2) %>% pull(y) %>% max(),
  #       ymax = df_pb_data %>% filter(panel_name == facet_var, x >= min(Group1.x, Group2.x), x<= max(Group1.x, Group2.x)) %>% pull(ymax) %>% max(na.rm = TRUE),
  #       mid.x = mean(c(Group1.x, Group2.x))
  #     ) %>%
  #     ungroup()
  # } else {
  #   df_stat <- contrast_df %>%
  #     rowwise() %>%
  #     mutate(
  #       Group1.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group1) %>% pull(x) %>% first(),
  #       Group2.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group2) %>% pull(x) %>% first(),
  #       Group1.y = df_pb_data %>% filter(!!sym(contrast_group_name) == Group1) %>% pull(y) %>% max(),
  #       Group2.y = df_pb_data %>% filter(!!sym(contrast_group_name) == Group2) %>% pull(y) %>% max(),
  #       ymax = df_pb_data %>% filter(x >= min(Group1.x, Group2.x), x<= max(Group1.x, Group2.x)) %>% pull(ymax) %>% max(na.rm = TRUE),
  #       mid.x = mean(c(Group1.x, Group2.x))
  #     ) %>%
  #     ungroup()
  # }
  
  if (!is.null(facet_var_name)) {
    df_pb_data$panel_name <- fct_facet_levels[df_pb_data$PANEL]
    # The column with facet and its column name variable must be preserved if the plot has facets.
    # Instead of replicating the column, another solution is to use !!sym().
    contrast_df$facet_var <- contrast_df[[facet_var_name]]
    df_stat <- contrast_df %>%
      rowwise() %>%
      mutate(
        Group1.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group1) %>% pull(x) %>% mean(na.rm = TRUE),
        Group2.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group2) %>% pull(x) %>% mean(na.rm = TRUE),
        Group1.y = df_pb_data %>% filter(panel_name == facet_var, !!sym(contrast_group_name) == Group1) %>% pull(y) %>% max(na.rm = TRUE),
        Group2.y = df_pb_data %>% filter(panel_name == facet_var, !!sym(contrast_group_name) == Group2) %>% pull(y) %>% max(na.rm = TRUE),
        
        # Known issue: If jittered points are used as the main geom layer, the next line does not return the max y value of all points.
        # The solution is to calculate the range of x for the data points instead of using calculated Group1.x and Group2.x
        ymax = df_pb_data %>% filter(panel_name == facet_var, x >= min(Group1.x, Group2.x), x<= max(Group1.x, Group2.x)) %>% pull(ymax) %>% max(na.rm = TRUE),
        mid.x = mean(c(Group1.x, Group2.x))
      ) %>%
      ungroup()
  } else {
    df_stat <- contrast_df %>%
      rowwise() %>%
      mutate(
        Group1.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group1) %>% pull(x) %>% mean(na.rm = TRUE),
        Group2.x = df_pb_data %>% filter(geom_idx == main_geom_idx, !!sym(contrast_group_name) == Group2) %>% pull(x) %>% mean(na.rm = TRUE),
        Group1.y = df_pb_data %>% filter(!!sym(contrast_group_name) == Group1) %>% pull(y) %>% max(na.rm = TRUE),
        Group2.y = df_pb_data %>% filter(!!sym(contrast_group_name) == Group2) %>% pull(y) %>% max(na.rm = TRUE),
        ymax = df_pb_data %>% filter(x >= min(Group1.x, Group2.x), x<= max(Group1.x, Group2.x)) %>% pull(ymax) %>% max(na.rm = TRUE),
        mid.x = mean(c(Group1.x, Group2.x))
      ) %>%
      ungroup()
  }
  
  # Retrieve y ranges. 
  y_ranges <- map_dbl(pb$layout$panel_scales_y, function(x) max(x$range$range, na.rm = TRUE)-min(x$range$range, na.rm = TRUE))
  
  if (length(y_ranges)>1) {
    df_stat <- df_stat %>% 
      group_by(facet_var) %>%
      mutate(
        overlapping_group = get_overlapping_groups(Group1.x, Group2.x)
      ) %>%
      mutate(
        ypad = y_ranges[match(facet_var, fct_facet_levels)] * ypad_ratio
      ) %>%
      ungroup()
  } else {
    df_stat <- df_stat %>%
      mutate(
        overlapping_group = get_overlapping_groups(Group1.x, Group2.x)
      ) %>%
      mutate(
        ypad = y_ranges * ypad_ratio
      ) %>%
      ungroup()
  }
  
  if (!is.null(facet_var_name)) {
    df_stat <- df_stat %>% group_by(facet_var, overlapping_group)
  } else {
    df_stat <- df_stat %>% group_by(overlapping_group)
  }
  
  df_stat <- df_stat %>%
    arrange(ymax, abs(Group1.x-Group2.x), .by_group = TRUE) %>%
    mutate(
      ymax_adj = nudge_y(ymax, ypad) + ypad * 2, 
      Group1.y_adj = Group1.y + ypad, 
      Group2.y_adj = Group2.y + ypad
    ) %>% 
    ungroup()
  
  if (!is.null(facet_var_name)) {
    df_stat <- df_stat %>%
      group_by(facet_var) %>%
      mutate(
        mid.x_adj = nudge_x(mid.x, Group1.x, Group2.x, ymax_adj, !!sym(facet_var_name), pb_data = df_pb_data)
      ) %>%
      ungroup()
  } else {
    df_stat <- df_stat %>%
      mutate(
        mid.x_adj = nudge_x(mid.x, Group1.x, Group2.x, ymax_adj, panel_names = NULL, pb_data = df_pb_data)
      ) %>%
      ungroup()
  }
  
  return(df_stat)
}

# Wraper function for drawing
draw_stat <- function(p, contrast_df, main_geom_idx=1, contrast_group_name, ypad_ratio = 0.05, tolerance = 0.1) {
  df_stat_annot <- get_annotation_df(p, contrast_df, main_geom_idx = main_geom_idx, contrast_group_name = contrast_group_name, ypad_ratio = ypad_ratio, tolerance = tolerance)
  p + 
    geom_segment(data = df_stat_annot, aes(x = Group1.x, y = ymax_adj, xend = Group2.x, yend = ymax_adj), inherit.aes = FALSE) +
    geom_segment(data = df_stat_annot, aes(x = Group1.x, y = Group1.y_adj, xend = Group1.x, yend = ymax_adj), inherit.aes = FALSE) +
    geom_segment(data = df_stat_annot, aes(x = Group2.x, y = Group2.y_adj, xend = Group2.x, yend = ymax_adj), inherit.aes = FALSE)+
    geom_text(data = df_stat_annot, aes(x = mid.x_adj, y = ymax_adj, label = Label), size = 5, inherit.aes = FALSE)
}