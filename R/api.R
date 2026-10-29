.ds_style <- function(ypad_ratio, segment_linewidth, label_size, label_color,
                      label_hjust, label_vjust, label_family, label_fontface,
                      label_lineheight, clearance_mm) {
  positive <- list(segment_linewidth = segment_linewidth, label_size = label_size,
    label_lineheight = label_lineheight, clearance_mm = clearance_mm)
  for (name in names(positive)) {
    value <- positive[[name]]
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value <= 0) .ds_stop(name, " must be a positive finite number.")
  }
  if (!is.null(ypad_ratio) && (!is.numeric(ypad_ratio) || length(ypad_ratio) != 1 || !is.finite(ypad_ratio) || ypad_ratio < 0)) .ds_stop("ypad_ratio must be NULL or a nonnegative finite number.")
  for (value in list(label_hjust, label_vjust)) {
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0 || value > 1) .ds_stop("Label justification must be between 0 and 1.")
  }
  list(ypad_ratio = ypad_ratio, segment_linewidth = segment_linewidth, label_size = label_size,
    label_color = label_color, label_hjust = label_hjust, label_vjust = label_vjust,
    label_family = label_family, label_fontface = label_fontface,
    label_lineheight = label_lineheight, clearance_mm = clearance_mm)
}

#' Add long-leg comparison brackets to a ggplot.
#' Existing positional arguments are retained. Layout is resolved on the output
#' device when printing/saving, including after adding themes or changing size.
draw_stat <- function(p, contrast_df, contrast_vars = NULL, sep = " ",
                      main_geom_idx = NULL, ypad_ratio = NULL, tolerance = 0.1,
                      segment_linewidth = 0.4, label_size = 12, label_color = "black",
                      label_hjust = 0.5, label_vjust = 0.5,
                      clearance_layers = NULL, geometry = NULL, clearance_mm = 0.8,
                      label_family = NULL, label_fontface = "plain", label_lineheight = 1.2) {
  if (inherits(p, "draw_stat_plot")) .ds_stop("This plot already has draw_stat annotations. Combine comparisons in one contrast_df.")
  config <- .ds_prepare(p, contrast_df, contrast_vars, main_geom_idx, clearance_layers, geometry)
  style <- .ds_style(ypad_ratio, segment_linewidth, label_size, label_color,
    label_hjust, label_vjust, label_family, label_fontface, label_lineheight, clearance_mm)
  attr(p, "draw_stat_spec") <- list(comparisons = contrast_df,
    contrast_vars = config$contrast_vars, main_geom_idx = main_geom_idx,
    clearance_layers = clearance_layers, geometry = geometry, sep = sep, style = style)
  class(p) <- c("draw_stat_plot", class(p))
  p
}

#' Inspect resolved coordinates, groups, layers, and plotted extents.
#' device="current" requires an open device and gives that rendering context.
#' Otherwise output_size is in inches and a temporary pdf/png is used; without
#' an output_size the returned coordinates are an explicit 7x7-inch estimate.
inspect_stat <- function(p, contrast_df, contrast_vars = NULL, sep = " ",
                         main_geom_idx = NULL, ypad_ratio = NULL, tolerance = 0.1,
                         clearance_layers = NULL, geometry = NULL,
                         output_size = NULL, device = c("pdf", "png", "current"),
                         label_size = 12, label_family = NULL, label_fontface = "plain",
                         label_lineheight = 1.2, clearance_mm = 0.8,
                         segment_linewidth = 0.4, label_hjust = 0.5, label_vjust = 0.5) {
  device <- match.arg(device)
  if (inherits(p, "draw_stat_plot")) p <- .ds_strip(p)
  context <- if (device == "current") "current device" else if (is.null(output_size)) "estimate: 7 x 7 inch PDF/PNG" else paste(output_size, collapse = " x ")
  if (device == "current") {
    if (grDevices::dev.cur() == 1) .ds_stop("device='current' requires an open graphics device.")
    if (!is.null(output_size)) .ds_stop("output_size cannot be used with device='current'.")
  } else {
    if (is.null(output_size)) output_size <- c(7, 7)
    if (!is.numeric(output_size) || length(output_size) != 2 || any(!is.finite(output_size) | output_size <= 0)) .ds_stop("output_size must be two positive dimensions in inches.")
    file <- tempfile(fileext = paste0(".", device))
    if (device == "pdf") grDevices::pdf(file, width = output_size[1], height = output_size[2])
    else grDevices::png(file, width = output_size[1], height = output_size[2], units = "in", res = 144)
    on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  }
  config <- .ds_prepare(p, contrast_df, contrast_vars, main_geom_idx, clearance_layers, geometry)
  style <- .ds_style(ypad_ratio, segment_linewidth, label_size, "black", label_hjust,
    label_vjust, label_family, label_fontface, label_lineheight, clearance_mm)
  model <- .ds_model(p, contrast_df, config, sep, geometry)
  resolved <- .ds_render_model(model, style)
  attr(resolved$annotations, "layout_context") <- context
  list(annotations = resolved$annotations, groups = model$groups,
    geometry = model$geometry, identities = model$identities, layers = config,
    built = resolved$built, context = context)
}
get_annotation_df <- function(p, contrast_df, contrast_vars = NULL, sep = " ",
                              main_geom_idx = NULL, ypad_ratio = NULL, tolerance = 0.1, ...) {
  inspect_stat(p, contrast_df, contrast_vars, sep, main_geom_idx, ypad_ratio, tolerance, ...)$annotations
}

# Retain useful legacy helpers with well-defined empty-input behavior.
get_overlapping_groups <- function(x1, x2) {
  if (!length(x1)) return(integer())
  indices <- order(pmin(x1, x2), pmax(x1, x2)); result <- integer(length(x1))
  end <- -Inf; group <- 0L
  for (i in indices) {
    if (min(x1[i], x2[i]) > end) { group <- group + 1L; end <- -Inf }
    end <- max(end, x1[i], x2[i]); result[i] <- group
  }
  result
}
nudge_y <- function(x, pad) {
  if (length(x) < 2) return(x)
  pad <- rep_len(pad, length(x))
  for (i in 2:length(x)) x[i] <- max(x[i], x[i - 1] + pad[i])
  x
}
safe_max <- function(x) if (any(is.finite(x))) max(x[is.finite(x)]) else NA_real_
