.ds_strip <- function(p) {
  class(p) <- setdiff(class(p), "draw_stat_plot")
  attr(p, "draw_stat_spec") <- NULL
  p
}
.ds_panel_sizes <- function(built) {
  table <- ggplot2::ggplot_gtable(built)
  slots <- table$layout[grepl("^panel($|-)", table$layout$name), , drop = FALSE]
  panels <- built$layout$layout
  panels <- panels[order(panels$ROW, panels$COL), , drop = FALSE]
  # facet_wrap can include unused cells in its last row. Match physical cells
  # by ROW/COL rather than assuming every panel grob represents a data panel.
  slot_row <- match(slots$t, sort(unique(slots$t)))
  slot_col <- match(slots$l, sort(unique(slots$l)))
  indices <- match(paste(panels$ROW, panels$COL), paste(slot_row, slot_col))
  if (anyNA(indices)) .ds_stop("Unsupported facet layout: cannot measure panel cells.")
  slots <- slots[indices, , drop = FALSE]
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(
    length(table$heights), length(table$widths), widths = table$widths,
    heights = table$heights, respect = table$respect)))
  on.exit(grid::popViewport())
  sizes <- lapply(seq_len(nrow(slots)), function(i) {
    grid::pushViewport(grid::viewport(layout.pos.row = slots$t[i]:slots$b[i],
      layout.pos.col = slots$l[i]:slots$r[i]))
    size <- c(width = grid::convertWidth(grid::unit(1, "npc"), "mm", valueOnly = TRUE),
      height = grid::convertHeight(grid::unit(1, "npc"), "mm", valueOnly = TRUE))
    grid::popViewport()
    data.frame(PANEL = as.integer(panels$PANEL[i]), width = size[1], height = size[2])
  })
  .ds_bind(sizes)
}
.ds_range <- function(params, axis) params[[paste0(axis, ".range")]]
.ds_set_y_range <- function(params, range) {
  params$y.range <- range
  for (axis in c("y", "y.sec")) {
    if (!is.null(params[[axis]])) {
      params[[axis]] <- local({
        parent <- params[[axis]]
        force(parent)
        ggplot2::ggproto(NULL, parent, continuous_range = range)
      })
    }
  }
  params
}
.ds_render_model <- function(model, style) {
  pb <- model$built
  if (is.null(style$label_family)) {
    style$label_family <- pb$plot$theme$text$family
    if (is.null(style$label_family)) style$label_family <- ggplot2::theme_get()$text$family
    if (is.null(style$label_family)) style$label_family <- ""
  }
  if (!nrow(model$annotations)) return(list(built = pb, annotations = model$annotations, model = model))
  sizes <- .ds_panel_sizes(pb)
  if (any(!is.finite(as.matrix(sizes[c("width", "height")])) | as.matrix(sizes[c("width", "height")]) <= 0)) {
    .ds_stop("Output is too small to contain the plot panels. Increase output width/height.")
  }
  original <- pb$layout$panel_params
  ranges <- lapply(original, .ds_range, axis = "y")
  if (any(vapply(ranges, function(r) length(r) != 2 || diff(r) <= 0 || any(!is.finite(r)), logical(1)))) {
    .ds_stop("A finite, increasing y scale is required; reversed y scales are unsupported.")
  }
  panels <- pb$layout$layout
  result <- NULL
  for (iteration in seq_len(60L)) {
    placed <- vector("list", nrow(panels))
    required <- vapply(ranges, `[`, numeric(1), 2)
    for (i in seq_len(nrow(panels))) {
      panel <- as.integer(panels$PANEL[i])
      rows <- model$annotations[model$annotations$PANEL == panel, , drop = FALSE]
      if (!nrow(rows)) next
      size <- sizes[match(panel, sizes$PANEL), , drop = FALSE]
      placed[[i]] <- .ds_layout_panel(rows, model$geometry[model$geometry$PANEL == panel, , drop = FALSE],
        .ds_range(original[[i]], "x"), ranges[[i]], size$width, size$height, style)
      top <- max(placed[[i]]$label.ymax) + style$clearance_mm / size$height * diff(ranges[[i]])
      required[i] <- max(.ds_range(original[[i]], "y")[2], top)
    }
    # Shared y scales reserve a common range, including panels without contrasts.
    for (scale in unique(panels$SCALE_Y)) {
      ids <- which(panels$SCALE_Y == scale)
      required[ids] <- max(required[ids])
    }
    error_mm <- max(vapply(seq_along(ranges), function(i) {
      abs(required[i] - ranges[[i]][2]) / diff(ranges[[i]]) * sizes$height[match(as.integer(panels$PANEL[i]), sizes$PANEL)]
    }, numeric(1)))
    result <- .ds_bind(placed)
    if (error_mm < 0.02) break
    for (i in seq_along(ranges)) ranges[[i]][2] <- required[i]
    if (iteration == 60L || any(vapply(seq_along(ranges), function(i) {
      diff(ranges[[i]]) > diff(.ds_range(original[[i]], "y")) * 100
    }, logical(1)))) .ds_stop("Annotation headroom does not fit this output size. Increase output height, shorten labels, or reduce label_size.")
  }
  # Freeze the resolved coordinate mapping. No extra builds or random position
  # adjustments occur during fitting, so jitter and statistics are unchanged.
  for (i in seq_along(ranges)) {
    panel <- .ds_set_y_range(original[[i]], ranges[[i]])
    # Axis guide positions were trained before headroom fitting. Refresh only
    # those guides with the resolved mapping; do not rebuild data/stat layers.
    if (!is.null(panel$guides)) panel <- pb$layout$coord$train_panel_guides(
      panel, pb$plot$layers, pb$layout$coord_params)
    pb$layout$panel_params[[i]] <- panel
  }
  context <- new.env(parent = emptyenv())
  context$annotations <- result
  annotation_data <- panels[c("PANEL", model$config$facets)]
  annotation_data$PANEL <- NULL
  annotation_data$.draw_stat <- seq_len(nrow(panels))
  geom <- .ds_geom()
  layer <- ggplot2::layer(data = annotation_data, mapping = NULL, geom = geom,
    stat = "identity", position = "identity", inherit.aes = FALSE,
    params = list(context = context, style = style))
  # The annotation is already in built scale coordinates. Appending built data
  # avoids passing transformed values through the user's scales a second time.
  layer$computed_geom_params <- list(context = context, style = style, na.rm = FALSE)
  pb$plot$layers <- c(pb$plot$layers, list(layer))
  pb$data <- c(pb$data, list(data.frame(PANEL = panels$PANEL, group = -1L)))
  attr(result, "panel_sizes_mm") <- sizes
  attr(result, "panel_ranges") <- ranges
  attr(result, "iterations") <- iteration
  attr(result, "coordinate_system") <- "built scales"
  attr(pb, "draw_stat_annotations") <- result
  list(built = pb, annotations = result, model = model)
}
# Mask horizontal bars behind label rectangles. Keep both vertical arms intact:
# a wide label can otherwise erase the entire short arm of its own comparison.
.ds_bracket_segments <- function(rows, panel_params, coord, style) {
  transform <- function(x, y) coord$transform(data.frame(x = x, y = y), panel_params)
  n <- nrow(rows)
  start <- transform(c(rows$Group1.x, rows$Group1.x, rows$Group2.x),
    c(rows$ymax_adj, rows$Group1.y_adj, rows$Group2.y_adj))
  end <- transform(c(rows$Group2.x, rows$Group1.x, rows$Group2.x), rep(rows$ymax_adj, 3))
  parts <- rep(c("bar", "left", "right"), each = n)
  ids <- rep(seq_len(n), 3)
  mask <- identical(style$label_position, "left")
  if (mask) {
    lo <- transform(rows$label.xmin, rows$label.ymin)
    hi <- transform(rows$label.xmax, rows$label.ymax)
    boxes <- data.frame(left = pmin(lo$x, hi$x) - .002, right = pmax(lo$x, hi$x) + .002,
      bottom = pmin(lo$y, hi$y) - .002, top = pmax(lo$y, hi$y) + .002)
  }
  segments <- list()
  for (i in seq_len(nrow(start))) {
    if (parts[i] != "bar") {
      segments[[length(segments) + 1L]] <- data.frame(x = start$x[i], y = start$y[i],
        xend = end$x[i], yend = end$y[i], comparison = ids[i], part = parts[i])
      next
    }
    pieces <- list(sort(c(start$x[i], end$x[i])))
    if (mask) for (j in seq_len(nrow(boxes))) {
      box <- boxes[j, ]
      if (start$y[i] < box$bottom || start$y[i] > box$top) next
      cut <- c(box$left, box$right)
      pieces <- unlist(lapply(pieces, function(v) {
        out <- list()
        if (v[1] < min(v[2], cut[1])) out <- c(out, list(c(v[1], min(v[2], cut[1]))))
        if (max(v[1], cut[2]) < v[2]) out <- c(out, list(c(max(v[1], cut[2]), v[2])))
        out
      }), recursive = FALSE)
    }
    for (piece in pieces) segments[[length(segments) + 1L]] <- data.frame(x = piece[1],
      y = start$y[i], xend = piece[2], yend = end$y[i], comparison = ids[i], part = parts[i])
  }
  if (!length(segments)) return(data.frame(x = numeric(), y = numeric(), xend = numeric(),
    yend = numeric(), comparison = integer(), part = character()))
  do.call(rbind, segments)
}

.ds_geom <- function() ggplot2::ggproto("GeomDrawStat", ggplot2::Geom,
  required_aes = character(), default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, context, style) {
    rows <- context$annotations[context$annotations$PANEL == as.integer(data$PANEL[1]), , drop = FALSE]
    if (!nrow(rows)) return(grid::nullGrob())
    transform <- function(x, y) coord$transform(data.frame(x = x, y = y), panel_params)
    segments <- .ds_bracket_segments(rows, panel_params, coord, style)
    text <- transform(rows$label.x, rows$label.y)
    grid::grobTree(
      grid::segmentsGrob(segments$x, segments$y, segments$xend, segments$yend, default.units = "npc",
        gp = grid::gpar(col = style$label_color, lwd = style$segment_linewidth * 72.27 / 25.4, lineend = "butt")),
      grid::textGrob(as.character(rows$Label), x = text$x, y = text$y, default.units = "npc",
        hjust = style$label_hjust, vjust = style$label_vjust,
        gp = grid::gpar(col = style$label_color, fontsize = style$label_size,
          fontfamily = style$label_family, fontface = style$label_fontface, lineheight = style$label_lineheight)))
  })
ggplot_build.draw_stat_plot <- function(plot, ...) {
  spec <- attr(plot, "draw_stat_spec")
  p <- .ds_strip(plot)
  config <- .ds_prepare(p, spec$comparisons, spec$contrast_vars, spec$main_geom_idx, spec$clearance_layers, spec$geometry)
  model <- .ds_model(p, spec$comparisons, config, spec$sep, spec$geometry)
  .ds_render_model(model, spec$style)$built
}

