.ds_stop <- function(...) stop(..., call. = FALSE)
.ds_bind <- function(xs) {
  xs <- Filter(function(x) !is.null(x) && nrow(x), xs)
  if (!length(xs)) return(data.frame())
  columns <- unique(unlist(lapply(xs, names)))
  xs <- lapply(xs, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[columns]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}
.ds_indices <- function(x, n, name) {
  if (!is.numeric(x) || !length(x) || anyNA(x) ||
      any(x != as.integer(x) | x < 1 | x > n)) {
    .ds_stop(name, " must contain layer indices between 1 and ", n, ".")
  }
  unique(as.integer(x))
}
.ds_mapping <- function(p, layer) {
  mapping <- if (isTRUE(layer$inherit.aes)) as.list(p$mapping) else list()
  utils::modifyList(mapping, as.list(layer$mapping))
}
.ds_data <- function(p, layer) {
  data <- layer$layer_data(p$data)
  if (!is.data.frame(data)) .ds_stop("Layer data must resolve to a data frame.")
  data
}
.ds_supported <- function(layer) {
  any(vapply(c("GeomCol", "GeomBar", "GeomPoint", "GeomErrorbar", "GeomLinerange",
               "GeomPointrange", "GeomBoxplot", "GeomViolin", "GeomCrossbar"),
             function(cls) inherits(layer$geom, cls), logical(1)))
}
.ds_infer <- function(p, layer, data) {
  mapping <- .ds_mapping(p, layer)
  candidates <- intersect(c("x", "group", "fill", "colour"), names(mapping))
  variables <- unique(unlist(lapply(mapping[candidates], function(expr) {
    intersect(all.vars(rlang::get_expr(expr)), names(data))
  })))
  variables <- variables[vapply(data[variables], function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
  }, logical(1))]
  if (!length(variables)) {
    .ds_stop("Cannot infer contrast_vars. Supply column names explicitly (including numeric groups).")
  }
  variables
}
.ds_key <- function(data, variables) {
  # Length-prefixed identities avoid separator collisions, including facets.
  pieces <- lapply(data[variables], function(x) {
    x <- enc2utf8(as.character(x))
    ifelse(is.na(x), "N;", paste0(nchar(x, type = "bytes"), ":", x, ";"))
  })
  if (!length(pieces)) return(rep("", nrow(data)))
  do.call(paste0, pieces)
}
.ds_labels <- function(data, variables, sep) {
  do.call(paste, c(lapply(data[variables], as.character), sep = sep))
}
get_facet_var_names <- function(p) {
  facet <- p$facet
  if (inherits(facet, "FacetNull")) return(character())
  if (inherits(facet, "FacetWrap")) return(names(facet$params$facets))
  if (inherits(facet, "FacetGrid")) return(unique(c(names(facet$params$rows), names(facet$params$cols))))
  .ds_stop("Unsupported facet class: ", class(facet)[1], ". Use facet_wrap() or facet_grid().")
}
.ds_prepare <- function(p, contrast_df, contrast_vars, main_geom_idx, clearance_layers, geometry = NULL) {
  if (!inherits(p, "ggplot")) .ds_stop("p must be a ggplot object.")
  version <- utils::packageVersion("ggplot2")
  if (version < "3.5.2" || version >= "4.1.0") {
    .ds_stop("Supported ggplot2 versions are >= 3.5.2 and < 4.1.0; found ", version, ".")
  }
  if (!inherits(p$coordinates, "CoordCartesian") || inherits(p$coordinates, "CoordFlip") ||
      inherits(p$coordinates, "CoordTrans")) {
    .ds_stop("Only vertical Cartesian plots are supported. Flip/transform the data before plotting.")
  }
  if (!is.null(p$coordinates$limits$x) || !is.null(p$coordinates$limits$y)) {
    .ds_stop("Explicit coord limits are unsupported: filter visible data or use scale limits instead.")
  }
  if (inherits(p$coordinates, "CoordFixed") || !is.null(p$coordinates$ratio) ||
      (!is.null(p$coordinates$reverse) && p$coordinates$reverse != "none")) {
    .ds_stop("Fixed aspect ratios and coordinate reversal are unsupported; use ordinary coord_cartesian().")
  }
  if (!is.data.frame(contrast_df)) .ds_stop("contrast_df must be a data frame.")
  missing_cols <- setdiff(c("Group1", "Group2", "Label"), names(contrast_df))
  if (length(missing_cols)) .ds_stop("contrast_df is missing required columns: ", paste(missing_cols, collapse = ", "))
  if (anyNA(contrast_df[c("Group1", "Group2", "Label")])) .ds_stop("Comparison endpoints and labels cannot be missing.")
  n <- length(p$layers)
  if (!n) .ds_stop("p has no layers.")
  supported <- which(vapply(p$layers, .ds_supported, logical(1)))
  inferred_anchor <- is.null(main_geom_idx)
  if (inferred_anchor) {
    candidates <- supported[vapply(p$layers[supported], function(l) {
      !inherits(l$position, "PositionJitter") && !inherits(l$position, "PositionJitterdodge")
    }, logical(1))]
    if (!length(candidates)) candidates <- supported
    if (!length(candidates) && is.null(geometry)) .ds_stop("No supported anchor layer. Supply geometry overrides.")
    main_geom_idx <- if (length(candidates)) candidates[1] else 1L
  }
  main_geom_idx <- .ds_indices(main_geom_idx, n, "main_geom_idx")
  if (length(main_geom_idx) != 1) .ds_stop("main_geom_idx must select exactly one anchor layer.")
  if (is.null(clearance_layers)) {
    ignored <- setdiff(seq_len(n), supported)
    ignored <- ignored[!vapply(p$layers[ignored], function(l) inherits(l$geom, "GeomBlank"), logical(1))]
    if (length(ignored) && is.null(geometry)) .ds_stop("Unsupported clearance layer(s): ", paste(ignored, collapse = ", "),
      ". Select clearance_layers explicitly or provide geometry overrides.")
    clearance_layers <- if (length(supported)) supported else main_geom_idx
  }
  clearance_layers <- .ds_indices(clearance_layers, n, "clearance_layers")
  data <- if (is.null(geometry)) .ds_data(p, p$layers[[main_geom_idx]]) else geometry
  if (is.null(contrast_vars)) contrast_vars <- .ds_infer(p, p$layers[[main_geom_idx]], data)
  if (!is.character(contrast_vars) || !length(contrast_vars) || anyNA(contrast_vars) || anyDuplicated(contrast_vars)) {
    .ds_stop("contrast_vars must be distinct column names.")
  }
  missing_vars <- setdiff(contrast_vars, names(data))
  if (length(missing_vars)) .ds_stop("contrast_vars not found in anchor layer data: ", paste(missing_vars, collapse = ", "))
  facets <- get_facet_var_names(p)
  if (length(setdiff(facets, names(contrast_df)))) .ds_stop("contrast_df must contain facet columns: ", paste(facets, collapse = ", "))
  list(contrast_vars = contrast_vars, main_geom_idx = main_geom_idx, inferred_anchor = inferred_anchor,
       clearance_layers = clearance_layers, facets = facets)
}

# Trace a numeric row token before a stat, then tag each stat result before
# positioning. ggplot assigns PANEL and group; we never reconstruct its IDs.
.ds_trace_layer <- function(layer, data, store, index) {
  force(index)
  force(layer)
  data$.draw_stat_row <- seq_len(nrow(data))
  mapping <- layer$mapping
  mapping[[".draw_stat_row"]] <- rlang::new_quosure(quote(.draw_stat_row), env = baseenv())
  parent_stat <- layer$stat
  traced_stat <- ggplot2::ggproto(NULL, parent_stat,
    compute_layer = function(self, data, params, layout) {
      source <- data
      data$.draw_stat_row <- NULL
      result <- parent_stat$compute_layer(data, params, layout)
      store[[as.character(index)]] <- list(source = source, stat = result)
      result$.draw_stat_piece <- seq_len(nrow(result))
      result
    })
  ggplot2::ggproto(NULL, layer, data = data, mapping = mapping, stat = traced_stat)
}
.ds_without_jitter <- function(position) {
  force(position)
  # Inherit options such as reverse without depending on constructor arguments
  # added between ggplot2 releases.
  ggplot2::ggproto(NULL, position, jitter.width = 0, jitter.height = 0, seed = 1)
}
.ds_piece_keys <- function(trace, data, variables, index) {
  source <- trace$source
  stat <- trace$stat
  if (!nrow(stat)) return(character())
  source_key <- .ds_key(data[as.integer(source$.draw_stat_row), , drop = FALSE], variables)
  vapply(seq_len(nrow(stat)), function(i) {
    candidates <- which(source$PANEL == stat$PANEL[i] & source$group == stat$group[i])
    keys <- unique(source_key[candidates])
    if (length(keys) > 1 && "x" %in% names(stat)) {
      candidates <- candidates[abs(as.numeric(source$x[candidates]) - as.numeric(stat$x[i])) < 1e-8]
      keys <- unique(source_key[candidates])
    }
    if (length(keys) != 1) .ds_stop("Layer ", index,
      " combines multiple comparison groups in one statistical result. Map group/fill explicitly,",
      " choose clearance_layers, or provide geometry overrides.")
    keys
  }, character(1))
}
.ds_extents <- function(d, layer, index) {
  if (isTRUE(any(d$flipped_aes))) .ds_stop("Horizontal geoms are unsupported (layer ", index, ").")
  numeric_col <- function(name, fallback) if (name %in% names(d)) as.numeric(d[[name]]) else fallback
  x <- numeric_col("x", rep(NA_real_, nrow(d)))
  y <- numeric_col("y", rep(NA_real_, nrow(d)))
  ymin <- numeric_col("ymin", y)
  ymax <- numeric_col("ymax", y)
  outlier_shape <- layer$geom_params$outlier.shape
  if (is.null(outlier_shape)) outlier_shape <- layer$geom_params$outlier_gp$shape
  outlier_alpha <- layer$geom_params$outlier.alpha
  if (is.null(outlier_alpha)) outlier_alpha <- layer$geom_params$outlier_gp$alpha
  visible_outliers <- inherits(layer$geom, "GeomBoxplot") && "outliers" %in% names(d) &&
    !isTRUE(is.na(outlier_shape)) && !isTRUE(outlier_alpha == 0)
  if (visible_outliers) {
    for (j in seq_len(nrow(d))) {
      outliers <- d$outliers[[j]]
      outliers <- outliers[is.finite(outliers)]
      if (length(outliers)) { ymin[j] <- min(ymin[j], outliers); ymax[j] <- max(ymax[j], outliers) }
    }
  }
  ymin <- pmin(ymin, y, na.rm = TRUE)
  ymax <- pmax(ymax, y, na.rm = TRUE)
  point <- inherits(layer$geom, "GeomPoint") || inherits(layer$geom, "GeomPointrange")
  fatten <- if (inherits(layer$geom, "GeomPointrange")) layer$geom_params$fatten else 1
  if (is.null(fatten)) fatten <- 4
  radius <- if (point) numeric_col("size", rep(1.5, nrow(d))) * fatten / 2 + numeric_col("stroke", rep(0, nrow(d))) / 2 else rep(0, nrow(d))
  radius <- pmax(radius, numeric_col("linewidth", rep(0.5, nrow(d))) / 2, na.rm = TRUE)
  if (visible_outliers) {
    outlier_size <- layer$geom_params$outlier.size
    if (is.null(outlier_size)) outlier_size <- layer$geom_params$outlier_gp$size
    if (is.null(outlier_size)) outlier_size <- numeric_col("size", rep(1.5, nrow(d)))
    outlier_stroke <- layer$geom_params$outlier.stroke
    if (is.null(outlier_stroke)) outlier_stroke <- layer$geom_params$outlier_gp$stroke
    if (is.null(outlier_stroke)) outlier_stroke <- 0.5
    has_outlier <- vapply(d$outliers, function(x) any(is.finite(x)), logical(1))
    radius <- pmax(radius, ifelse(has_outlier, outlier_size / 2 + outlier_stroke / 2, 0))
  }
  visible <- is.finite(x) & is.finite(ymax) & is.finite(ymin)
  if ("alpha" %in% names(d)) visible <- visible & (is.na(d$alpha) | d$alpha > 0)
  for (aesthetic in intersect(layer$geom$non_missing_aes, names(d))) {
    visible <- visible & !is.na(d[[aesthetic]])
  }
  data.frame(PANEL = as.integer(d$PANEL), key = d$.draw_stat_key,
    layer = index, x = x, xmin = numeric_col("xmin", x), xmax = numeric_col("xmax", x),
    ymin = ymin, ymax = ymax, radius_mm = radius, visible = visible)
}
.ds_model <- function(p, comparisons, config, sep, geometry = NULL) {
  store <- new.env(parent = emptyenv())
  selected <- if (is.null(geometry)) unique(c(config$main_geom_idx, config$clearance_layers)) else integer()
  originals <- p$layers
  input <- vector("list", length(originals))
  for (i in selected) {
    input[[i]] <- .ds_data(p, originals[[i]])
    if (!all(config$contrast_vars %in% names(input[[i]]))) {
      .ds_stop("Layer ", i, " lacks contrast_vars. Select clearance_layers or provide geometry overrides.")
    }
    p$layers[[i]] <- .ds_trace_layer(originals[[i]], input[[i]], store, i)
  }
  pb <- ggplot2::ggplot_build(p)
  groups <- extents <- list()
  layer_centres <- list()
  for (i in selected) {
    d <- pb$data[[i]]
    if (!nrow(d)) next
    piece_keys <- .ds_piece_keys(store[[as.character(i)]], input[[i]], config$contrast_vars, i)
    if (!".draw_stat_piece" %in% names(d)) .ds_stop("Layer ", i, " drops identity tokens; provide geometry overrides.")
    d$.draw_stat_key <- piece_keys[d$.draw_stat_piece]
    e <- .ds_extents(d, originals[[i]], i)
    extents[[i]] <- e[e$visible, , drop = FALSE]
    position <- originals[[i]]$position
    if (inherits(position, "PositionJitterdodge")) {
      position <- .ds_without_jitter(position)
    } else if (inherits(position, "PositionJitter")) {
      position <- ggplot2::position_identity()
    } else if (inherits(position, "PositionQuasirandom") || inherits(position, "PositionBeeswarm")) {
      if (!is.null(position$dodge.width) && position$dodge.width != 0) {
        .ds_stop("Dodged beeswarm centres require an explicit geometry override.")
      }
      position <- ggplot2::position_identity()
    } else position <- NULL
    centre_x <- as.numeric(d$x)
    if (!is.null(position)) {
      stat <- store[[as.character(i)]]$stat
      stat$.draw_stat_piece <- seq_len(nrow(stat))
      stat <- originals[[i]]$geom$setup_data(stat, pb$plot$layers[[i]]$computed_geom_params)
      params <- position$setup_params(stat)
      stat <- position$setup_data(stat, params)
      centred <- position$compute_layer(stat, params, pb$layout)
      centre_x <- as.numeric(centred$x[match(d$.draw_stat_piece, centred$.draw_stat_piece)])
    }
    if (i == config$main_geom_idx || config$inferred_anchor) {
      pairs <- unique(data.frame(PANEL = as.integer(d$PANEL), key = d$.draw_stat_key))
      resolved_centres <- lapply(seq_len(nrow(pairs)), function(j) {
        mask <- as.integer(d$PANEL) == pairs$PANEL[j] & d$.draw_stat_key == pairs$key[j]
        centres <- unique(round(centre_x[mask & is.finite(centre_x)], 10))
        # Dodge2 can distribute repeated observations within a group's width.
        # Their centre is meaningful only if the pre-position x is unique.
        source <- store[[as.character(i)]]$source
        source_ids <- as.integer(source$.draw_stat_row)
        source_keys <- .ds_key(input[[i]][source_ids, , drop = FALSE], config$contrast_vars)
        nominal <- unique(source$x[as.integer(source$PANEL) == pairs$PANEL[j] & source_keys == pairs$key[j]])
        if (length(nominal) != 1) .ds_stop("Anchor layer ", i,
          " has multiple x positions per comparison group. Include x in contrast_vars or select a different anchor layer.")
        centres <- mean(centres)
        data.frame(PANEL = pairs$PANEL[j], key = pairs$key[j], x = centres)
      })
      layer_centres[[i]] <- .ds_bind(resolved_centres)
      if (i == config$main_geom_idx) groups <- resolved_centres
    }
  }
  groups <- .ds_bind(groups)
  if (config$inferred_anchor && length(layer_centres)) {
    for (i in setdiff(selected, config$main_geom_idx)) {
      centres <- layer_centres[[i]]
      if (is.null(centres) || !nrow(centres)) next
      ref <- paste(groups$PANEL, groups$key)
      matched <- match(paste(centres$PANEL, centres$key), ref)
      if (any(abs(centres$x - groups$x[matched]) > 1e-6, na.rm = TRUE)) {
        .ds_stop("Layers resolve different group centres. Set main_geom_idx explicitly to choose the anchor layer.")
      }
    }
  }
  extents <- .ds_bind(extents[config$clearance_layers])
  identities <- unique(.ds_bind(lapply(input[selected], function(data) data[config$contrast_vars])))
  if (is.null(geometry)) {
    identities$key <- .ds_key(identities, config$contrast_vars)
    identities$label <- .ds_labels(identities, config$contrast_vars, sep)
  }
  panels <- pb$layout$layout
  if (!is.null(geometry)) {
    required <- c("PANEL", config$contrast_vars, "x", "xmin", "xmax", "ymin", "ymax")
    if (!is.data.frame(geometry) || length(setdiff(required, names(geometry)))) {
      .ds_stop("geometry must contain PANEL, contrast_vars, x, xmin, xmax, ymin, ymax in built scale coordinates.")
    }
    geometry$PANEL <- suppressWarnings(as.integer(as.character(geometry$PANEL)))
    if (anyNA(geometry$PANEL) || any(!geometry$PANEL %in% as.integer(panels$PANEL)) ||
        any(!is.finite(as.matrix(geometry[c("x", "xmin", "xmax", "ymin", "ymax")])))) {
      .ds_stop("geometry contains invalid panel IDs or non-finite coordinates.")
    }
    geometry$key <- .ds_key(geometry, config$contrast_vars)
    geometry$radius_mm <- if ("radius_mm" %in% names(geometry)) geometry$radius_mm else 0
    if (any(geometry$xmin > geometry$xmax | geometry$ymin > geometry$ymax) ||
        any(!is.finite(geometry$radius_mm) | geometry$radius_mm < 0)) {
      .ds_stop("geometry bounds must be ordered and radius_mm must be finite and nonnegative.")
    }
    geometry$layer <- NA_integer_
    extents <- geometry
    groups <- unique(geometry[c("PANEL", "key", "x")])
    if (anyDuplicated(paste(groups$PANEL, groups$key))) {
      .ds_stop("geometry must define one anchor centre per group and panel.")
    }
    identities <- unique(geometry[config$contrast_vars])
    identities$key <- .ds_key(identities, config$contrast_vars)
    identities$label <- .ds_labels(identities, config$contrast_vars, sep)
  }
  rows <- list()
  for (i in seq_len(nrow(comparisons))) {
    row <- comparisons[i, , drop = FALSE]
    panel_matches <- if (length(config$facets)) which(.ds_key(panels, config$facets) == .ds_key(row, config$facets)) else 1L
    if (!length(panel_matches)) { warning("Comparison row ", i, " has no matching panel; dropped.", call. = FALSE); next }
    endpoint <- function(name) {
      value <- row[[name]]
      if (is.list(value)) {
        value <- value[[1]]
        if (is.null(names(value)) || !setequal(names(value), config$contrast_vars)) {
          .ds_stop(name, " list endpoints must name every contrast variable.")
        }
        return(.ds_key(as.data.frame(as.list(value), stringsAsFactors = FALSE), config$contrast_vars))
      }
      keys <- unique(identities$key[identities$label == as.character(value)])
      if (length(keys) > 1) .ds_stop("Ambiguous pasted group label '", value, "'. Use named list-column endpoints.")
      if (!length(keys)) return(NA_character_)
      keys
    }
    keys <- c(endpoint("Group1"), endpoint("Group2"))
    for (pi in panel_matches) {
      panel <- as.integer(panels$PANEL[pi])
      g <- groups[groups$PANEL == panel, , drop = FALSE]
      positions <- g$x[match(keys, g$key)]
      e <- extents[extents$PANEL == panel, , drop = FALSE]
      heights <- vapply(keys, function(key) {
        ys <- e$ymax[e$key == key & !is.na(e$key)]
        if (length(ys)) max(ys) else NA_real_
      }, numeric(1))
      if (any(!is.finite(c(positions, heights)))) {
        warning("Comparison row ", i, " has missing/non-visible groups in panel ", panel, "; dropped.", call. = FALSE)
        next
      }
      if (abs(diff(positions)) < 1e-8) .ds_stop("Comparison row ", i, " has coincident endpoints. Map grouping/dodge explicitly.")
      row$PANEL <- panel; row$.comparison <- i
      row$Group1.x <- positions[1]; row$Group2.x <- positions[2]
      row$Group1.y <- heights[1]; row$Group2.y <- heights[2]
      row$.key1 <- keys[1]; row$.key2 <- keys[2]
      row$panel_name <- if (length(config$facets)) paste(panels[pi, config$facets], collapse = " | ") else "1"
      rows[[length(rows) + 1L]] <- row
    }
  }
  # Retain computed parameters on the built layers; the input plot is untouched.
  list(built = pb, annotations = .ds_bind(rows), groups = groups,
       geometry = extents, identities = identities, config = config)
}
