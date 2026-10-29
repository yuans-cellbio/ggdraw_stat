.ds_overlap <- function(a, b, c, d, gap = 0) a < d + gap - 1e-8 && b > c - gap + 1e-8
.ds_permutations <- function(x) {
  if (length(x) <= 1) return(list(x))
  unlist(lapply(seq_along(x), function(i) {
    lapply(.ds_permutations(x[-i]), function(tail) c(x[i], tail))
  }), recursive = FALSE)
}
.ds_label_metrics <- function(labels, style) {
  lapply(as.character(labels), function(label) {
    grob <- grid::textGrob(label, gp = grid::gpar(fontsize = style$label_size,
      fontfamily = style$label_family, fontface = style$label_fontface,
      lineheight = style$label_lineheight))
    c(width = grid::convertWidth(grid::grobWidth(grob), "mm", valueOnly = TRUE),
      height = grid::convertHeight(grid::grobHeight(grob), "mm", valueOnly = TRUE))
  })
}
.ds_place_order <- function(rows, order, geometry, width, height, gap, line_mm, all_legs) {
  placed <- list()
  for (idx in order) {
    r <- rows[idx, , drop = FALSE]
    left <- min(r$x1, r$x2); right <- max(r$x1, r$x2)
    relevant <- geometry$xmax >= left & geometry$xmin <= right
    baseline <- max(c(r$a1, r$a2, geometry$top[relevant] + r$pad)) + r$pad + line_mm / 2
    if (r$lw + 2 * gap > width) return(NULL)
    min_x <- max(gap + r$lw / 2, left)
    max_x <- min(width - gap - r$lw / 2, right)
    if (r$lw + 2 * gap <= right - left) {
      min_x <- max(min_x, left + gap + r$lw / 2)
      max_x <- min(max_x, right - gap - r$lw / 2)
    }
    if (min_x > max_x) return(NULL)
    # Candidate gaps include all leg positions, so a lower label can avoid a
    # future long leg. Endpoints remain attached to the original group centres.
    edges <- sort(unique(c(0, all_legs, width)))
    mids <- (head(edges, -1) + tail(edges, -1)) / 2
    centres <- unique(pmax(min_x, pmin(max_x, c(mean(c(left, right)), mids))))
    centres <- centres[order(abs(centres - mean(c(left, right))), centres)]
    best <- NULL; best_score <- Inf
    for (cx in centres) {
      lx <- cx - r$lw / 2; rx <- cx + r$lw / 2
      # A shifted or long label may extend beyond its own bracket interval.
      under_label <- geometry$xmax >= lx - gap & geometry$xmin <= rx + gap
      y <- max(c(baseline, geometry$top[under_label] + gap))
      # Each update clears at least one previously placed rectangle/segment.
      for (iteration in seq_len(2 * length(placed) + 3L)) {
        old_y <- y
        bottom <- y + gap + line_mm / 2
        top <- bottom + r$lh
        for (p in placed) {
          if (.ds_overlap(left, right, p$lx, p$rx, gap) &&
              y >= p$bottom - gap && y <= p$top + gap) y <- max(y, p$top + gap + line_mm)
          if (.ds_overlap(left, right, p$left, p$right, gap) && abs(y - p$y) < gap + line_mm) {
            y <- max(y, p$y + gap + line_mm)
          }
          if (.ds_overlap(lx, rx, p$lx, p$rx, gap) && .ds_overlap(bottom, top, p$bottom, p$top, gap)) {
            y <- max(y, p$top + gap)
          }
          if (.ds_overlap(lx, rx, p$left, p$right, gap) && p$y >= bottom - gap && p$y <= top + gap) {
            y <- max(y, p$y + gap)
          }
          for (k in 1:2) {
            px <- c(p$x1, p$x2)[k]; py <- c(p$a1, p$a2)[k]
            if (px > lx - gap && px < rx + gap && .ds_overlap(bottom, top, py, p$y, gap)) {
              y <- max(y, p$y + gap)
            }
          }
        }
        if (abs(y - old_y) < 1e-8) break
      }
      bottom <- y + gap + line_mm / 2; top <- bottom + r$lh
      blocked <- FALSE
      for (p in placed) {
        for (k in 1:2) {
          x <- c(r$x1, r$x2)[k]; anchor <- c(r$a1, r$a2)[k]
          if (x > p$lx - gap && x < p$rx + gap && .ds_overlap(anchor, y, p$bottom, p$top, gap)) blocked <- TRUE
        }
      }
      if (blocked) next
      # Penalize potential future leg collisions to break otherwise equal ties.
      remaining <- order[match(idx, order):length(order)]
      remaining <- setdiff(remaining, idx)
      future_legs <- unique(c(rows$x1[remaining], rows$x2[remaining]))
      future <- sum(future_legs > lx - gap & future_legs < rx + gap)
      score <- top + abs(cx - mean(c(left, right))) * 1e-4 + future * (height + 1)
      if (score < best_score) {
        best_score <- score
        best <- list(index = idx, x1 = r$x1, x2 = r$x2, a1 = r$a1, a2 = r$a2,
          left = left, right = right, y = y, cx = cx, lx = lx, rx = rx,
          bottom = bottom, top = top)
      }
    }
    if (is.null(best)) return(NULL)
    placed[[length(placed) + 1L]] <- best
  }
  placed
}
.ds_compact <- function(rows, geometry, width, height, gap, line_mm) {
  n <- nrow(rows)
  if (!n) return(list())
  span <- abs(rows$x2 - rows$x1)
  base <- pmax(rows$a1, rows$a2)
  orders <- list(order(span, base, rows$.comparison), order(base, span, rows$.comparison),
    order(-span, base, rows$.comparison), order(-base, span, rows$.comparison))
  # Exhaustive order search on small cases; larger cases use four deterministic
  # orders. This is an optimum over these candidate positions, not a global proof.
  if (n <= 5) orders <- .ds_permutations(seq_len(n))
  all_legs <- sort(unique(c(rows$x1, rows$x2)))
  layouts <- lapply(orders, function(order) .ds_place_order(rows, order, geometry,
    width, height, gap, line_mm, all_legs))
  valid <- which(vapply(layouts, function(x) !is.null(x), logical(1)))
  if (!length(valid)) .ds_stop("Labels cannot clear the bracket legs at this output size. Enlarge the plot, shorten labels, or reduce label_size.")
  scores <- vapply(layouts[valid], function(layout) {
    headroom <- max(vapply(layout, `[[`, numeric(1), "top"))
    displacement <- sum(vapply(layout, function(p) abs(p$cx - mean(c(p$x1, p$x2))), numeric(1)))
    crossings <- 0
    for (i in seq_along(layout)) for (j in seq_along(layout)) {
      if (i == j) next
      bar <- layout[[i]]; leg <- layout[[j]]
      for (k in 1:2) {
        x <- c(leg$x1, leg$x2)[k]; y <- c(leg$a1, leg$a2)[k]
        if (x > bar$left && x < bar$right && bar$y > y && bar$y < leg$y) crossings <- crossings + 1
      }
    }
    c(headroom = headroom, crossings = crossings, displacement = displacement)
  }, numeric(3))
  best <- order(round(scores[1, ], 6), scores[2, ], scores[3, ])[1]
  result <- layouts[[valid[best]]]
  result[order(vapply(result, `[[`, integer(1), "index"))]
}
.ds_layout_panel <- function(annotations, geometry, range_x, range_y, width, height, style) {
  if (!nrow(annotations)) return(annotations)
  to_x <- function(x) (x - range_x[1]) / diff(range_x) * width
  to_y <- function(y) (y - range_y[1]) / diff(range_y) * height
  from_x <- function(x) range_x[1] + x / width * diff(range_x)
  from_y <- function(y) range_y[1] + y / height * diff(range_y)
  g <- geometry
  g$xmin <- to_x(g$xmin) - g$radius_mm
  g$xmax <- to_x(g$xmax) + g$radius_mm
  g$top <- to_y(g$ymax) + g$radius_mm
  r <- annotations
  r$x1 <- to_x(r$Group1.x); r$x2 <- to_x(r$Group2.x)
  if (any(c(r$x1, r$x2) < 0 | c(r$x1, r$x2) > width)) .ds_stop("Comparison groups lie outside the visible x range.")
  metrics <- .ds_label_metrics(r$Label, style)
  r$lw <- vapply(metrics, `[[`, numeric(1), "width")
  r$lh <- vapply(metrics, `[[`, numeric(1), "height")
  data_range <- diff(range(c(geometry$ymin, geometry$ymax), finite = TRUE))
  r$pad <- max(style$clearance_mm, if (is.null(style$ypad_ratio)) 0 else style$ypad_ratio * data_range / diff(range_y) * height)
  anchor <- function(keys) vapply(keys, function(key) max(g$top[g$key == key]) + r$pad[1], numeric(1))
  r$a1 <- anchor(r$.key1); r$a2 <- anchor(r$.key2)
  placed <- .ds_compact(r, g, width, height, style$clearance_mm, style$segment_linewidth)
  for (column in c("ymax", "mid.x", "ymax_adj", "Group1.y_adj", "Group2.y_adj", "mid.x_adj",
                   "label.y", "label.xmin", "label.xmax", "label.ymin", "label.ymax", "ypad")) r[[column]] <- NA_real_
  for (i in seq_along(placed)) {
    p <- placed[[i]]
    r$ymax[i] <- from_y(max(g$top[g$xmax >= p$left & g$xmin <= p$right]))
    r$mid.x[i] <- mean(c(r$Group1.x[i], r$Group2.x[i]))
    r$ymax_adj[i] <- from_y(p$y)
    r$Group1.y_adj[i] <- from_y(p$a1); r$Group2.y_adj[i] <- from_y(p$a2)
    r$mid.x_adj[i] <- from_x(p$cx)
    r$label.xmin[i] <- from_x(p$lx); r$label.xmax[i] <- from_x(p$rx)
    r$label.ymin[i] <- from_y(p$bottom); r$label.ymax[i] <- from_y(p$top)
    r$label.y[i] <- from_y(p$bottom + style$label_vjust * (p$top - p$bottom))
    # The text origin depends on hjust; the exposed midpoint stays the box centre.
    r$label.x[i] <- from_x(p$lx + style$label_hjust * (p$rx - p$lx))
    r$ypad[i] <- r$pad[i] / height * diff(range_y)
  }
  r$label_width_mm <- r$lw; r$label_height_mm <- r$lh
  r$overlapping_group <- get_overlapping_groups(r$Group1.x, r$Group2.x)
  r[c("x1", "x2", "a1", "a2", "lw", "lh", "pad")] <- NULL
  r
}
