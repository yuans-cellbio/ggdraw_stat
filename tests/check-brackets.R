# Run from any directory: Rscript tests/check-brackets.R [evidence-directory]
# Only the normal ggplot2/rlang runtime dependencies are needed.
args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- normalizePath(file.path(dirname(script[1]), ".."))
source(file.path(root, "ggplot_draw_stat.R"))
library(ggplot2)

run_checks <- function() {
  checks <- character()
  check <- function(condition, label) {
    if (!isTRUE(condition)) stop(label, call. = FALSE)
    checks <<- c(checks, label)
  }
  arguments <- commandArgs(TRUE)
  evidence <- if (length(arguments)) arguments[1] else tempfile("draw-stat-checks-")
  dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
  if (!length(arguments)) on.exit(unlink(evidence, recursive = TRUE), add = TRUE)

  # Physical-space reference: 3-4 must be below the enclosing 2-4 bracket.
  rows <- data.frame(.comparison = c("2-4", "3-4", "1-2"), x1 = c(20, 30, 10),
    x2 = c(40, 40, 20), a1 = 12, a2 = 12, pad = 2, lw = 1, lh = 2)
  geometry <- data.frame(xmin = c(10, 20, 30, 40), xmax = c(10, 20, 30, 40), top = 10)
  for (left in c(FALSE, TRUE)) {
    place <- function(r = rows, gap = 6) .ds_bracket_rows(r, geometry, 60, 80, gap, .3, left)
    p <- place()
    check(p[[2]]$y < p[[1]]$y && p[[1]]$y - p[[2]]$y >= 6, "Shorter enclosed spans below outer spans; 6-mm minimum")
    check(abs(p[[2]]$y - 12.15) < 1e-10, "Data clearance is applied once")
    tall <- rows; tall$lh <- 9
    q <- place(tall, 3)
    check(q[[1]]$y - q[[2]]$y >= 9, "Measured label height is the minimum row step")
    clear <- rows; clear$pad <- 7; clear$a1 <- clear$a2 <- 17
    q <- place(clear)
    check(abs(q[[2]]$y - p[[2]]$y - 5) < 1e-10 && abs(q[[1]]$y - q[[2]]$y - (p[[1]]$y - p[[2]]$y)) < 1e-10,
      "Data clearance is independent of bracket spacing")
    order <- c(3, 1, 2); q <- place(rows[order, ])
    check(isTRUE(all.equal(vapply(p, `[[`, numeric(1), "y"), vapply(q, `[[`, numeric(1), "y")[match(seq_len(3), order)])),
      "Placement is stable under row permutation")
  }

  d <- expand.grid(group = factor(LETTERS[1:4]), replicate = 1:5)
  d$value <- 5 + c(-.4, -.1, 0, .1, .4)[d$replicate]
  p <- ggplot(d, aes(group, value)) + stat_summary(fun = mean, geom = "col", fill = "#4c78a8") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .15) + theme_classic()
  comparisons <- data.frame(Group1 = c("B", "C", "A"), Group2 = c("D", "D", "B"), Label = "*")
  for (position in c("auto", "left")) {
    info <- inspect_stat(p, comparisons, output_size = c(6, 5), clearance_mm = 2,
      bracket_gap_mm = 6, label_position = position)
    a <- info$annotations; size <- attr(a, "panel_sizes_mm"); range <- attr(a, "panel_ranges")[[1]]
    scale <- size$height / diff(range)
    check(nrow(a) == 3L && (a$ymax_adj[1] - a$ymax_adj[2]) * scale >= 6 - .03,
      paste(position, "native device respects bracket spacing and span order"))
    check(all((a$Group1.y_adj - a$Group1.y) * scale >= 2 - .03), paste(position, "native data-to-bracket clearance"))
  }

  # A long label covers the short right arm in the old masking implementation.
  identity_coord <- list(transform = function(data, params) data)
  covered <- data.frame(Group1.x = .2, Group2.x = .5, ymax_adj = .8,
    Group1.y_adj = .2, Group2.y_adj = .75, label.xmin = .25, label.xmax = .8,
    label.ymin = .7, label.ymax = .9)
  segments <- .ds_bracket_segments(covered, NULL, identity_coord, list(label_position = "left"))
  arms <- segments[segments$part != "bar", ]
  check(nrow(arms) == 2L && all(arms$x == arms$xend) && all(arms$yend > arms$y), "Both vertical arms survive label masking")
  check(any(arms$part == "right" & arms$x == .5 & arms$y == .75 & arms$yend == .8), "Covered short right arm retains its exact endpoints")
  bars <- segments[segments$part == "bar", ]
  check(nrow(bars) == 1L && bars$xend < .25, "Only horizontal label intersections are masked")
  empty <- .ds_geom()$draw_panel(data.frame(PANEL = 2L), NULL, NULL,
    list(annotations = data.frame(PANEL = 1L)), list())
  check(inherits(empty, "null"), "Panels without comparisons return a valid empty grob")

  pairs <- t(combn(LETTERS[1:4], 2))
  dense <- data.frame(Group1 = pairs[, 1], Group2 = pairs[, 2], Label = "Adjusted P = 0.000123")
  q <- draw_stat(p, dense, label_position = "left", clearance_mm = 2,
    bracket_gap_mm = 3, label_size = 10, short_legs = TRUE)
  info <- inspect_stat(p, dense, output_size = c(7, 6), label_position = "left",
    clearance_mm = 2, bracket_gap_mm = 3, label_size = 10, short_legs = TRUE)
  a <- info$annotations; scale <- attr(a, "panel_sizes_mm")$height / diff(attr(a, "panel_ranges")[[1]])
  check(nrow(a) == 6L, "Dense long-label layout retains the complete comparison set")
  panel <- info$built$layout$panel_params[[1]]
  ticks <- panel$guides$get_params("y")$key
  expected <- info$built$layout$coord$transform(data.frame(x = 1, y = ticks$.value), panel)$y
  check(max(abs(ticks$y - expected), na.rm = TRUE) < 1e-10,
    "Axis ticks use the same resolved coordinates as the data after headroom fitting")
  check(all((a$ymax_adj - a$Group1.y_adj) * scale <= 2 + .03) &&
    all(a$ymax_adj > a$Group1.y_adj) && all(a$ymax_adj > a$Group2.y_adj), "Short arms are positive and capped at 2 mm")
  ggsave(file.path(evidence, "left-labels.pdf"), q, width = 7, height = 6)
  ggsave(file.path(evidence, "left-labels.png"), q, width = 7, height = 6, dpi = 150)
  # Some facets have no comparisons; real gtable drawing must succeed.
  faceted_data <- rbind(transform(d, panel = "One"), transform(d, panel = "Two"))
  faceted <- ggplot(faceted_data, aes(group, value)) + geom_point() + facet_wrap(~panel)
  one_panel <- data.frame(Group1 = "A", Group2 = "B", Label = "*", panel = "One")
  ggsave(file.path(evidence, "empty-panel.pdf"), draw_stat(faceted, one_panel,
    bracket_gap_mm = 3, label_position = "left"), width = 7, height = 4)
  check(all(file.info(file.path(evidence, c("left-labels.pdf", "left-labels.png", "empty-panel.pdf")))$size > 1000), "Native PDF/PNG saving and unannotated facets succeed")
  counter <- new.env(); counter$n <- 0L
  counted <- ggplot(d, aes(group, value)) + stat_summary(fun = function(x) {
    counter$n <- counter$n + 1L; mean(x)
  }, geom = "point")
  config <- .ds_prepare(counted, dense, NULL, NULL, NULL, NULL)
  model <- .ds_model(counted, dense, config, " ", NULL)
  before <- counter$n
  resolved <- .ds_render_model(model, attr(q, "draw_stat_spec")$style)
  check(counter$n == before && nrow(resolved$annotations) == 6L,
    "Guide refresh and headroom fitting never rerun statistical layers")
  check(inherits(try(draw_stat(p, comparisons, bracket_gap_mm = -1), silent = TRUE), "try-error"), "Invalid minimum spacing is rejected")
  writeLines(c(paste("R", getRversion(), "/ ggplot2", packageVersion("ggplot2")), checks), file.path(evidence, "checks.txt"))
  cat("PASS", length(checks), "bracket regression checks\n")
}
run_checks()
