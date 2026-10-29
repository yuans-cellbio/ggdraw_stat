# Source this entry point; no packages are attached to the search path.
.draw_stat_source <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(.draw_stat_source)) .draw_stat_source <- "ggplot_draw_stat.R"
.draw_stat_root <- dirname(normalizePath(.draw_stat_source, mustWork = TRUE))
for (.draw_stat_module in c("resolve.R", "layout.R", "render.R", "api.R")) {
  sys.source(file.path(.draw_stat_root, "R", .draw_stat_module), envir = environment())
}
rm(.draw_stat_source, .draw_stat_root, .draw_stat_module)
