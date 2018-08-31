#' Build a jekyll site
#'
#' The `jekyll` function builds a jekyll site from your
#' package's roxygen.
#'
#' @param pkg A character string specifying a file path to an R package,
#'   defaults to `"."`, the current directory.
#'
#' @param dir A folder path, relative to `pkg`, where the jekyll site will be
#'   built, defaults to `"docs"`.
#'
#' @details
#'
#' Collections are currently overwritten. However, if a family's name is
#' changed then the old, corresponding collection will not be deleted. This is
#' to prevent custom collections from accidentally being removed. This behaviour
#' is subject to change.
#'
#' @export
#' @examples
#'
#' ## Creating your jekyll site
#' 1:5
#'
#' ## Another example
#' shiny::tags$p("hello, world!")
#'
jekyll <- function(pkg = ".", dir = "docs") {
  base_dir <- path(pkg, dir)
  r_dir <- path(pkg, "R")

  if (!dir_exists(pkg)) {
    stop(
      glue("invalid `jekyll()` call, argument `pkg` path does not exist"),
      call. = FALSE
    )
  }

  tag_registry <- roxygen2:::default_tags()
  tag_registry$examples <- roxygen2::tag_examples

  blocks <- parse_package(pkg, env = NULL, registry = tag_registry)

  pages <- map(blocks, as_page, package = pkg)

  dir_create(base_dir)

  message("Writing pages")
  walk(pages, write_out, dir = base_dir)

  message("Writing layouts")
  walk(get_layouts(), write_out, dir = base_dir)

  message("Writing includes")
  walk(get_includes(), write_out, dir = base_dir)

  message("Writing assets")
  assets <- map(get_assets(), write_out, dir = base_dir)

  sass <- path_filter(assets, glob = "*.scss")
  create_main_scss(sass, dir = base_dir)

  get_versions <- function() {
    path_file(dir_ls(base_dir, type = "directory", regexp = "[0-9][0-9.]*"))
  }

  message("Writing templates")
  walk(get_templates(), write_out, dir = base_dir)

  invisible()
}
