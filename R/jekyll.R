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
  tag_registry$examples <- function(x) x #roxygen2::tag_examples
  tag_registry$layout <- function(x) x

  blocks <- parse_package(pkg, env = NULL, registry = tag_registry)

  message("Creating pages")

  env <- suppressMessages(env_package(pkg))

  pages <- compact(map(blocks, as_page, package = pkg, env = env))

  names(pages) <- map_chr(pages, ~ .$roxygen$name %||% .$this)

  pages <- map(pages, ~ {
    if (is.null(.$roxygen$rdname)) {
      return(.)
    }

    new <- pages[names(pages) == .$roxygen$rdname][[1]]
    new$this <- .$this
    new$filename <- .$filename

    attr(new, "path") <- path(path_dir(new %@% "path"), path_file(. %@% "path"))

    new
  })

  dir_create(base_dir)

  dir_delete(path(base_dir, desc_get("Version", pkg)))

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

  file_copy(
    path(pkg, "inst", "www", "yonder", "css", "yonder.min.css"),
    path(base_dir, "assets", "css", "yonder.min.css"),
    overwrite = TRUE
  )

  file_copy(
    path(pkg, "inst", "www", "bootstrap", "css", "bootstrap.min.css"),
    path(base_dir, "assets", "css", "bootstrap.min.css"),
    overwrite = TRUE
  )

  dir_create(base_dir, "assets", "js")
  file_copy(
    path(pkg, "inst", "www", "bootstrap", "js", "bootstrap.min.js"),
    path(base_dir, "assets", "js", "bootstrap.min.js"),
    overwrite = TRUE
  )

  invisible()
}
