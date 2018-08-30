#' Write pages, templates, layouts
#'
#' Create final page files, layouts folder, etc.
#'
#' @export
write_out <- function(x, ...) {
  UseMethod("write_out")
}

#' @export
write_out.page <- function(x, dir = getwd(), ...) {
  dest <- path(dir, x %@% "path", ext = "md")

  dir_create(path_dir(dest), recursive = TRUE)
  file_create(dest)

  content <- c("---\n", as_yaml(x), "---\n")

  tryCatch(
    cat(content, file = dest, sep = ""),
    error = function(e) {
      message("failed to create page ", dest)
    }
  )

  invisible()
}

write_out.template <- function(x, ...) {
  filled <- glue::glue(x$content, .envir = parent.frame())

  dest <- path(getwd(), x$rel_path)
  dir_create(path_dir(dest))

  tryCatch(
    cat(filled, file = dest),
    error = function(e) {
      message("failed to create template ", dest)
    }
  )

  invisible()
}

#' @export
write_out.layout <- function(x, dir = getwd(), ...) {
  dest <- path(dir, "_layouts", x$name)
  dir_create(path_dir(dest))

  tryCatch(
    cat(x$content, file = dest),
    error = function(e) {
      message("failed to create layout ", dest)
    }
  )
}

#' @export
write_out.include <- function(x, dir = getwd(), ...) {
  dest <- path(dir, "_includes", x$name)
  dir_create(path_dir(dest))

  tryCatch(
    cat(x$content, file = dest),
    error = function(e) {
      message("failed to create include ", dest)
    }
  )
}

#' @export
write_out.asset <- function(x, dir = getwd(), ...) {
  dest <- path(dir, "_sass", x$name)
  dir_create(path_dir(dest))

  tryCatch(
    cat(x$content, file = dest),
    error = function(e) {
      warning("failed to create asset ", dest, call. = FALSE)
    }
  )

  invisible(x$name)
}
