#' Write pages, templates, layouts
#'
#' Create final page files, layouts folder, etc.
#'
#' @family output
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

#' @export
write_out.template <- function(x, dir = getwd(), ...) {
  with_dir(path_dir(dir), {
    filled <- glue::glue(x$content, .envir = parent.frame(2))
  })

  dest <- path(dir, x$name)
  dir_create(path_dir(dest))

  tryCatch(
    cat(filled, file = dest),
    error = function(e) {
      warning("failed to create template ", dest, call. = FALSE)
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
