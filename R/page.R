#' Convert to page
#'
#' For now, takes a roxygen block and converts it to a page.
#'
#' @export
as_page <- function(x, ...) {
  UseMethod("as_page")
}

#' @export
as_page.roxy_block <- function(x, package = ".", ...) {
  if (!inherits(x, "roxy_block")) {
    stop(
      "invalid `as_page()` argument, `x` must be a roxygen block",
      call. = FALSE
    )
  }

  package <- path_abs(package)

  parameters <- {
    if ("param" %in% names(x)) {
      unname(x[names(x) == "param"])
    } else {
      list()
    }
  }

  sections <- {
    if ("section" %in% names(x)) {
      map(unname(x[names(x) == "section"]), ~ {
        split <- strsplit(., ":\n\n", fixed = TRUE)[[1]]
        names(split) <- c("title", "body")
        as.list(split)
      })
    }
  }

  examples <- {
    if ("examples" %in% names(x)) {
      this <- strsplit(x$examples, "\\n##\\s+", perl = TRUE)[[1]]

      with_package(path_file(package), {
        map(this[this != ""], ~ {
          . <- strsplit(., "\\n+")[[1]]

          title <- .[1]
          source <- paste(.[-1], collapse = "\n")
          output <- as.character(eval(parse(text = source)))

          list(
            title = title,
            source = source,
            output = output
          )
        })
      })
    }
  }

  name <- if (length(x %@% "call") > 1) {
    as.character((x %@% "call")[[2]])
  } else {
    ""
  }

  uri <- path(
    desc_get("Version", file = package),
    x$family %||% "",
    name
  )

  structure(
    list(
      name = name,
      filename = path_rel(attr(x, "filename"), package),
      layout = "page",
      roxygen = list(
        title = x$title,
        description = x$description,
        parameters = parameters,
        sections = sections,
        `return` = x$`return`,
        family = x$family,
        examples = examples
      )
    ),
    path = uri,
    class = "page"
  )
}
