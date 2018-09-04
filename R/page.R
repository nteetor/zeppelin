#' @export
as_page <- function(x, ...) {
  UseMethod("as_page")
}

#' @export
as_page.roxy_block <- function(x, package = ".", env = env_package(package),
                               ...) {
  if (!inherits(x, "roxy_block")) {
    stop(
      "invalid `as_page()` argument, `x` must be a roxygen block",
      call. = FALSE
    )
  }

  if (is.null(x$descripion %||% x$title %||% x$rdname)) {
    return(NULL)
  }

  package <- path_abs(package)

  description <- {
    if (!is.null(x$description)) {
      replace_links(x$description, package)
    }
  }

  parameters <- {
    if ("param" %in% names(x)) {
      map(unname(x[names(x) == "param"]), ~ {
        .$description <- replace_links(.$description, package)
        .
      })
    } else {
      list()
    }
  }

  sections <- {
    if ("section" %in% names(x)) {
      map(unname(x[names(x) == "section"]), ~ {
        split <- as.list(strsplit(., ":\n\n", fixed = TRUE)[[1]])
        names(split) <- c("title", "body")
        split$body <- replace_links(split$body, package)
        split
      })
    }
  }

  examples <- {
    if ("examples" %in% names(x)) {
      this <- strsplit(x$examples, "\\n##\\s+", perl = TRUE)[[1]]

      map(this[!(this %in% c("", "\n"))], ~ {
        . <- strsplit(., "\\n+")[[1]]

        title <- .[1]
        source <- paste(.[-1], collapse = "\n\n")
        output <- compact(map(parse(text = source), ~ {
          res <- eval(., envir = env)

          if (inherits(res, "shiny.tag")) {
            as.character(res)
          }
        }))

        list(
          title = title,
          source = source,
          output = output
        )
      })
    }
  }

  name <- if (length(x %@% "call") > 1) {
    as.character((x %@% "call")[[2]])
  } else if (length(x %@% "call") == 1) {
    x %@% "call"
  } else {
    x$name
  }

  uri <- path(
    desc_get("Version", file = package),
    x$family %||% "",
    name
  )

  structure(
    list(
      this = name,
      filename = path_rel(attr(x, "filename"), package),
      layout = x$layout %||% "page",
      roxygen = list(
        title = x$title,
        description = description,
        parameters = parameters,
        sections = sections,
        `return` = x$`return`,
        family = x$family,
        name = x$name,
        rdname = x$rdname,
        examples = examples
      )
    ),
    path = uri,
    class = "page"
  )
}

replace_links <- function(x, package) {
  if (grepl("\\[[^]]+\\]", x)) {
    with_dir(package, {
      stringr::str_replace_all(
        x,
        "\\[([^]]+)\\]",
        glue('[\\1](/{ desc_get("Package") }/{ desc_get("Version") }/\\1.html)')
      )
    })
  } else {
    x
  }
}
