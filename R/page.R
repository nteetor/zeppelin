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
      .sections <- tryCatch(
        parse_examples(x$examples),
        error = function(e) {
          stop(
            "could not parse examples in ", x %@% "filename",
            call. = FALSE
          )
        }
      )

      compact(map(.sections, ~ {
        if (is.null(.$value) ||
            length(.$value) == 0 ||
            (is.character(.$value) && !nzchar(.$value))) {
          return(NULL)
        }

        if (.$type == "output") {
          result <- eval(.$value, envir = env)

          if (inherits(result, "shiny.tag")) {
            .$value <- as.character(result)
          } else if (class(result)[1] == "list") {
            .$value <- paste(map_chr(result, as.character), collapse = "\n")
          } else {
            .$type <- "code"
            .$value <- as.character(.$value)
          }
        }

        .
      }))
    }
  }

  name <- if (length(x %@% "call") > 1) {
    as.character((x %@% "call")[[2]])
  } else if (length(x %@% "call") == 1) {
    if (x %@% "call" == "_PACKAGE") {
      "index"
    } else {
      x %@% "call"
    }
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
      x <- stringr::str_replace_all(
        x,
        "([^\\[])\\[([a-zA-Z][a-zA-Z0-9]*)\\]",
        glue('\\1[\\2](/{ desc_get("Package") }/{ desc_get("Version") }/{{: get_family("\\2") :}}\\2.html)')
      )
      x <- stringr::str_replace_all(
        x,
        "([^\\[])\\[([a-zA-Z][a-zA-Z0-9]*)\\(\\)\\]",
        glue('\\1[\\2()](/{ desc_get("Package") }/{ desc_get("Version") }/{{: get_family("\\2") :}}\\2.html)')
      )
      x
    })
  } else {
    x
  }
}

type_markdown <- function(value) {
  list(
    type = "markdown",
    value = commonmark::markdown_html(value)
  )
}

type_output <- function(value) {
  list(
    type = "output",
    value = parse(text = value)
  )
}

type_source <- function(value) {
  list(
    type = "source",
    value = value
  )
}

parse_examples <- function(x) {
  tokens <- sourcetools::tokenize_string(x)

  if (all(tokens$type == "whitespace")) {
    return(list())
  }

  sections <- tokens %>%
    group_by(row) %>%
    summarise(
      value = paste(value, collapse = ""),
      type = if (length(type) > 1) "code" else type
    ) %>%
    mutate(
      type = dplyr::case_when(
        type == "comment" ~ "comment",
        type == "whitespace" & value == "\\n\\n" ~ "whitespace",
        TRUE ~ "code"
      ),
      type2 = dplyr::lag(type, default = type[1]),
      inc = as.double(type != type2),
      order = accumulate(inc, `+`)
    ) %>%
    group_by(order) %>%
    summarise(
      value = paste(value, collapse = ""),
      value = gsub("\n#", "", value, fixed = TRUE),
      value = gsub("\\n\\s*$", "", value),
      value = gsub("^# ", "", value),
      type = unique(type)
    )

  flatten(map2(sections$value, sections$type, ~ {
    if (.y == "comment") {
      return(list(type_markdown(.x)))
    }

    list(
      type_source(.x),
      type_output(.x)
    )
  }))
}
