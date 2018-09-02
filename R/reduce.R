reduce_pages <- function(pages) {
  # names(pages) <- map_chr(pages, ~ .$roxygen$name %||% "")

  browser()

  wat <- dplyr::data_frame(
    object = map_chr(pages, ~ .$this %||% NA),
    name = map_chr(pages, ~ .$roxygen$name %||% NA),
    rdname = map_chr(pages, ~ .$roxygen$rdname %||% NA),
    page = pages
  )

  map(unique(names(pages)), ~ {
    subset <- pages[names(pages) == .]

  })
}

page_reduce.list <- function(x, y, ...) {
  if (page_name(y) %in% map_chr(x, page_name)) {
    map_if(x, ~ page_name(.) == page_name(y), ~ page_merge(., y))
  } else {
    c(x, list(y))
  }
}

page_reduce.page <- function(x, y, ...) {
  if (page_name(x) == page_name(y)) {
    return(list_merge(x, y))
  }

  list(x, y)
}

page_merge <- function(x, y, ...) {
  UseMethod("page_merge")
}

page_merge.page_set <- function(x, y, ...) {

}

page_merge.page <- function(x, y, ...) {
  structure(
    list(
      layout = "page-set",
      roxygen = list(

      )
    )
  )
}

page_name <- function(x) {
  stopifnot(inherits(x, "page"))
  x$roxygen$rdname %||% x$roxygen$name
}


