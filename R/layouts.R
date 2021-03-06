get_layouts <- function() {
  layouts_dir <- system.file("layouts", package = "zeppelin")

  map(dir_ls(layouts_dir, recursive = TRUE, type = "file"), ~ {
    structure(
      list(
        content = paste(readLines(.), collapse = "\n"),
        name = path_file(.)
      ),
      class = "layout"
    )
  })
}
