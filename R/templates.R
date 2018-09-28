get_templates <- function() {
  templates_dir <- system.file("templates", package = "zeppelin")

  map(dir_ls(templates_dir, recursive = TRUE, type = "file"), ~ {
    structure(
      list(
        content = paste(readLines(.), collapse = "\n"),
        name = path_file(.)
      ),
      class = "template"
    )
  })
}
