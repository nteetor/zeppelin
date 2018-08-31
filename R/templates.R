get_templates <- function() {
  templates_dir <- path_package("zeppelin", "inst", "templates")

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
