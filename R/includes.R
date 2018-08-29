get_includes <- function() {
  includes_dir <- path_package("zeppelin", "inst", "includes")

  map(dir_ls(includes_dir, recursive = TRUE, type = "file"), ~ {
    structure(
      list(
        content = paste(readLines(.), collapse = "\n"),
        name = path_file(.)
      ),
      class = "include"
    )
  })
}
