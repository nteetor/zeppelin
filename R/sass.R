get_assets <- function() {
  sass_dir <- system.file("sass", package = "zeppelin")

  assets_files <- flatten_chr(map(c(sass_dir), dir_ls, type = "file"))

  map(assets_files, ~ {
    structure(
      list(
        content = paste0(readLines(.), collapse = "\n"),
        name = path_file(.)
      ),
      class = "asset"
    )
  })
}

create_main_scss <- function(files, dir = getwd()) {
  files <- path_ext_remove(files)
  dest <- path(dir, "assets", "css")
  dir_create(dest)

  cat0(
    "---",
    "---",
    "",
    map_chr(files, ~ glue("@import '{ sub('^_', '', .) }';")),
    sep = "\n",
    file = path(dest, "main", ext = "scss")
  )
}
