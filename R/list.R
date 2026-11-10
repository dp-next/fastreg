#' List SAS files in a directory
#'
#' Lists all SAS register files (with the extension `.sas7bdat`
#' case-insensitively) in the specified directory and its subdirectories.
#'
#' @param path Directory to search.
#'
#' @returns The path(s) to the found SAS file(s).
#'
#' @export
#' @examples
#' list_sas_files(fs::path_package("fastreg", "extdata"))
list_sas_files <- function(path) {
  # Check input.
  checkmate::assert_directory(path)
  checkmate::assert_string(path)

  # List all SAS files in the directory and its subdirectories.
  # (?i) makes the regex case-insensitive.
  sas_files <- fs::dir_ls(
    path,
    regexp = "(?i)\\.sas7bdat$",
    recurse = TRUE
  ) |>
    sort()

  if (length(sas_files) == 0) {
    cli::cli_abort("No SAS files found in {.path {path}}.")
  }

  sas_files
}

#' List Parquet datasets or files in a project
#'
#' Only lists Parquet files that end in `part-*.parquet`. For datasets,
#' it will only look for Parquet files with a `year=YYYY` in its path.
#' This function will search the whole system for the project ID, so it might
#' be slow sometimes.
#'
#' @name list_parquet
#' @rdname list_parquet
#' @returns The path(s) to the Parquet datasets (as directories) or files.
NULL

#' @describeIn list_parquet List all Parquet (Hive partitioned by year) datasets.
#' @export
list_parquet_datasets <- function() {
  list_parquet_files() |>
    fs::path_filter(
      regexp = "year=[[:digit:]]{4}|__HIVE_DEFAULT_PARTITION__"
    ) |>
    fs::path_dir() |>
    fs::path_dir() |>
    unique() |>
    fs::path()
}

#' @describeIn list_parquet List all Parquet files within a project.
#' @export
list_parquet_files <- function() {
  data_paths <- purrr::list_c(
    list(
      if (!is.null(getOption("fastreg.project_rawdata_dir"))) {
        get_project_rawdata_dir()
      },
      if (!is.null(getOption("fastreg.project_workdata_dir"))) {
        get_project_workdata_dir()
      }
    )
  )

  if (length(data_paths) == 0) {
    cli::cli_abort(
      "Cannot list Parquet files as neither {.code fastreg.project_rawdata_dir} nor {.code fastreg.project_workdata_dir} is set in options. Please provide at least one."
    )
  }

  fs::dir_ls(
    # Start from root of system.
    data_paths,
    regexp = ".*/part-.*\\.parquet$",
    recurse = TRUE,
    fail = FALSE,
    type = "file"
  )
}
