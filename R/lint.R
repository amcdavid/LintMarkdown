#' Checks that `=` is used for assignment
#'
#' @inherit lintr::T_and_F_symbol_linter
#' @export
arrow_lintr = function(source_file) {
  lapply(lintr::ids_with_token(source_file, "LEFT_ASSIGN"), function(id) {
    parsed = lintr::with_id(source_file, id)
    lintr::Lint(
      filename = source_file$filename,
      line_number = parsed$line1,
      column_number = parsed$col1,
      type = "style",
      message = "Use =, not <-, for assignment.",
      line = source_file$lines[as.character(parsed$line1)],
      linter = "arrow_assignment_linter"
    )
  })
}

linters = lintr::with_defaults(
  assignment_linter = NULL,
  arrow_assign = arrow_lintr,
  single_quotes_linter = NULL,
  trailing_whitespace_linter = NULL,
  line_length_linter =  NULL,
  lintr::line_length_linter(90),
  commented_code_linter = NULL,
  lintr::T_and_F_symbol_linter,
  lintr::undesirable_operator_linter,
  lintr::undesirable_function_linter(),
  lintr::semicolon_terminator_linter
)

#' Lint all .R and .Rmd recursively in a directory
#'
#' @inheritDotParams lintr::lint_dir
#' @inherit lintr::lint_dir
#' @export
lint_dir_rmd = function(...) {
    # check .Rmd, .R, .r
    val = lintr::lint_dir(pattern = "\\.[Rr](md)?$", linters = linters, ...)
    return(val)
}

#' @describeIn lint_dir_rmd exact alias
#' @export
lint_assignment = lint_dir_rmd


#' Lint all .R and .Rmd package dirs
#' @inheritDotParams lintr::lint_package
#' @inherit lintr::lint_package
#' @export
lint_package_rmd = function(...) {
  lintr::lint_package(pattern = "\\.[Rr](md)?$", linters = linters, ...)
}
