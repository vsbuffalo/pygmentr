has_pygments <- function() {
  exit_status <- system("pygmentize -V", ignore.stdout=TRUE, ignore.stderr=TRUE)
  if (exit_status != 0)
    stop("Pygments 'pygmentize' not found - have you installed Pygments?")
  invisible(exit_status == 0)
}

#' Build and run command with system(..., intern=TRUE)
#'
#' @param cmd command to execute with \code{system(....)}.
#' @param input string input to be passed through standard input to \code{cmd}.
pipe_in <- function(cmd, input=NULL) {
  out <- system(cmd, input=input, intern=TRUE)
  paste(out, sep="\n", collapse="\n")
}

#' Setup pygmentr syntax highlighting for LaTeX files
#'
#' @export
pygmentr_latex <- function() {
  has_pygments()
  pygmentr_opts$set(formatter="latex")
  cat(get_header("latex"))
}

#' Setup pygmentr syntax highlighting for HTML files
#'
#' @export
pygmentr_html <- function() {
  has_pygments()
  pygmentr__opts$set(formatter="html")
  style_tmp = '
<style type="text/css">
/* automatically generated with Pygments */
%s
</style>'
  cat(sprintf(style_tmp, get_header("html")))
}


#' Create pygmentr highlight header for LaTeX or HTML
#'
#' @param formatter Pygments formatter to use; either "latex" or "html".
get_header <- function(formatter=c("latex", "html")) {
  has_pygments()
  formatter <- match.arg(formatter)
  style <- pygmentr_opts$get("style")
  pipe_in(cmd=sprintf('pygmentize -S "%s" -f "%s"', style, formatter))
}


#' Highlight code syntax
#'
#' Uses Python Pygements to highlight code.
#'
#' @param x character model specification.
#'
#' @export
hlsyntax <- function(x, lexer) {
  has_pygments()
  formatter <- pygementr_opts$get("formatter")
  cat(pipe_in(cmd=sprintf('pygmentize -f "%s" -l "%s"', formatter, lexer),
              input=x))
}
