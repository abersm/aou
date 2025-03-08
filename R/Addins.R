# Insert text -------------------------------------------------------------

#' Insert %in%
#'
#' Shortcut: cmd + I
#' @rdname re
#' @export
insert_subset_pipe <- function() rstudioapi::insertText(" %in% ")

#' Insert section title using highlighted or copied text
#'
#' Shortcut: cmd + T
#' @rdname re
#' @export
insert_section_title <- function() {
  highlighted_text <- rstudioapi::getActiveDocumentContext()
  line_number <- highlighted_text$selection[[1L]]$range$start["row"]
  text <- highlighted_text$selection[[1L]]$text
  text <- text %||% ""
  text <- trimws(text)
  text <- gsub("\n", "", text, fixed = TRUE)
  text <- paste0("# ", text, " ")
  n_dash <- 75L - nchar(text)
  if (n_dash > 3L) {
    text <- paste0(text, strrep("-", n_dash))
    rstudioapi::modifyRange(location = rstudioapi::document_position(c(line_number, 1L), c(line_number, 75L)), text = text)
  }
}

# Quotes ------------------------------------------------------------------

#' Separate function arguments onto separate lines
#'
#' Functionality from Lionel Henry's excellent package codegrip
#' @returns Code silently edited
#' @export
addin_reshape <- function(...) {
  pkg_required("codegrip")
  addin_reshape_unsafe <- function() {
    context <- rstudioapi::getActiveDocumentContext()
    lines <- context$contents
    sel <- context$selection[[1L]]$range

    # No reshaping for selections
    if (!identical(sel$start, sel$end)) return()

    line <- sel$start[[1L]]
    col <- sel$start[[2L]]

    parse_info <- codegrip:::parse_info(lines = lines)
    out <- codegrip:::reshape_info(line, col, info = parse_info)

    pos1 <- rstudioapi::document_position(out$start[["line"]], out$start[["col"]])
    pos2 <- rstudioapi::document_position(out$end[["line"]], out$end[["col"]])
    range <- rstudioapi::document_range(pos1, pos2)

    rstudioapi::modifyRange(range, out$reshaped)
    #rstudioapi::setCursorPosition(range$start)
    rstudioapi::setCursorPosition(sel)
  }
  tryNULL(addin_reshape_unsafe())
}
