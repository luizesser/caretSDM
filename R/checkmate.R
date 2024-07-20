#' @keywords internal
#' A internal version of checkmate package returning cli standardized messages.
#' @return Cli standardized messages
#' @import checkmate
#' @import cli
#' @export
check_int_cli <- function(...){
  return(checkmate::check_int(...))
}

#' @export
assert_int_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_int(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_directory_cli <- function(...){
  return(checkmate::checkDirectory(...))
}

#' @export
assert_directory_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::checkDirectory(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_numeric_cli <- function(...){
  return(checkmate::check_numeric(...))
}

#' @export
assert_numeric_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_numeric(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_names_cli <- function(...){
  return(checkmate::check_names(...))
}

#' @export
assert_names_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_names(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_character_cli <- function(...){
  return(checkmate::check_character(...))
}

#' @export
assert_character_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_character(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_data_frame_cli <- function(...){
  return(checkmate::check_data_frame(...))
}

#' @export
assert_data_frame_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_data_frame(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_class_cli <- function(...){
  return(checkmate::check_class(...))
}

#' @export
assert_class_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  res <- make_assertion(
    x,
    checkmate::check_class(x, ...),
    .var.name,
    add
  )
  return(invisible(res))
}

#' @export
check_vector_cli <- function(...){
  return(checkmate::check_vector(...))
}

#' @export
assert_vector_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_vector(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
check_list_cli <- function(...){
  return(checkmate::check_list(...))
}

#' @export
assert_list_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_list(x, ...),
      .var.name,
      add
    )
  ))
}

#' @export
cli_fmt <- function(res){
  if (isTRUE(res)){
    return(res)
  } else {
    return(
      res |>
        stringr::str_replace_all(stringr::str_escape("{"), "{{") |>
        stringr::str_replace_all(stringr::str_escape("}"), "}}")
    )
  }
}

#' @export
make_assertion = function(x, res, var.name, collection) {
  if (!isTRUE(res)) {
    checkmate::assertString(var.name, .var.name = ".var.name")

    if (is.null(collection)) {
      cli::cli_abort(
        c(
          "Assertion on { var.name } failed.",
          "x" = res
        ),
        .internal = TRUE,
        call = parent.frame(n=2)
      )
    }
    checkmate::assertClass(collection, "AssertCollection", .var.name = "add")
    collection$push(
      glue::glue("Variable '{ var.name }': {res}")
    )
  }
  return(invisible(res))
}

#' @export
assert_cli <- function(..., combine = "or", .var.name = NULL, add = NULL) {
  checkmate::assertChoice(combine, c("or", "and"))
  checkmate::assertClass(add, "AssertCollection", .var.name = "add", null.ok = TRUE)
  dots = match.call(expand.dots = FALSE)$...
  checkmate::assertCharacter(.var.name, null.ok = TRUE, min.len = 1L, max.len = length(dots))
  env = parent.frame()
  if (combine == "or") {
    if (is.null(.var.name))
      .var.name = vapply(dots, function(x) as.character(x)[2L], FUN.VALUE = NA_character_)
    msgs = character(length(dots))
    for (i in seq_along(dots)) {
      val = eval(dots[[i]], envir = env)

      if (isTRUE(val))
        return(invisible(TRUE))
      msgs[i] = as.character(val)
    }
    .mstopOrPush(res = msgs, v_name = .var.name, collection = add)
  } else {
    for (i in seq_along(dots)) {
      val = eval(dots[[i]], envir = env)
      if (!isTRUE(val)) {
        if (is.null(.var.name))
          .var.name = as.character(dots[[i]])[2L]
        msgs = as.character(val)
        .mstopOrPush(res = msgs, v_name = .var.name, collection = add)
      }
    }
  }
  invisible(TRUE)
}

#' @export
.mstopOrPush = function(res, v_name, collection = NULL) {
  if (!is.null(collection)) {
    purrr::map2(res, names(res), \(e, n) glue::glue("({n}): {e}")) |>
      as.character() |>
      collection$push()
  } else if (length(v_name) > 1L) {
    v_name <- v_name |>
      unique()
    names(res) <- rep("x", length(res))
    msg <- c(
      glue::glue("Assertion on < v_name > failed.", .open = "<", .close = ">"),
      res
    )
    cli::cli_abort(
      msg,
      .internal = TRUE,
      call = parent.frame(n=2)
    )
  } else {
    msg <- c(
      glue::glue("Assertion on < v_name > failed.", .open = "<", .close = ">"),
      c("x" = res)
    )
    cli::cli_abort(
      msg,
      .internal = TRUE,
      call = parent.frame(n=2)
    )
  }
}
