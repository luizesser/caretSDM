#' @import checkmate
check_int_cli <- function(...){
  return(checkmate::check_int(...))
}

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

check_logical_cli <- function(...){
  return(checkmate::check_logical(...))
}

assert_logical_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_logical(x, ...),
      .var.name,
      add
    )
  ))
}


check_directory_cli <- function(...){
  return(checkmate::checkDirectory(...))
}

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


check_numeric_cli <- function(...){
  return(checkmate::check_numeric(...))
}

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

check_number_cli <- function(...){
  return(checkmate::check_number(...))
}

assert_number_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_number(x, ...),
      .var.name,
      add
    )
  ))
}


check_names_cli <- function(...){
  return(checkmate::check_names(...))
}

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

check_character_cli <- function(...){
  return(checkmate::check_character(...))
}

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


check_data_frame_cli <- function(...){
  return(checkmate::check_data_frame(...))
}

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


check_class_cli <- function(...){
  return(checkmate::check_class(...))
}

assert_class_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  res <- make_assertion(
    x,
    checkmate::check_class(x, ...),
    .var.name,
    add
  )
  return(invisible(res))
}

check_vector_cli <- function(...){
  return(checkmate::check_vector(...))
}

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

check_list_cli <- function(...){
  return(checkmate::check_list(...))
}

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


sanitize_cli <- function(res){
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

fmt_bullet_cli <- function(res, cli_bullet="i"){
   if (isTRUE(res)){
     return(res)
   } else {
     res <- res |>
       sanitize_cli() |>
       setNames(rep("i", length(res)))

     return(res)
   }
 }

make_assertion <- function(x, res, var.name, collection) {
  if (!isTRUE(res)) {
    checkmate::assertString(var.name, .var.name = ".var.name")

    if (is.null(collection)) {
      cli::cli_abort(
        c(
          "x" = "Assertion on { var.name } failed.",
          "i" = res
        ),
        #.internal = TRUE,
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


.mstopOrPush = function(res, v_name, collection = NULL) {
  if (!is.null(collection)) {
    purrr::map2(res, names(res), \(e, n) glue::glue("({n}): {e}")) |>
      as.character() |>
      collection$push()
  } else if (length(v_name) > 1L) {
    v_name <- v_name |>
      unique()
    names(res) <- rep("i", length(res))
    msg <- c(
      glue::glue("Assertion on < v_name > failed.", .open = "<", .close = ">"),
      res
    )
    cli::cli_abort(
      msg,
      #.internal = TRUE,
      call = parent.frame(n=2)
    )
  } else {
    msg <- c(
      glue::glue("Assertion on < v_name > failed.", .open = "<", .close = ">"),
      c("i" = res)
    )
    cli::cli_abort(
      msg,
      #.internal = TRUE,
      call = parent.frame(n=2)
    )
  }
}
