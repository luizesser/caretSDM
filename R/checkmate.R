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

check_subset_cli <- function(...){
  return(checkmate::check_subset(...))
}

assert_subset_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_subset(x, ...),
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

check_file_exists_cli <- function(...) {
  checkmate::check_file_exists(...)
}

assert_file_exists_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_file_exists(x, ...),
      .var.name,
      add
    )
  ))
}

check_choice_cli <- function(...) {
  checkmate::check_choice(...)
}

assert_choice_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_choice(x, ...),
      .var.name,
      add
    )
  ))
}

check_true_cli <- function(...) {
  checkmate::check_true(...)
}

assert_true_cli <- function(x, ..., .var.name = vname(x), add = NULL){
  return(invisible(
    make_assertion(
      x,
      checkmate::check_true(x, ...),
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
       stats::setNames(rep("i", length(res)))

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


###################################################################################################
### Scalars / atomic ####

check_atomic_cli <- function(...) {
  checkmate::check_atomic(...)
}

assert_atomic_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_atomic(x, ...),
      .var.name,
      add
    )
  )
}

check_atomic_vector_cli <- function(...) {
  checkmate::check_atomic_vector(...)
}

assert_atomic_vector_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_atomic_vector(x, ...),
      .var.name,
      add
    )
  )
}

check_scalar_cli <- function(...) {
  checkmate::check_scalar(...)
}

assert_scalar_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_scalar(x, ...),
      .var.name,
      add
    )
  )
}

check_scalar_na_cli <- function(...) {
  checkmate::check_scalar_na(...)
}

assert_scalar_na_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_scalar_na(x, ...),
      .var.name,
      add
    )
  )
}

check_integer_cli <- function(...) {
  checkmate::check_integer(...)
}

assert_integer_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_integer(x, ...),
      .var.name,
      add
    )
  )
}

check_integerish_cli <- function(...) {
  checkmate::check_integerish(...)
}

assert_integerish_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_integerish(x, ...),
      .var.name,
      add
    )
  )
}

check_double_cli <- function(...) {
  checkmate::check_double(...)
}

assert_double_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_double(x, ...),
      .var.name,
      add
    )
  )
}

check_complex_cli <- function(...) {
  checkmate::check_complex(...)
}

assert_complex_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_complex(x, ...),
      .var.name,
      add
    )
  )
}

check_count_cli <- function(...) {
  checkmate::check_count(...)
}

assert_count_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_count(x, ...),
      .var.name,
      add
    )
  )
}

check_string_cli <- function(...) {
  checkmate::check_string(...)
}

assert_string_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_string(x, ...),
      .var.name,
      add
    )
  )
}

check_flag_cli <- function(...) {
  checkmate::check_flag(...)
}

assert_flag_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_flag(x, ...),
      .var.name,
      add
    )
  )
}

check_null_cli <- function(...) {
  checkmate::check_null(...)
}

assert_null_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_null(x, ...),
      .var.name,
      add
    )
  )
}

check_false_cli <- function(...) {
  checkmate::check_false(...)
}

assert_false_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_false(x, ...),
      .var.name,
      add
    )
  )
}

### Containers / data structures ####

check_array_cli <- function(...) {
  checkmate::check_array(...)
}

assert_array_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_array(x, ...),
      .var.name,
      add
    )
  )
}

check_matrix_cli <- function(...) {
  checkmate::check_matrix(...)
}

assert_matrix_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_matrix(x, ...),
      .var.name,
      add
    )
  )
}

check_factor_cli <- function(...) {
  checkmate::check_factor(...)
}

assert_factor_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_factor(x, ...),
      .var.name,
      add
    )
  )
}

check_environment_cli <- function(...) {
  checkmate::check_environment(...)
}

assert_environment_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_environment(x, ...),
      .var.name,
      add
    )
  )
}

check_function_cli <- function(...) {
  checkmate::check_function(...)
}

assert_function_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_function(x, ...),
      .var.name,
      add
    )
  )
}

check_formula_cli <- function(...) {
  checkmate::check_formula(...)
}

assert_formula_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_formula(x, ...),
      .var.name,
      add
    )
  )
}

check_r6_cli <- function(...) {
  checkmate::check_r6(...)
}

assert_r6_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_r6(x, ...),
      .var.name,
      add
    )
  )
}

check_raw_cli <- function(...) {
  checkmate::check_raw(...)
}

assert_raw_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_raw(x, ...),
      .var.name,
      add
    )
  )
}

### Names, sets, OS ####

check_named_cli <- function(...) {
  checkmate::check_named(...)
}

assert_named_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_named(x, ...),
      .var.name,
      add
    )
  )
}

check_set_equal_cli <- function(...) {
  checkmate::check_set_equal(...)
}

assert_set_equal_cli <- function(x, y, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_set_equal(x, y, ...),
      .var.name,
      add
    )
  )
}

check_disjunct_cli <- function(...) {
  checkmate::check_disjunct(...)
}

assert_disjunct_cli <- function(x, y, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_disjunct(x, y, ...),
      .var.name,
      add
    )
  )
}

check_os_cli <- function(...) {
  checkmate::check_os(...)
}

assert_os_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_os(x, ...),
      .var.name,
      add
    )
  )
}

### Files / directories / paths ####

check_directory_exists_cli <- function(...) {
  checkmate::check_directory_exists(...)
}

assert_directory_exists_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_directory_exists(x, ...),
      .var.name,
      add
    )
  )
}

check_path_for_output_cli <- function(...) {
  checkmate::check_path_for_output(...)
}

assert_path_for_output_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_path_for_output(x, ...),
      .var.name,
      add
    )
  )
}

### Date / time and special classes ####

check_date_cli <- function(...) {
  checkmate::check_date(...)
}

assert_date_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_date(x, ...),
      .var.name,
      add
    )
  )
}

check_posixct_cli <- function(...) {
  checkmate::check_posixct(...)
}

assert_posixct_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_posixct(x, ...),
      .var.name,
      add
    )
  )
}

check_multi_class_cli <- function(...) {
  checkmate::check_multi_class(...)
}

assert_multi_class_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_multi_class(x, ...),
      .var.name,
      add
    )
  )
}

check_permutation_cli <- function(...) {
  checkmate::check_permutation(...)
}

assert_permutation_cli <- function(x, ..., .var.name = vname(x), add = NULL) {
  invisible(
    make_assertion(
      x,
      checkmate::check_permutation(x, ...),
      .var.name,
      add
    )
  )
}
