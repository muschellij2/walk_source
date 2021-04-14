walk_source <- function(
  file = NULL, 
  delay = 1.5, 
  clear_each_cmd = TRUE,
  clear_before_first_cmd = TRUE,
  wait_for_press = TRUE,
  style = styler::tidyverse_style,
  record_screen = TRUE,
  character_delay = 0.15,
  ...
) {
  if (record_screen) {
    stopifnot(requireNamespace("recordscreen", quietly = TRUE))
  }
  options("walk_source_cancel" = FALSE)
  if (is.null(file)) {
    txt <- rstudioapi::getSourceEditorContext()$contents
  } else {
    txt <- readLines(file, warn = FALSE)
  }
  calls <- rlang::parse_exprs(paste(txt, collapse = "\n"))
  i <- 0
  op <- options(digits.secs = 2)
  on.exit({
    options(op)
  })
  book <<- data.frame(
    call = c(as.character(calls), NA_character_),
    index = 1:(length(calls) + 1),
    stringsAsFactors = FALSE
  )
  book$start = book$end = NA
  next_call <- function() {
    i <<- i + 1
    if (clear_each_cmd | ( i == 1 & clear_before_first_cmd) ) cat("\f")
    text <- rlang::expr_text(calls[[i]])
    if (!is.null(style)) {
      text <- styler::style_text(text, style = style)
    }
    text <- paste(text, collapse = "\n")
    book$start[i] <<- Sys.time()
    for (j in seq_len(nchar(text))) {
      if (isTRUE(getOption("walk_source_cancel", FALSE))) {
        stop("canceled")
      }
      if (substring(text, j, j) == " ") {
        # Sys.sleep(runif(1, 0.1, 0.333))
      } else {
        # Sys.sleep(runif(1, 0.01, 0.25))
      }
      Sys.sleep(character_delay)
      
      rstudioapi::sendToConsole("", FALSE, focus = FALSE)
      rstudioapi::sendToConsole(
        code = substring(text, 1, j),
        execute = j == nchar(text)
      )
    }
    book$end[i] <<- Sys.time()
    if (i < length(calls)) {
      later::later(next_call, delay)
    }
    if (i == length(calls)) {
      book$end = as.POSIXct(book$end, origin = "1970-01-01")      
      book$start = as.POSIXct(book$start, origin = "1970-01-01")
      assign("book", book, envir = .GlobalEnv) 
      assign("book", book, envir = parent.frame()) 
    }
  }
  if (wait_for_press) {
    readline("Press enter when ready!")
    cat("\f")
  }
  outside_env = environment()
  if (record_screen) {
    outside_env$screen_record_result = recordscreen::start_screen_record(...)
    record_start_time <- Sys.time()
    Sys.sleep(0.25) # unsure about this
  } 
  book$outfile = outside_env$screen_record_result$outfile
  later::later(next_call, delay)
  if (record_screen) {
    end_func = function() {
      # print("ending")
      # print(paste0("i: ", i))
      recordscreen::end_screen_record(outside_env$screen_record_result)
      i <<- length(calls) + 1
      book$end[i] <<- Sys.time()
      book$start[i] <<- record_start_time
      # print(book)
      book$end = as.POSIXct(book$end, origin = "1970-01-01")      
      book$start = as.POSIXct(book$start, origin = "1970-01-01") 
      assign("book", book, envir = .GlobalEnv) 
      assign("book", book, envir = parent.frame())
    }
    # later::later(end_func, delay = delay * length(calls) + 1)
    later::later(end_func, 
                 delay = delay * length(calls) + 
                   sum(nchar(txt)) * character_delay + 2 + 0.25)
  }
  # print(book)
  
  # if (record_screen) {
  #   return()
  # }
  return(book)
}

walk_cancel <- function() {
  options("walk_source_cancel" = TRUE)
}

walk_cancel()
# walk_source(clear_each_cmd = FALSE, file = "example.R", wait_for_press = FALSE)
