rmd_to_df = function(file, execute = FALSE, 
                     output_format = rmarkdown::html_document()) {
  # set up unexported
  pandoc_path_arg = rmarkdown:::pandoc_path_arg
  knitr_files_dir = rmarkdown:::knitr_files_dir
  knitr_cache_dir = rmarkdown:::knitr_cache_dir
  adjust_dev = rmarkdown:::adjust_dev
  pandoc_output_file = rmarkdown:::pandoc_output_file
  
  x = xfun::read_utf8(file)
  res = knitr:::split_file(x, patterns = knitr::all_patterns$md)
  if (!is.null(output_format)) {
    pandoc_to = output_format$pandoc$to
  }
  
  output_file <- pandoc_output_file(file, output_format$pandoc)
  
  
  ###################################################
  # Taken from Rmarkdown
  ###################################################

  knitr::render_markdown()
  knitr::opts_chunk$set(tidy = FALSE, error = FALSE)
  # the retina option does not make sense to non-HTML output formats
  if (!grepl('[.]html$', output_file)) knitr::opts_chunk$set(fig.retina = NULL)  
  
  # Restorer 
  optk <- knitr::opts_knit$get()
  on.exit(knitr::opts_knit$restore(optk), add = TRUE)
  optc <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$restore(optc), add = TRUE)
  hooks <- knitr::knit_hooks$get()
  on.exit(knitr::knit_hooks$restore(hooks), add = TRUE)
  ohooks <- knitr::opts_hooks$get()
  on.exit(knitr::opts_hooks$restore(ohooks), add = TRUE)
  templates <- knitr::opts_template$get()
  on.exit(knitr::opts_template$restore(templates), add = TRUE)
  on.exit(knitr::knit_code$restore(), add = TRUE)
  

  output_dir <- dirname(file)
  # use output filename based files dir
  files_dir_slash <- file.path(output_dir, knitr_files_dir(basename(file)))

  # use filename based figure and cache directories
  base_pandoc_to <- gsub('[-+].*', '', pandoc_to)
  if (base_pandoc_to == 'html4') base_pandoc_to <- 'html'
  knitr::opts_chunk$set(fig.path = paste0(
    pandoc_path_arg(files_dir_slash, backslash = FALSE),
    "/figure-", base_pandoc_to, "/"
  ))
  cache_dir <- knitr_cache_dir(file, base_pandoc_to)
  knitr::opts_chunk$set(cache.path = cache_dir)
  
  if (!is.null(output_format$knitr)) {
    knitr::opts_knit$set(as.list(output_format$knitr$opts_knit))
    knitr::opts_chunk$set(adjust_dev(as.list(output_format$knitr$opts_chunk)))
    knitr::opts_template$set(as.list(output_format$knitr$opts_template))
    knitr::knit_hooks$set(as.list(output_format$knitr$knit_hooks))
    knitr::opts_hooks$set(as.list(output_format$knitr$opts_hooks))
  }
  ####################################################
  
  classes = sapply(res, attr, "class")
  blocks = res[classes %in% "block"]
  params = lapply(blocks, function(el) {
    if (attr(el, "class") %in% "block") {
      knitr:::block_params(el, verbose = FALSE)
    } else {
      NULL
    }
  })
  if (execute) {
    execs = lapply(blocks, function(el) {
      if (attr(el, "class") %in% "block") {
        # el$params$echo = FALSE
        el$params$message = FALSE
        el$params$warning = FALSE
        knitr:::call_block(el, collapse = "\n\n")
      } else {
        NULL
      }
    })
  }
  df = lapply(params, function(x) {
    if (!is.null(x)) {
      keep_names = c("eval", "echo", "engine", 
                     "label", "code", "hash", "cache",
                     "cache.path", "cache.vars", 
                     "cache.lazy", "fig.path", "fig.ext",
                     "fig.cur",
                     "fig.env", "error", "warning", "message",
                     "include", "params.src", "purl")
      x$code = paste(x$code, collapse = "\n")
      x = lapply(keep_names, function(r) {
        xx = x[[r]]
        if (is.null(xx)) {
          xx = NA
        }
        xx
      })
      names(x) = keep_names
      as.data.frame(x[keep_names])
    } else {
      NULL
    }
  })
  if (execute) {
    df = mapply(function(x, y) {
      x$output = y
      x
    }, df, execs, SIMPLIFY = FALSE)
  }
  df = do.call("rbind", df)
  return(df)
}




