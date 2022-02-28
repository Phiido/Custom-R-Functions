

#' Render reports
#' 
#' Render multiple reports from Rmd-template
#' Can be used inside %>%
#' 
#' @author Thomas Roosdorp
#'
#' @param df Input as data frame
#' @param UID A vector containing IDs for each report
#' @param template File path with Rmd-template for rendering
#' 
#' @param by Which variable should be used for grouping outputs, or none
#' @param output_format Format that Rmarkdown uses
#' @param overwrite Whether existing files should be overwritten
#' @param is_quiet Whether Rmarkdown should be quiet
#' @param output_folder Folder to use for output. Default: ./output/reports
#'
render_report <- function(df, UID, template, ...) {
  
  libs <- c("tidyverse", "fs", "knitr", "rmarkdown", "pbapply", "textclean")
  req <- sapply(libs, require, character.only = TRUE)
  need <- libs[req == FALSE]
  
  if (!is_empty(need)) {
    input <- readline("Install missing required packages? Y/N: ")
    
    if (!(str_to_lower(input) %in% c("y", "yes")))
      return()
    
    install.packages(need)
    lapply(need, library, character.only = TRUE)
  }
  
  args_default <- list(by = NULL,
                       output_format = "pdf_document",
                       overwrite = FALSE,
                       is_quiet = TRUE,
                       output_folder = file.path("output", "reports"))
  
  args_in <- lapply(substitute(list(...)), 
                    function(x) ifelse(is.character(x) | is.null(x) | is.logical(x), x, deparse(x)))
  
  args <- modifyList(args_default, args_in)
  
  if (!is.data.frame(df)) stop("Input must be a data frame")
  if (!file.exists(template)) stop("Could not find valid template file")
  
  # Pull UID vector using symbol if it is part of df, else assume atomic vector
  if (is.vector(df[[ensym(UID)]])) {
    UID <- pull(df, ensym(UID))
  }
  
  by <- if(!is_empty(args$by)) sym(args$by)
  UID <- unique(UID[!is.na(UID)]) # Remove duplicates and NA
  output_folder <- args$output_folder
  file_ext <- paste0(".", sub("_document", "", args$output_format))
  
  # Check for existing files to exclude if overwrite is FALSE
  if (!args$overwrite) {
    rmUID <- NULL
    
    if (is_empty(by)) {
      files <- list.files(output_folder)
      UID_exist <- path_ext_remove(files)
      rmUID <- UID_exist
      
    } else {
      groups <- df %>%
        filter(!is.na(!!by)) %>%
        distinct(!!by) %>%
        mutate(groupings = replace_non_ascii(!!by)) %>%
        pull(groupings)
      
      for (group in groups) {
        target_folder <- file.path(output_folder, group)
        
        if (!dir.exists(target_folder)) next
        
        files <- list.files(target_folder)
        UID_exist <- path_ext_remove(files)
        append(rmUID, UID_exist)
      }
    }
    UID <- setdiff(UID, rmUID)
  }
  
  n_UID <- length(UID)
  
  if (is_empty(UID)) 
    stop("Files already exists and argument overwrite is FALSE")
  
  # If using grouping, find pairwise UID and group combination
  if (!is_empty(by)) {
    
    # Grab column name containing the UID-values, as symbol
    col_ID <- names(df)[which(df == UID[1], arr.ind = TRUE)[, "col"]] %>%
      unique() %>%
      sym()
    
    UID_group <- df %>%
      filter_all(any_vars(. %in% UID)) %>%
      mutate(groupings = replace_non_ascii(!!by)) %>%
      distinct(!!col_ID, groupings)
  }
  
  progress_bar <- timerProgressBar(min = 0, max = n_UID, initial = 0, style = 3)
  
  # Render each UID
  for (i in seq_along(UID)) {
    
    ID <- UID[i]
    file_name <- paste0(ID, file_ext)
    
    # Set target folder by output or group value
    if (is_empty(by)) {
      target_folder <- output_folder
    } else {
      group <- UID_group %>%
        filter(!!col_ID == ID) %>%
        distinct(groupings)
      
      target_folder <- file.path(output_folder, group)
    }
    
    # Create folder if it doesn't exist
    if (!dir.exists(target_folder)) {
      dir.create(target_folder, recursive = TRUE)
    }
    
    file_out <- render(template, 
                       output_format = args$output_format, 
                       quiet = args$is_quiet)
    
    file.rename(file_out, file.path(target_folder, file_name))
    
    setTimerProgressBar(progress_bar, i)
  }
  
  closepb(progress_bar)
  
  cat("\n",
      "Finished rendering of", n_UID, ifelse(n_UID > 1, "reports", "report"))
}