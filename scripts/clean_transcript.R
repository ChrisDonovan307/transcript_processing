#' Clean Transcript
#' 2024-08-19

#' This will be the front-facing function to interact with. It will call the
#' other functions based on format and whether it is by file or by folder.

source('scripts/clean_teams_transcript.R')
source('scripts/clean_zoom_transcript.R')

clean_transcript <- function(format = c('Zoom', 'Teams'),
                             raw_file = NULL,
                             raw_folder = NULL,
                             clean_path,
                             min_str_length = 6,
                             font_family = 'Aptos',
                             font_size = 12,
                             line_spacing = 1) {
  
  # Assertions --------------------------------------------------------------
  stopifnot(
    '** The format argument should either be "Zoom" or "Teams" **' = 
      format %in% c('Zoom', 'Teams'),
    '** The raw_file argument should be a character string **' =
      is.character(raw_file) | is.null(raw_file),
    '** The raw_folder argument should be a character string **' =
      is.character(raw_folder) | is.null(raw_folder),
    '** The path argument should be a character string **' =
      is.character(clean_path) | is.null(clean_path)
  )
  
  if ((is.null(raw_file) && is.null(raw_folder)) | 
      (!is.null(raw_file) && !is.null(raw_folder))) {
    stop(
      '** Please specify EITHER a raw_file OR a raw_folder of files to clean **',
      call. = FALSE
    )
  }
  

  
  # By Folder ---------------------------------------------------------------
  
  # Make sure path to clean folder ends with slash
  if (str_detect(clean_path, '/$', negate = TRUE)) {
    clean_folder <- paste0(clean_path, '/')
  } else {
    clean_folder <- clean_path
  }
  
  if (!is.null(raw_folder)) {
    
    # Get file paths and names
    file_paths <- list.files(raw_folder,
                             pattern = '*.docx',
                             full.names = TRUE)
    file_names <- list.files(raw_folder,
                             pattern = '*.docx',
                             full.names = FALSE)
    
    # Check format and clean folders
    if (format == 'Zoom') {
      ## Clean Zoom Folder -----
      walk2(file_paths, file_names, \(file_path, file_name) {
        clean_zoom_transcript(
          raw_docx_file = file_path,
          clean_docx_file = paste0(clean_folder, 'clean_', file_name),
          min_str_length = min_str_length,
          font_family = font_family,
          font_size = font_size,
          line_spacing = line_spacing
        )
      })
    
    } else if (format == 'Teams') {
      ## Clean Teams Folder -----
      walk2(file_paths, file_names, \(file_path, file_name) {
        clean_teams_transcript(
          raw_docx_file = file_path,
          clean_docx_file = paste0(clean_folder, 'clean_', file_name),
          min_str_length = min_str_length,
          font_family = font_family,
          font_size = font_size,
          line_spacing = line_spacing
        )
      })
    }
  } 
  
  # By File -----------------------------------------------------------------
  if (!is.null(raw_file)) {
    
    if (format == 'Zoom') {
      ## Zoom File -----
      clean_zoom_transcript(
        raw_docx_file = raw_file,
        clean_docx_file = clean_path,
        min_str_length = min_str_length,
        font_family = font_family,
        font_size = font_size,
        line_spacing = line_spacing
      )
    } else if (format == 'Teams') {
        ## Teams File -----
        clean_teams_transcript(
          raw_docx_file = raw_file,
          clean_docx_file = clean_path,
          min_str_length = min_str_length,
          font_family = font_family,
          font_size = font_size,
          line_spacing = line_spacing
        )
      }
    }
} 

