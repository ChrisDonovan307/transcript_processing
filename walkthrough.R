#' Walkthrough


# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  officer,
  purrr,
  stringr
)

# Load functions
paths <- list.files(path = 'functions', full.names = TRUE)
for (i in paths) source(i)



# Clean Transcripts -------------------------------------------------------


#' The first argument, format, lets you choose between "Zoom" and "Teams".
#' There is a raw_file argument and a raw_folder argument in the function.
#' You must choose only one of these arguments. To clean a single file, 
#' specify both the full path to the raw file and the full path to where you 
#' want the clean file to go.

#' For cleaning a folder, note that every .docx file in the folder must be that
#' same format. Also, in the clean_path argument, just specify the path to a 
#' the folder where you want the files. The files will automatically be renamed
#' by adding "clean_" to the beginning of the filename. I have two raw file 
#' folders set up now, one for zoom files and one for teams files. I am saving
#' files to the test_docx folder.

#' Single teams file. These can now handle having multiple colons. I added a 
#' few colons and a timestamp in this script, and you can see it has no problem.
clean_transcript(
  format = 'Teams',
  raw_file = 'raw_teams_docx/file3.docx',
  raw_folder = NULL,
  clean_path = 'test_docx/clean_transcript.docx'
)

# Folder of teams files
clean_transcript(
  format = 'Teams',
  raw_file = NULL,
  raw_folder = 'raw_teams_docx',
  clean_path = 'test_docx'
)

# Single Zoom file
clean_transcript(
  format = 'Zoom',
  raw_file = 'raw_zoom_docx/raw_transcript.docx',
  raw_folder = NULL,
  clean_path = 'test_docx/clean_transcript.docx'
)

#' Single Zoom file with a warning because of > 1 colon in a paragraph. Note 
#' that I have not found an easy way to fix the multiple colon problem for 
#' zoom files. So it throws a warning and also prints out the first few words
#' of the lines that have the multiple colons so you can check it out.
clean_transcript(
  format = 'Zoom',
  raw_file = 'raw_zoom_docx/raw_transcript_2.docx',
  raw_folder = NULL,
  clean_path = 'test_docx/clean_transcript.docx'
)

#' Multiple zoom files. If any individual file fails, it will throw a warning
#' but continue on and process the other files.
clean_transcript(
  format = 'Zoom',
  raw_file = NULL,
  raw_folder = 'raw_zoom_docx',
  clean_path = 'test_docx'
)
#' Here, there is a colon issue, and it throws a warning but still processes
#' all files. 

# Note that there are also more arguments for formatting:
clean_transcript(
  format = 'Teams',
  raw_file = NULL,
  raw_folder = 'raw_teams_docx',
  clean_path = 'test_docx',
  min_str_length = 6,
  font_family = 'Times',
  font_size = 11,
  line_spacing = 2
)
