clean_language_data <- function(language){
  language %>%
    janitor::clean_names() %>%
    rename(
      subject_id = id
      ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("lang_score"),
      names_to = "time",
      names_prefix = "lang_score",
      values_to = "score"
    ) %>%
    # Move time so that 0 is first time measured
    mutate(
      time = as.numeric(time)
      ) %>%
    mutate(
      time = time - min(time)
    )
}

# Get a dataset from the multilevel modeling in R textbook

get_language_data <- function(data_directory){
  # Check if directory exists, and if it doesn't, then creates it
  if (!file.exists(data_directory)){
    dir.create(data_directory)
  }

  # Looks in directory for data file.  If it isn't present, downloads it
  filenames <- list.files(data_directory)

  if ("language.rds" %in% filenames){

    language <- readr::read_rds(
      file = file.path(data_directory,"language.rds")
    )
  } else {
    language <- readr::read_csv(
      file = "http://www.mlminr.com/data-sets/Language.csv?attredirects=0",
      show_col_types = FALSE
    ) %>%
      clean_language_data()

    readr::write_rds(
      x = language,
      file = file.path(data_directory,"language.rds")
      )
  }

  return(language)
}

