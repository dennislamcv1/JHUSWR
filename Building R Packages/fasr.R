#' Add together two numbers
#'
#' @param filename Name of a csv file.
#'
#' @returns A tibble.
#' @examples fas_read('fasdata.csv')
#' @details The function may return errors when the filename cannot be found
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Makde filenames in a format of accident_YY.csv.bz2
#' @param year Year number
#'
#' @returns A character "accident_year.csv.bz2" where year is the input
#' @examples make_filename(year)
#' @details The function may return errors when year cannot be converted to an integer
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read in data and make sure years in the data are consistent with their fileanmes
#' @param years A string of year numbers
#' @import dplyr
#' @returns A list of dataframes. Each data frame contains two columns, month and year
#' @details The function may return errors when any of the years cannot be found in the filename
#' @examples
#' library(dplyr)
#' make_filename(2013:2015)

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize number of months appearing in the fars data by year
#' @param year A string of year numbers
#' @import dplyr
#' @import tidyr
#' @returns A dataframe summarizing number of months appearing in the fars data by year
#' @examples
#' library(dplyr)
#' library(tidyr)
#' fars_summarize_years(2013:2014)

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot the map of incidences occuring in the state and the year set by the use
#' @param stat.num state number
#' @param year Year number
#' @import dplyr
#' @import tidyr
#' @import maps
#' @returns A dataframe summarizing number of months appearing in the fars data by year
#' @details The function may return errors when any of the years cannot be found in the filename.
#' Or the stat.num is invalid.
#' @examples
#' library(dplyr)
#' library(maps)
#' fars_map_state(1, 2013)

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
