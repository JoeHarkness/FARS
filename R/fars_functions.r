#'  Read FARS data
#'
#'  Creates a dataframe from csv formatted file of data from the US National Highway Traffic Safety Administration's
#'  Fatality Analysis #'  Reporting System (FARS), which is a nationwide census providing the American public yearly
#'  data regarding fatal injuries suffered in motor vehicle traffic crashes.  If full file path is not provided, this
#'  function will look in the current working directory for the file
#'
#' @param filename Name of csv file with FARS data
#'
#' @return This function returns a dataframe with FARS data if the file exists
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @examples
#' \dontrun{fars_data <- fars_read(accident_2013.csv.bz2)}
#'
#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a FARS csv input data filename given a year
#'
#' @param year Year of FARS input data to read
#' @return A FARS data filename for a specified year
#' @examples
#' \dontrun{fars_fileame <- make_filename(2013)}
#'
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Creates a dataframe from FARS data from multiple years as specified by the /code{years} parameter
#'
#' This function also requires the %>% function from the package dplyr, so you that package must be loaded
#' for the function to work.  (It seems like there should be a tag to indicate things like this, ditto with map below)
#'
#' @param years Vector of Years of FARS input data to read
#' @return A FARS dataframe for the specified year
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @examples
#' \dontrun{fars_data <- fars_read_years(c(2013, 2014, 2015)}
#'
#' @export

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

#' Creates a dataframe summarizing the number of observations by in FARS data from multiple years as specified by the
#' /code{years} parameter
#'
#' @param years Vector of Years of FARS input data to read
#' @return A dataframe summarizing the number of FARS data observations by year for the specified years
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summary <- fars_summarize_years(c(2013, 2014, 2015)}
#'
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Creates a map of accident locations for one as specified by the /code{year} parameter for the
#' state specified by FIPS code using the /code{state.num} parameter.
#'
#' This function requires the package map to be loaded or if map is installed to run the assignment
#' stateMapEnv <- "R_MAP_DATA_DIR".  I don't know how to properly include this warning in the documentation.
#'
#' @param year Year of FARS input data to use
#' @param state.num FIPS code of the state to create a map for
#' @return A map of accident locations for state with FIPS code /code{state.num}  in year /code{year}.
#'
#' @importFrom dplyr filter
#' @importFrom map map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(24,2013)}
#'
#' @export

library(maps)
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

