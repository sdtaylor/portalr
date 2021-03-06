#' @importFrom magrittr "%>%"

#' @name get_stake_data
#'
#' @title Return cleaned Portal rodent individual data
#'
#' @description This function cleans and subsets the data based on a number
#'   of arguments. It returns stake number and individual level data.
#'
#' @param path path to location of downloaded Portal data; or "repo" to
#'   retrieve data from github repo
#' @param type specify subset of species; either all "Rodents" or only
#'   "Granivores"
#' @param length specify subset of plots; use "All" plots or only "Longterm"
#'   plots (plots that have had same treatment for entire time series)
#' @param unknowns either removes all individuals not identified to species
#'   (unknowns = FALSE) or sums them in an additional column (unknowns = TRUE)
#' @param incomplete either removes all data from incomplete trapping sessions
#'   (incomplete = FALSE) or includes them (incomplete = TRUE)
#' @param time specify the format of the time index in the output, either
#'   "period" (sequential Portal surveys), "newmoon" (lunar cycle numbering),
#'   "date" (calendar date)
#' @param fillweight specify whether to fill in unknown weights with other
#'   records from that individual or species, where possible
#'
#' @return a data.frame
#'
#' @export
#'
get_stake_data <- function(path = '~', type = "Rodents",
                            length = "all", unknowns = FALSE, incomplete = FALSE,
                            time = "period", fillweight = FALSE) {

  #### Get Data ----
  data_tables <- loadData(path)

  #### Do initial cleaning ----
  rodents <- clean_rodent_data(data_tables, fillweight, type,
                               unknowns, incomplete, length)

  #### Filter by length and add treatment types

  trapping <- filter_plots(data_tables$trapping, length)
  rodents <- join_trapping_to_rodents(rodents, trapping, incomplete) %>%
    join_plots_to_rodents(data_tables$plots_table) %>%
    dplyr::select(period, month, day = day.x, year, treatment, plot, stake, species, sex, hfl, wgt, tag, ltag)

  #### use new moon number as time index if time == "newmoon" ----
  rodents = add_time(rodents, data_tables$newmoons_table, time)

  return(rodents)

}
