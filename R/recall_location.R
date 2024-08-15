
create_search_param <- function(input, param_name) {
  if (!is.null(input)) {
    input <- strsplit(input, ", ")[[1]]
    input <- gsub(" ", "+", input)
    param_search <- NULL

    if (length(input) == 1) {
      param_search <- paste0(param_name, ":(%22", input, "%22)")
    } else {
      param_search <- paste0("%22", input, "%22", collapse = "+OR+")
      param_search <- paste0(param_name, ":(", param_search, ")")
    }
  } else {
    param_search <- NULL
  }
  return(param_search)
}

#' This function scrapes the openFDA API for food product recall enforcement reports based on user inputs of location
#'
#' @param api_key Your free api key from openFDA API website
#' @param limit The number of rows to return for that query
#' @param city City where food company is located
#' @param country The country where the food was produced
#' @param distribution_pattern Locations where food was distributed to
#' @param recalling_firm The company recalling the product
#' @param search_mode This gives the user flexibility to search for exact matches of inputs or any combination of inputs
#' @param state The U.S. state in which the recalling firm is located
#' @param status The status of the recall
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr  %>%
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate ymd
#' @importFrom tibble tibble
#' @return A data frame with the returned results of the users query to the API
#' @examples
#' \dontrun{
#' recall_location(api_key = api_key, city = "Ames", state = "Iowa")
#' recall_location(api_key = api_key, recalling_firm = "Target")
#' recall_location(api_key = api_key, distribution_pattern = "Colorado", status = "Ongoing")
#' recall_location(api_key = api_key, city = "Iowa City, Ames, Des Moines", state = "Iowa")
#' }
#' @export
recall_location <- function(api_key,
                            limit = NULL,
                            city = NULL,
                            country = NULL,
                            distribution_pattern = NULL,
                            recalling_firm = NULL,
                            search_mode = NULL,
                            state = NULL,
                            status = NULL) {

  address_1 <- NULL
  address_2 <- NULL
  center_classification_date <- NULL
  classification <- NULL
  code_info <- NULL
  event_id <- NULL
  initial_firm_notification <- NULL
  postal_code <- NULL
  product_description <- NULL
  product_quantity <- NULL
  recall_initiation_date <- NULL
  recall_number <- NULL
  report_date <- NULL
  termination_date <- NULL
  voluntary_mandated <- NULL

  if (!is.null(state)) {
    state_vector <- unlist(strsplit(state, ", "))

    state <- sapply(state_vector, function(x) {
      if (x %in% datasets::state.abb) {
        return(x)
      } else {
        x <- tolower(x)
        state_name <- tolower(datasets::state.name)
        if (x %in% state_name) {
          index <- match(x, state_name)
          return(datasets::state.abb[index])
        }
      }
    })
    state <- paste(state, collapse = ", ")
  }

  state_search <- create_search_param(state, "state")
  city_search <- create_search_param(city, "city")
  country_search <- create_search_param(country, "country")
  distribution_pattern_search <- create_search_param(distribution_pattern, "distribution_pattern")
  recalling_firm_search <- create_search_param(recalling_firm, "recalling_firm")
  status_search <- create_search_param(status, "status")

  if (!is.null(limit)) {
    if (limit > 1000){
      warning("The openFDA API is limited to 1000 results per API call. Defaulting to 1000 results. Try a more specific search to return a dataset that contains all of the desired results.")
      limit <- paste0("&limit=", 1000)
    } else {
      limit <- paste0("&limit=", limit)
      }
  } else {
    limit <- paste0("&limit=", 1000)
  }

  if (!is.null(search_mode)) {
    search_mode <- toupper(search_mode)
    while (search_mode != "AND" && search_mode != "OR") {
      search_mode <- readline("Invalid input. Enter either 'AND' or 'OR':")
    }
    search_mode <- paste0("+", search_mode, "+")
  } else {
    search_mode <- "+AND+"
  }

  base_url <- paste0("https://api.fda.gov/food/enforcement.json?api_key=", api_key, "&search=")

  search_parameters <- list(city_search, country_search, distribution_pattern_search, recalling_firm_search, state_search, status_search)

  search_parameters <- search_parameters[!sapply(search_parameters, is.null)]

  search_string <- paste0(search_parameters, collapse = search_mode)

  url <- paste0(base_url, search_string, limit)

  fda_data <- httr::GET(url = url)

  data <- jsonlite::fromJSON(httr::content(fda_data, "text"))

  if ("error" %in% names(data)) {
    if (data$error$message == "No matches found!") {
      print("No matches were found for the given input.")
    }
  } else if (fda_data$status_code !=200) {
    stop("The API call failed. Make sure the inputs were entered correctly. Retry the request again.")
  }

  if (data$meta$results$total >1000) {
    warning("The total number of results is greater than the number of returned results; therefore, the returned results may be an incomplete representation of the data. Try a more specific search criteria to return a more complete dataset containing all the desired results.")
  }

  new_stuff <- tibble::tibble(recall_number = data$results$recall_number,
                              recalling_firm = data$results$recalling_firm,
                              recall_initiation_date = data$results$recall_initiation_date,
                              center_classification_date = data$results$center_classification_date,
                              report_date = data$results$report_date,
                              termination_date = data$results$termination_date,
                              voluntary_mandated = data$results$voluntary_mandated,
                              classification = data$results$classification,
                              initial_firm_notification = data$results$initial_firm_notification,
                              status = data$results$status,
                              country = data$results$country,
                              state = data$results$state,
                              city = data$results$city,
                              address_1 = data$results$address_1,
                              address_2 = data$results$address_2,
                              postal_code = data$results$postal_code,
                              reason_for_recall = data$results$reason_for_recall,
                              product_description = data$results$product_description,
                              product_quantity = data$results$product_quantity,
                              code_info = data$results$code_info,
                              distribution_pattern = data$results$distribution_pattern,
                              event_id = data$results$event_id)

  if (!("termination_date" %in% names(new_stuff))) {
    new_stuff <- new_stuff %>%
      dplyr::mutate(termination_date = NA) %>%
      dplyr::relocate(termination_date, .after = report_date)
  }

  new_stuff <- new_stuff %>%
    dplyr::mutate_all(~replace(., . == "", NA)) %>%
    dplyr::mutate(
      recall_initiation_date = as.character(lubridate::ymd(recall_initiation_date)),
      report_date = as.character(lubridate::ymd(report_date)),
      center_classification_date = as.character(lubridate::ymd(center_classification_date)),
      termination_date = as.character(lubridate::ymd(termination_date))) %>%
    dplyr::arrange((city)) %>%
    dplyr::arrange(dplyr::desc(report_date))



  return(new_stuff)
}
