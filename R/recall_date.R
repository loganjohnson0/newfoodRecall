
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
date_search_param <- function(input, param_name) {
  if (!is.null(input)) {
    if(!is.character(input)) {
      stop("Please enter the date as a character vector. Example: '01-01-2023' or 'January 1, 2023'")
    } else {
      input <- strsplit(input, " to ")[[1]]
      input_search <- NULL
      if (length(input) == 1) {
        input <- lubridate::parse_date_time(input, orders = c("ymd", "mdy", "dmy", "Y", "my"), quiet = TRUE)
        # warning(sprintf("Defaulting to a range of %s to %s.", input, lubridate::today()))
        today <- lubridate::today()
        input <- gsub("-", "", input)
        today <- gsub("-", "", today)
        input <- paste0(input, "+TO+", today)
        input_search <- paste0(param_name,":([", input, "])")
      }
      if (length(input) > 2) {
        stop("Please enter only two date options.")
      } else if (length(input) == 2) {
        input <- lubridate::parse_date_time(input, orders = c("ymd", "mdy", "dmy", "Y", "my"), quiet = TRUE)
        input <- gsub("-", "", input)
        input <- paste0(input, collapse = "+TO+")
        input_search <- paste0(param_name,":([", input, "])")
    }
    }
  } else {
    input_search <- NULL
  }
  return(input_search)
}
#' This function scrapes the openFDA API for food product recall enforcement reports based on user inputs of date and time
#'
#' @param api_key Your free api key from FDA website
#' @param limit The number of rows to return for that query
#' @param center_classification_date A way the FDA classifies a date
#' @param product_description Description of product
#' @param recall_initiation_date Date for which recall was initiated
#' @param recalling_firm The company recalling the product
#' @param report_date The date the FDA issued the enforcement report for the product recall
#' @param search_mode This gives the user flexibility to search for exact matches of inputs or any combination of inputs
#' @param status The status of the recall
#' @param termination_date The date the recall was terminated
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate ymd
#' @importFrom lubridate today
#' @importFrom tibble tibble
#' @return A data frame with the returned results of the users query to the API
#' @examples
#' \dontrun{
#' recall_date(api_key = api_key, termination_date = "2021 to 2022)
#' recall_date(api_key = api_key, report_date = "January 1, 2023, status = "Ongoing")
#' recall_date(api_key = api_key, product_description = "Milk", recall_initiation_date = "2022")
#' }
#' @export
recall_date <- function(api_key,
                        center_classification_date = NULL,
                        limit = NULL,
                        product_description = NULL,
                        recall_initiation_date = NULL,
                        recalling_firm = NULL,
                        report_date = NULL,
                        search_mode = NULL,
                        status = NULL,
                        termination_date = NULL) {

  address_1 <- NULL
  address_2 <- NULL
  city <- NULL
  classification <- NULL
  code_info <- NULL
  event_id <- NULL
  initial_firm_notification <- NULL
  postal_code <- NULL
  product_quantity <- NULL
  recall_number <- NULL
  voluntary_mandated <- NULL

  recall_initiation_date_search <- date_search_param(recall_initiation_date, "recall_initiation_date")
  center_classification_date_search <- date_search_param(center_classification_date, "center_classification_date")
  report_date_search <- date_search_param(report_date, "report_date")
  termination_date <- date_search_param(termination_date, "termination_date")
  product_description_search <- create_search_param(product_description, "product_description")
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

  search_parameters <- list(recall_initiation_date_search,
                            center_classification_date_search,
                            report_date_search,
                            recalling_firm_search,
                            termination_date,
                            status_search,
                            product_description_search)

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
