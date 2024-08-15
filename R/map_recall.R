#' Function for getting lat and long data from addresses
#'
#' @param data The dataframe used as input with addresses to map latitude and longitudinally
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr summarize
#' @importFrom leaflet leaflet
#' @importFrom leaflet addProviderTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet addLayersControl
#' @importFrom leaflet labelOptions
#' @importFrom tidygeocoder geocode
#' @importFrom htmltools HTML
#' @export
map_recall <- function(data) {

  address_1 <- NULL
  city <- NULL
  count <- NULL
  event_id <- NULL
  event_data <- NULL
  full_address <- NULL
  latitude <- NULL
  longitude <- NULL
  postal_code <- NULL
  product_data <- NULL
  state <- NULL

  event_data <- data %>%
    dplyr::distinct(event_id, address_1, .keep_all = TRUE) %>%
    dplyr::group_by(address_1) %>%
    dplyr::summarize(event_count = dplyr::n())

  product_data <- data %>%
    dplyr::group_by(address_1, city) %>%
    dplyr::summarize(product_count = dplyr::n())

  data <- data %>%
    dplyr::left_join(product_data, by = c("address_1", "city")) %>%
    dplyr::left_join(event_data, by = "address_1") %>%
    dplyr::mutate(full_address = paste(address_1, city, state, postal_code)) %>%
    tidygeocoder::geocode(full_address, method = 'arcgis', lat = latitude, long = longitude)

  data$product_radius <- ifelse(data$product_count < 5, 10, data$product_count + 10)
  data$event_radius <- ifelse(data$event_count < 5, 10, data$event_count + 10)

  data$label <- paste(
    "<p>","<b>", data$recalling_firm, "</b>","<br>",
    data$address_1,"<br>",
    ifelse(is.na(data$address_2), "", paste(data$address_2, "<br>")),
    data$city, data$state, data$postal_code, data$country,"<br>",
    "Number of Product Recalls:", data$event_count, "<br>",
    "Number of Individual Products Recalled:", data$product_count, "<p/>")

  plot <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Stamen.Toner") %>%
    leaflet::addCircleMarkers(
      data = data,
      lat = ~ latitude,
      lng = ~ longitude,
      radius = ~ data$event_radius,
      group = "Number of Recall Events",
      label =  lapply(data$label, htmltools::HTML),
      labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
      stroke = F,
      fillColor = "red") %>%
    leaflet::addCircleMarkers(
      data = data,
      lat = ~ latitude,
      lng = ~ longitude,
      radius = ~ data$product_radius,
      group = "Number of Products Recalled",
      label =  lapply(data$label, htmltools::HTML),
      labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
      stroke = F,
      fillColor = "red") %>%
    leaflet::addLayersControl(
      baseGroups = c("Number of Recall Events", "Number of Products Recalled"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  plot
}
