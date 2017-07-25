#' Search dataset in KN
#'
#' @param q Search query
#' @param offset Search offset
#' @param limit Number of results to return
#' @return Data frame result as <pid, title> from KN
#' @export
#' @examples
#' kn_data_search('water', 0, 5)
kn_data_search <- function(q, offset, limit) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (identical(q, character(0)) | identical(q, ""))  {
    return(NULL)
  }

  result <- knsearch_list(q, 'data', offset, limit)

  print(result)
  if(length(result) <= 0) {
    return(NULL)
  }

  mylist <- list()
  for(i in 1:length(result)){
    curr <- result[[i]]
    pid <- curr$pid
    title <- curr$title
    row <- c(pid,title)
    print(row)
    mylist[[i]] <- row
  }
  df <- do.call("rbind",mylist) #combine all vectors into a matrix
  colnames(df) <- c("pid", "title")
  return(df)
}

#' Search geo-feature in KN
#'
#' @param q Search query
#' @param offset Search offset
#' @param limit Number of results to return
#' @return Data frame result as <pid, title> from KN
#' @export
#' @examples
#' kn_feat_search('namoi', 0, 5)
kn_feat_search <- function(q, offset, limit) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (identical(q, character(0)) | identical(q, ""))  {
    return(NULL)
  }


  result <- knsearch_list(q, 'feature', offset, limit)

  print(result)
  if(length(result) <= 0) {
    return(NULL)
  }

  mylist <- list()

  for(i in 1:length(result)){
    curr <- result[[i]]
    pid <- curr$pid
    name <- curr$name
    row <- c(pid,name)
    print(row)
    mylist[[i]] <- row
  }
  df <- do.call("rbind",mylist) #combine all vectors into a matrix
  colnames(df) <- c("pid", "name")
  return(df)
}




#' Get GeoJSON from KN
#'
#' @param pid KN Spatial Identifier
#' @param asSP Return as spatial object (TRUE, default) or plain geojson (FALSE)
#' @return Geojson object from KN
#' @export
#' @examples
#' kn_get_geojson('http://oznome.csiro.au/id/geo/geofabric/9400238')
kn_get_geojson <- function(pid, asSP=TRUE) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (identical(pid, character(0)) | identical(q, ""))  {
    return(NULL)
  }

  get_url <- "http://kn.csiro.au/api/feature"
  resp <- GET(get_url,
              query = list(id = pid)
  )
  cont <- content(resp, as = "parsed", type = "application/json")

  print("Received geojson")

  if(!is.null(cont[["status"]])){
    if(identical(cont[["status"]], "fail")){
      return(NULL)
    }
  }
  if(asSP) {
    g <- geojsonio::as.json(cont)
    spdf <- geojsonio::geojson_sp(g)
    return(spdf)
  }
  else {
    return(cont)
  }

}




#' Search dataset in KN and return a list
#'
#' @param q Search query
#' @param type Type of thing to search for - either "data" or "feature"
#' @param offset Search offset
#' @param limit Number of results to return
#' @return Result list from KN with all fields in each item
#' @export
#' @examples
#' knsearch_list('water', 'data', 0, 5)
knsearch_list <- function(q, type, offset, limit) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("httr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (identical(q, character(0)) | identical(q, ""))  {
    return(NULL)
  }

  get_url <- paste("http://kn.csiro.au/api/search/", type, sep="")
  resp <- GET(get_url,
              query = list(q = q, start = offset, limit = limit)
  )
  cont <- content(resp, as = "parsed", type = "application/json")

  return(cont)
}
