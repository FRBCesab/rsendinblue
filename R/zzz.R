api_url <- function() "https://api.sendinblue.com/v3/"

get_token <- function(key = "SENDINBLUE") {
  
  sib_token <- Sys.getenv(key)
  
  if (sib_token == "") {
    stop("Missing SENDINBLUE API Token.\n",
         "Please make sure you:\n",
         " 1. have obtained you own token, and\n",
         " 2. have stored the value in the `.Renviron` file with the name ",
         "SENDINBLUE.")
  }
  
  return(sib_token)
}


get_contacts <- function(n_per_page = 100) {
  
  query <- paste0(api_url(), "contacts", "?limit=", n_per_page, "&offset=", 0)
  
  response <- httr::GET(url    = query, 
                        config = httr::add_headers(
                          `accept`  = 'application/json',
                          `api-key` = get_token()))
  
  httr::stop_for_status(response)
  
  response <- httr::content(response, as = "text")
  response <- jsonlite::fromJSON(response)
  
  n_queries <- ceiling(response$count / n_per_page)
  
  contacts      <- list()
  contacts[[1]] <- response$contacts
  
  for (i in 2:n_queries) {
    
    Sys.sleep(.1)
    
    query <- paste0(api_url(), "contacts", "?limit=", n_per_page, 
                    "&offset=", n_per_page * (i - 1) - 1)
    
    response <- httr::GET(url    = query, 
                          config = httr::add_headers(
                            `accept`  = 'application/json',
                            `api-key` = get_token()))
    
    httr::stop_for_status(response)
    
    response <- httr::content(response, as = "text")
    response <- jsonlite::fromJSON(response)
    
    contacts[[i]] <- response$contacts
  }
  
  return(contacts)
}


update_contacts <- function(data) {
  
  responses <- data.frame(
    id = data$id,
    email = NA, 
    organism = NA
  )
  
  for (i in 1:nrow(data)) {
    
    cat("Updating contact", i, "on", nrow(data), "...\r")
    
    query <- paste0(api_url(), "contacts/", data[i, "id"])
    
    response <- httr::PUT(url    = query,
                          config = httr::add_headers(
                            `Accept`  = "application/json",
                            `Content-Type` = "application/json",
                            `api-key` = get_token()),
                          body   = list("attributes" = 
                                          list(`EMAIL`     = data[i, "email"])),
                          encode = "json")
    
    responses[i, "email"] <- response$"status_code"
    
    response <- httr::PUT(url    = query,
                          config = httr::add_headers(
                            `Accept`  = "application/json",
                            `Content-Type` = "application/json",
                            `api-key` = get_token()),
                          body   = list("attributes" = 
                                          list(`ORGANISME` = data[i, "organism"])),
                          encode = "json")  
    
    responses[i, "organism"] <- response$"status_code"
    
    Sys.sleep(sample(x = seq(0, 1, length.out = 500), size = 1))
  }
  
  invisible(responses)
}



delete_contacts <- function(data) {
  
  responses <- data.frame(id = data$"id", response = NA)
  
  for (i in 1:nrow(data)) {
    
    cat("Deleting contact", i, "on", nrow(data), "...\r")
    
    query <- paste0(api_url(), "contacts/", data[i, "id"])
    
    response <- httr::DELETE(url    = query,
                          config = httr::add_headers(
                            `Accept`  = "application/json",
                            `Content-Type` = "application/json",
                            `api-key` = get_token()),
                          encode = "json")
    
    responses[i, "response"] <- response$"status_code"
    Sys.sleep(sample(x = seq(0, 1, length.out = 500), size = 1))
  }
  
  invisible(responses)
}
