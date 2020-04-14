#' @title "Agify" function
#' @description Connects directly to the \href{https://agify.io/}{agify.io API}
#'   and parses the response to return the predicted age of a name.
#' @param name Name/s to estimate the age. Can be a single \code{character}
#'   string or a \code{character} vector.
#' @param country_id Responses will in a lot of cases be more accurate if the
#'   data is narrowed to a specific country. This optional parameter allows to
#'   specify a specific country. The API follows the common
#'   \href{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2}
#'   country code convention. To see a list of the supported countries use the
#'   \code{\link{supported_countries}} function or visit the following
#'   \href{https://agify.io/our-data}{link}.
#' @param simplify Logical parameter, which defines if the result should be
#'   returned as a \code{character} vector or a \code{data.frame} with
#'   additional information. By default set to \code{TRUE}, which returns a
#'   vector.
#' @param apikey Optional API key parameter. The API is free for up to 1000
#'   names/day. No sign up or API key needed. Yet, if more requests would be
#'   needed, visit the \href{https://store.agify.io/}{agify.io store} and the
#'   obtained API key can be passed through this parameter. The API can also be
#'   saved one time through the \code{\link{save_key}} function, so it is not
#'   necessary to call again.
#' @param meta Logical parameter to define if API related information should be
#'   returned. By default set to \code{FALSE}. Returns information about:
#'   \itemize{ \item The amount of names available in the current time window
#'   \item The number of names left in the current time window \item Seconds
#'   remaining until a new time window opens }
#' @return The estimated age in a single \code{character} vector form or a
#'   \code{data.frame} with additional information.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @details The function automatically handles pagination (max. 10 names per API
#'   request), missing values & duplicated values. If a name is duplicated for
#'   the same \code{country_id} (if given), it will handle the request as a a
#'   single name to save requests, returning the same value for the duplicated
#'   names. To see more details about the API documentation, visit the
#'   \href{https://agify.io/}{agify.io website}.
#' @seealso \code{\link{genderize}}, \code{\link{nationalize}}
#' @section Warning: Please be aware about local privacy protection regulations
#'   such as
#'   \href{https://en.wikipedia.org/wiki/General_Data_Protection_Regulation}{GDPR}
#'   when dealing with personal data.
#' @examples
#' agify(name=c("Ben", "Maria"))
#' @export

agify <- function(name,
                  country_id = NULL,
                  simplify = TRUE,
                  apikey = get_key("agify"),
                  meta = FALSE) {
  y <- country_distributor(x = name,
                           type = "age",
                           country_id = country_id,
                           sliced = TRUE,
                           apikey = apikey)
  if (simplify) {
    y <- y$age
  } else {
    if (!meta) {
      cols <- colnames(y)
      y <- y[!grepl("^api_", cols)]
    }
  }
  return(y)
}

#' @title "Genderize" function
#' @description Connects directly to the
#'   \href{https://genderize.io/}{genderize.io API} to return the predicted
#'   gender of a name.
#' @param name Name/s to estimate the gender. Can be a single \code{character}
#'   string or a \code{character} vector.
#' @param country_id Responses will in a lot of cases be more accurate if the
#'   data is narrowed to a specific country. This optional parameter allows to
#'   specify a specific country. The API follows the common
#'   \href{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2}
#'   country code convention. To see a list of the supported countries use the
#'   \code{\link{supported_countries}} function or visit the following
#'   \href{https://genderize.io/our-data}{link}.
#' @param simplify Defines if the result should be returned as a single vector
#'   or a \code{data.frame} with additional information. By default set to
#'   \code{TRUE}, which returns a vector. If set to \code{TRUE}, it will include
#'   additional information about: \itemize{ \item The probability indicates the
#'   certainty of the assigned gender. Basically the ratio of male to females.
#'   \item The count represents the number of data rows examined in order to
#'   calculate the response. }
#' @param apikey Optional API key parameter. The API is free for up to 1000
#'   names/day. No sign up or API key needed. Yet, if more requests would be
#'   needed, visit the \href{https://store.genderize.io/}{genderize.io store}
#'   and the obtained API key can be passed through this parameter. The API can
#'   also be saved one time through the \code{\link{save_key}} function, so it
#'   is not necessary to call again.
#' @param meta Logical parameter to define if API related information should be
#'   returned. By default set to \code{FALSE}. Returns information about:
#'   \itemize{ \item The amount of names available in the current time window
#'   \item The number of names left in the current time window \item Seconds
#'   remaining until a new time window opens }
#' @return The estimated age in a single \code{character} vector form or a
#'   \code{data.frame} with additional information.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @details The function automatically handles pagination (max. 10 names per API
#'   request), missing values & duplicated values. If a name is duplicated for
#'   the same \code{country_id} (if given), it will handle the request as a a
#'   single name to save requests, returning the same value for the duplicated
#'   names. To see more details about the API documentation, visit the
#'   \href{https://genderize.io/}{genderize.io website}.
#' @seealso \code{\link{nationalize}}, \code{\link{agify}}
#' @section Warning: Please be aware about local privacy protection regulations
#'   such as
#'   \href{https://en.wikipedia.org/wiki/General_Data_Protection_Regulation}{GDPR}
#'    when dealing with personal data.
#' @examples
#' genderize(name=c("Ben", "Maria"))
#' @export

genderize <- function(name,
                      country_id = NULL,
                      simplify = TRUE,
                      apikey = get_key("genderize"),
                      meta = FALSE) {
  y <- country_distributor(x = name,
                           type = "gender",
                           country_id = country_id,
                           sliced = TRUE,
                           apikey = apikey)
  if (simplify) {
    y <- y$gender
  } else {
    if (!meta) {
      cols <- colnames(y)
      y <- y[!grepl("^api_", cols)]
    }
  }
  return(y)
}


#' @title "Nationalize" function
#' @description Returns the estimated nationality from a given name.
#' @param name Name/s to estimate the nationality. Can be a single
#'   \code{character} string or a \code{character} vector.
#' @param simplify Defines if the result should be returned as a single vector
#'   or a \code{data.frame} with additional information. By default set to
#'   \code{TRUE}, which returns a vector.
#' @param sliced Names can have multiple estimated nationalities ranked by
#'   probabilty. This logical parameter allows to "slice"/keep only the
#'   parameter with the highest probability to keep a single estimate for each
#'   name.
#' @param apikey Optional API key parameter. The API is free for up to 1000
#'   names/day. No sign up or API key needed. Yet, if more requests would be
#'   needed, visit the \href{https://store.nationalize.io/}{nationalize.io
#'   store} and the obtained API key can be passed through this parameter. The
#'   API can also be saved one time through the \code{\link{save_key}}
#'   function, so it is not necessary to call again.
#' @param meta Logical parameter to define if API related information should be
#'   returned. By default set to \code{FALSE}. Returns information about:
#'   \itemize{ \item The amount of names available in the current time window
#'   \item The number of names left in the current time window \item Seconds
#'   remaining until a new time window opens }
#' @return Returns the estimated nationality as the common
#'   \href{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2}
#'   country code. The result can be in a single \code{character} vector form or
#'   a \code{data.frame} with additional information.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @details The function automatically handles pagination (max. 10 names per API
#'   request), missing values & duplicated values. If a name is duplicated for
#'   the same \code{country_id} (if given), it will handle the request as a a
#'   single name to save requests, returning the same value for the duplicated
#'   names. To see more details about the API documentation, visit the
#'   \href{https://nationalize.io/}{nationalize.io website}.
#' @seealso \code{\link{genderize}}, \code{\link{agify}}
#' @section Warning: Please be aware about local privacy protection regulations
#'   such as
#'   \href{https://en.wikipedia.org/wiki/General_Data_Protection_Regulation}{GDPR}
#'    when dealing with personal data.
#' @examples
#' nationalize(name=c("Ben", "Maria"))
#' @export

nationalize <- function(name,
                        simplify = TRUE,
                        sliced = TRUE,
                        apikey = get_key("genderize"),
                        meta = FALSE) {
  y <- country_distributor(x = name,
                           type = "nationality",
                           country_id = NULL,
                           sliced = sliced,
                           apikey = get_key("nationalize"))
  if (simplify) {
    y <- y$country_id
  } else {
    if (!meta) {
      cols <- colnames(y)
      y <- y[!grepl("^api_", cols)]
    }
  }
  return(y)
}


#' @title Retrieve the supported countries for each API
#' @description Scrapes the API websites to retrieve an updated list of
#'   supported countries for each API.
#' @param type Obligatory parameter to define the API from which to obtain the
#'   supported countries. Must be one of the following \code{character} strings:
#'   \itemize{ \item \code{genderize} - Available countries for the Genderize.io
#'   API. \item \code{agify} - Available countries for the Agify.io API. \item
#'   \code{nationalize} - Available countries for the Nationalize.io API. }
#' @return Returns a \code{data.frame} with the supported
#'   \href{http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2}{ISO 3166-1 alpha-2}
#'   country code, country name the number of items (names) for each country.
#' @section Warning: Please be conscious that this function directly scrapes the
#'   website, do not overuse it as it might overwhelm the server.
#' @examples
#' \dontrun{
#' supported_countries(type="genderize")
#' }
#' @export

supported_countries <- function(type) {
  url <- switch(type,
                "genderize" = "https://genderize.io/js/Genderize.js",
                "agify" = "https://agify.io/js/Agify.js",
                "nationalize" = "https://nationalize.io/js/Nationalize.js")
  char <- readLines(url, warn = FALSE)
  char <- strsplit(char, split = ";")[[1]]
  char <- char[grepl("ISO Code", char)]
  char <- gsub("^.*f=|(}])},function.*", "\\1", char)
  char <- gsub("\\{country_id\\:", '{"country_id":', char)
  char <- gsub("\\,total\\:", ',"total":', char)
  char <- gsub("\\,name\\:", ',"name":', char)
  df <- jsonlite::fromJSON(char)
  return(df)
}


#' @title Saves the key for future functions
#' @description Saves the key from the in the users environment. It has the
#'   advantage that it is not necessary to explicitly publish the key in the
#'   users code. Just do it one time and you're set. To update the key kust save
#'   again and it will overwrite the old key. To explictly printing the key, use
#'   the \code{\link{get_key}} function. To remove the key use the
#'   \code{\link{remove_key}} function.
#' @param key Key obtained from the specific website.Must be one of the
#'   following: \itemize{ \item \code{gender} -
#'   \href{https://store.agify.io/signup}{Genderize} \item \code{age} -
#'   \href{https://store.agify.io/signup}{Agify} \item \code{nationality} -
#'   \href{https://nationalize.io/signup}{Nationalize} }
#' @param type Must be one of the following: \itemize{ \item \code{genderize} -
#'   Genderize.io API key \item \code{agify} - Agify.io API key \item
#'   \code{nationalize} - Nationalize.io API key }
#' @return Does save the key in the environment.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @section Warning: Please be careful when dealing with API keys and other
#'   secrets & tokens - keep them private and do not publish them.
#' @examples
#' \dontrun{
#' save_key(key="__YOUR_API_KEY__", type="gender")
#' }
#' @export

save_key <- function(key, type) {
  env_name <- switch(type,
                     "genderize" = "GENDERIZE_KEY_PAT",
                     "agify" = "AGIFY_KEY_PAT",
                     "nationalize" = "NATIONALIZE_KEY_PAT")
  names(key) <- env_name
  Sys.setenv(key)
}

#' @title Get previously saved API keys
#' @description Function to get the previously saved API key. It has the
#'   advantage that it is not necessary to explicitly publish the key in the
#'   users code. Just do it one time and you're set. To save an API, use the
#'   \code{\link{save_key}} function. To remove a previously saved key, use
#'   the \code{\link{remove_key}} function.
#' @param type Obligatory parameter to define which key to retrieve: \itemize{
#'   \item \code{genderize} - Genderize.io API key \item \code{agify} - Agify.io
#'   API key \item \code{nationalize} - Nationalize.io API key }
#' @return Returns the saved API key in the environment. If no API key has been
#'   saved, returns \code{NULL} value.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @section Warning: Please be careful when dealing with API keys and other
#'   secrets & tokens - keep them private and do not publish them.
#' @examples
#' \dontrun{
#' get_key(type="genderize")
#' get_key(type="agify")
#' get_key(type="nationalize")
#' }
#' @export

get_key <- function(type) {
  env_name <- switch(type,
                     "genderize" = "GENDERIZE_KEY_PAT",
                     "agify" = "AGIFY_KEY_PAT",
                     "nationalize" = "NATIONALIZE_KEY_PAT")
  key <- Sys.getenv(env_name, NA)
  if (is.na(key)) {
    key <- NULL
  }
  return(key)
}

#' @title Removes saved key
#' @description Removes saved keys for the DemografixeR APIs (Genderize.io,
#'   Agify.io, Nationalize.io)
#' @param type Choose the type of key to remove from the environment variables:
#'   \itemize{ \item \code{genderize} - Genderize.io API key \item \code{agify}
#'   - Agify.io API key \item \code{nationalize} - Nationalize.io API key }
#' @param verbose Logical parameter to define if a verbose message should be
#'   printed.
#' @return Does not return any object.
#' @author Matthias Brenninkmeijer -
#'   \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @section Warning: Please be careful when dealing with API keys and other
#'   secrets & tokens - keep them private and do not publish them.
#' @examples
#' \dontrun{
#' remove_key(type="genderize")
#' remove_key(type="agify")
#' remove_key(type="nationalize")
#' }
#' @export

remove_key <- function(type, verbose = TRUE) {
  env_name <- switch(type,
                     "genderize" = "GENDERIZE_KEY_PAT",
                     "agify" = "AGIFY_KEY_PAT",
                     "nationalize" = "NATIONALIZE_KEY_PAT")
  Sys.unsetenv(env_name)
  if (verbose) {
    cat(
      sprintf(
        "<%s key saved at environment as %s has been removed>",
        type,
        env_name)
      )
    }
}

api_response <- function(status) {
  response <- switch(as.character(status),
                     "200" = "Success! Everything is OK",
                     "400" = "Something is wrong on your end",
                     "401" = "Invalid API key",
                     "402" = "Subscription is not active",
                     "429" = "Request limit reached",
                     "500" = "Something is wrong on our end",
                     sprintf("An error has ocurred - Error code %s", status)
  )
  return(response)
}

add_id <- function(x, n) {
  if (nrow(x) > 0) {
    x$id <- n
    return(x)
  }
}

null_to_na <- function(x) {
  if (is.null(x) || !length(x)) {
    x <- NA
  }
  return(x)
}

api_request <- function(x, type, country_id=NULL, sliced=TRUE, apikey=NULL) {
  #Define service to call
  url <- switch(type,
                "gender" = "https://api.genderize.io",
                "age" = "https://api.agify.io",
                "nationality" = "https://api.nationalize.io")
  #Remove country for nationality search, as no API parameter
  if (type == "nationality" && !is.null(country_id)) {
    warning(
      sprintf(
        "<country_id %s is ignored - invalid nationalize.io API parameter>",
        country_id),
      call. = FALSE)
    country_id <- NULL
  }
  #Ensure duplicated requests are not made & remove whitespace
  names_param <- stats::setNames(as.list(x), rep("name", length(x)))
  #Add optional parameters for country & apikey
  query <- c(names_param, list(country_id = country_id), list(apikey = apikey))
  #Bring everything together defining GET url
  request_url <- httr::modify_url(url = url, query = query)
  #GET request
  request  <-  httr::GET(request_url,
                         encode = "json",
                         httr::user_agent("github.com/matbmeijer"))
  #Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  #Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text"),
                                simplifyDataFrame = TRUE)

  #Stop if errors
  if (httr::http_error(request)) {
    if (any(names(content) %in% "error")) {
      stop(sprintf("Error Code %s - %s",
                   request$status_code,
                   content$error),
           call. = FALSE)
    } else {
      stop(sprintf("Error Code %s - %s",
                   request$status_code,
                   api_response(request$status_code)),
           call. = FALSE)
    }
  }
  content$api_rate_limit <- as.integer(request$headers[["x-rate-limit-limit"]])
  content$api_rate_remaining <- as.integer(request$headers[["x-rate-limit-remaining"]])
  content$api_rate_reset <- as.integer(request$headers[["x-rate-reset"]])
  content$api_request_timestamp <- request$date

  #More than one row to NA
  content[unlist(lapply(lapply(content, unlist, recursive = T), is.null))] <- NA

  if (!is.data.frame(content)) {
    content <- lapply(content, null_to_na)
    content <- data.frame(content, stringsAsFactors = FALSE)
  }

  #Nationality multiline
  is_list <- unlist(lapply(content, is.list))

  if (any(is_list)) {
    base_content <- content[!is_list]
    base_content$id <- seq_along(content[[1]])
    merge_list <- mapply(add_id,
                         content[, is_list],
                         base_content$id,
                         SIMPLIFY = FALSE)
    #Slice first row only if not expanded
    if (sliced) {
      merge_list <- lapply(merge_list, utils::head, 1)
    }
    merge_content <- do.call(rbind, merge_list[lengths(merge_list) > 0])
    content <- merge(base_content, merge_content, by = "id", all.x = TRUE)
    content$id <- NULL
    } else if (sliced && length(x) == 1 && nrow(content) > 1) {
      content <- utils::head(content, 1)
  }
  cols <- colnames(content)
  colnames(content) <- gsub(".*\\.", "", cols)
  return(content)
}

sequencer <- function(input, type, sliced = TRUE, apikey = NULL) {
  x <- input$x
  country_id <- input$country_id[1]
  if (!is.null(country_id) && country_id == "NO COUNTRY") {
    country_id <- NULL
  }
  #Ensure duplicated requests are not made & remove whitespace
  y <- unique(trimws(as.character(x)))
  #Remove empty characters and NA
  y <- y[!(y %in% c(NaN, NA, ""))]
  y_list <- split(y, paste0("group_", ceiling(seq_along(y) / 10)))
  content_list <- lapply(y_list,
                         api_request,
                         type = type,
                         country_id = country_id,
                         sliced = sliced,
                         apikey = apikey)
  content <- do.call(rbind, content_list)
  df <- data.frame(id = input$id,
                   name = x,
                   type = type,
                   stringsAsFactors = FALSE)
  df <- merge.data.frame(df, content, by = "name", all.x = TRUE)
  return(df)
}

country_distributor <- function(x,
                                type,
                                country_id = NULL,
                                sliced = TRUE,
                                apikey = NULL) {
  if (!is.null(country_id)) {
    if (!(length(country_id) %in% c(1, length(x)))) {
      stop("<country_id must be a single string or a multistring with the same length as x>",
           call. = FALSE)
    }
    df <- data.frame(id = seq_along(x),
                     x = x,
                     country_id = country_id,
                     stringsAsFactors = FALSE)
    df[is.na(df)] <- "NO COUNTRY"
    pass_list <- split(df, df$country_id)
  } else {
    pass_list <- list(data.frame(id = seq_along(x),
                                 x = x,
                                 stringsAsFactors = FALSE))
  }
  res_list <- lapply(pass_list,
                     sequencer,
                     type = type,
                     sliced = sliced,
                     apikey = apikey)
  res <- rbind_fill(res_list)
  rownames(res) <- seq_along(res[[1]])
  res <- res[order(res$id), ]
  res$id <- NULL
  return(res)
}

rbind_fill <- function(l) {
  r <- unique(unlist(lapply(l, nrow)))
  l <- l[r > 0]
  cols <- unique(unlist(lapply(l, names)))
  res <- do.call(rbind, lapply(l, fill_df_nas, cols))
  return(res)
}

fill_df_nas <- function(x, cols) {
  x_cols <- names(x)
  miss_cols <- setdiff(cols, x_cols)
  x[, miss_cols] <- NA
  x <- x[, cols]
  return(x)
}
