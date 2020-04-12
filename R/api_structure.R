#' @title Agify function
#' @description Returns the estimated age from a given name.
#' @param name Name/s to evaluate from. Can be a single string or a vector.
#' @param country_id Optional parameter to estimate for a specific country. ISO2 code countrycode.
#' @param simplify Defines if the result should be returned as a single vector or a \code{data.frame} with additional information.
#' By default set to \code{TRUE}, which returns a vector.
#' @param apikey API KEY from \href{https://agify.io/}{Agify.io}.
#' @param meta Logical parameter to define if API related information should be returned. By default set to \code{FALSE}.
#' @return The estimated nationality in a single character vector form or a \code{data.frame} with additional information (e.g. probability).
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @export

agify<-function(name,
                country_id=NULL,
                simplify=TRUE,
                apikey=NULL,
                meta=FALSE){
  y<-country_distributor(x=name,
                          type="age",
                          country_id=country_id,
                          sliced=TRUE,
                          apikey=apikey)
  if(simplify){
    y<-y$age
  }else{
    if(!meta){
      cols<-colnames(y)
      y<-y[!grepl("^api_", cols)]
    }
  }
  return(y)
}

#' @title Genderize function
#' @description Returns the estimated gender from a given name.
#' @param name Name/s to evaluate from. Can be a single string or a vector.
#' @param country_id Optional parameter to estimate for a specific country. ISO2 code countrycode.
#' @param simplify Defines if the result should be returned as a single vector or a \code{data.frame} with additional information.
#' By default set to \code{TRUE}, which returns a vector.
#' @param apikey API KEY from \href{https://genderize.io/}{Genderize.io}.
#' @param meta Logical parameter to define if API related information should be returned. By default set to \code{FALSE}.
#' @return The estimated nationality in a single character vector form or a \code{data.frame} with additional information (e.g. probability).
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @export

genderize<-function(name,
                    country_id=NULL,
                    simplify=TRUE,
                    apikey=NULL,
                    meta=FALSE){
  y<-country_distributor(x=name,
                         type="gender",
                         country_id=country_id,
                         sliced=TRUE,
                         apikey=apikey)
  if(simplify){
    y<-y$gender
  }else{
    if(!meta){
      cols<-colnames(y)
      y<-y[!grepl("^api_", cols)]
    }
  }
  return(y)
}


#' @title Nationalize function
#' @description Returns the estimated nationality from a given name.
#' @param name Name/s to evaluate from. Can be a single string or a vector.
#' @param simplify Defines if the result should be returned as a single vector or a \code{data.frame} with additional information.
#' By default set to \code{TRUE}, which returns a vector.
#' @param sliced Names can have multiple estimated countries ranked by probabilty. This logical parameter allows to "slice"/keep only
#' the parameter with the highest probability to keep a single estimate for each name.
#' @param apikey API KEY from \href{https://nationalize.io/}{Nationalize.io}.
#' @param meta Logical parameter to define if API related information should be returned. By default set to \code{FALSE}.
#' @return The estimated nationality in a single character vector form or a \code{data.frame} with additional information (e.g. probability).
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @export


nationalize<-function(name,
                      simplify=TRUE,
                      sliced=TRUE,
                      apikey=NULL,
                      meta=FALSE){
  y<-country_distributor(x=name,
                         type="nationality",
                         country_id=NULL,
                         sliced=sliced,
                         apikey=apikey)
  if(simplify){
    y<-y$country_id
  }else{
    if(!meta){
      cols<-colnames(y)
      y<-y[!grepl("^api_", cols)]
    }
  }
  return(y)
}


#' @title Saves the key for future functions
#' @description Saves the key from the in the users environment as .
#' It has the advantage that it is not necessary to explicitly publish the key in the users code.
#' Just do it one time and you're set. To update the key kust save again and it will overwrite the old key.
#' To remove the key use the \code{remove_key()} function.
#' @param key Key obtained from the specific website.Must be one of the following:
#' \itemize{
#' \item \code{gender} - \href{https://store.agify.io/signup}{Genderize}
#' \item \code{age} - \href{https://store.agify.io/signup}{Agify}
#' \item \code{nationality} - \href{https://nationalize.io/signup}{Nationalize}
#' }
#' @param type Must be one of the following:
#' \itemize{
#' \item \code{gender} - Genderize.io key
#' \item \code{age} - Agify.io key
#' \item \code{nationality} - Nationalize.io key
#' }
#' @return Does save the key in the environment.
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @examples
#' \dontrun{
#' save_key(key="__YOUR_API_KEY__", type="gender")
#' }
#' @export

save_key <- function(key, type){
  env_name<-switch(type,
              "gender"="GENDERIZE_KEY_PAT",
              "age" = "AGIFY_KEY_PAT",
              "nationality"="NATIONALIZE_KEY_PAT")
  names(key)<-env_name
  Sys.setenv(key)
}

#' @title Removes saved key
#' @description Removes saved keys for the DemografixeR APIs (Genderize.io, Agify.io, Nationalize.io)
#' @param type Choose the type of key to remove from the environment variables:
#' \itemize{
#' \item \code{gender} - Genderize.io key
#' \item \code{age} - Agify.io key
#' \item \code{nationality} - Nationalize.io key
#' }
#' @param verbose Logical parameter to define if a verbose message should be shown.
#' @return Does not return any object
#' @author Matthias Brenninkmeijer - \href{https://github.com/matbmeijer}{https://github.com/matbmeijer}
#' @examples
#' \dontrun{
#' remove_key(type="gender")
#' remove_key(type="age")
#' remove_key(type="nationality")
#' }
#' @export

remove_key<-function(type, verbose=TRUE){
  env_name<-switch(type,
                   "gender"="GENDERIZE_KEY_PAT",
                   "age" = "AGIFY_KEY_PAT",
                   "nationality"="NATIONALIZE_KEY_PAT")
  Sys.unsetenv(env_name)
  if(verbose){
      cat(sprintf("< %s key saved at environment as %s has been removed >",type, env_name ))
    }
}

api_response<-function(status){
  response<-switch(as.character(status),
            "200"="Success! Everything is OK",
            "400"="Something is wrong on your end",
            "401"="Invalid API key",
            "402"="Subscription is not active",
            "429"="Request limit reached",
            "500"="Something is wrong on our end",
            sprintf("An error has ocurred - Error code %s", status)
  )
  return(response)
}

add_id<-function(x, n){
  if(nrow(x)>0){
    x$id<-n
    return(x)
  }
}

null_to_NA<-function(x){
  if(is.null(x)||!length(x)){
    x<-NA
  }
  return(x)
}

api_request<-function(x, type, country_id=NULL,sliced=TRUE, apikey=NULL){
  #Define service to call
  url<-switch(type,
              "gender"="https://api.genderize.io",
              "age" = "https://api.agify.io",
              "nationality"="https://api.nationalize.io")
  #Remove country in case of search of nationality, as no parameter in app
  if(type=="nationality" && !is.null(country_id)){
    warning(call.=FALSE, sprintf("< The parameter country_id %s is ignored as it is not a parameter for nationalize.io API >", country_id))
    country_id<-NULL
  }
  #Keep unique values to ensure duplicated requests are not made & remove whitespace
  names_param<-stats::setNames(as.list(x), rep("name", length(x)))
  #Add optional parameters for country & apikey
  query<-c(names_param, list(country_id=country_id), list(apikey=apikey))
  #Bring everything together defining GET url
  request_url<-httr::modify_url(url = url, query = query)
  #GET request
  request <- httr::GET(request_url, encode = "json", httr::user_agent("github.com/matbmeijer"))
  #Ensure request is in json format
  if (httr::http_type(request) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  #Format the data to a data.frame
  content <- jsonlite::fromJSON(httr::content(request, "text"), simplifyDataFrame = TRUE)

  #Stop if errors
  if(httr::http_error(request)){
    if(any(names(content) %in% "error")){
      stop(call. = FALSE, sprintf("Error Code %s - %s", request$status_code, content$error))
    } else {
      stop(call. = FALSE, sprintf("Error Code %s - %s", request$status_code, api_response(request$status_code)))
    }
  }
  content$api_rate_limit<-as.integer(request$headers[["x-rate-limit-limit"]])
  content$api_rate_remaining<-as.integer(request$headers[["x-rate-limit-remaining"]])
  content$api_rate_reset<-as.integer(request$headers[["x-rate-reset"]])
  content$api_request_timestamp<-request$date

  #More than one row to NA
  content[unlist(lapply(lapply(content, unlist, recursive = T), is.null))]<-NA

  if(!is.data.frame(content)){
    content<-lapply(content, null_to_NA)
    content<-data.frame(content, stringsAsFactors = FALSE)
  }

  #Nationality multiline
  is_list<-unlist(lapply(content, is.list))

  if(any(is_list)){
    base_content<-content[!is_list]
    base_content$id<-1:nrow(content)
    merge_list<-mapply(add_id, content[,is_list], base_content$id, SIMPLIFY = FALSE)
    #Slice first row only if not expanded
    if(sliced){
      merge_list<-lapply(merge_list, utils::head, 1)
    }
    merge_content<-do.call(rbind, merge_list[lengths(merge_list)>0])
    content<-merge(base_content,merge_content, by="id", all.x = TRUE)
    content$id<-NULL
    }else if(sliced && length(x)==1 && nrow(content)>1){
      content<-utils::head(content, 1)
  }
  cols<-colnames(content)
  colnames(content)<-gsub(".*\\.", "", cols)
  return(content)
}

sequencer<-function(input, type, sliced=TRUE, apikey=NULL){
  x<-input$x
  country_id<-input$country_id[1]
  if(!is.null(country_id) && country_id=="NO COUNTRY"){
    country_id<-NULL
  }
  #Keep unique values to ensure duplicated requests are not made & remove whitespace
  y<-unique(trimws(as.character(x)))
  #Remove empty characters and NA
  y<-y[!(y %in% c(NaN, NA, ""))]
  y_list<-split(y, paste0("group_", ceiling(seq_along(y)/10)))
  content_list<-lapply(y_list, api_request, type=type, country_id=country_id, sliced=sliced,apikey=apikey)
  content<-do.call(rbind, content_list)
  df<-data.frame(id=input$id, name=x,type=type, stringsAsFactors = FALSE)
  df<-merge.data.frame(df, content, by="name", all.x= TRUE)
  return(df)
}

country_distributor<-function(x, type, country_id=NULL, sliced=TRUE, apikey=NULL){
  if(!is.null(country_id)){
    if(!(length(country_id) %in% c(1, length(x)))){
      stop(call. = FALSE, "< country_id must be a single string or a multistring with the same length as x >")
    }
    n<-length(unique(country_id))
    df<-data.frame(id=seq_along(x), x=x, country_id=country_id, stringsAsFactors = FALSE)
    df[is.na(df)]<-"NO COUNTRY"
    pass_list<-split(df, df$country_id)
  }else{
    pass_list<-list(data.frame(id=seq_along(x), x=x, stringsAsFactors = FALSE))
  }
  res_list<-lapply(pass_list, sequencer, type=type, sliced=sliced, apikey=apikey)
  res<-rbind_fill(res_list)
  rownames(res) <- 1:nrow(res)
  res<-res[order(res$id),]
  res$id<-NULL
  return(res)
}

rbind_fill<-function(l){
  r<-unique(unlist(lapply(l, nrow)))
  l<-l[r>0]
  cols<-unique(unlist(lapply(l, names)))
  res<-do.call(rbind, lapply(l, fill_df_NAs, cols))
  return(res)
}

fill_df_NAs<-function(x, cols){
  x_cols<-names(x)
  miss_cols<-setdiff(cols, x_cols)
  x[,miss_cols]<-NA
  x<-x[,cols]
  return(x)
}
