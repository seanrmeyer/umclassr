require(jsonlite)
require(tidyverse)
require(knitr)
require(httr)
require(tidyjson)
require(dplyr)

#' This function creates an access token. Its used by the subsequent functions for API authorization.
#' @param client_id Defaults to System Environment variable "UMSCHED_CLIENT_ID"
#' @param client_secret Defaults to System Environment variable "UMSCHED_CLIENT_SECRET"
#' @keywords auth
#' @export
#' @examples
#' \dontrun{
#' get_umsched_access_token()
#' }

get_umsched_access_token <- function (client_id = Sys.getenv("UMSCHED_CLIENT_ID"), client_secret = Sys.getenv("UMSCHED_CLIENT_SECRET")){
   post <- POST('https://apigw.it.umich.edu/um/aa/oauth2/token',
                accept_json(), authenticate(client_id, client_secret),
                body = list(grant_type = 'client_credentials', scope = "umscheduleofclasses"),
                encode = 'form', httr::config(http_version = 2)) %>% content
   if (!is.null(post$error)) {
     stop(str_glue('Could not authenticate with given credentials:\n\t{post$error_description}'))
   }
   access_token <- post$access_token
   return(access_token)
}


#' This function gets all schools offering courses for a term
#' @param term term code
#' @param access_token access token, retrieves a token if one isn't provided
#' @keywords school description
#' @export
#' @examples
#' \dontrun{
#' get_schools('2210')
#' }

get_schools <- function(term, access_token = get_umsched_access_token()){
  client_id <- Sys.getenv("UMSCHED_CLIENT_ID");
  res <- GET(str_glue('https://apigw.it.umich.edu/um/Curriculum/SOC/Terms/{term}/Schools'),
             add_headers(.headers = c(
               authorization = paste("Bearer", get_umsched_access_token()),
               'X-IBM-Client-Id' = client_id,
               accept = "application/json")))
  schools <- res$content %>%
    rawToChar() %>%
    fromJSON() %>%
    as.data.frame() %>% # as.tibble > stringsasfactors, check.names=F
    select(2:4)
  names(schools) <- c("SchoolCode", "SchoolDescr", "SchoolShortDescr")
  return(schools)
}

#' This function gets all schools offering courses for a term
#' @param term term code
#' @param schoolcode term code
#' @param access_token access token, retrieves a token if one isn't provided
#' @keywords school description
#' @export
#' @examples
#' \dontrun{
#' get_schools('2210', 'DEN')
#' }

# Subjects
get_subjects <- function(term, schoolcode, access_token = get_umsched_access_token()){
  client_id <- Sys.getenv("UMSCHED_CLIENT_ID");
  res <- GET(str_glue('https://apigw.it.umich.edu/um/Curriculum/SOC/Terms/{term}/Schools/{schoolcode}/Subjects'),
             add_headers(.headers = c(
               authorization = paste("Bearer", get_umsched_access_token()),
               'X-IBM-Client-Id' = client_id,
               accept = "application/json")))
  subjects <- res$content %>%
    rawToChar() %>%
    fromJSON() %>%
    as.data.frame() %>%
    select(2:4)
  names(subjects) <- c("SubjectCode", "SubjectDescr", "SubjectShortDescr")
  return(subjects)
}

#' This function gets a course description
#' @param term term code
#' @param sub subject
#' @param catnum course number
#' @param access token, retrieves a token if one isn't provided
#' @keywords course description
#' @export
#' @examples
#' \dontrun{
#' get_course_description('2210', 'IOE', '436')
#' }

get_course_description <- function(term, sub, catnum, access_token = get_umsched_access_token()) {
  client_id <- Sys.getenv("UMSCHED_CLIENT_ID");
  res <- GET(str_glue('https://apigw.it.umich.edu/um/Curriculum/SOC/Terms/{term}/Schools/_/Subjects/{sub}/CatalogNbrs/{catnum}'),
             add_headers(.headers = c(
               authorization = paste("Bearer", get_umsched_access_token()),
               'X-IBM-Client-Id' = client_id,
               accept = "application/json")))
  description <- res$content %>%
    rawToChar() %>%
    fromJSON() %>%
    as.data.frame() %>%
    select(2)
  names(description) <- c("CourseDescr")
  return(description)
}

#' This function checks whether a course is offered in a term
#' @param term term code
#' @param sub subject
#' @param catnum course number
#' @param access token, retrieves a token if one isn't provided
#' @keywords course description
#' @export
#' @examples
#' \dontrun{
#' get_course_description('2210', 'IOE', '436')
#' }

get_course_offering <- function(term, sub, catnum, access_token = get_umsched_access_token()){
  client_id <- Sys.getenv("UMSCHED_CLIENT_ID");
  res <- GET(str_glue('https://apigw.it.umich.edu/um/Curriculum/SOC/Terms/{term}/Classes/Search/{sub}%20{catnum}'),
             add_headers(.headers = c(
               authorization = paste("Bearer", get_umsched_access_token()),
               'X-IBM-Client-Id' = client_id,
               accept = "application/json")))
  offered <- res$content %>%
    rawToChar() %>%
    fromJSON() %>%
    as.data.frame()
  if("searchSOCClassesResponse.NoResult" %in% colnames(offered))
    return(0)
  else return(1)
}
