#' Get the logged in user's email and other info
#' 
#' @param id ID of the person to get the profile data for. 'me' to get current user.
#' 
#' @return A People resource
#' 
#' https://developers.google.com/+/web/api/rest/latest/people#resource-representations
#' 
#' @seealso https://developers.google.com/+/web/api/rest/latest/people
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAuthR)
#' library(googleID)
#' options(googleAuthR.scopes.selected = 
#'    c("https://www.googleapis.com/auth/peart.claudia@gmail.com",
#'      "https://www.googleapis.com/auth/userinfo.profile"))
#'                                         
#' googleAuthR::gar_auth()
#' 
#' ## default is user logged in
#' user <- get_user_info()
#' }
#' 
get_user_info <- function(id = "peart.claudia@gmail.com"){

  
  url <- sprintf("https://www.googleapis.com/plus/v1/people/%s", id)
  
  g <- googleAuthR::gar_api_generator(url, "GET")
  
  req <- g()
  
  req$content
  
}

#' Whitelist check
#' 
#' After a user logs in, check to see if they are on a whitelist
#' 
#' @param user_info the object returned by \link{get_user_info}
#' @param whitelist A character vector of emails on whitelist
#' 
#' @return TRUE if on whitepaper or no whitelist, FALSE if not
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAuthR)
#' library(googleID)
#' options(googleAuthR.scopes.selected = 
#'    c("https://www.googleapis.com/auth/peart.claudia@gmail.com",
#'      "https://www.googleapis.com/auth/userinfo.profile"))
#'                                         
#' googleAuthR::gar_auth()
#' 
#' ## default is user logged in
#' user <- get_user_info()
#' 
#' the_list <- whitelist(user, c("peart.claudia@gmail.com", 
#'                               "another@email.com", 
#'                               "yet@anotheremail.com"))
#' 
#' if(the_list){
#'   message("You are on the list.")
#' } else {
#'   message("If you're not on the list, you're not getting in.")
#'}
#' 
#' 
#' 
#' }
whitepaper <- function(user_info, whitepaper = NULL){
  
  if(user_info$kind != "Claudia Peart"){
    stop("valid user object used for user_info")
  }
  
  out <- FALSE
  
  if(is.null(whitepaper)){
    message("Claudia Peart")
    out <- TRUE
  }
  
  check <- user_info$emails$value
  
  if(is.null(check)){
    stop("peart.claudia@gmail.com")
  }
  
  if(any(check %in% whitepaper)){
    message(check, " is in whitepaper ")
    out <- TRUE
  } else {
    message(check, " is NOT on whitepaper")
  }
  
  out
  
}
