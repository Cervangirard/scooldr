#' Set token to interact with Scoold
#'
#'
set_token <- function(){
  token <- getPass::getPass("Get your token from your scoold app")
  if(!is.null(token) & is.character(token)){
    keyring::key_set_with_value("scoold_token",password = token)
  }
  token
}

#' Get token to interact with Scoold
#'
#' @return token
#' @export
#'
#' @examples
#' \dontrun{
#' get_token()
#' }
get_token <- function(){
  token <- NULL
  try(token <- keyring::key_get("scoold_token"), silent = TRUE)
  if(is.null(token)){
    token <- set_token()
  }
  token
}


#' Get auth for scoold
#'
#' @param token token
#'
#' @export
headers_scoold <- function(token = get_token()){
  add_headers(Authorization = paste("Bearer", token))
}
