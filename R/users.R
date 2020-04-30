#' Get users
#' @export
get_users <- function(){
  GET(url = get_url("users"), headers_scoold()) %>%
    content()
}
#' Get user
#'
#' @param id id of user
#'
#' @export
get_user <- function(id){
  GET(url = get_url(paste0("users/",id)), headers_scoold()) %>%
    content()
}
#' Add user
#'
#' @param mail Mail
#' @param name name
#' @param pwd password
#'
#' @importFrom purrr map
#' @export
add_user <- function(mail, name, pwd){
  id <- paste0("116",
               paste0(sample(1:10, 16, replace = TRUE), collapse = ""),
               collape = "")

  be_sure_id <- get_users() %>%
    map("creatorid")

  while (id %in% be_sure_id) {
    id <- paste0("116",
                 paste0(sample(1:10, 16, replace = TRUE), collapse = ""),
                 collape = "")
  }

  suppressMessages(
    POST(url = get_url("users"),
         headers_scoold(),
         body = list(  "id"= id,
                       "identifier"= mail,
                       "name"= name,
                       "picture"= "https://www.gravatar.com/avatar/09c84e7329620dfa4fabc20b61221fe7?size=400&d=mm&r=pg",
                       "groups"= "users",
                       "email"= mail,
                       "active"= TRUE,
                       "password"= pwd),
         encode = "json")) %>%
    content()
}
