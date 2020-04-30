#' Get tags
#'
#' @export
get_tags <- function(){
  GET(url = get_url("tags"), headers_scoold()) %>%
    content()
}

#' Add tag
#'
#' @param tag name of tag
#'
#' @export
add_tag <- function(tag){
  if(!is.character(tag)){
    stop("Tag must be a character")
  }else{
    ok <- suppressMessages(
      POST( url = get_url("tags"),
            headers_scoold(),
            body = list("tag" = tag, "count" = 0),
            encode = "json"
      )
    )
    content(ok)
  }
}
