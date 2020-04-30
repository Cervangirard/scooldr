#' Set up url scoold
#'
#'
#' @export
set_url_api <- function(){
  home <- Sys.getenv("HOME")
  renviron <- file.path(home,".Renviron")
  message("Find .Renviron file")
  url <- getPass("Url for your Scoold app")
  if(file.exists(renviron)){
    message("Adding SCOOLD_URL var env")

    be_sure <- readLines(renviron)

    if(any(grepl(pattern = "SCOOLD_URL", be_sure))){
      stop("URL already set !")
    }
    write(x = paste0("SCOOLD_URL=\"", url,"\""), file = renviron, append = TRUE)
  }else{
    message("Creating .Renviron file")
    file.create(renviron)
    write(x = paste0("SCOOLD_URL=", url), file = renviron, append = TRUE)
  }
  message("Restart R to acess of your SCOOLD_URL var")
}

#' Get url
#'
#' @param endpoint can be tag or something else
#'
#' @export
get_url <- function(endpoint = NULL){
  url <- Sys.getenv("SCOOLD_URL")
  if(url == ""){
    set_url_api()
  }else{
    if(!is.null(endpoint)){
      paste0(url,"/", endpoint)
    }else{
      url
    }
  }
}
