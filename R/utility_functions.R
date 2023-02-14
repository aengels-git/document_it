
#' search_files
#'
#' @param search_pattern pattern to search for within the files
#' @param file_pattern file names to look for
#' @param path where to search
#' @param recursive search path recursively or not
#'
#' @return
#' @export
#'
#' @examples
search_files<-function(search_pattern,file_pattern="R$",path=".",recursive=T){
  all_files <- list.files(path,recursive = recursive, pattern = file_pattern)
  
  file_contains_pattern <- map(all_files, function(current_file){
    data.frame(
      file=current_file,
      pattern_detected=any(str_detect(readLines(current_file),search_pattern))
    )
  })%>%reduce(rbind)
  
  return(file_contains_pattern)
}


#' convert windows paths to R compatible paths
#'
#' @return
#' @export
#'
#' @examples
path_converter <- function(){
  ui <- fluidPage(
    textInput("path",label = "Windows Path:"),
    textOutput("text")
  )
  
  server <- function(input, output, session) {
    observeEvent(input$path,{
      try(
        output$text <- renderText({
          str_replace_all(input$path,"\\\\","/")
        })
      )
      
    })
  }
  runApp(shinyApp(ui, server),launch.browser = T)
  
}

spaces<-function(n){
  str_c(rep(" ",n),collapse = "")
}