#' doc_reactable
#'
#' @param table 
#' @param fontsize 
#' @param headersize 
#' @param resizable 
#' @param groupBy 
#' @param total_width 
#' @param percent_vector 
#'
#' @return
#' @export
#'
#' @examples
doc_reactable <- function(table,fontsize=14,headersize=16,resizable = FALSE,groupBy=NULL, total_width=780,percent_vector=NULL){
  if(is.null(percent_vector)==F){
    #Gurantee that the values add up to total_width
    correction_factor <-     total_width/sum(percent_vector*total_width) 
    column_definitions<-list2( !!!setNames(map(percent_vector,~colDef(width = .x*total_width*correction_factor)), names(table) ))
    
    
  } else {
    column_definitions<-NULL
  }
  theme <- reactableTheme(
    style = list(fontFamily = "Arial", fontSize = glue("{fontsize}px")),
    headerStyle =list(background = "#f7f7f8",fontFamily = "Arial", fontSize = glue("{headersize}px")),
    paginationStyle = list(fontFamily = "Arial", fontSize = glue("{fontsize}px")),
  )
  print(total_width/(dim(table)/2))
  rt <- reactable(
    table,
    defaultColDef = colDef(
      header = function(value) gsub(".", " ", value, fixed = TRUE),
      cell = function(value) format(value, nsmall = 1),
      align = "center",
      minWidth = total_width/(dim(table)[2]),
    ),
    columns =column_definitions,
    bordered = TRUE,
    highlight = TRUE, 
    resizable = resizable,
    theme = theme,
    groupBy=groupBy,
    fullWidth = FALSE
  )
  return(rt)
}
