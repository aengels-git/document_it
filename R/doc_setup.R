#' Creates documentation folder in current working directory and sets up files 
#'
#' @return
#' @export
#'
#' @examples
doc_start<-function(){
  if(dir.exists("Dokumentation")==F){
    dir.create("Dokumentation")
  }
  file_name_vertices = glue("Dokumentation/{today()} Dokumentation Vertices.json")
  file_name_edges = glue("Dokumentation/{today()} Dokumentation Edges.json")
  if(file.exists(file_name_vertices)==F && file.exists(file_name_edges)==F){
    stream_out(data.frame(),
               con  = file(file_name_vertices),
               pagesize = 10000)
    stream_out(data.frame(),
               con  = file(file_name_edges),
               pagesize = 10000)
  }
}

#' Loads current state of the documentation
#'
#' @param top_directory folder that contains the documentation folder, if null assumes that the documentation folder is in the current working directory
#'
#' @return
#' @export
#'
#' @examples
doc_load<-function(top_directory=NULL){
  if(is_null(top_directory)==FALSE){
    setwd(top_directory)
  }
  
  latest_date<-str_extract_all(list.files("Dokumentation"),
                               "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}")%>%
    reduce(c)%>%ymd()%>%max()
  #Handle Empty Vertices Dataframes:
  tryCatch({
    #vertices<-read.csv(glue("Dokumentation/{latest_date} Dokumentation Vertices.csv"),nrows = 1)
    vertices<-stream_in(con = file(glue("Dokumentation/{latest_date} Dokumentation Vertices.json")),
                        pagesize = 10000)
    vertices$Datum<-ymd(vertices$Datum)
    env_poke(nm = "list_data_vertices",
             value =vertices%>%
               select_if(str_detect(names(.),"^X$")==F)%>%
               as_tibble() ,
             env = global_env())
  },
  error=function(cond){
    env_poke(nm = "list_data_vertices",
             value =data.frame()%>%
               as_tibble() ,
             env = global_env())
  })
  
  #Handle Empty Edges Dataframes:
  tryCatch({
    edges<-stream_in(con = file(glue("Dokumentation/{latest_date} Dokumentation Edges.json")),
                     pagesize = 10000)
    edges$Datum<-ymd(edges$Datum)
    env_poke(nm = "list_data_edges",
             value = edges%>%
               select_if(str_detect(names(.),"^X$")==F)%>%
               as_tibble(),
             env = global_env())
  },
  error=function(cond){
    env_poke(nm = "list_data_edges",
             value = data.frame()%>%
               as_tibble(),
             env = global_env())
  })
}

#' Save current state of the documentation
#'
#' @param top_directory folder that contains the documentation folder, if null assumes that the documentation folder is in the current working directory
#'
#' @return
#' @export
#'
#' @examples
doc_save<-function(top_directory=NULL){
  
  if(is_null(top_directory)==FALSE){
    setwd(top_directory)
  }
  file_name_vertices = glue("Dokumentation/{today()} Dokumentation Vertices.json")
  file_name_edges = glue("Dokumentation/{today()} Dokumentation Edges.json")
  
  stream_out(env_get(nm = "list_data_vertices",env = global_env()),
             con  = file(file_name_vertices),
             pagesize = 10000)
  stream_out(env_get(nm = "list_data_edges",env = global_env()),con  = file(file_name_edges),pagesize = 10000)
}



