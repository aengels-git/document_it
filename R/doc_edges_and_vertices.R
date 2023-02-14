
#' doc_multi_vertex
#'
#' @param name 
#' @param milestone 
#' @param folder 
#' @param files 
#' @param selector 
#' @return
#' @export
#'
#' @examples
doc_multi_vertex<-function(name,milestone,folder,files=NULL,selector=NULL){

  folder<-str_replace(folder,pattern = "/$",replacement = "")
  if(is_null(files)){
    stopifnot(is_null(selector)==F)
    files<-list.files(folder)
    files<-files[str_detect(files,selector)]
    detectable_files<-map_chr(files,~as.character(glue("{folder}/{.x}")))
  } else {
    detectable_files<-map_chr(files,~as.character(glue("{folder}/{.x}")))
  }
  walk(detectable_files,function(file){
    if(!basename(file) %in% list_data_vertices$Dateiname){
      new_data_vertex(df=data.frame(),filename = basename(file),
                      dirname=dirname(file),Meilenstein=Meilenstein)
    }
  })
  
  result<-map2(split(list_data_vertices,list_data_vertices$Dateiname %in% files),
               unique(list_data_vertices$Dateiname %in% files),function(df,crit){
                 if(crit==T){
                   df$Dateiname=unlist(df$Dateiname)
                   df%>%
                     summarise_all(min)%>%
                     mutate(Dateiname=list(df$Dateiname),
                            Name=name)
                 } else {
                   df
                 }
               })%>%
    reduce(rbind)%>%
    as_tibble()
  env_poke(nm = "list_data_vertices",
           value = result,
           env = global_env())
}

#' new_data_vertex
#'
#' @param df 
#' @param filename 
#' @param dirname 
#' @param id 
#' @param Meilenstein 
#'
#' @return
#' @export
#'
#' @examples
new_data_vertex<-function(df=NA_character_,filename=NA_character_,dirname=NA_character_,
                          id=NA_character_,Meilenstein=NA_character_){
  value = list(
    "Name"=str_extract(filename,".+(?=\\.)")[1],
    "Dateiname"=filename,
    "Dateityp"=str_extract(filename,"(?<=\\.).+")[1],
    "Dateiordner"=dirname,
    "ID"=ifelse(is.na(id),NA_character_,id),
    "Einzigartige_IDs"=ifelse(is.na(id),NA_character_,length(unique(df%>%pull(id)))),
    "Laenge"=ifelse(is_na(df),NA_character_,dim(df)[1]),
    "Breite"=ifelse(is_na(df),NA_character_,dim(df)[2]),
    "Variablen"=ifelse(is_na(df),
                       NA_character_,
                       map2_chr(names(df),map_chr(df,~class(.x)[1]),~paste0(.x," (",.y,")"))%>%str_c(.,collapse=", ")),
    "vid"=NA_character_,
    "Datum"=today(),
    "Meilenstein"=Meilenstein
  )%>%
    data.frame()%>%
    as_tibble()
  print(value)
  print(list_data_vertices)
  env_poke(nm = "list_data_vertices",
           value = env_get(nm = "list_data_vertices",env = global_env())%>%
             rbind(.,value)%>%
             mutate(vid=as.numeric(rownames(.)))%>%
             as_tibble(),
           env = global_env())
}


#' new_data_edge
#'
#' @param from 
#' @param to 
#' @param comments 
#' @param path 
#'
#' @return
#' @export
#'
#' @examples
new_data_edge<-function(from,to,comments="none",path=rstudioapi::getSourceEditorContext()$path){
  skript_info<-str_split(path,"\\/")[[1]]
  value=list(
    "from"=from,
    "to"=to,
    "Location"=dirname(path),
    "Syntax_Name"=skript_info[length(skript_info)],
    "comments"=comments,
    "Datum"=today()
  )%>%
    data.frame()
  
  env_poke(nm = "list_data_edges",
           value = env_get(nm = "list_data_edges",env = global_env())%>%
             rbind(.,value),
           env = global_env())
}

#' doc_edge
#'
#' @param comments 
#'
#' @return
#' @export
#'
#' @examples
doc_edge<-function(comments="none"){
  cat(str_c(paste0(list_data_vertices$Name,"(id=",list_data_vertices$vid,")"),collapse = "\n"))
  from <- readline("from (type 1 or 1,2 etc.):")
  from <- str_split(from,",")[[1]]
  cat(str_c(paste0(list_data_vertices$Name,"(id=",list_data_vertices$vid,")"),collapse = "\n"))
  to<-readline("to (type 1 or 1,2 etc.):")
  to<-str_split(to,",")[[1]]
  if(length(to)>1 & length(from)>1){
    stop("Either to or from has to be 1!")
  } 
  if(length(to)>1){
    walk(to,function(current_to){
      new_data_edge(to = list_data_vertices$Name[list_data_vertices$vid==current_to],
                    from = list_data_vertices$Name[list_data_vertices$vid==from],
                    comments=comments)
    })
  } else {
    walk(from,function(current_from){
      new_data_edge(to = list_data_vertices$Name[list_data_vertices$vid==to],
                    from = list_data_vertices$Name[list_data_vertices$vid==current_from],
                    comments=comments)
    })
  }
}