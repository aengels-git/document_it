
#' doc_read_data
#'
#' @param file file to read
#' @param milestone project step or working package
#' @param sep optional seperator for csv files
#' @param encoding encoding
#' @param sheet sheet
#' @param pagesize only relevant for json files
#' @param id optional id to count unique values
#' @param ... arguments to pass to the reading function
#'
#' @return
#' @export
#'
#' @examples
doc_read_data<-function(file,milestone,sep = ",",encoding = "UTF-8",sheet=NULL,pagesize = 100000,id=NA_character_,...){
  file<-fs::path_abs(file)
  extension<-fs::path_ext(file)
  if(!extension %in% c("csv","xlsx","json","rds","parquet")){
    stop(glue("The extension {extension} is not supported yet!"))
  }
  
  if(extension=="csv"){
    if(sep==","){
      df<-read_csv(file,...)%>%
        select_if(str_detect(names(.),"^X$")==F)
    }else{
      df<-read_csv2(file,...)%>%
        select_if(str_detect(names(.),"^X$")==F)
    }
    
  } else if(extension=="xlsx"){

    df<-read_excel(file,sheet=sheet,...)
    
  } else if(extension=="json"){
    df<-stream_in(con = file(file),pagesize = pagesize)
    
  } else if(extension=="rds"){
    df<-readRDS(file,...)
    
  } else if(extension=="parquet"){
    df<-read_parquet(file,...)
  }
  
  if(!basename(file) %in% list_data_vertices$Dateiname){
    new_data_vertex(df=df,filename = basename(file),
                    dirname=dirname(file),id=id,Meilenstein=milestone)
  }
  return(df)
}

#' doc_write_data
#'
#' @param data 
#' @param milestone 
#' @param file 
#' @param sheetName 
#' @param id 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
doc_write_data<-function(data, file, milestone,id=NA_character_,...){
  file<-fs::path_abs(file)
  extension<-fs::path_ext(file)
  if(!extension %in% c("csv","xlsx","json","rds","parquet")){
    stop(glue("The extension {extension} is not supported yet!"))
  }
  
  if(extension=="csv"){
    write_csv(x = data,file = file,...)
    
  } else if(extension=="xlsx"){
    write_xlsx(x = data,path  = file,...)
    
  } else if(extension=="json"){
    stream_out(x = data, con = file(file),pagesize=pagesize)
    
  } else if(extension=="rds"){
    saveRDS(object = data,file,...)
    
  } else if(extension=="parquet"){
    write_parquet(data,sink = file)
  }
  
  if(!basename(file) %in% list_data_vertices$Dateiname){
    new_data_vertex(df=data,filename = basename(file),
                    dirname=dirname(file),id=id,Meilenstein=milestone)
  }
}

#' doc_ggsave
#'
#' @param plot 
#' @param milestone 
#' @param filename 
#' @param device 
#' @param width 
#' @param height 
#' @param units 
#' @param ... 
doc_ggsave<-function(plot,milestone,filename,device="png",width=10,height=6,units="in",...){
  ggsave(filename = filename,plot = plot,width = width,height = height,
         device = device,units = "in")
  if(!basename(filename) %in% list_data_vertices$Dateiname){
    new_data_vertex(filename = basename(filename),
                    dirname=file_path_as_absolute(dirname(filename)),Meilenstein=milestone)
  }
}