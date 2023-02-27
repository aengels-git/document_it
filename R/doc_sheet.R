YamlHeader<-R6Class("YamlHeader", list(
  title = NULL,
  author = NULL,
  date = NULL,
  output_format = NULL,
  theme = NULL,
  bs_version = NULL
))
Snippet<-R6Class("Snippet", list(
  include = NULL,
  echo = NULL,
  message = NULL,
  warning  = NULL,
  eval = NULL,
  content = NULL,
  fig.width = NULL,
  fig.height = NULL,
  out.width = NULL,
  out.height = NULL,
  initialize = function(content="",include = T,echo = F,message = F,warning  = F,eval=T,
                        fig.width=NULL,fig.height=NULL,out.width="100%",out.height="100%") {
    self$include = include
    self$echo = echo
    self$message = message
    self$warning  = warning
    self$eval = eval
    self$fig.width = fig.width
    self$fig.height = fig.height
    self$out.width = out.width
    self$out.height = out.height
    self$content = content
  },
  create = function(){
    if(is.null(self$fig.width)==F){
      settings = glue("fig.width = {self$fig.width},fig.height = {self$fig.height},echo={self$echo},include={self$include},message={self$message},warning={self$warning},eval={self$eval}")
    } else {
      settings = glue("out.width = \'{self$out.width}\',out.height = \'{self$out.height}\',echo={self$echo},include={self$include},message={self$message},warning={self$warning},eval={self$eval}")
      
    }
    chunk_code <- glue("```{r,<<settings>>}\n<@self$content@>\n```",.open = "<<",.close = ">>")
    chunk_code <- as.character(glue(chunk_code,.open = "<@",.close = "@>"))
    return(chunk_code)
  }
))

MdText<-R6Class("MdText", list(
  text = NULL,
  comment = NULL,
  initialize = function(text,style="",level=0,comment = NULL) {
    indent <- ifelse(level==0,"",paste0(str_c(rep("#",level),collapse = "")," "))
    self$text=paste0(as.character(glue("{indent}{text}{style}")))
    self$text = str_replace_all(self$text,"\n[\n]+","\n\n")
    self$comment <- as.character(glue("<!-- {comment} -->"))
  },
  create = function(){
    if(is.null(self$comment)){
      return(self$text) # paste0(self$text,"\n"))
    } else {
      return(paste0(self$text,"\n",self$comment))
    }
  }
))
HtmlHeader <- R6Class("HtmlHeader", 
                      inherit = YamlHeader,
                      private = list(
                        create_theme_string = function(style,bs_version=5){
                          return(paste0(spaces(4),"theme: \n",
                                        spaces(6),glue("version: {bs_version}"),"\n",
                                        spaces(6),glue("bootswatch: {style}")))
                        }
                      ),
                      public = list(
                        available_themes = c("default","bootstrap","cerulean","cosmo","darkly","flatly","journal","lumen",
                                             "paper","readable","sandstone","simplex","spacelab","united","yeti"),
                        initialize = function(title,author="",theme="default",bs_version=5) {
                          self$title <- title
                          self$author <- author
                          self$date <- as.character(today())
                          self$output_format <- paste0(spaces(2),"html_document")
                          self$theme <- theme
                          self$bs_version <- bs_version
                        },
                        
                        create = function(){
                          title <- glue("title: {self$title}")
                          theme <- private$create_theme_string(style=self$theme,bs_version = self$bs_version)
                          author <- glue("author: {self$author}")
                          output <- glue("output:\n{self$output_format}:\n{theme}")
                          result <- str_c(title,author,output,sep = "\n")
                          return(as.character(glue("---\n{result}\n---\n")))
                        }
                      )
)
#' R6 Class representing a HTML Report that can modify and render itself
#' 
#' @export
Report<-R6Class("Report", 
                private = list(
                  add_data = function(content,type="chunk",...){
                    self$data <- rbind(self$data,tibble(
                      type=c(type),
                      objects=list(Snippet$new(content = content,...))
                    ))
                  },
                  open_file = function(file){
                    if(self$filemanager=="browseURL"){
                      browseURL(file)
                    } else {
                      pander::openFileInOS(file)
                    }
                  },
                  write_rmd = function(){
                    raw_markdown_text <- map(1:length(self$data$objects),function(i){
                      self$data$objects[[i]]$create()
                    })%>%str_c(.,collapse = "\n\n")
                    self$filename <- tempfile(fileext = ".rmd")
                    writeLines(raw_markdown_text,self$filename)
                  }
                ),
                public = list(
                  data = NULL,
                  enviroment = NULL,
                  filename = NULL,
                  filemanager = NULL,
                  initialize = function(title,author="",theme="default",bs_version=5,filemanager="browseURL") {
                    self$data <- tibble(
                      type=c("header"),
                      objects=list(HtmlHeader$new(title=title,author=author,theme=theme,bs_version=bs_version))
                    )
                    self$enviroment <-new.env()
                    self$filemanager = filemanager
                  },
                  add_object = function(object,name=NULL,header=NULL,footer=NULL,width=8,height=4,...){
                    if(is.null(name)){
                      name <- deparse(substitute(object))
                    }
                    self$enviroment[[name]] <- object
                    
                    if(is.null(header)==F){
                      self$add_text(text = header,level = 1)
                    }
                    private$add_data(content = name,fig.height=height,fig.width=width,...)
                    if(is.null(footer)==F){
                      self$add_text(text = footer,level = 0)
                    }
                  },
                  add_text = function(text,level=0){
                    self$data <- rbind(self$data,tibble(
                      type=c("text"),
                      objects=list(MdText$new(text=text,level=level))
                    ))
                  },
                  add_df_info = function(data,id=NULL){
                    name <- deparse(substitute(data))
                    description <- glue("Der Datensatz {name} hat {dim(data)[1]} Zeilen und {dim(data)[2]} Spalten.")
                    description<-paste0(description,"\n")
                    if(is.null(id)==F){
                      description <- paste0(description,glue("Die ID Variable {id} hat {length(unique(data[[id]]))} einzigartige Werte."),"\n")
                    }
                    self$add_text(description)
                  },
                  open = function(){
                    private$write_rmd()
                    private$open_file(self$filename)
                  },
                  reload = function(){
                    content <- readLines(self$filename)
                    header_range <- which(str_detect(content,"^---|---$"))
                    classes <- map_chr(self$data$objects,~class(.x)[1])
                    
                    self$data <- rbind(self$data[which(str_detect(classes,"Header|Footer")),],tibble(
                      type=c("text"),
                      objects=list(MdText$new(text=str_c(content[-c(header_range[1]:header_range[2])],collapse = "\n"),level=0))
                    ))
                  },
                  render = function(){
                    private$write_rmd()
                    render(self$filename,envir = self$enviroment,quiet = T)
                    private$open_file(paste0(fs::path_ext_remove(self$filename),".html"))
                  },
                  save = function(path){
                    private$write_rmd()
                    render(self$filename,envir = self$enviroment,quiet = T)
                    ext <- fs::path_ext(path)
                    if(!ext %in% c("html","rds") ){
                      stop("The path should end with .html or .rds")
                    }
                    if(ext == "html"){
                      file_move(paste0(fs::path_ext_remove(self$filename),".html"),new_path = path)
                    } else {
                      saveRDS(self,path)
                    }
                  }
                ))

#' Create a Report Object that can interactively create a rmd html document
#'
#' @param title 
#' @param author 
#' @param theme a bootswatch theme
#' @param bs_version 
#' @param filemanager 
#'
#' @return
#' @export
#'
#' @examples
doc_sheet<-function(title,author="",theme="default",bs_version=5,filemanager="browseURL"){
  report <- Report$new(title=title,author="",theme=theme,bs_version=bs_version,filemanager=filemanager)
  return(report)
}