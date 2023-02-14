generate_netplot<-function(Nodes,Edges){
  MyClickScript<-'
  var x = jQuery("g.node")[d.index].transform["baseVal"][0]["matrix"]["e"];
  var y = jQuery("g.node")[d.index].transform["baseVal"][0]["matrix"]["f"];
  jQuery("line.link").each(function(line){
  const condition1 = ($(this)[0].x1.baseVal.value > x-5 && $(this)[0].x1.baseVal.value < x+5)|($(this)[0].x2.baseVal.value > x-5 && $(this)[0].x2.baseVal.value < x+5)
  const condition2 = ($(this)[0].y1.baseVal.value > y-5 && $(this)[0].y1.baseVal.value < y+5)|($(this)[0].y2.baseVal.value > y-5 && $(this)[0].y2.baseVal.value < y+5)
  const x1= $(this)[0].x1.baseVal.value 
  const x2= $(this)[0].x2.baseVal.value 
  const y1= $(this)[0].y1.baseVal.value 
  const y2= $(this)[0].y2.baseVal.value 
  
  const mp_x = Math.min(x1,x2) + Math.abs(x2-x1)/2
  const mp_y = Math.min(y1,y2) + Math.abs(y2-y1)/2
  console.log(mp_x,mp_y)
  if(condition1 && condition2){
      $(this)[0].style="stroke:red;"
    } else {
      $(this)[0].style="stroke:black;"
    }
  });
  
  Shiny.setInputValue("current_vertex", d.index);
  jQuery("g.node")[d.index].style="fill:red;";
  '
  
  forceNetwork(Links = Edges, 
               Nodes = Nodes, Source = "source",
               Target = "target", Value = "value", NodeID = "name",
               linkWidth = JS("function(d) { return 1; }"),
               colourScale = JS("d3.scaleOrdinal(d3.schemeCategory20);"),
               Group = "group", zoom = TRUE,opacity = 1,
               arrows = F,clickAction = MyClickScript,
               legend=T)
}

prepare_data<-function(group,min_date,max_date){
  Nodes<-list_data_vertices%>%
    filter(Datum>=min_date & Datum<=max_date)%>%
    select(Name,!!parse_expr(group))%>%
    rename(name=Name,group=!!parse_expr(group))%>%
    mutate(size=5)
  
  var<-Nodes$name 
  
  names(var)<-seq(1:length(Nodes$name))-1
  Edges<-list_data_edges%>%
    filter(from %in% Nodes$name & to %in% Nodes$name)%>%
    mutate(source=fct_recode(from,!!!var),
           target=fct_recode(to,!!!var),
           value=1)
  return(list(Nodes=Nodes, Edges=Edges))
}

#' doc_plot
#'
#' @param list_data_vertices 
#' @param list_data_edges 
#'
#' @return
#' @export
#'
#' @examples
doc_plot<-function(list_data_vertices=NULL,list_data_edges=NULL){
  #list_data_vertices can be assigned or exist in the global enviroment
  if(is_null(list_data_vertices)){
    list_data_vertices<-env_get(nm = "list_data_vertices",env = global_env())
  }
  if(is_null(list_data_edges)){
    list_data_edges<-env_get(nm = "list_data_edges",env = global_env())
  }
  #########################################
  # User Interface
  #########################################
  ui <- fluidPage(
    tags$style(
      "body, span.irs {
      font-size: 10px;
    }"
    ),
    tags$h5("Interaktive Dokumentation: ",style="text-align:center",),
    column(4,
           selectInput(inputId = "group",
                       label = "Füllattribut:",
                       choices = c("Dateityp","Meilenstein"),
                       selected = "Dateityp")),
    column(4,
           textInput(inputId = "path",
                     label = "Ordnerpfad"),
           actionButton("submit",label = "öffnen")),
    column(4,
           sliderInput(inputId = "date_range",
                       label = "Zu berücksichtigender Zeitraum:",
                       value = c(min(list_data_vertices$Datum),
                                 max(list_data_vertices$Datum)),
                       max = max(list_data_vertices$Datum),
                       min = min(list_data_vertices$Datum))
    ),
    column(8,forceNetworkOutput("netplot")),
    column(4,
           tags$h5("Informationen der aktuell ausgewählten Datei:",style="text-align:center"),
           dataTableOutput("vertex_info")),
    
    column(12,
           tags$h5("Informationen zu den Syntaxen, die die aktuell ausgewählte Datei verwenden:",
                   style="text-align:center"),
           dataTableOutput("edge_info"))
  )
  
  #########################################
  # Server 
  #########################################
  
  server <- function(input, output) {
    plot_basis <- eventReactive(c(input$group,input$date_range),{
      print(input$group)
      print(input$date_range[1])
      prepare_data(group=input$group,
                   min_date = input$date_range[1],
                   max_date = input$date_range[2])
    })
    
    observeEvent(input$submit,{
      utils::browseURL(input$path)
    })
    output$netplot<-renderForceNetwork({
      req(plot_basis())
      #p1$x$links$colour[p1$x$links$source==as.numeric(input$current_vertex)|
      #                    p1$x$links$target==as.numeric(input$current_vertex)]<-"FireBrick"
      generate_netplot(Nodes = plot_basis()$Nodes,
                       Edges = plot_basis()$Edges)
    })
    
    
    
    observeEvent(input$current_vertex,{
      output$edge_info<-renderDataTable({
        tab<-(plot_basis()$Edges)%>%
          filter(source==as.numeric(input$current_vertex)|
                   target==as.numeric(input$current_vertex))%>%
          datatable(.,options = list(dom = 't'),
                    rownames= FALSE)
        DT::formatStyle(tab,columns = colnames(tab), fontSize = '50%')
      })
    })
    
    observeEvent(input$current_vertex,{
      output$vertex_info<-renderDataTable({
        
        liste<-list_data_vertices%>%
          filter(vid==paste0(as.numeric(input$current_vertex)+1))
        info<-map_chr(liste,function(x){
          if(is.list(x)){
            str_c(x%>%unlist(),collapse = ", ")
          }else if(is.Date(x)){
            format(x, "%Y-%m-%d") 
          }else{
            x
          }
        })
        tibble(
          Variable=names(liste),
          Info=info
        )%>%
          datatable(.,options = list(dom = 't',
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$('body').css({'font-size': '10px'});",
                                       "}"
                                     )),
                    rownames= FALSE)
        
        #DT::formatStyle(tab,columns = colnames(tab), fontSize = '10%')
      })
    })
  }
  
  app=shinyApp(ui=ui, server=server)
  runApp(app,launch.browser = TRUE)
}