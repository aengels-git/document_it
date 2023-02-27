library(devtools)
load_all()
#Basic usage of doc_sheet
report <- doc_sheet(title = "Dokumentation")
report$add_df_info(data = diamonds)
report$add_object(object = viskit::vis_barplot(prepkit::prep_freq_table(diamonds,cut),cut,n),name = "diamonds_barplot",
                  width = 8,height = 4)
report$add_text("Hello World",level = 1)
report$add_text("Es gibt so viel zu erzählen über diesen Datensatz. Ich würde gerne bestimmte Schliffarten entfernen")
report$add_object(object = doc_reactable((prepkit::prep_freq_table(diamonds,cut))),name = "diamonds_table")
report$render()

#Change filemanager and edit interactively:
# report$filemanager<-"pander"
# report$open()
# report$reload()
# report$render()

#Basic documentation
doc_start()
doc_load()

doc_write_data(diamonds,"Tests/data/diamonds.csv",milestone="Datenaufbereitung")
library(prepkit)
library(viskit)

tab <- prep_freq_table(diamonds,cut)
plot1 <- vis_barplot(tab,x=cut,y=n)

doc_ggsave(plot = plot1,filename = "Tests/data/diamonds.png",milestone="Datenaufbereitung")

doc_edge()

doc_plot()

install()


