server<- function(input, output){
  
  #柱状图
  observeEvent(input$action,{if(input$select == "1"){ datapath1 <- input$file1$datapath
  # 对数据进行分组排序
  dt<-fread(datapath1)
  dt <- dt %>% 
    arrange(GO_Class, desc(number_of_out)) %>%
    mutate(GO_Term = factor(GO_Term, levels = unique(GO_Term)))
  count <- count(dt[,1])
  
  top.mar=0.5
  right.mar=0.5
  bottom.mar=0.2
  left.mar=2
  mytheme1<-theme_classic()+
    theme(
      plot.title = element_text(face = input$f2, colour = "black", size = input$slider2, hjust = 0.4, vjust = 3,family = input$t2),
      axis.line = element_blank(),
      axis.ticks = element_line(size = 0.6,colour = "gray30"),
      axis.ticks.length = unit(1.5,units = "mm"),
      axis.title.y = ggplot2::element_text(size = input$slider3,face=input$f3,vjust = 3,family = input$t3),
      axis.text.y = element_text(family = input$tx1, size = input$sliderx1, color="black",angle=0,vjust=0.5,face = input$fx1),
      axis.text.x = element_text(family = input$t1, size = input$slider1, color="black",angle=0,face = input$f1),
      
      legend.text = element_text(family = input$tx, size = input$sliderx, color = "black",face = input$fx),
      plot.margin=unit(x=c(top.mar,right.mar,bottom.mar,left.mar),
                       units="inches"))
  mycolor <- c(input$color1,input$color2,input$color3)
  scolor <- mycolor[3:5]
  output$p1 <- renderPlot({
    p1 <<- ggbarplot(dt, x = "GO_Term", y = "number_of_out",
                     fill = "GO_Class",
                     dot.size=6,
                     palette = mycolor,
                     add = "segments",
                     sort.val = "desc",
                     group ="GO_Class",
                     
                     xlab = "",
                     ylab = "Gene Number",
                     x.text.col=TRUE,
                     add.params = list(color = "GO_Class", size = 1),
                     title = input$text1,
                     ggtheme = mytheme1) +
      theme(legend.spacing.y = unit(input$slider7, 'cm')) +
      geom_text(label = dt$number_of_out, vjust = -0.5, colour = "black",size = input$slider8,family = input$t8,fontface=input$f8)+
      guides(fill = guide_legend(byrow = TRUE))+ theme(axis.text.x = element_text(colour = c(rep(mycolor[1],table(dt[,1])[1]),rep(mycolor[2],table(dt[,1])[2]),rep(mycolor[3],table(dt[,1])[3])),angle = input$slider6,vjust = 1,hjust = 1))
    ggpar(p1,legend.title = "")
  },height=input$h, width=input$w)}})
  #棒状图
  
  
  observeEvent(input$action,if(input$select == "2"){ datapath2 <- input$file2$datapath
  dt<-fread(datapath2)
  head(dt,10)
  
  top.mar=0.5
  right.mar=0.5
  bottom.mar=0.2
  left.mar=0.2
  mytheme2<-theme_classic()+
    theme(text=element_text(family = input$tbz1,colour ="gray30",size = input$bz1,face = input$fbz1),
          plot.title = element_text(face = input$fbz2, colour = "black", size = input$bz2, hjust = 0.4, vjust = 3,family = input$tbz3),
          axis.line = element_blank(),
          axis.ticks = element_line(size = 0.6,colour = "gray30"),
          axis.text.y = element_text(family = input$tbzx1, size = input$bzx1, color="black",angle=0,vjust=0.5,face = input$fbzx1),
          legend.text = element_text(family = input$tbzx, size = input$bzx, color = "black",face = input$fbzx),
          axis.ticks.length = unit(1.5,units = "mm"),
          axis.title.y = ggplot2::element_text(size = input$bz3,face=input$fbz3,vjust = 3,family = input$tbz3),
          plot.margin=unit(x=c(top.mar,right.mar,bottom.mar,left.mar),
                           units="inches"))
  mycolor <- c(input$color4,input$color5,input$color6)
  scolor <- mycolor[3:5]
  output$p1 <- renderPlot({
    p1 <<- ggdotchart(dt, x = "GO_Term", y = "number_of_out",
                      color = "GO_Class",
                      dot.size=input$bz4,
                      palette = mycolor,
                      add = "segments",
                      sorting = "descending",
                      group ="GO_Class",
                      label = "number_of_out",
                      font.label = list(color = "white",face = input$fbz10,size = input$bz10,vjust = 0.5,family=input$tbz10),
                      xlab = "",
                      ylab = "Gene Number",
                      x.text.col=TRUE,
                      add.params = list(color = "GO_Class", size = input$bz5),
                      title = input$text2,
                      
                      ggtheme = mytheme2) +
      theme(axis.text.x = element_text(angle = input$bz9,vjust = 1,hjust = 1))
    
    
    ggpar(p1,legend.title = "")},height = input$h, width = input$w)})
  
  #气泡图
  observeEvent(input$action, if(input$select == "3"){
    datapath3 <- input$file3$datapath
    e2<-fread(datapath3)
    output$p1 <- renderPlot({
      
      p1 <<- ggplot2::ggplot(e2,aes(x = Richfactor,y = GO_desc,color = Corrected_P,size = Expressed_GO))+
        geom_point()+
        scale_size(range=c(1, 8))+
        scale_color_gradient2(low = input$color7,mid = input$color8,trans="log10")+
        theme_bw()+ ggtitle(input$text3)+
        xlab("Gene Ratio")+ylab("")+ 
        labs(color="pvalue",size = "Gene_number") + 
        guides(color = guide_colourbar(order = 2),size = guide_legend(order = 1)) +
        ggplot2::scale_x_continuous(limits = c(0, 0.75), breaks = c(0,0.25,0.5,0.75)) +
        ggplot2::scale_y_discrete(labels=function(x) stringr::str_wrap(x, width=60)) +
        ggplot2::theme(plot.title = element_text(face = input$fqp1,family = input$tqp1, colour = "black", size = input$qp1, hjust = 0.9, vjust = 3),
                       axis.title=element_text(size=input$qp2,colour = 'black',family = input$tqp2,face = input$fqp2),
                       
                       axis.text=element_text(size=input$qp3,colour = 'black',face = input$fqp3,family = input$tqp3),
                       axis.line = element_line(linewidth=0.5, colour = 'black'),
                       panel.background = element_rect(color='black'),
                       legend.key = element_blank())+
        ggplot2::theme(legend.title=element_text(size=input$qp4,family = input$tqp4,face = input$fqp4), 
                       legend.text = element_text(size=input$qp5,family = input$tqp5,face = input$fqp5),
                       axis.text.x = element_text(size=input$qp8,color="black",angle=0,vjust=2,family = input$tqp8,face = input$fqp8),
                       text = ggplot2::element_text(family = "sans"),
                       plot.margin = unit( c(0.8, 0.8, 0.8, 0),"cm")
        )
      ggpar(p1, legend.title = (""))
      
      
    }, height = input$h, width = input$w)
  })
  
  observe({
    
    ## *** Download PDF file ***
    output$downloadpdf <- downloadHandler(
      filename = function(){
        paste("plot.pdf")},
      content <- function(file){
        pdf(file, height = input$w, width = input$h)
        print(p1)
        dev.off()
      },contentType = "application/pdf"
    )
    ## *** Download SVG file ***
    output$downloadsvg <- downloadHandler(
      filename <- function(){ paste('plot.svg') },
      content <- function(file){
        svg(file,width = input$w,height = input$h)
        print(p1)
        dev.off()
      }, contentType = 'image/svg')
    ## *** Download example data ***
    output$Download1 <- downloadHandler(
      filename <- function() {
        paste('柱形图棒状图示例数据.txt')
      },
      content <- function(file) {
        input_file <- "柱形图棒状图示例数据.txt"
        example_dat <- read.table(input_file, head = T, as.is = T, sep = "\t", quote = "")
        write.table(example_dat, file = file, row.names = F, quote = F, sep = "\t")
      }, contentType = 'text/csv')
    ## *** Download example data ***
    output$Download2 <- downloadHandler(
      filename <- function() {
        paste('柱形图棒状图示例数据.txt')
      },
      content <- function(file) {
        input_file <- "柱形图棒状图示例数据.txt"
        example_dat <- read.table(input_file, head = T, as.is = T, sep = "\t", quote = "")
        write.table(example_dat, file = file, row.names = F, quote = F, sep = "\t")
      }, contentType = 'text/csv')
    ## *** Download example data ***
    output$Download3 <- downloadHandler(
      filename <- function() {
        paste('气泡图示例数据.txt')
      },
      content <- function(file) {
        input_file <- "气泡图示例数据.txt"
        example_dat <- read.table(input_file, head = T, as.is = T, sep = "\t", quote = "")
        write.table(example_dat, file = file, row.names = F, quote = F, sep = "\t")
      }, contentType = 'text/csv')
  })}