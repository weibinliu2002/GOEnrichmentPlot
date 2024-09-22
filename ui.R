library(ggplot2)
library(ggpubr)
library(dplyr)
library(data.table)
library(colourpicker)
library(markdown)
# library(showtext)
# font <-
#   font_files() %>% select('file', 'family', 'face') %>% filter(face == 'Bold' |
#                                                                  face == 'Bold Italic' |
#                                                                  face == 'Italic' |
#                                                                  face == 'Regular')  %>% unique()
# fonts <- data.frame(table(font$family))
# %>%filter(Freq>=4)
# fonts
# list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei") <-
#   unlist(windowsFonts()) %>% data.frame() %>% row.names() %>% append(as.character(fonts$Var1))
# list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
# for (i in fonts$Var1) {
#   a <- filter(font, family == i)
#   # 对于 Regular 样式，确保它总是有一个文件路径
#   if (length(which(a$face == 'Regular')) == 1) {
#     regular1 <- a$file[which(a$face == 'Regular')]
#   } else {
#     # 可以在这里添加错误处理或提示信息
#     # stop("Regular 字体样式不存在或存在多个。")
#   }
#   # 对于 Bold 样式
#   if (length(which(a$face == 'Bold')) == 1) {
#     bold1 <- a$file[which(a$face == 'Bold')]
#   } else {
#     bold1 <- NULL
#   }
#   # 对于 Italic 样式
#   if (length(which(a$face == 'Italic')) == 1) {
#     italic1 <- a$file[which(a$face == 'Italic')]
#   } else {
#     italic1 <- NULL
#   }
#   # 对于 Bold Italic 样式
#   if (length(which(a$face == 'Bold Italic')) == 1) {
#     bolditalic1 <- a$file[which(a$face == 'Bold Italic')]
#   } else {
#     bolditalic1 <- NULL
#   }
#   font_add(
#     family = i,
#     regular = regular1,
#     bold =  bold1,
#     italic =  italic1,
#     bolditalic =  bolditalic1
#   )
# }
# showtext_auto(enable = TRUE)
ui <- navbarPage(
  title = "GO Enrichment Plot",
  tabPanel(
    "GO Enrichment Plot",
    sidebarPanel(
      selectInput(
        "select",
        label = "图形样式",
        choices = list(
          "柱状图" = 1,
          "棒状图" = 2,
          "气泡图" = 3
        )
      ),
      conditionalPanel(
        condition = "input.select == '1'",
        fileInput("file1", label = "选择文件", accept = ".txt"),
        downloadButton("Download1", "Download example data"),
        br(),
        br(),
        textInput("text1", "标题:",
                  value = c("")),
        h3('图形参数'),
        checkboxInput("condition1", "字体", FALSE),
        conditionalPanel(
          condition = "input.condition1",
          selectInput(
            "select1",
            label = "字体位置",
            choices = list(
              "标题" = 1,
              "横坐标" = 2,
              "纵坐标" = 3,
              "图注" = 4,
              "纵坐标标题" = 5,
              "数据标签" = 6
            )
          ),
          conditionalPanel(
            condition = "input.select1 == '1'",
            radioButtons(
              "f2",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("t2", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "slider2",
              label = "标题",
              min = 1,
              max = 40,
              value = 15
            )
          ),
          conditionalPanel(
            condition = "input.select1 == '2'",
            radioButtons(
              "f1",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("t1", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "slider1",
              label = "横坐标字体大小",
              min = 5,
              max = 40,
              value = 10
            )
          ),
          conditionalPanel(
            condition = "input.select1 == '3'",
            radioButtons(
              "fx1",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tx1", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "sliderx1",
              label = "纵坐标字体大小",
              min = 5,
              max = 40,
              value = 10
            )
          ),
          conditionalPanel(
            condition = "input.select1 == '4'",
            radioButtons(
              "fx",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tx", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "sliderx",
              label = "图注字体大小",
              min = 10,
              max = 40,
              value = 12
            )
          ),
          conditionalPanel(
            condition = "input.select1 == '5'",
            radioButtons(
              "f3",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("t3", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "slider3",
              label = "纵坐标标题字体大小",
              min = 1,
              max = 40,
              value = 10
            )
          ),
          conditionalPanel(
            condition = "input.select1 == '6'",
            radioButtons(
              "f8",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("t8", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "slider8",
              label = "标签字体大小",
              min = 1,
              max = 20,
              value = 5
            )
          )
        ),
        checkboxInput("condition12", "颜色", FALSE),
        conditionalPanel(
          condition = "input.condition12",
          colourInput("color1", label = "BP",
                      value = "#239BEB"),
          colourInput("color2", label = "CC",
                      value = "#16F7EC"),
          colourInput("color3", label = "MF",
                      value = "#6CF213")
        ),
        
        checkboxInput("condition13", "其他", FALSE),
        conditionalPanel(
          condition = "input.condition13",
          
          sliderInput(
            "slider6",
            label = "坐标轴字体倾斜度",
            min = 45,
            max = 90,
            value = 90,
            step = 1
          ),
          sliderInput(
            "slider7",
            label = "图例距离",
            min = 0,
            max = 1,
            value = 0.1,
            step = 0.1
          )
        )
      ),
      conditionalPanel(
        condition = "input.select == '2'",
        fileInput("file2", label = "选择文件", accept = ".txt"),
        downloadButton("Download2",
                       "Download example data"),
        br(),
        br(),
        textInput("text2", "标题:",
                  value = c("")),
        h3('图形参数'),
        checkboxInput("condition2", "字体", FALSE),
        conditionalPanel(
          condition = "input.condition2",
          selectInput(
            "select2",
            label = "字体位置",
            choices = list(
              "标题" = 1,
              "横坐标" = 2,
              "纵坐标" = 3,
              "图注" = 4,
              "纵坐标标题" = 5,
              "数据标签" = 6
            )
          ),
          conditionalPanel(
            condition = "input.select2 == '1'",
            radioButtons(
              "fbz2",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tbz2", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "bz2",
              label = "标题字体大小",
              min = 10,
              max = 30,
              value = 14
            )
          ),
          conditionalPanel(
            condition = "input.select2 == '2'",
            radioButtons(
              "fbz1",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tbz1", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "bz1",
              label = "横坐标字体大小",
              min = 10,
              max = 30,
              value = 12
            )
          ),
          conditionalPanel(
            condition = "input.select2 == '3'",
            radioButtons(
              "fbzx1",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tbzx1", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "bzx1",
              label = "纵坐标字体大小",
              min = 5,
              max = 25,
              value = 10
            )
          ),
          conditionalPanel(
            condition = "input.select2 == '4'",
            radioButtons(
              "fbzx",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tbzx", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "bzx",
              label = "图注字体大小",
              min = 10,
              max = 30,
              value = 12
            )
          ),
          conditionalPanel(
            condition = "input.select2 == '5'",
            radioButtons(
              "fbz3",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tbz3", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "bz3",
              label = "纵坐标标题字体大小",
              min = 10,
              max = 30,
              value = 15
            )
          ),
          conditionalPanel(
            condition = "input.select2 == '6'",
            radioButtons(
              "fbz10",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tbz10", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "bz10",
              label = "标签字体大小",
              min = 10,
              max = 30,
              value = 15
            )
          )
        ),
        checkboxInput("condition22", "颜色", FALSE),
        conditionalPanel(
          condition = "input.condition22 ",
          colourInput("color4", label = "BP",
                      value = "#239BEB"),
          colourInput("color5", label = "CC",
                      value = "#16F7EC"),
          colourInput("color6", label = "MF",
                      value = "#6CF213")
        ),
        checkboxInput("condition23", "其他", FALSE),
        conditionalPanel(
          condition = "input.condition23",
          sliderInput(
            "bz4",
            label = "棒球大小",
            min = 1,
            max = 20,
            value = 6
          ),
          sliderInput(
            "bz5",
            label = "棒棍大小",
            min = 1,
            max = 5,
            value = 1
          ),
          sliderInput(
            "bz9",
            label = "坐标轴字体倾斜度",
            min = 45,
            max = 90,
            value = 90,
            step = 1
          ),
        )
      ),
      conditionalPanel(
        condition = "input.select == '3'",
        fileInput("file3", label = "选择文件", accept = ".txt"),
        downloadButton("Download3",
                       "Download example data"),
        br(),
        br(),
        textInput("text3", "标题:",
                  value = c("")),
        h3('图形参数'),
        checkboxInput("condition3", "字体", FALSE),
        conditionalPanel(
          condition = "input.condition3",
          selectInput(
            "select3",
            label = "字体位置",
            choices = list(
              "标题" = 1,
              "横坐标轴标题" = 2,
              "横坐标" = 3,
              "纵坐标标题" = 4,
              "图例标题" = 5,
              "图例数字" = 6
            )
          ),
          conditionalPanel(
            condition = "input.select3 == '1'",
            radioButtons(
              "fqp1",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tqp1", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "qp1",
              label = "标题字体大小",
              min = 10,
              max = 40,
              value = 14
            )
          ),
          conditionalPanel(
            condition = "input.select3 == '2'",
            radioButtons(
              "fqp2",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tqp2", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "qp2",
              label = "横坐标轴标题字体大小",
              min = 10,
              max = 40,
              value = 14
            )
          ),
          conditionalPanel(
            condition = "input.select3 == '3'",
            radioButtons(
              "fqp8",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tqp8", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "qp8",
              label = "横坐标字体大小",
              min = 10,
              max = 40,
              value = 14
            )
          ),
          conditionalPanel(
            condition = "input.select3 == '4'",
            radioButtons(
              "fqp3",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tqp3", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "qp3",
              label = "纵坐标标题字体大小",
              min = 10,
              max = 40,
              value = 10
            )
          ),
          conditionalPanel(
            condition = "input.select3 == '5'",
            radioButtons(
              "fqp4",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tqp4", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "qp4",
              label = "图例标题字体大小",
              min = 10,
              max = 40,
              value = 14
            )
          ),
          conditionalPanel(
            condition = "input.select3 == '6'",
            radioButtons(
              "fqp5",
              label = h4("字体粗斜"),
              choices = list(
                "默认" = "plain",
                "粗体" = "bold",
                "斜体" = "italic",
                "粗斜体" = "bold.italic"
              )
            ),
            radioButtons("tqp5", label = h4("字体样式"),
                         choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")),
            sliderInput(
              "qp5",
              label = "图例数字大小",
              min = 5,
              max = 25,
              value = 10
            )
          )
        ),
        checkboxInput("condition32", "颜色", FALSE),
        conditionalPanel(
          condition = "input.condition32",
          colourInput("color7", label = "low",
                      value = "red"),
          colourInput("color8", label = "mid",
                      value = "yellow")
        )
      ),
      h4("图片下载"),
      numericInput("h", "Plot download height", value = "700"),
      numericInput("w", "Plot download width", value = "900"),
      actionButton("action",
                   label = "go")
    ),
    mainPanel(
      downloadButton("downloadpdf", "Download pdf-file"),
      downloadButton("downloadsvg", "Download svg-file"),
      plotOutput("p1")#,width = "60%", height = "700px")
    )
  ),
  tabPanel("Help",
           includeMarkdown("README1.md"))
)
server <- function(input, output) {
  #柱状图
  observeEvent(input$action, {
    if (input$select == "1") {
      datapath1 <- input$file1$datapath
      # 对数据进行分组排序
      dt <- fread(datapath1)
      dt <- dt %>%
        arrange(GO_Class, desc(number_of_out)) %>%
        mutate(GO_Term = factor(GO_Term, levels = unique(GO_Term)))
      count <- count(dt[, 1])
      top.mar = 0.5
      right.mar = 0.5
      bottom.mar = 0.2
      left.mar = 2
      mytheme1 <- theme_classic() +
        theme(
          plot.title = element_text(
            face = input$f2,
            colour = "black",
            size = input$slider2,
            hjust = 0.4,
            vjust = 3,
            family = input$t2
          ),
          axis.line = element_blank(),
          axis.ticks = element_line(size = 0.6, colour = "gray30"),
          axis.ticks.length = unit(1.5, units = "mm"),
          axis.title.y = ggplot2::element_text(
            size = input$slider3,
            face = input$f3,
            vjust = 3,
            family = input$t3
          ),
          axis.text.y = element_text(
            family = input$tx1,
            size = input$sliderx1,
            color = "black",
            angle = 0,
            vjust = 0.5,
            face = input$fx1
          ),
          axis.text.x = element_text(
            family = input$t1,
            size = input$slider1,
            color = "black",
            angle = 0,
            face = input$f1
          ),
          legend.text = element_text(
            family = input$tx,
            size = input$sliderx,
            color = "black",
            face = input$fx
          ),
          plot.margin = unit(
            x = c(top.mar, right.mar, bottom.mar, left.mar),
            units = "inches"
          )
        )
      print(dt)
      mycolor <- c(input$color1, input$color2, input$color3)
      scolor <- mycolor[3:5]
      print(dt)
      output$p1 <- renderPlot({
        p1 <<- ggbarplot(
          dt,
          x = "GO_Term",
          y = "number_of_out",
          fill = "GO_Class",
          dot.size = 6,
          palette = mycolor,
          add = "segments",
          sort.val = "desc",
          group = "GO_Class",
          xlab = "",
          ylab = "Gene Number",
          x.text.col = TRUE,
          add.params = list(color = "GO_Class", size = 1),
          title = input$text1,
          ggtheme = mytheme1
        ) +
          theme(legend.spacing.y = unit(input$slider7, 'cm')) +
          geom_text(
            label = dt$number_of_out,
            vjust = -0.5,
            colour = "black",
            size = input$slider8,
            family = input$t8,
            fontface = input$f8
          ) +
          guides(fill = guide_legend(byrow = TRUE)) + theme(axis.text.x = element_text(
            colour = c(
              rep(mycolor[1], table(dt[, 1])[1]),
              rep(mycolor[2], table(dt[, 1])[2]),
              rep(mycolor[3], table(dt[, 1])[3])
            ),
            angle = input$slider6,
            vjust = 1,
            hjust = 1
          ))
        ggpar(p1, legend.title = "")
      }, height = input$h, width = input$w)
    }
  })
  #棒状图
  observeEvent(input$action, if (input$select == "2") {
    datapath2 <- input$file2$datapath
    dt <- fread(datapath2)
    head(dt, 10)
    top.mar = 0.5
    right.mar = 0.5
    bottom.mar = 0.2
    left.mar = 0.2
    mytheme2 <- theme_classic() +
      theme(
        text = element_text(
          family = input$tbz1,
          colour = "gray30",
          size = input$bz1,
          face = input$fbz1
        ),
        plot.title = element_text(
          face = input$fbz2,
          colour = "black",
          size = input$bz2,
          hjust = 0.4,
          vjust = 3,
          family = input$tbz3
        ),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.6, colour = "gray30"),
        axis.text.y = element_text(
          family = input$tbzx1,
          size = input$bzx1,
          color = "black",
          angle = 0,
          vjust = 0.5,
          face = input$fbzx1
        ),
        legend.text = element_text(
          family = input$tbzx,
          size = input$bzx,
          color = "black",
          face = input$fbzx
        ),
        axis.ticks.length = unit(1.5, units = "mm"),
        axis.title.y = ggplot2::element_text(
          size = input$bz3,
          face = input$fbz3,
          vjust = 3,
          family = input$tbz3
        ),
        plot.margin = unit(
          x = c(top.mar, right.mar, bottom.mar, left.mar),
          units = "inches"
        )
      )
    mycolor <- c(input$color4, input$color5, input$color6)
    scolor <- mycolor[3:5]
    output$p1 <- renderPlot({
      p1 <<- ggdotchart(
        dt,
        x = "GO_Term",
        y = "number_of_out",
        color = "GO_Class",
        dot.size = input$bz4,
        palette = mycolor,
        add = "segments",
        sorting = "descending",
        group = "GO_Class",
        label = "number_of_out",
        font.label = list(
          color = "white",
          face = input$fbz10,
          size = input$bz10,
          vjust = 0.5,
          family = input$tbz10
        ),
        xlab = "",
        ylab = "Gene Number",
        x.text.col = TRUE,
        add.params = list(color = "GO_Class", size = input$bz5),
        title = input$text2,
        
        ggtheme = mytheme2
      ) +
        theme(axis.text.x = element_text(
          angle = input$bz9,
          vjust = 1,
          hjust = 1
        ))
      ggpar(p1, legend.title = "")
    }, height = input$h, width = input$w)
  })
  #气泡图
  observeEvent(input$action, if (input$select == "3") {
    datapath3 <- input$file3$datapath
    e2 <- fread(datapath3)
    output$p1 <- renderPlot({
      p1 <<-
        ggplot2::ggplot(e2,
                        aes(
                          x = Richfactor,
                          y = GO_desc,
                          color = Corrected_P,
                          size = Expressed_GO
                        )) +
        geom_point() +
        scale_size(range = c(1, 8)) +
        scale_color_gradient2(low = input$color7,
                              mid = input$color8,
                              trans = "log10") +
        theme_bw() + ggtitle(input$text3) +
        xlab("Gene Ratio") + ylab("") +
        labs(color = "pvalue", size = "Gene_number") +
        guides(color = guide_colourbar(order = 2), size = guide_legend(order = 1)) +
        ggplot2::scale_x_continuous(limits = c(0, 0.75),
                                    breaks = c(0, 0.25, 0.5, 0.75)) +
        ggplot2::scale_y_discrete(
          labels = function(x)
            stringr::str_wrap(x, width = 60)
        ) +
        ggplot2::theme(
          plot.title = element_text(
            face = input$fqp1,
            family = input$tqp1,
            colour = "black",
            size = input$qp1,
            hjust = 0.9,
            vjust = 3
          ),
          axis.title = element_text(
            size = input$qp2,
            colour = 'black',
            family = input$tqp2,
            face = input$fqp2
          ),
          
          axis.text = element_text(
            size = input$qp3,
            colour = 'black',
            face = input$fqp3,
            family = input$tqp3
          ),
          axis.line = element_line(linewidth = 0.5, colour = 'black'),
          panel.background = element_rect(color = 'black'),
          legend.key = element_blank()
        ) +
        ggplot2::theme(
          legend.title = element_text(
            size = input$qp4,
            family = input$tqp4,
            face = input$fqp4
          ),
          legend.text = element_text(
            size = input$qp5,
            family = input$tqp5,
            face = input$fqp5
          ),
          axis.text.x = element_text(
            size = input$qp8,
            color = "black",
            angle = 0,
            vjust = 2,
            family = input$tqp8,
            face = input$fqp8
          ),
          text = ggplot2::element_text(family = "sans"),
          plot.margin = unit(c(0.8, 0.8, 0.8, 0), "cm")
        )
      ggpar(p1, legend.title = (""))
    }, height = input$h, width = input$w)
  })
  observe({
    ## *** Download PDF file ***
    output$downloadpdf <- downloadHandler(
      filename = function() {
        paste("plot.pdf")
      },
      content <- function(file) {
        pdf(file, height = input$w, width = input$h)
        print(p1)
        dev.off()
      },
      contentType = "application/pdf"
    )
    ## *** Download SVG file ***
    output$downloadsvg <- downloadHandler(filename <-
                                            function() {
                                              paste('plot.svg')
                                            },
                                          content <- function(file) {
                                            svg(file, width = input$w, height = input$h)
                                            print(p1)
                                            dev.off()
                                          }, contentType = 'image/svg')
    ## *** Download example data ***
    output$Download1 <- downloadHandler(filename <- function() {
      paste('柱形图棒状图示例数据.txt')
    },
    content <- function(file) {
      input_file <- "柱形图棒状图示例数据.txt"
      example_dat <-
        read.table(
          input_file,
          head = T,
          as.is = T,
          sep = "\t",
          quote = ""
        )
      write.table(
        example_dat,
        file = file,
        row.names = F,
        quote = F,
        sep = "\t"
      )
    }, contentType = 'text/csv')
    ## *** Download example data ***
    output$Download2 <- downloadHandler(filename <- function() {
      paste('柱形图棒状图示例数据.txt')
    },
    content <- function(file) {
      input_file <- "柱形图棒状图示例数据.txt"
      example_dat <-
        read.table(
          input_file,
          head = T,
          as.is = T,
          sep = "\t",
          quote = ""
        )
      write.table(
        example_dat,
        file = file,
        row.names = F,
        quote = F,
        sep = "\t"
      )
    }, contentType = 'text/csv')
    ## *** Download example data ***
    output$Download3 <- downloadHandler(filename <- function() {
      paste('气泡图示例数据.txt')
    },
    content <- function(file) {
      input_file <- "气泡图示例数据.txt"
      example_dat <-
        read.table(
          input_file,
          head = T,
          as.is = T,
          sep = "\t",
          quote = ""
        )
      write.table(
        example_dat,
        file = file,
        row.names = F,
        quote = F,
        sep = "\t"
      )
    }, contentType = 'text/csv')
  })
}
shinyApp(ui = ui, server = server)