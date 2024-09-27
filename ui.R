library(ggplot2)
library(ggpubr)
library(dplyr)
library(data.table)
library(colourpicker)
library(markdown)
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
        downloadButton("Download1", "示例数据"),
        br(),
        br(),
        textInput("text1", "标题:",
                  value = '柱状图'),
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
            radioButtons(
              "t2",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "t1",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tx1",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tx",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "t3",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "t8",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
                       "示例数据"),
        br(),
        br(),
        textInput("text2", "标题:",
                  value = c("棒状图")),
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
            radioButtons(
              "tbz2",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tbz1",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tbzx1",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tbzx",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tbz3",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tbz10",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
                       "示例数据"),
        br(),
        br(),
        textInput("text3", "标题:",
                  value = c("气泡图")),
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
            radioButtons(
              "tqp1",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tqp2",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tqp8",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tqp3",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tqp4",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
            radioButtons(
              "tqp5",
              label = h4("字体样式"),
              choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
            ),
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
      numericInput("h", "图形高度", value = "700"),
      numericInput("w", "图形宽度", value = "900"),
      actionButton("action",
                   label = "提交")
    ),
    mainPanel(
      downloadButton("downloadpdf", "PDF 下载"),
      downloadButton("downloadsvg", "SVG 下载"),
      plotOutput("p1")#,width = "60%", height = "700px"
    )
  ),
  tabPanel("Help",
           includeMarkdown("README.md"))
)