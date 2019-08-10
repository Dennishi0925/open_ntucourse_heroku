pkgs <- c('showtext', "shiny", 'tidyverse', 'rlang', 'shinythemes', 'shinyjs', 'shinydashboard',
          'tidyverse','googlesheets','openxlsx','psych','clipr','formattable', 'shinyWidgets', 'DT',
          'data.table', "magrittr", 'reshape2', 'lubridate', 'stringr', 'dplyr', 'tidyr')
lapply(pkgs, require, character.only = TRUE)
showtext_auto()
#font_add("jh", "msjh.ttc")
options(shiny.usecairo = FALSE)
# setwd("/Users/dtseng02/Documents/Dennis/ntucourse")

# jie = c(1:10, "A","B","C")
# day = c("一","二","三","四","五","六")
# jie
# day
# gg = vector()
# for (i in 1:6) {
#   for (j in 1:13) {
#     gg <- c(gg, str_c(day[i], jie[j]))
#   }
# }
# gg %>% as_tibble() %>%
#   mutate(expr_value = str_c("'",value, "'=", "'",value, "'")) %>%
#   pull(expr_value) %>% str_c(collapse = ", ")# %>% write_clip()
aaaaa = 5
bbbbb = 6
table_tidy_final <- read_rds("table_tidy_final.rds")
table_index <- table_tidy_final %>% select(matches("課程_"))
table_teacher <- table_tidy_final %>% select(課程_ID, matches("老師_"))
table_detail <- table_tidy_final %>% select(課程_ID, 課程_授課教師, 課程_名稱, matches("詳細_"))

# Define UI for application that plots features of movies
library(shiny)
library(plotly)
library(DT)
library(shinythemes)

# table_index <- readRDS("table_index.rds")
# table_index %>% select(matches("授課對象"), everything()) %>%
#   distinct(課程_授課對象) %>% count()
#   filter(str_detect(課程_授課對象_A, '系'))
# table_index %>% get_colnames()
table_index_print <- table_index %>%
  mutate(課程_授課對象 = as.factor(課程_授課對象)) %>%
  mutate(課程_學分 = as.factor(課程_學分)) %>%
  select(課程_ID, 課程_名稱, 課程_流水號, 課程_授課對象, 課程_課號, 課程_班次, 課程_學分, 課程_必_選修, 課程_授課教師, 課程_加選方式, 課程_時間教室, 課程_總人數, 課程_選課限制條件)
table_detail_print <- table_detail %>% select(-詳細_schedule) 

# statedata <- read_rds("statedata.rds")

mapcolors <- c("#808080", "#B2182B", "#8D2B4B", "#693F6B", "#45528B", "#2166AC")
#colnames(table_index_print)
columns <- colnames(table_index_print)

getdeps <- function() {
  htmltools::attachDependencies(
    htmltools::tagList(),
    c(
      htmlwidgets:::getDependency("datatables","DT")
    )
  )
}

transpose_df <- function(df) {
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_data_frame(x = .)
  return(t_df)
}

ui <- fluidPage(theme = shinytheme("yeti"),
                #tags$head(includeScript("google_analytics.js")),
                titlePanel("Open NTU Course Selection"),
                tabsetPanel(type = "tabs",
                            #tabPanel("Map", align="center", plotlyOutput("statebins", width = "900px", height = "600px")),
                            tabPanel("Searchable Table",
                                     fluidPage(
                                       checkboxGroupButtons(inputId = "必選修", 
                                                            label = "Label",
                                                            choices = c("必" = "必",
                                                                        "選" = "選"),
                                                            selected = c("必", "選"),
                                                            checkIcon = list(
                                                              yes = icon("ok",
                                                                         lib = "glyphicon")
                                                            )
                                       ),
                                       checkboxGroupButtons(inputId = "星期一", 
                                                            label = NULL,
                                                            choices  = c('一1'='一1', '一2'='一2', '一3'='一3', '一4'='一4', '一5'='一5', '一6'='一6', '一7'='一7', '一8'='一8', '一9'='一9', '一10'='一10', '一A'='一A', '一B'='一B', '一C'='一C'),
                                                            selected = c('一1'='一1', '一2'='一2', '一3'='一3', '一4'='一4', '一5'='一5', '一6'='一6', '一7'='一7', '一8'='一8', '一9'='一9', '一10'='一10', '一A'='一A', '一B'='一B', '一C'='一C'),
                                                            size =  'xs',
                                                            checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                            )
                                       ),
                                       checkboxGroupButtons(inputId = "星期二", 
                                                            label = NULL,
                                                            choices  = c('二1'='二1', '二2'='二2', '二3'='二3', '二4'='二4', '二5'='二5', '二6'='二6', '二7'='二7', '二8'='二8', '二9'='二9', '二10'='二10', '二A'='二A', '二B'='二B', '二C'='二C'),
                                                            selected = c('二1'='二1', '二2'='二2', '二3'='二3', '二4'='二4', '二5'='二5', '二6'='二6', '二7'='二7', '二8'='二8', '二9'='二9', '二10'='二10', '二A'='二A', '二B'='二B', '二C'='二C'),
                                                            size =  'xs',
                                                            checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                            )
                                       ),
                                       checkboxGroupButtons(inputId = "星期三", 
                                                            label = NULL,
                                                            choices  = c('三1'='三1', '三2'='三2', '三3'='三3', '三4'='三4', '三5'='三5', '三6'='三6', '三7'='三7', '三8'='三8', '三9'='三9', '三10'='三10', '三A'='三A', '三B'='三B', '三C'='三C'),
                                                            selected = c('三1'='三1', '三2'='三2', '三3'='三3', '三4'='三4', '三5'='三5', '三6'='三6', '三7'='三7', '三8'='三8', '三9'='三9', '三10'='三10', '三A'='三A', '三B'='三B', '三C'='三C'),
                                                            size =  'xs',
                                                            checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                            )
                                       ),
                                       checkboxGroupButtons(inputId = "星期四", 
                                                            label = NULL,
                                                            choices  = c('四1'='四1', '四2'='四2', '四3'='四3', '四4'='四4', '四5'='四5', '四6'='四6', '四7'='四7', '四8'='四8', '四9'='四9', '四10'='四10', '四A'='四A', '四B'='四B', '四C'='四C'),
                                                            selected = c('四1'='四1', '四2'='四2', '四3'='四3', '四4'='四4', '四5'='四5', '四6'='四6', '四7'='四7', '四8'='四8', '四9'='四9', '四10'='四10', '四A'='四A', '四B'='四B', '四C'='四C'),
                                                            size =  'xs',
                                                            checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                            )
                                       ),
                                       checkboxGroupButtons(inputId = "星期五", 
                                                            label = NULL,
                                                            choices  = c('五1'='五1', '五2'='五2', '五3'='五3', '五4'='五4', '五5'='五5', '五6'='五6', '五7'='五7', '五8'='五8', '五9'='五9', '五10'='五10', '五A'='五A', '五B'='五B', '五C'='五C'),
                                                            selected = c('五1'='五1', '五2'='五2', '五3'='五3', '五4'='五4', '五5'='五5', '五6'='五6', '五7'='五7', '五8'='五8', '五9'='五9', '五10'='五10', '五A'='五A', '五B'='五B', '五C'='五C'),
                                                            size =  'xs',
                                                            checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                            )
                                       ),
                                       checkboxGroupButtons(inputId = "星期六", 
                                                            label = NULL,
                                                            choices  = c('六1'='六1', '六2'='六2', '六3'='六3', '六4'='六4', '六5'='六5', '六6'='六6', '六7'='六7', '六8'='六8', '六9'='六9', '六10'='六10', '六A'='六A', '六B'='六B', '六C'='六C'),
                                                            selected = c('六1'='六1', '六2'='六2', '六3'='六3', '六4'='六4', '六5'='六5', '六6'='六6', '六7'='六7', '六8'='六8', '六9'='六9', '六10'='六10', '六A'='六A', '六B'='六B', '六C'='六C'),
                                                            size =  'xs',
                                                            checkIcon = list(yes = icon("ok",lib = "glyphicon")
                                                            )
                                       ),
                                       column(4,
                                              selectizeInput("課程_學分", "Choose 課程_學分",
                                                             c("All"= "All", levels(table_index_print$課程_學分)),
                                                             multiple = TRUE,
                                                             selected = "3",
                                                             width = '200px'
                                              )),
                                       column(4,
                                              selectizeInput("課程_授課對象", "Choose State(s)",
                                                             c("All"= "All", levels(table_index_print$課程_授課對象)),
                                                             multiple = TRUE,
                                                             selected = "工管系",
                                                             width = '200px'
                                              )),
                                       column(12,
                                              DTOutput("data" )))
                            ),
                            # tabPanel("Course Info", align="center", numericInput("length","Length",0,0,10)
                            #          ),
                            # tabPanel("Course Info", inputPanel(
                            #   numericInput("課程_ID2", label = "課程ID:", 0, min(table_index_print$課程_ID), max(table_index_print$課程_ID))
                            # ),
                            tabPanel("Course Info", textOutput("page2"),
                                     #numericInput("課程_ID2", label = "課程ID:", 0, min(table_index_print$課程_ID), max(table_index_print$課程_ID)),
                                     DT::dataTableOutput("table2"),getdeps()
                                     ),
                            
                            #textOutput("page2", DT::dataTableOutput("table2"),getdeps()),
                            tabPanel("About",
                                     fluidPage(
                                       column(10,
                                              h3("An App to View NTU Course Data More Easily"),
                                              p("I created this app to improve students' experiences when viewing course data",
                                                a("a list",
                                                  href = "http://cawp.rutgers.edu/buzz-2018-potential-women-candidates-us-congress-and-statewide-elected-executive",
                                                  target = "_blank"),
                                                "of women potentially running for US Congress and State offices. The data are current as of 6/20/18."),
                                              p(a("R", href = "https://www.r-project.org", target = "_blank"),
                                                "code and details of data processing and visualization are available on",
                                                a("GitHub.", icon("github"), href = "https://github.com/JListman/Scrape_WomenRunning_CAWP", target = "_blank"),
                                                "Find me, Jenny Listman, on", a("Twitter", icon("twitter"),
                                                                                href = "https://twitter.com/jblistman", target = "_blank"),
                                                a("LinkedIn", icon("linkedin"),
                                                  href = "https://www.linkedin.com/in/jenniferlistman/", target = "_blank"),
                                                "or read more about my data science and consulting work on my",
                                                a("website.", href = "https://twitter.com/jblistman", target = "_blank")),
                                              HTML("<br><br><br>")
                                              #)
                                       )
                                     )
                            ),
                            tags$div(class="footer", checked=NA, tags$p("An interactive app to view course data powered by ",
                                                                        a("Dennis", href = "http://www.cawp.rutgers.edu", target = "_blank")),
                                     tags$p(a("View code",icon("github"), href = "https://github.com/JListman/Scrape_WomenRunning_CAWP", target = "_blank"))
                            )
                )
)

server <- function(input, output) {
  
  output$data <- renderDataTable({
    weekday = c(input$星期一,input$星期二,input$星期三,input$星期四,input$星期五,input$星期六)

    if ("All" %in% input$課程_學分 & "All" %in% input$課程_授課對象)
      datatable(data.frame(table_index_print  %>%
                             mutate(課程_ID = paste0("<a href='#table2'", "alt='",課程_ID,                                    
                                                   "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('課程_ID', getAttribute('alt'));\">",
                                                   課程_ID,"</a>"))
      ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
    else if ("All" %in% input$課程_學分 & input$課程_授課對象 != "All")
      datatable(data.frame(table_index_print %>% filter(課程_授課對象 %in% input$課程_授課對象)  %>%
                             mutate(課程_ID = paste0("<a href='#table2'", "alt='",課程_ID,                                    
                                                   "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('課程_ID', getAttribute('alt'));\">",
                                                   課程_ID,"</a>"))
      ),filter = list(position = 'top', clear = FALSE), escape = FALSE)# %>% arrange(desc(college))
    else if (input$課程_學分 != "All" & "All" %in% input$課程_授課對象)
      datatable(data.frame(table_index_print %>% filter(課程_學分 %in% input$課程_學分) %>%
                             mutate(課程_ID = paste0("<a href='#table2'", "alt='",課程_ID,                                    
                                                   "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('課程_ID', getAttribute('alt'));\">",
                                                   課程_ID,"</a>"))
      ),filter = list(position = 'top', clear = FALSE), escape = FALSE)# %>% arrange(desc(college))
    else
      datatable(data.frame(table_index_print %>%
                             filter(str_detect(課程_必_選修,str_c(input$必選修, collapse = "|"))) %>%#weekday
                             filter(str_detect(課程_時間教室,str_c(weekday, collapse = "|"))) %>%#weekday
                             # filter(str_detect(課程_時間教室,str_c(input$星期一, collapse = "|"))) %>%
                             # filter(str_detect(課程_時間教室,str_c(input$星期二, collapse = "|"))) %>%
                             # filter(str_detect(課程_時間教室,str_c(input$星期三, collapse = "|"))) %>%
                             # filter(str_detect(課程_時間教室,str_c(input$星期四, collapse = "|"))) %>%
                             # filter(str_detect(課程_時間教室,str_c(input$星期五, collapse = "|"))) %>%
                             # filter(str_detect(課程_時間教室,str_c(input$星期六, collapse = "|"))) %>%
                             filter(課程_學分 %in% input$課程_學分) %>% filter(課程_授課對象 %in% input$課程_授課對象) %>%
                             mutate(課程_ID = paste0("<a href='#table2'", "alt='",課程_ID,                                    
                                                   "'onclick=\"tabs = $('.tabbable .nav.nav-tabs li');tabs.each(function() {$(this).removeClass('active')});$(tabs[1]).addClass('active');tabsContents = $('.tabbable .tab-content .tab-pane');tabsContents.each(function() {$(this).removeClass('active')});$(tabsContents[1]).addClass('active');$('#table2').trigger('change').trigger('shown');Shiny.onInputChange('課程_ID', getAttribute('alt'));\">",
                                                   課程_ID,"</a>"))
      ),filter = list(position = 'top', clear = FALSE), escape = FALSE)
  }
  #,escape = FALSE
  
  
  # callback = JS(
  #   'table.on("click.dt", "tr", function() {
  #   tabs = $(".tabbable .nav.nav-tabs li a");
  #   $(tabs[1]).click();})'
  # )
  
  #   callback=JS(
  #     'table.on("click.dt", "tr", function() {
  #     
  #     tabs = $(".tabbable .nav.nav-tabs li a");
  #     var data=table.row(this).data();
  #     document.getElementById("課程_ID").value=data[1];
  #     Shiny.onInputChange("課程_ID",data[1]);
  #     $(tabs[1]).click();
  #     table.row(this).deselect();
  # })'
  #                                       )
  )
  #var data=table.row(this).data();
  #var data=table.cell(".selected", 0).data();
  #['title']
  output$table2 <- DT::renderDataTable({
    # selected <- input$data_rows_selected
    # if(is.null(selected)){
    #   datatable(table_detail_print)
    # } else {
    #   datatable(table_detail_print %>% filter(課程_ID==input$課程_ID) %>% select(-課程_ID) %>% transpose_df())
    # }
    # 
    if(!is.null(input$課程_ID)){
      datatable(table_detail_print %>% filter(課程_ID==input$課程_ID) %>% select(-課程_ID) %>% transpose_df())
    # }else if(!is.null(input$課程_ID2)){
    #   datatable(table_detail_print)
    }else {
      datatable(table_detail_print)
    }
  })
  
  output$page2 <- renderText({
    #print(input$課程_ID)
    course_name_print = table_index_print %>% filter(課程_ID == input$課程_ID) %>% pull(課程_名稱)
    paste0("Detailed course information for  ", course_name_print, " :")

  })
  
}
# table_detail_print %>% filter(課程_ID == 8800)
shinyApp(ui = ui, server = server)

###尚缺的功能
#標籤

#PTT search
#https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
#這篇是有google search的link預覽



#################################################
# library(shiny)
# library(dplyr)
# ## Create item pricing data
# set.seed(1234)
# init_items = function() {
#   item.id=1:1000
#   ensemble.id=rep(1:100,each=10)
#   cost=round(runif(1000,10,100), 2)
#   profit=round(cost*runif(1000,0.01,0.15), 2)
#   price=cost+profit
#   
#   data.frame(item.id, ensemble.id, cost, price, profit)
# }
# items = init_items()
# 
# ## Create ensemble pricing data
# init_ensembles = function(items) {
#   items %>% group_by(ensemble.id) %>% summarize_each(funs(sum), cost, price, profit)
# }
# ensembles = init_ensembles(items)
# 
# ## Attach dependencies
# ## https://github.com/timelyportfolio/functionplotR/issues/1#issuecomment-224369431
# getdeps <- function() {
#   htmltools::attachDependencies(
#     htmltools::tagList(),
#     c(
#       htmlwidgets:::getDependency("datatables","DT")
#     )
#   )
# }
# 
# # Define UI for application
# ui <- shinyUI(fluidPage(
#   tabsetPanel(#id="Linked Table Test",
#     tabPanel("Page 1", DT::dataTableOutput("page1")),
#     tabPanel("Page 2", inputPanel(
#       numericInput("ensemble.id", label = "Ensemble ID:", 0, min(ensembles$ensemble.id), max(ensembles$ensemble.id))
#     ),
#     textOutput("page2"), DT::dataTableOutput("table2"),getdeps())
#   )
# ))
# 
# # Define server logic
# server <- shinyServer(function(input, output, session) {
#   output$page1 <- DT::renderDataTable(ensembles, rownames = FALSE,
#                                       callback=JS(
#                                         'table.on("click.dt", "tr", function() {
#                                         
#                                         tabs = $(".tabbable .nav.nav-tabs li a");
#                                         var data=table.row(this).data();
#                                         document.getElementById("ensemble.id").value=data[0];
#                                         Shiny.onInputChange("ensemble.id",data[0]);
#                                         $(tabs[1]).click();
#                                         table.row(this).deselect();
# })'                     
#                                       ))
#   
#   
#   output$table2 <- DT::renderDataTable(items %>% filter(ensemble.id==input$ensemble.id) %>% select(-ensemble.id), rownames = FALSE)
#   
#   output$page2 <- renderText({
#     print(input$ensemble.id)
#     paste0("Detailed pricing information for ensemble #",input$ensemble.id,":")
#   })
#   })
# 
# 
# # Run the application
# shinyApp(ui = ui, server = server)