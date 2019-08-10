#
# init.R
# shiny-heroku
#
# Created by blakiseskream on 5/7/2018
# MIT License and shit
#

my_packages = c('showtext', "shiny", 'tidyverse', 'rlang', 'shinythemes', 'shinyjs', 'shinydashboard',
                'tidyverse','googlesheets','openxlsx','psych','clipr','formattable', 'shinyWidgets', 'DT',
                'data.table', "magrittr", 'reshape2', 'lubridate', 'stringr', 'dplyr', 'tidyr')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
