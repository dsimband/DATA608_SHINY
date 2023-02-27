library(shiny)
library(dplyr)
library(ggplot2)
library('rsconnect')
library(plotly)



rsconnect::setAccountInfo(name='dsimband',
                          token='460951A4EBE9DB7AAE9C58CD497A8B59',
                          secret='AhKb4Rzy/NVTlfM07lIwJ14LBchri4eqpiE4NPt0')



rsconnect::deployApp('./m3_viz')

