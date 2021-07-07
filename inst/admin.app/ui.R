library(shiny)
library(shinydashboard)
library(yaml)
library(shinyjs)

shinyUI(fluidPage(

    dashboardPage(
        dashboardHeader(title = "bdchecks Admin", tags$li(class = "dropdown", actionButton("setting", "", icon = icon("gears"), style='padding:10px; font-size:125%'))),
        dashboardSidebar(sidebarMenuOutput("sideBar_menu_UI"), width = 400),
        dashboardBody(
            tags$head(
                tags$link(
                    rel = "stylesheet",
                    type = "text/css",
                    href = "style.css"
                )
            ),
            useShinyjs(),
            uiOutput("tab")
        )
    )
))
