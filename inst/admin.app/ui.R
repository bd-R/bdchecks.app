library(shiny)
library(shinydashboard)
library(yaml)
library(shinyjs)

shinyUI(fluidPage(

    dashboardPage(
        dashboardHeader(title = "bdchecks Admin"),
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
