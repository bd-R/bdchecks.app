# Module UI

#' @title   mod_configure_checks_ui and mod_configure_checks_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_configure_checks
#'
#' @keywords internal
#' @export
#' @import shiny bdchecks
mod_configure_checks_ui <- function(id) {
  ns <- NS(id)
  
  darwinCoreClass <- get_dc_groups("DarwinCoreClass")
  
  components <- list()
  
  for (check in bdchecks::data.checks@dc_body) {
    components[[length(components) + 1]] <- tagList(
    div(
      class = paste("element-item checksListContent", darwinCoreClass[check@name, ]$group),
      
      HTML(
        paste(
          "<input type=checkbox name=", ns("typeInput"), " value=", check@name, ">"
        )
      ),
      
      fluidRow(column(6, div(h4(check@name), class = "leftSide")), column(6, div("", class = "rightSide"))), 
      
      conditionalPanel(
        "input['bdChecksConfigure-showDetailed'] == true",
        div(
          fluidRow(
            div(class = "checksListTopic col-sm-4", p("Description: ")),
            div(class = "checksListTitle col-sm-8",
                p(check@information$description))
          ),
          
          fluidRow(
            div(class = "checksListTopic col-sm-4", p("Sample Passing Data: ")),
            div(class = "checksListTitle col-sm-8",
                p(paste(check@example$pass, check@example$input_pass)))
          ),
          
          fluidRow(
            div(class = "checksListTopic col-sm-4", p("Sample Failing Data: ")),
            div(class = "checksListTitle col-sm-8",
                p(paste(check@example$fail, check@example$input_fail)))
          ),
          
          fluidRow(
            div(class = "checksListTopic col-sm-4", p("Category of Check: ")),
            div(class = "checksListTitle col-sm-8",
                p(check@information$darwin_core_class))
          ),
          
          fluidRow(
            div(class = "checksListTopic col-sm-4", p("DWC Field Targetted: ")),
            div(class = "checksListTitle col-sm-8",
                p(check@input$target))
          ),
          
          fluidRow(
            div(class = "checksListTopic col-sm-4", p("Sorting Flags: ")),
            div(class = "checksListTitle col-sm-8",
                p(check@information$keywords))
          )
        )
      )
    ))
  }
  
  tagList(column(
    12,
    
    fluidRow(column(
      5,
      div(
        p("Filter Checks By: "),
        class = "btn-group btn-group-justified",
        "role" = "group",
        actionButton(
          "noFilter",
          class = "button is-checked ",
          label = "All",
          "data-sort-value" = "*"
        ),
        actionButton(
          "filterByLocation",
          class = "button is-checked ",
          label = "Location",
          "data-sort-value" = ".location"
        ),
        actionButton(
          "filterByTaxon",
          class = "button is-checked ",
          label = "Taxon",
          "data-sort-value" = ".taxon"
        ),
        actionButton(
          "filterByEvent",
          class = "button is-checked ",
          label = "Event",
          "data-sort-value" = ".event"
        ),
        actionButton(
          "filterByOccurrence",
          class = "button is-checked ",
          label = "Occurrence",
          "data-sort-value" = ".occurrence"
        ),
        actionButton(
          "filterByTerms",
          class = "button is-checked ",
          label = "Record-level Terms",
          "data-sort-value" = ".record-level_terms"
        )
      )
    ),
    column(
      6,
      column(
        3,
        p("Show Detailed View: "),
        checkboxInput(ns("showDetailed"), label = "", value = FALSE)
      )
      ,
      column(
        4,
        p("Quick Options:"),
        actionButton(
          ns("all"),
          label = "Select All"
        ),
        actionButton(
          ns("none"),
          label = "Deselect All"
        )
      )
     
    )),
    
    div(id = ns("typeInput"),
        class = "form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input grid",
        components)
    
  ))
}

# Module Server

#' @rdname mod_configure_checks
#' @export
#' @import shinyjs
#' @keywords internal
mod_configure_checks_server <- function(input, output, session) {
  ns <- session$ns
  classes <- get_dc_groups("DarwinCoreClass")
  values <- NULL
  
  returnData <- character()
  
  observeEvent(input$typeInput, {
    returnData <<- input$typeInput
  })
  
  
  observeEvent(input$showDetailed, {
    shinyjs::runjs("$grid.isotope( 'reloadItems' ).isotope();")
  })
  
  observeEvent(input$all, {
    if (input$currentSort == "All" ||
        input$currentSort == "AllLocationTaxonEventOccurrenceRecord-level Terms") {
      names <- names(bdchecks::data.checks@dc_body)
    } else {
      names  <-
        as.character(classes[classes$groupName == tolower(input$currentSort), 1])
    }
    
    if (!is.null(values)) {
      names <- union(names, values)
    }
    values <<- names
    
    updateCheckboxGroupInput(
      session,
      "typeInput",
      selected = names
    )
  })
  
  observeEvent(input$none, {
    if (input$currentSort == "All" ||
        input$currentSort == "AllLocationTaxonEventOccurrenceRecord-level Terms") {
      values <<- NA
    } else {
      values  <<-
        setdiff(values,
                as.character(classes[classes$groupName == tolower(input$currentSort), 1]))
      if (length(values) == 0) {
        values <<- NA
      }
    }
    
    updateCheckboxGroupInput(
      session,
      "typeInput",
      selected = values
    )
  })
  
  returnDataReact <- reactive({
    # Input actions that need to trigger new dataframe return
    input$typeInput
    
    returnData
  })
  
  
  return(returnDataReact)
  
}
