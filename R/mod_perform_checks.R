# Module UI

#' @title   mod_perform_checks_ui and mod_perform_checks_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_perform_checks
#'
#' @keywords internal
#' @export
#' @import shiny DT bdutilities
mod_perform_checks_ui <- function(id) {
  ns <- NS(id)
  tagList(column(12,
                 div(
                   id = "cleanPane",
                   h1("Perform Data Checks"),
          
                   column(
                     12,
                     fluidRow(column(4,
                                     h4("Records Submitted", class = "control-header"),
                                     div(class = "control-header stats-green", textOutput(ns("total")))),
                              column(4,
                                     h4("Data Checks Performed", class = "control-header"),
                                     div(class = "control-header stats-green", textOutput(ns("checks")))),
                              column(4,
                                     h4("Records With Filters", class = "control-header"),
                                     div(class = "control-header stats-green", textOutput(ns("filtered"))))),
                     
                     fluidRow(
                       column(
                         12,
                         actionButton(ns("perform"), "Perform Data Checks", width = "14%", class = "activeButton"),
                         actionButton(ns("passed"), "Select All Passed Records", width = "14%"),
                         actionButton(ns("failed"), "Select All Failed Records", width = "14%"),
                         actionButton(ns("missed"), "Select All Missing Records", width = "14%"),
                         actionButton(ns("clear"), "Clear Selections", width = "14%"),
                         actionButton(ns("accept"), "Filter Out Selected", width = "14%"),
                         downloadButton(ns("download"), "Download Data", width = "14%")
                       )
                       
                     ),
                     
                     br(),
                     
                     column(
                       12,
                       DT::dataTableOutput(ns("summaryTable"))
                     ),
                     br(),
                     column(
                       12,
                       uiOutput(ns("notPerformed"))
                     )
                   )
                   
                 )))
}

# Module Server

#' @rdname mod_perform_checks
#' @export
#' @keywords internal
#' @import bdchecks shinyjs DT bdutilities
mod_perform_checks_server <- function(input, output, session, user_data, quality_checks) {
  ns <- session$ns
  
  check_result <- list()
  check_summary <- data.frame()
  clean_data <- data.frame()
  not_performed <- data.frame()
  
  observeEvent(input$perform, {
    withProgress(message = "Performing Data Checks", {
      if (length(bdutilities::return_core(user_data)) == 0 || length(quality_checks()) == 0) {
        showNotification("Please add data and checks first",
                         duration = 6)
        
        return()
      }
      
      check_result <<-
        bdchecks::perform_dc(bdutilities::return_core(user_data), quality_checks())
      
      check_summary <<-
        bdchecks::summary_dc(check_result, fancy = FALSE, filtering_dt = TRUE)
      
      not_performed <<- check_result@not_performed
      
      shinyjs::runjs(code = paste('$("#', ns("accept"), '").addClass("activeButton");', sep = ""))
      shinyjs::runjs(code = paste('$("#', ns("perform"), '").removeClass("activeButton");', sep = ""))
      shinyjs::runjs(code = paste('$("#', ns("perform"), '").addClass("readyButton");', sep = ""))
    })
    
  })
  
  observeEvent(input$accept, {
    if(length(input$summaryTable_cells_selected) == 0){
      showNotification("Select Filters!",
                       duration = 6) 
      return()
    }
    
    summary_filter <- bdchecks:::dc_filter_generate(check_summary, input$summaryTable_cells_selected)
    clean_data <<- bdchecks:::dc_filter(bdutilities::return_core(user_data), check_result, summary_filter)
    
    showNotification("Filters Applied!",
                     duration = 6) 
    
    shinyjs::runjs(code = paste('$("#', ns("download"), '").addClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("accept"), '").removeClass("activeButton");', sep = ""))
    shinyjs::runjs(code = paste('$("#', ns("accept"), '").addClass("readyButton");', sep = ""))
    
  })
  
  output$summaryTable <- DT::renderDataTable(
    DT::datatable({
      input$perform
      check_summary
    },
    options = list(pageLength = 100,
                   columnDefs = list(
                     list(className = "no_select",
                          targets = 0:1)
                   )),
    selection = list(target = "cell"),
    callback = DT::JS(
      "table.on('click', 'td.no_select', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if ($(td.node()).hasClass('selected')) {
      $(td.node()).removeClass('selected');
    }});"
    ))
  )
  
  output$notPerformed <- renderUI({
    input$perform
    if (nrow(not_performed) > 0) {
      return(tagList(
        h2("Skipped Data Checks"),
        DT::datatable(not_performed)
      ))
    }
  })
  
  
  output$download <- shiny::downloadHandler(
    filename = function() {
      format(Sys.time(), "filteredData_%Y_%b_%d.csv")
    },
    content = function(file) {
      write.csv(clean_data, file, row.names = FALSE)
    }
  )
  
  proxy <- DT::dataTableProxy("summaryTable")
  
  observeEvent(input$passed, {
    a <- input$summaryTable_cells_selected
    if (length(a) == 0)
      a <- NULL
    
    DT::selectCells(proxy, rbind(cbind(1:nrow(check_summary), 2), a))
  })
  
  observeEvent(input$failed, {
    a <- input$summaryTable_cells_selected
    if (length(a) == 0)
      a <- NULL
    
    DT::selectCells(proxy, rbind(cbind(1:nrow(check_summary), 3), a))
  })
  
  observeEvent(input$missed, {
    a <- input$summaryTable_cells_selected
    if (length(a) == 0)
      a <- NULL
    
    DT::selectCells(proxy, rbind(cbind(1:nrow(check_summary), 4), a))
  })
  
  observeEvent(input$clear, {
    DT::selectCells(proxy, NULL)
  })
  
  output$total <-
    renderText({
      nrow(bdutilities::return_core(user_data))
    })
  
  output$checks <-
    renderText({
      input$perform
      return(nrow(check_summary))
    })
  
  output$filtered <-
    renderText({
      input$accept
      nrow(clean_data)
    })
}
