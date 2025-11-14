library(shiny)
library(DT)
library(dplyr)
library(DBI)
library(RSQLite)
library(pool)
library(shinyjs)

# ---------------------------
# Global / setup
# ---------------------------
db_dir <- "/data"
if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE)
db_file <- file.path(db_dir, "timers.sqlite")

pool_db <- dbPool(
  drv    = RSQLite::SQLite(),
  dbname = db_file
)

# Create table if missing
dbExecute(pool_db, "
CREATE TABLE IF NOT EXISTS timers (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  sample_id TEXT,
  instrument TEXT,
  mode TEXT,
  step INTEGER,
  total_secs INTEGER,
  remaining_secs INTEGER,
  status TEXT,
  start_time DATETIME,
  end_time DATETIME,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);
")

# Ensure instrument column exists
cols <- dbListFields(pool_db, "timers")
if (!"instrument" %in% cols) {
  dbExecute(pool_db, "ALTER TABLE timers ADD COLUMN instrument TEXT;")
}

format_hms <- function(sec) {
  hrs <- sec %/% 3600
  mins <- (sec %% 3600) %/% 60
  secs <- sec %% 60
  sprintf("%02d:%02d:%02d", hrs, mins, secs)
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Urine Stability Timers"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("sample_id", "Enter Sample ID:"),
      selectInput("instrument", "Select Instrument:",
                  choices = c("DxU", "iQ200", "Mission 120")),
      selectInput("mode", "Select Test:",
                  choices = c("Room Temperature", "Refrigerated")),
      actionButton("start_btn", "Start Timers")
    ),
    mainPanel(
      DTOutput("timers_dt")
    )
  ),
  
  tags$style(HTML("
    .active-row { background-color: #d4edda !important; }
    .completed-row { background-color: #f8f9fa !important; }
    .pending-row { background-color: #fff3cd !important; }
    .warning-row { background-color: #fff3cd !important; }
    .btn-delete { color: white; background-color: #dc3545; border: none; padding:4px 8px; cursor:pointer; }
  "))
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  durations_list <- list(
    "Room Temperature" = c(1*3600, 2*3600, 2.5*3600),
    "Refrigerated"     = c(8*3600, 24*3600, 48*3600, 52*3600)
  )
  
  # Start timers
  observeEvent(input$start_btn, {
    req(input$sample_id, input$instrument)
    mode_sel <- input$mode
    durations <- durations_list[[mode_sel]]
    now <- Sys.time()
    
    for (i in seq_along(durations)) {
      dbExecute(pool_db,
                "INSERT INTO timers (sample_id, instrument, mode, step, total_secs, remaining_secs, status, start_time)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(input$sample_id,
                              input$instrument,
                              mode_sel,
                              i,
                              durations[i],
                              durations[i],
                              'Active',
                              now))
    }
    showNotification(paste("Timers started for sample", input$sample_id), type = "message")
  })
  
  # Update remaining time every second (server-side)
  autoInvalidate <- reactiveTimer(1000, session)
  observe({
    autoInvalidate()
    now <- Sys.time()
    
    active <- dbGetQuery(pool_db, "
      SELECT id, start_time, total_secs
      FROM timers
      WHERE status = 'Active'
    ")
    
    if (nrow(active) > 0) {
      for (i in seq_len(nrow(active))) {
        id_i <- active$id[i]
        start_i <- as.POSIXct(active$start_time[i], tz = "")
        total_i <- active$total_secs[i]
        elapsed <- as.numeric(difftime(now, start_i, units = "secs"))
        remaining <- max(0, total_i - floor(elapsed))
        
        status <- ifelse(remaining <= 0, "Completed", "Active")
        end_time <- ifelse(remaining <= 0, now, NA)
        
        dbExecute(pool_db, "
          UPDATE timers
          SET remaining_secs = ?, status = ?, end_time = COALESCE(end_time, ?)
          WHERE id = ?",
                  params = list(remaining, status, end_time, id_i))
      }
    }
  })
  
  # Render DT
  output$timers_dt <- renderDT({
    autoInvalidate()  # <- ensures table refreshes every second
    
    df <- dbGetQuery(pool_db, "SELECT * FROM timers ORDER BY created_at DESC")
    if (nrow(df) == 0) return(datatable(data.frame(Message="No timers"), escape=FALSE))
    
    df2 <- df %>%
      mutate(
        Step = sprintf("%.1fh", total_secs/3600) %>% sub("\\.0h$", "h", .),
        Remaining = format_hms(remaining_secs),
        Delete = paste0('<button id="del_', id, '" class="btn-delete">Delete</button>')
      ) %>%
      select(sample_id, instrument, mode, Step, Remaining, status, Delete)
    
    datatable(df2,
              escape = FALSE,
              rownames = FALSE,
              colnames = c("Sample ID", "Instrument", "Test", "Step", "Remaining Time", "Status", "Delete"),
              selection = 'none',
              options = list(
                paging = FALSE,
                searching = TRUE,
                ordering = TRUE,
                createdRow = JS("
                  function(row, data){
                    if(data[5] == 'Active') $(row).addClass('active-row');
                    else if(data[5] == 'Completed') $(row).addClass('completed-row');
                    else $(row).addClass('pending-row');
                  }
                "),
                columnDefs = list(list(targets=6, orderable=FALSE))
              )
    )
  }, server = FALSE)
  
  # JS Delete button hookup
  proxy <- dataTableProxy("timers_dt")
  observe({
    session$onFlushed(function() {
      runjs("
        $(document).on('click', 'button.btn-delete', function(){
          var id = $(this).attr('id').replace('del_', '');
          Shiny.setInputValue('delete_button', id, {priority:'event'});
        });
      ")
    }, once = TRUE)
  })
  
  # Delete logic
  observeEvent(input$delete_button, {
    id_to_del <- as.integer(input$delete_button)
    sample_to_del <- dbGetQuery(pool_db,
                                "SELECT sample_id FROM timers WHERE id = ?",
                                params = list(id_to_del))$sample_id
    
    showModal(modalDialog(
      title = paste0("Confirm Deletion: Sample ", sample_to_del),
      paste0("Are you sure you want to delete the timer for Sample ID = \"", sample_to_del, "\"?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class="btn btn-danger")
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$confirm_delete, {
      dbExecute(pool_db, "DELETE FROM timers WHERE id = ?", params=list(id_to_del))
      removeModal()
      showNotification(paste("Deleted timer for Sample ID", sample_to_del), type="message")
    }, once = TRUE)
  })
}

# ---------------------------
# Run app
# ---------------------------
shinyApp(ui, server)

