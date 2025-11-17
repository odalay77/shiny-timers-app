library(shiny)
library(DT)
library(dplyr)
library(DBI)
library(RSQLite)
library(pool)
library(shinyjs)

# -- Global / setup
db_file <- normalizePath(file.path("data", "timers.sqlite"), mustWork = FALSE)

if (!dir.exists(dirname(db_file))) {
  dir.create(dirname(db_file), recursive = TRUE)
  message("Created directory: ", dirname(db_file))
}

pool_db <- dbPool(
  drv    = RSQLite::SQLite(),
  dbname = db_file
)

# --- Create or alter timers table
create_table_sql <- "
CREATE TABLE IF NOT EXISTS timers (
  id             INTEGER PRIMARY KEY AUTOINCREMENT,
  sample_id      TEXT,
  instrument     TEXT,
  mode           TEXT,
  step           INTEGER,
  total_secs     INTEGER,
  remaining_secs INTEGER,
  status         TEXT,
  start_time     DATETIME,
  end_time       DATETIME,
  created_at     DATETIME DEFAULT CURRENT_TIMESTAMP
);
"
dbExecute(pool_db, create_table_sql)

# Ensure instrument column exists
cols <- dbListFields(pool_db, "timers")
if (!"instrument" %in% cols) {
  dbExecute(pool_db, "ALTER TABLE timers ADD COLUMN instrument TEXT;")
}

# Helper: seconds → HH:MM:SS
format_hms <- function(sec) {
  hrs  <- sec %/% 3600
  mins <- (sec %% 3600) %/% 60
  secs <- sec %% 60
  sprintf("%02d:%02d:%02d", hrs, mins, secs)
}

# ============================
# UI
# ============================
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
  
  # Row and warning styles
  tags$style(HTML("
    .active-row { background-color: #d4edda !important; }
    .completed-row { background-color: #f8f9fa !important; }
    .pending-row { background-color: #fff3cd !important; }
    .warning-row { background-color: #fff3cd !important; }  /* yellow warning */
    .btn-delete { color: white; background-color: #dc3545; border: none; padding:4px 8px; cursor:pointer; }
  ")),
  
  # --- JavaScript countdown + yellow warning logic
  tags$script(HTML("
    function formatHMS(sec) {
      var h = Math.floor(sec / 3600);
      var m = Math.floor((sec % 3600) / 60);
      var s = sec % 60;
      return ('0' + h).slice(-2) + ':' + ('0' + m).slice(-2) + ':' + ('0' + s).slice(-2);
    }

    setInterval(function(){
      $('span.remaining').each(function(){
        var secs = parseInt($(this).attr('data-secs'));
        if (secs > 0) {
          secs -= 1;
          $(this).attr('data-secs', secs);
          $(this).text(formatHMS(secs));
          
          // Apply yellow highlight when under 10 minutes
          var row = $(this).closest('tr');
          if (secs <= 600 && !row.hasClass('warning-row')) {
            row.removeClass('active-row').addClass('warning-row');
          }
        }
      });
    }, 1000);
  "))
)

# ============================
# SERVER
# ============================
server <- function(input, output, session) {
  
  # Timer durations (in seconds)
  durations_list <- list(
    "Room Temperature" = c(1*3600, 2*3600, 2.5*3600),
    # 8-hour timer FIRST for refrigerated test
    "Refrigerated"     = c(8*3600, 24*3600, 48*3600, 52*3600)
  )
  
  # -- Add new timers
  observeEvent(input$start_btn, {
    req(input$sample_id, input$instrument)
    mode_sel  <- input$mode
    durations <- durations_list[[mode_sel]]
    now       <- Sys.time()
    
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
    showNotification(paste("Timers started for sample", input$sample_id, "on", input$instrument), type = "message")
  })
  
  # -- Periodic DB sync (every 30 sec)
  autoInvalidate <- reactiveTimer(30000, session = session)
  observe({
    autoInvalidate()
    now <- Sys.time()
    
    timers_active <- dbGetQuery(pool_db,
                                "SELECT id, start_time, total_secs, status
                                 FROM timers
                                 WHERE status = 'Active'")
    
    if (nrow(timers_active) > 0) {
      for (i in seq_len(nrow(timers_active))) {
        id_i     <- timers_active$id[i]
        start_i  <- as.POSIXct(timers_active$start_time[i], tz = "")
        total_i  <- timers_active$total_secs[i]
        
        elapsed   <- as.numeric(difftime(now, start_i, units = "secs"))
        remaining <- total_i - floor(elapsed)
        
        if (remaining <= 0) {
          dbExecute(pool_db,
                    "UPDATE timers
                     SET remaining_secs = 0,
                         status = 'Completed',
                         end_time = ?
                     WHERE id = ?",
                    params = list(now, id_i))
        } else {
          dbExecute(pool_db,
                    "UPDATE timers
                     SET remaining_secs = ?
                     WHERE id = ?",
                    params = list(remaining, id_i))
        }
      }
    }
  })
  
  # -- Delete logic
  observeEvent(input$delete_button, {
    id_to_del <- as.integer(input$delete_button)
    sample_to_del <- dbGetQuery(pool_db,
                                "SELECT sample_id FROM timers WHERE id = ?",
                                params = list(id_to_del))$sample_id
    
    showModal(modalDialog(
      title = paste0("Confirm Deletion: Sample ", sample_to_del),
      paste0("Are you sure you want to delete the timer for Sample ID = \"",
             sample_to_del, "\" (internal record ID = ", id_to_del, ")?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn btn-danger")
      ),
      easyClose = TRUE
    ))
    
    observeEvent(input$confirm_delete, {
      dbExecute(pool_db, "DELETE FROM timers WHERE id = ?", params = list(id_to_del))
      removeModal()
      showNotification(paste("Deleted timer for Sample ID", sample_to_del), type = "message")
    }, once = TRUE)
  })
  
  output$timers_dt <- renderDT({
    df <- dbGetQuery(pool_db, "SELECT * FROM timers ORDER BY created_at DESC")
    
    if (nrow(df) == 0) {
      datatable(data.frame(Message = "No timers found"), escape = FALSE, rownames = FALSE)
    } else {
      
      df2 <- df %>%
        mutate(
          # --- Format Step as original duration in hours (1h, 2h, 2.5h, 8h, etc.)
          Step = {
            hours <- total_secs / 3600
            # Format: remove .0, keep .5, .25, etc.
            formatted <- sprintf("%.1fh", hours)
            sub("\\.0h$", "h", formatted)
          },
          
          # HTML remaining time countdown
          remaining = paste0(
            '<span class=\"remaining\" data-secs=\"', remaining_secs, '\">',
            format_hms(remaining_secs),
            '</span>'
          ),
          
          # Delete button
          Delete = paste0('<button id=\"del_', id, '\" class=\"btn-delete\">Delete</button>')
        ) %>%
        select(sample_id, instrument, mode, Step, remaining, status, Delete)
      
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
                  function(row, data, dataIndex){
                    if(data[5] == 'Active') {
                      $(row).addClass('active-row');
                    } else if(data[5] == 'Completed') {
                      $(row).addClass('completed-row');
                    } else {
                      $(row).addClass('pending-row');
                    }
                  }
                "),
                  columnDefs = list(list(targets = 6, orderable = FALSE))
                ))
    }
  }, server = FALSE)
  
  # -- Delete button JS hookup
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
}

# ============================
# Run App
# ============================
shinyApp(ui, server)
