# GES Teacher Support Helpline & Query Tracking System
# Enhanced Version with Case Details Management
# Configured for Digital Ocean MySQL hosting

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(DBI)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(shinycssloaders)
library(fresh)
library(shinyWidgets)
library(shinyjs)
library(pool)
library(openxlsx)
library(dplyr)

# install.packages("bcrypt")  # run once
library(bcrypt)








# Helper function to replace %||%
`%or%` <- function(a, b) if (is.null(a) || is.na(a) || a == "") b else a

# Create professional theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#2c5aa0",
    blue = "#1e3a8a", 
    navy = "#0f172a",
    green = "#16a085",
    red = "#dc2626",
    yellow = "#f59e0b",
    orange = "#ea580c"
  ),
  adminlte_sidebar(
    dark_bg = "#1e3a8a",
    dark_hover_bg = "#2c5aa0",
    dark_color = "#ffffff"
  )
)

# Enhanced database connection for Digital Ocean MySQL
create_db_pool <- function() {
  tryCatch({
    # Get environment variables with defaults
    db_host <- Sys.getenv("DB_HOST", "")
    db_port <- as.integer(Sys.getenv("DB_PORT", ""))
    db_name <- Sys.getenv("DB_NAME", "")
    db_user <- Sys.getenv("DB_USER", "")
    db_password <- Sys.getenv("DB_PASSWORD", "")
    
    # Check if required environment variables are set
    if (any(c(db_host, db_name, db_user, db_password) == "")) {
      stop("Required database environment variables are not set")
    }
    
    cat("Connecting to Digital Ocean MySQL...\n")
    cat("Host:", db_host, "\n")
    cat("Port:", db_port, "\n")
    cat("Database:", db_name, "\n")
    cat("User:", db_user, "\n")
    
    # For Digital Ocean MySQL, try different SSL configurations
    # For Digital Ocean MySQL, try different SSL configurations
    ssl_configs <- list(
      # Configuration 1: Standard SSL with server certificate verification disabled
      list(ssl.mode = "REQUIRED", ssl.verify.server.cert = FALSE),
      # Configuration 2: SSL disabled (only if SSL is not required)
      list(ssl.mode = "DISABLED"),
      # Configuration 3: SSL with minimal verification
      list(ssl.mode = "PREFERRED", ssl.verify.server.cert = FALSE)
    )
    
    pool <- NULL
    last_error <- NULL
    
    for (i in seq_along(ssl_configs)) {
      cat("Trying SSL configuration", i, "\n")
      
      try_config <- ssl_configs[[i]]
      
      pool <- tryCatch({
        dbPool(
          drv = RMariaDB::MariaDB(),
          host = db_host,
          port = db_port,
          dbname = db_name,
          user = db_user,
          password = db_password,
          ssl.mode = try_config$ssl.mode,
          ssl.verify.server.cert = try_config$ssl.verify.server.cert,
          # Additional connection parameters for stability
          timeout = 60,
          reconnect = TRUE,
          # Connection pool settings
          minSize = 1,
          maxSize = 10,
          idleTimeout = 300000
        )
      }, error = function(e) {
        last_error <- e
        cat("Configuration", i, "failed:", e$message, "\n")
        return(NULL)
      })
      
      # Test the connection
      if (!is.null(pool)) {
        test_result <- tryCatch({
          conn <- poolCheckout(pool)
          result <- dbGetQuery(conn, "SELECT 1 as test")
          poolReturn(conn)
          TRUE
        }, error = function(e) {
          cat("Connection test failed:", e$message, "\n")
          if (!is.null(pool)) poolClose(pool)
          FALSE
        })
        
        if (test_result) {
          cat("Successfully connected with configuration", i, "\n")
          return(pool)
        }
      }
    }
    
    # If all configurations failed, throw the last error
    stop(paste("Failed to connect to database. Last error:", last_error$message))
    
  }, error = function(e) {
    cat("Database connection error:", e$message, "\n")
    # Return NULL to allow app to start with limited functionality
    return(NULL)
  })
}

# Create the database pool
USE_LOCAL_DB <- TRUE   # switch to FALSE for DigitalOcean

if (USE_LOCAL_DB) {
  pool <- dbPool(
    drv      = RMariaDB::MariaDB(),
    dbname   = "teacher_query_7",
    host     = "127.0.0.1",
    port     = 3306,
    user     = "root",
    password = "Naayelah2021@"
  )
} else {
  pool <- create_db_pool()
}

onStop(function() {
  try(poolClose(pool), silent = TRUE)
})



# Enhanced connection helper with better error handling
con <- function() {
  if (is.null(pool)) {
    return(NULL)
  }
  
  tryCatch({
    pool
  }, error = function(e) {
    return(NULL)
  })
}




# Database helper functions (existing ones)
get_regional_performance <- function(con, months_back = 12) {
  if (is.null(con)) return(data.frame())
  
  q <- "
    SELECT r.region_name,
           COUNT(*) as total_cases,
           SUM(CASE WHEN t.status IN ('Resolved','Closed') THEN 1 ELSE 0 END) as resolved_cases,
           ROUND(SUM(CASE WHEN t.status IN ('Resolved','Closed') THEN 1 ELSE 0 END)/NULLIF(COUNT(*),0)*100,1) as resolution_rate,
           ROUND(AVG(CASE WHEN t.status IN ('Resolved','Closed')
                          THEN TIMESTAMPDIFF(HOUR, t.created_at, COALESCE(t.resolved_at, t.closed_at))
                     END), 1) as avg_resolution_hours,
           SUM(CASE WHEN t.status = 'Escalated' THEN 1 ELSE 0 END) as escalated_cases,
           ROUND(SUM(CASE WHEN t.status = 'Escalated' THEN 1 ELSE 0 END)/NULLIF(COUNT(*),0)*100,1) as escalation_rate,
           SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') THEN 1 ELSE 0 END) as open_cases
    FROM tickets t
    LEFT JOIN regions r ON t.region_id = r.region_id
    WHERE t.created_at >= DATE_SUB(CURDATE(), INTERVAL ? MONTH)
    GROUP BY r.region_name
    ORDER BY resolution_rate DESC, total_cases DESC
  "
  dbGetQuery(con, q, params = list(months_back))
}

get_category_trends <- function(con, months_back = 12) {
  if (is.null(con)) return(data.frame())
  
  q <- "
    SELECT DATE_FORMAT(t.created_at, '%Y-%m') as ym,
           COALESCE(c.category_name,'Unknown') as category_name,
           COUNT(*) as cases
    FROM tickets t
    LEFT JOIN issue_categories c ON t.category_id = c.category_id
    WHERE t.created_at >= DATE_SUB(CURDATE(), INTERVAL ? MONTH)
    GROUP BY ym, category_name
    ORDER BY ym ASC, cases DESC
  "
  dbGetQuery(con, q, params = list(months_back))
}

get_time_series <- function(con, months_back = 18) {
  if (is.null(con)) return(data.frame())
  
  q <- "
    SELECT DATE_FORMAT(created_at, '%Y-%m') as ym,
           COUNT(*) as created,
           SUM(CASE WHEN status IN ('Resolved','Closed') THEN 1 ELSE 0 END) as resolved,
           ROUND(AVG(CASE WHEN status IN ('Resolved','Closed')
                          THEN TIMESTAMPDIFF(HOUR, created_at, COALESCE(resolved_at, closed_at))
                     END),1) as avg_resolution_hours
    FROM tickets
    WHERE created_at >= DATE_SUB(CURDATE(), INTERVAL ? MONTH)
    GROUP BY ym
    ORDER BY ym ASC
  "
  dbGetQuery(con, q, params = list(months_back))
}

get_sla_overview <- function(con) {
  if (is.null(con)) return(list(summary=data.frame(), by_region=data.frame(), overdue=data.frame()))
  
  summary_q <- "
    SELECT
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') THEN 1 ELSE 0 END) as open_cases,
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') AND resolution_due_at IS NOT NULL AND resolution_due_at < NOW() THEN 1 ELSE 0 END) as overdue,
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') AND resolution_due_at IS NOT NULL AND resolution_due_at BETWEEN NOW() AND DATE_ADD(NOW(), INTERVAL 4 HOUR) THEN 1 ELSE 0 END) as due_soon,
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') AND (resolution_due_at IS NULL OR resolution_due_at > DATE_ADD(NOW(), INTERVAL 4 HOUR)) THEN 1 ELSE 0 END) as on_track
    FROM tickets
  "
  
  by_region_q <- "
    SELECT r.region_name,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') THEN 1 ELSE 0 END) as open_cases,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') AND t.resolution_due_at IS NOT NULL AND t.resolution_due_at < NOW() THEN 1 ELSE 0 END) as overdue,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') AND t.resolution_due_at IS NOT NULL AND t.resolution_due_at BETWEEN NOW() AND DATE_ADD(NOW(), INTERVAL 4 HOUR) THEN 1 ELSE 0 END) as due_soon,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') AND (t.resolution_due_at IS NULL OR t.resolution_due_at > DATE_ADD(NOW(), INTERVAL 4 HOUR)) THEN 1 ELSE 0 END) as on_track
    FROM tickets t
    LEFT JOIN regions r ON t.region_id = r.region_id
    GROUP BY r.region_name
    ORDER BY overdue DESC, due_soon DESC, open_cases DESC
  "
  
  overdue_q <- "
    SELECT ticket_id, case_code, priority, status, created_at, resolution_due_at,
           TIMESTAMPDIFF(HOUR, resolution_due_at, NOW()) as hours_overdue
    FROM tickets
    WHERE status NOT IN ('Resolved','Closed')
      AND resolution_due_at IS NOT NULL
      AND resolution_due_at < NOW()
    ORDER BY hours_overdue DESC
    LIMIT 200
  "
  
  list(
    summary  = dbGetQuery(con, summary_q),
    by_region = dbGetQuery(con, by_region_q),
    overdue  = dbGetQuery(con, overdue_q)
  )
}






# Database helper functions (existing ones)
get_regional_performance <- function(con, months_back = 12) {
  if (is.null(con)) return(data.frame())
  
  q <- "
    SELECT r.region_name,
           COUNT(*) as total_cases,
           SUM(CASE WHEN t.status IN ('Resolved','Closed') THEN 1 ELSE 0 END) as resolved_cases,
           ROUND(SUM(CASE WHEN t.status IN ('Resolved','Closed') THEN 1 ELSE 0 END)/NULLIF(COUNT(*),0)*100,1) as resolution_rate,
           ROUND(AVG(CASE WHEN t.status IN ('Resolved','Closed')
                          THEN TIMESTAMPDIFF(HOUR, t.created_at, COALESCE(t.resolved_at, t.closed_at))
                     END), 1) as avg_resolution_hours,
           SUM(CASE WHEN t.status = 'Escalated' THEN 1 ELSE 0 END) as escalated_cases,
           ROUND(SUM(CASE WHEN t.status = 'Escalated' THEN 1 ELSE 0 END)/NULLIF(COUNT(*),0)*100,1) as escalation_rate,
           SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') THEN 1 ELSE 0 END) as open_cases
    FROM tickets t
    LEFT JOIN regions r ON t.region_id = r.region_id
    WHERE t.created_at >= DATE_SUB(CURDATE(), INTERVAL ? MONTH)
    GROUP BY r.region_name
    ORDER BY resolution_rate DESC, total_cases DESC
  "
  dbGetQuery(con, q, params = list(months_back))
}

get_category_trends <- function(con, months_back = 12) {
  if (is.null(con)) return(data.frame())
  
  q <- "
    SELECT DATE_FORMAT(t.created_at, '%Y-%m') as ym,
           COALESCE(c.category_name,'Unknown') as category_name,
           COUNT(*) as cases
    FROM tickets t
    LEFT JOIN issue_categories c ON t.category_id = c.category_id
    WHERE t.created_at >= DATE_SUB(CURDATE(), INTERVAL ? MONTH)
    GROUP BY ym, category_name
    ORDER BY ym ASC, cases DESC
  "
  dbGetQuery(con, q, params = list(months_back))
}

get_time_series <- function(con, months_back = 18) {
  if (is.null(con)) return(data.frame())
  
  q <- "
    SELECT DATE_FORMAT(created_at, '%Y-%m') as ym,
           COUNT(*) as created,
           SUM(CASE WHEN status IN ('Resolved','Closed') THEN 1 ELSE 0 END) as resolved,
           ROUND(AVG(CASE WHEN status IN ('Resolved','Closed')
                          THEN TIMESTAMPDIFF(HOUR, created_at, COALESCE(resolved_at, closed_at))
                     END),1) as avg_resolution_hours
    FROM tickets
    WHERE created_at >= DATE_SUB(CURDATE(), INTERVAL ? MONTH)
    GROUP BY ym
    ORDER BY ym ASC
  "
  dbGetQuery(con, q, params = list(months_back))
}

get_sla_overview <- function(con) {
  if (is.null(con)) return(list(summary=data.frame(), by_region=data.frame(), overdue=data.frame()))
  
  summary_q <- "
    SELECT
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') THEN 1 ELSE 0 END) as open_cases,
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') AND resolution_due_at IS NOT NULL AND resolution_due_at < NOW() THEN 1 ELSE 0 END) as overdue,
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') AND resolution_due_at IS NOT NULL AND resolution_due_at BETWEEN NOW() AND DATE_ADD(NOW(), INTERVAL 4 HOUR) THEN 1 ELSE 0 END) as due_soon,
      SUM(CASE WHEN status NOT IN ('Resolved','Closed') AND (resolution_due_at IS NULL OR resolution_due_at > DATE_ADD(NOW(), INTERVAL 4 HOUR)) THEN 1 ELSE 0 END) as on_track
    FROM tickets
  "
  
  by_region_q <- "
    SELECT r.region_name,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') THEN 1 ELSE 0 END) as open_cases,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') AND t.resolution_due_at IS NOT NULL AND t.resolution_due_at < NOW() THEN 1 ELSE 0 END) as overdue,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') AND t.resolution_due_at IS NOT NULL AND t.resolution_due_at BETWEEN NOW() AND DATE_ADD(NOW(), INTERVAL 4 HOUR) THEN 1 ELSE 0 END) as due_soon,
      SUM(CASE WHEN t.status NOT IN ('Resolved','Closed') AND (t.resolution_due_at IS NULL OR t.resolution_due_at > DATE_ADD(NOW(), INTERVAL 4 HOUR)) THEN 1 ELSE 0 END) as on_track
    FROM tickets t
    LEFT JOIN regions r ON t.region_id = r.region_id
    GROUP BY r.region_name
    ORDER BY overdue DESC, due_soon DESC, open_cases DESC
  "
  
  overdue_q <- "
    SELECT ticket_id, case_code, priority, status, created_at, resolution_due_at,
           TIMESTAMPDIFF(HOUR, resolution_due_at, NOW()) as hours_overdue
    FROM tickets
    WHERE status NOT IN ('Resolved','Closed')
      AND resolution_due_at IS NOT NULL
      AND resolution_due_at < NOW()
    ORDER BY hours_overdue DESC
    LIMIT 200
  "
  
  list(
    summary  = dbGetQuery(con, summary_q),
    by_region = dbGetQuery(con, by_region_q),
    overdue  = dbGetQuery(con, overdue_q)
  )
}





##########
get_categories <- function(con) {
  if (is.null(con)) return(data.frame(category_id = integer(), category_name = character()))
  tryCatch({
    dbGetQuery(con, "SELECT category_id, category_name FROM issue_categories WHERE is_active = 1 ORDER BY category_name")
  }, error = function(e) {
    showNotification(paste("Error loading categories:", e$message), type = "error")
    data.frame(category_id = integer(), category_name = character())
  })
}

get_subcategories <- function(con, category_id) {
  if (is.null(con) || is.null(category_id) || category_id == "") {
    return(data.frame(subcategory_id = integer(), subcategory_name = character()))
  }
  tryCatch({
    dbGetQuery(con, 
               "SELECT subcategory_id, subcategory_name FROM issue_subcategories WHERE category_id = ? AND is_active = 1 ORDER BY subcategory_name", 
               params = list(category_id))
  }, error = function(e) {
    showNotification(paste("Error loading subcategories:", e$message), type = "error")
    data.frame(subcategory_id = integer(), subcategory_name = character())
  })
}

get_regions <- function(con) {
  if (is.null(con)) {
    return(data.frame(
      region_id = 1:16,
      region_name = c("Greater Accra Region", "Ashanti Region", "Western Region", "Eastern Region", 
                      "Central Region", "Northern Region", "Upper East Region", "Upper West Region", 
                      "Volta Region", "Brong Ahafo Region", "Western North Region", 
                      "Ahafo Region", "Bono East Region", "Oti Region", "North East Region", "Savannah Region")
    ))
  }
  
  tryCatch({
    result <- dbGetQuery(con, "SELECT region_id, region_name FROM regions WHERE is_active = 1 ORDER BY region_name")
    if (nrow(result) == 0) {
      data.frame(
        region_id = 1:16,
        region_name = c("Greater Accra Region", "Ashanti Region", "Western Region", "Eastern Region", 
                        "Central Region", "Northern Region", "Upper East Region", "Upper West Region", 
                        "Volta Region", "Brong Ahafo Region", "Western North Region", 
                        "Ahafo Region", "Bono East Region", "Oti Region", "North East Region", "Savannah Region")
      )
    } else {
      result
    }
  }, error = function(e) {
    showNotification(paste("Error loading regions:", e$message), type = "warning")
    data.frame(
      region_id = 1:16,
      region_name = c("Greater Accra Region", "Ashanti Region", "Western Region", "Eastern Region", 
                      "Central Region", "Northern Region", "Upper East Region", "Upper West Region", 
                      "Volta Region", "Brong Ahafo Region", "Western North Region", 
                      "Ahafo Region", "Bono East Region", "Oti Region", "North East Region", "Savannah Region")
    )
  })
}

get_channels <- function(con) {
  if (is.null(con)) {
    return(data.frame(
      channel_id = 1:9,
      channel_name = c("Regional Phone", "Toll-free", "Walk-in", "WhatsApp", "Email", "Telegram", "Facebook", "Portal", "Other")
    ))
  }
  
  tryCatch({
    result <- dbGetQuery(con, "SELECT channel_id, channel_name FROM channels WHERE is_active = 1 ORDER BY channel_name")
    if (nrow(result) == 0) {
      data.frame(
        channel_id = 1:9,
        channel_name = c("Regional Phone", "Toll-free", "Walk-in", "WhatsApp", "Email", "Telegram", "Facebook", "Portal", "Other")
      )
    } else {
      result
    }
  }, error = function(e) {
    showNotification(paste("Error loading channels:", e$message), type = "warning")
    data.frame(
      channel_id = 1:9,
      channel_name = c("Regional Phone", "Toll-free", "Walk-in", "WhatsApp", "Email", "Telegram", "Facebook", "Portal", "Other")
    )
  })
}

# NEW: Get individual case details with comprehensive information
get_case_details <- function(con, ticket_id) {
  if (is.null(con) || is.null(ticket_id)) return(NULL)
  
  tryCatch({
    query <- "
      SELECT t.*, 
             r.region_name, 
             ch.channel_name,
             c.category_name, 
             s.subcategory_name,
             u1.full_name as regional_assignee,
             u2.full_name as national_assignee,
             creator.full_name as created_by_name,
             TIMESTAMPDIFF(HOUR, t.created_at, COALESCE(t.resolved_at, NOW())) as hours_open,
             CASE 
               WHEN t.status NOT IN ('Resolved', 'Closed') AND t.resolution_due_at < NOW() THEN 'Overdue'
               WHEN t.status NOT IN ('Resolved', 'Closed') AND t.resolution_due_at < DATE_ADD(NOW(), INTERVAL 4 HOUR) THEN 'Due Soon'
               ELSE 'On Track'
             END as sla_status
      FROM tickets t
      LEFT JOIN regions r ON t.region_id = r.region_id
      LEFT JOIN channels ch ON t.channel_id = ch.channel_id
      LEFT JOIN issue_categories c ON t.category_id = c.category_id
      LEFT JOIN issue_subcategories s ON t.subcategory_id = s.subcategory_id
      LEFT JOIN users u1 ON t.assigned_region_user_id = u1.user_id
      LEFT JOIN users u2 ON t.assigned_national_user_id = u2.user_id
      LEFT JOIN users creator ON t.created_by_user_id = creator.user_id
      WHERE t.ticket_id = ?
    "
    
    result <- dbGetQuery(con, query, params = list(ticket_id))
    if (nrow(result) > 0) {
      return(result[1, ])
    } else {
      return(NULL)
    }
  }, error = function(e) {
    showNotification(paste("Error loading case details:", e$message), type = "error")
    return(NULL)
  })
}

# NEW: Get case action history/timeline
get_case_actions <- function(con, ticket_id) {
  if (is.null(con) || is.null(ticket_id)) return(data.frame())

  tryCatch({
    query <- "
      SELECT ta.*,
             u.full_name as action_by_name,
             CASE ta.action_type
               WHEN 'create' THEN 'Case Created'
               WHEN 'note' THEN 'Note Added'
               WHEN 'status_change' THEN 'Status Changed'
               WHEN 'escalation' THEN 'Escalated'
               WHEN 'reassignment' THEN 'Reassigned'
               WHEN 'first_response' THEN 'First Response'
               WHEN 'resolution' THEN 'Resolved'
               WHEN 'closure' THEN 'Closed'
               ELSE UPPER(ta.action_type)
             END as action_label
      FROM ticket_actions ta
      LEFT JOIN users u ON ta.action_by_user_id = u.user_id
      WHERE ta.ticket_id = ?
      ORDER BY ta.action_at DESC
    "

    dbGetQuery(con, query, params = list(ticket_id))
  }, error = function(e) {
    showNotification(paste("Error loading case actions:", e$message), type = "error")
    data.frame()
  })
}

# NEW: Get follow-ups for a specific case
get_case_followups <- function(con, ticket_id) {
  if (is.null(con) || is.null(ticket_id)) return(data.frame())

  tryCatch({
    query <- "
      SELECT f.*,
             u1.full_name as created_by_name,
             u2.full_name as completed_by_name,
             CASE
               WHEN f.status = 'Completed' THEN 'Completed'
               WHEN f.follow_up_date < CURDATE() THEN 'Overdue'
               WHEN f.follow_up_date = CURDATE() THEN 'Due Today'
               ELSE 'Upcoming'
             END as urgency
      FROM follow_ups f
      LEFT JOIN users u1 ON f.created_by_user_id = u1.user_id
      LEFT JOIN users u2 ON f.completed_by_user_id = u2.user_id
      WHERE f.ticket_id = ?
      ORDER BY f.follow_up_date DESC
    "

    dbGetQuery(con, query, params = list(ticket_id))
  }, error = function(e) {
    data.frame()
  })
}

# NEW: Add action to case
add_case_action <- function(con, ticket_id, action_type, action_text, user_id = NULL, old_status = NULL, new_status = NULL) {
  if (is.null(con) || is.null(ticket_id)) return(FALSE)
  
  tryCatch({
    query <- "
      INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, old_status, new_status)
      VALUES (?, ?, ?, ?, ?, ?)
    "
    
    rows_affected <- dbExecute(con, query, params = list(
      ticket_id, user_id, action_type, action_text, old_status, new_status
    ))
    
    return(rows_affected > 0)
  }, error = function(e) {
    showNotification(paste("Error adding action:", e$message), type = "error")
    return(FALSE)
  })
}

# NEW: Update case status with improved error handling
# FIXED: Update case status with poolWithTransaction
update_case_status <- function(pool_conn, ticket_id, new_status, notes = NULL, user_id = NULL) {
  if (is.null(pool_conn) || is.null(ticket_id) || is.null(new_status)) {
    showNotification("Missing required parameters for status update", type = "error")
    return(FALSE)
  }
  
  tryCatch({
    # Use poolWithTransaction for proper pool transaction handling
    result <- poolWithTransaction(pool_conn, function(conn) {
      
      # Get current status
      current_case <- dbGetQuery(conn, "SELECT status FROM tickets WHERE ticket_id = ?", params = list(ticket_id))
      if (nrow(current_case) == 0) {
        stop("Case not found")
      }
      
      old_status <- current_case$status[1]
      
      # Don't update if status is the same
      if (old_status == new_status) {
        showNotification("Case is already in this status", type = "info")
        return(TRUE)
      }
      
      # Update status and timestamp if needed
      update_query <- "UPDATE tickets SET status = ?, updated_at = NOW()"
      params <- list(new_status)
      
      if (new_status == "Resolved" && old_status != "Resolved") {
        update_query <- paste(update_query, ", resolved_at = NOW()")
      } else if (new_status == "Closed" && old_status != "Closed") {
        update_query <- paste(update_query, ", closed_at = NOW()")
      } else if (new_status == "In Progress" && old_status == "New") {
        update_query <- paste(update_query, ", first_response_at = NOW()")
      }
      
      update_query <- paste(update_query, "WHERE ticket_id = ?")
      params <- append(params, ticket_id)
      
      rows_affected <- dbExecute(conn, update_query, params = params)
      
      if (rows_affected == 0) {
        stop("Failed to update case status")
      }
      
      # Add action log
      action_text <- if (!is.null(notes) && notes != "" && !is.na(notes)) {
        paste("Status changed from", old_status, "to", new_status, "-", notes)
      } else {
        paste("Status changed from", old_status, "to", new_status)
      }
      
      # Add action (within the same transaction)
      action_query <- "
        INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text, old_status, new_status)
        VALUES (?, ?, ?, ?, ?, ?)
      "
      
      action_rows <- dbExecute(conn, action_query, params = list(
        ticket_id, user_id, "status_change", action_text, old_status, new_status
      ))
      
      if (action_rows == 0) {
        # Still allow the status update even if action log fails
        showNotification(paste("Status updated to", new_status, "but action log failed"), type = "warning")
      }
      
      return(TRUE)
    })
    
    if (result) {
      showNotification(paste("Status successfully updated to:", new_status), type = "message")
      return(TRUE)
    } else {
      return(FALSE)
    }
    
  }, error = function(e) {
    showNotification(paste("Error updating status:", e$message), type = "error")
    return(FALSE)
  })
}

# FIXED: Add note to case with proper pool handling
add_case_note <- function(pool_conn, ticket_id, note_text, user_id = NULL) {
  if (is.null(pool_conn) || is.null(ticket_id) || is.null(note_text) || note_text == "") return(FALSE)
  
  tryCatch({
    result <- poolWithTransaction(pool_conn, function(conn) {
      query <- "
        INSERT INTO ticket_actions (ticket_id, action_by_user_id, action_type, action_text)
        VALUES (?, ?, ?, ?)
      "
      
      rows_affected <- dbExecute(conn, query, params = list(
        ticket_id, user_id, "note", note_text
      ))
      
      return(rows_affected > 0)
    })
    
    if (result) {
      showNotification("Note added successfully", type = "message")
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    showNotification(paste("Error adding note:", e$message), type = "error")
    return(FALSE)
  })
}

# Insert ticket function (from original code)
insert_ticket <- function(con, region_id, channel_id, teacher_name, teacher_phone, teacher_staff_id,
                          school_name, district, category_id, subcategory_id = NULL, priority, summary, description = NULL) {
  if (is.null(con)) {
    showNotification("Database connection not available", type = "error")
    return(FALSE)
  }
  
  tryCatch({
    current_year <- year(Sys.Date())
    timestamp_id <- as.integer(as.numeric(Sys.time()) %% 1000000)
    case_code <- sprintf("GES-%d-%06d", current_year, timestamp_id)
    
    teacher_staff_id <- if (is.null(teacher_staff_id) || teacher_staff_id == "") "" else teacher_staff_id
    school_name <- if (is.null(school_name) || school_name == "") "" else school_name
    district <- if (is.null(district) || district == "") "" else district
    description <- if (is.null(description) || description == "") "" else description
    
    query <- "
      INSERT INTO tickets (
        case_code, region_id, channel_id, teacher_name, teacher_phone, teacher_staff_id,
        school_name, district, category_id, subcategory_id, priority, status, summary, description,
        created_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'New', ?, ?, NOW())
    "
    
    rows_affected <- dbExecute(con, query, params = list(
      case_code, region_id, channel_id, teacher_name, teacher_phone, teacher_staff_id,
      school_name, district, category_id, subcategory_id, priority, summary, description
    ))
    
    if (rows_affected > 0) {
      showNotification(paste("Case", case_code, "created successfully!"), type = "message")
      return(TRUE)
    } else {
      showNotification("Failed to create case - no rows affected", type = "error")
      return(FALSE)
    }
    
  }, error = function(e) {
    showNotification(paste("Error saving ticket:", e$message), type = "error")
    return(FALSE)
  })
}

# Fetch tickets function with enhanced filtering (search, date range, category)
fetch_tickets <- function(con, region_id = NULL, status_filter = NULL, category_id = NULL,
                          search_text = NULL, date_from = NULL, date_to = NULL, limit = 200) {
  if (is.null(con)) return(data.frame())
  
  tryCatch({
    base_query <- "
      SELECT t.ticket_id, t.case_code, t.created_at, t.status, t.priority,
             c.category_name, s.subcategory_name, t.teacher_name, t.teacher_phone, t.teacher_staff_id,
             t.school_name, t.district, t.summary, t.description,
             t.first_response_at, t.resolved_at, t.closed_at,
             TIMESTAMPDIFF(HOUR, t.created_at, COALESCE(t.resolved_at, NOW())) as hours_open
      FROM tickets t
      LEFT JOIN issue_categories c ON c.category_id = t.category_id
      LEFT JOIN issue_subcategories s ON s.subcategory_id = t.subcategory_id
      WHERE 1=1
    "
    
    params <- list()
    
    if (!is.null(region_id) && region_id != 0) {
      base_query <- paste(base_query, "AND t.region_id = ?")
      params <- append(params, region_id)
    }
    
    if (!is.null(status_filter) && status_filter != "All") {
      base_query <- paste(base_query, "AND t.status = ?")
      params <- append(params, status_filter)
    }
    
    if (!is.null(category_id) && category_id != 0) {
      base_query <- paste(base_query, "AND t.category_id = ?")
      params <- append(params, as.integer(category_id))
    }
    
    if (!is.null(search_text) && nchar(trimws(search_text)) >= 2) {
      search_pattern <- paste0("%", trimws(search_text), "%")
      base_query <- paste(base_query, "AND (t.teacher_name LIKE ? OR t.teacher_phone LIKE ? OR t.teacher_staff_id LIKE ? OR t.case_code LIKE ? OR t.school_name LIKE ?)")
      params <- append(params, list(search_pattern, search_pattern, search_pattern, search_pattern, search_pattern))
    }
    
    if (!is.null(date_from) && !is.na(date_from)) {
      base_query <- paste(base_query, "AND DATE(t.created_at) >= ?")
      params <- append(params, as.character(date_from))
    }
    
    if (!is.null(date_to) && !is.na(date_to)) {
      base_query <- paste(base_query, "AND DATE(t.created_at) <= ?")
      params <- append(params, as.character(date_to))
    }
    
    base_query <- paste(base_query, "ORDER BY t.created_at DESC LIMIT ?")
    params <- append(params, as.integer(limit))
    
    result <- dbGetQuery(con, base_query, params = params)
    return(result)
    
  }, error = function(e) {
    showNotification(paste("Error fetching tickets:", e$message), type = "error")
    return(data.frame())
  })
}

# Dashboard stats function (from original code)
get_dashboard_stats <- function(con, region_id = NULL) {
  if (is.null(con)) return(list(
    status = data.frame(status = character(), count = integer()),
    priority = data.frame(priority = character(), count = integer()),
    category = data.frame(category_name = character(), count = integer()),
    averages = data.frame(avg_resolution_hours = numeric(), escalation_rate = numeric())
  ))
  
  tryCatch({
    # 1. Single optimized query to DigitalOcean (parameterized)
    if (!is.null(region_id) && region_id != 0) {
      main_query <- "
        SELECT
          t.status,
          t.priority,
          COALESCE(c.category_name, 'Unknown') as category_name,
          t.created_at,
          t.resolved_at
        FROM tickets t
        LEFT JOIN issue_categories c ON t.category_id = c.category_id
        WHERE t.region_id = ?
      "
      raw_data <- dbGetQuery(con, main_query, params = list(region_id))
    } else {
      main_query <- "
        SELECT
          t.status,
          t.priority,
          COALESCE(c.category_name, 'Unknown') as category_name,
          t.created_at,
          t.resolved_at
        FROM tickets t
        LEFT JOIN issue_categories c ON t.category_id = c.category_id
      "
      raw_data <- dbGetQuery(con, main_query)
    }
    
    if (nrow(raw_data) == 0) {
      return(list(
        status = data.frame(status = character(), count = integer()),
        priority = data.frame(priority = character(), count = integer()),
        category = data.frame(category_name = character(), count = integer()),
        averages = data.frame(avg_resolution_hours = 0, escalation_rate = 0)
      ))
    }
    
    # 2. Grouping data in R (Fixed: use n() instead of count for specific renaming)
    status_data <- raw_data %>% group_by(status) %>% summarise(count = n())
    priority_data <- raw_data %>% group_by(priority) %>% summarise(count = n())
    category_data <- raw_data %>% 
      group_by(category_name) %>% 
      summarise(count = n()) %>%
      arrange(desc(count)) %>% 
      head(10)
    
    # 3. Calculate Averages (Fixed: replaced COALESCE with ifelse and synchronized time types)
    avg_data <- raw_data %>%
      mutate(
        # Convert created_at to R time format
        time_created = as.POSIXct(created_at),
        # If resolved_at is NA, use current time; otherwise use resolved_at
        time_resolved = as.POSIXct(ifelse(is.na(resolved_at), Sys.time(), resolved_at), origin = "1970-01-01"),
        # Calculate numeric hours
        hours = as.numeric(difftime(time_resolved, time_created, units = "hours"))
      ) %>%
      summarise(
        avg_resolution_hours = mean(hours, na.rm = TRUE),
        escalation_rate = (sum(status == "Escalated", na.rm = TRUE) / n()) * 100
      )
    
    return(list(
      status = status_data,
      priority = priority_data, 
      category = category_data,
      averages = avg_data
    ))
    
  }, error = function(e) {
    showNotification(paste("Error loading dashboard stats:", e$message), type = "error")
    return(list(
      status = data.frame(status = character(), count = integer()),
      priority = data.frame(priority = character(), count = integer()),
      category = data.frame(category_name = character(), count = integer()),
      averages = data.frame(avg_resolution_hours = 0, escalation_rate = 0)
    ))
  })
}



# UI Definition with Landing Page, Authentication, and Role-Based Access
ui <- tagList(
  useShinyjs(),
  
  # ========================================
  # LANDING PAGE OVERLAY
  # ========================================
  div(id = "landing_overlay",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 99999; background: linear-gradient(135deg, #0f172a 0%, #1e3a8a 40%, #2c5aa0 70%, #1e40af 100%); overflow-y: auto;",
      
      # Top navigation bar
      div(style = "padding: 20px 40px; display: flex; justify-content: space-between; align-items: center;",
          div(
            tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/8/89/Coat_of_arms_of_Ghana.svg/100px-Coat_of_arms_of_Ghana.svg.png",
                     height = "50px", style = "margin-right: 15px; vertical-align: middle;"),
            tags$span("GES Teacher Support Helpline", style = "color: white; font-size: 22px; font-weight: bold; vertical-align: middle;")
          ),
          div(
            actionButton("landing_analytics_btn", "View Analytics",
                         class = "btn", style = "background: transparent; border: 2px solid white; color: white; margin-right: 10px; padding: 8px 24px; border-radius: 25px; font-weight: 600;"),
            actionButton("landing_login_btn", "Staff Login",
                         class = "btn", style = "background: white; color: #1e3a8a; border: 2px solid white; padding: 8px 24px; border-radius: 25px; font-weight: 600;")
          )
      ),
      
      # Hero section
      div(style = "text-align: center; padding: 80px 40px 60px 40px; max-width: 900px; margin: 0 auto;",
          tags$h1("Ghana Education Service", style = "color: white; font-size: 48px; font-weight: 800; margin-bottom: 10px; letter-spacing: -1px;"),
          tags$h2("Teacher Support Helpline", style = "color: #93c5fd; font-size: 28px; font-weight: 400; margin-bottom: 30px;"),
          tags$p("A centralized query tracking and case management system serving teachers across all 16 regions of Ghana. Log cases, track resolutions, and monitor performance in real-time.",
                 style = "color: #cbd5e1; font-size: 18px; line-height: 1.7; max-width: 700px; margin: 0 auto 40px auto;"),
          
          div(style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap;",
              actionButton("hero_login_btn", "Staff Login",
                           class = "btn btn-lg", style = "background: #f59e0b; color: #0f172a; border: none; padding: 14px 40px; border-radius: 30px; font-size: 18px; font-weight: 700; box-shadow: 0 4px 15px rgba(245,158,11,0.4);"),
              actionButton("hero_analytics_btn", "View Public Analytics",
                           class = "btn btn-lg", style = "background: transparent; border: 2px solid #93c5fd; color: #93c5fd; padding: 14px 40px; border-radius: 30px; font-size: 18px; font-weight: 600;")
          )
      ),
      
      # Live Statistics Section
      div(style = "background: rgba(0,0,0,0.2); padding: 40px; margin: 0 40px 40px 40px; border-radius: 16px; max-width: 1120px; margin-left: auto; margin-right: auto;",
          tags$h3("Helpline Statistics", style = "color: white; text-align: center; margin-bottom: 30px; font-size: 24px; font-weight: 600;"),
          div(style = "display: flex; justify-content: space-around; flex-wrap: wrap; gap: 20px;",
              div(style = "text-align: center; min-width: 150px;",
                  tags$div(style = "font-size: 42px; font-weight: 800; color: #f59e0b;", uiOutput("landing_total_cases", inline = TRUE)),
                  tags$p("Cases Logged", style = "color: #94a3b8; margin: 5px 0 0 0; font-size: 14px;")
              ),
              div(style = "text-align: center; min-width: 150px;",
                  tags$div(style = "font-size: 42px; font-weight: 800; color: #10b981;", uiOutput("landing_resolved_cases", inline = TRUE)),
                  tags$p("Cases Resolved", style = "color: #94a3b8; margin: 5px 0 0 0; font-size: 14px;")
              ),
              div(style = "text-align: center; min-width: 150px;",
                  tags$div(style = "font-size: 42px; font-weight: 800; color: #3b82f6;", "16"),
                  tags$p("Regions Covered", style = "color: #94a3b8; margin: 5px 0 0 0; font-size: 14px;")
              ),
              div(style = "text-align: center; min-width: 150px;",
                  tags$div(style = "font-size: 42px; font-weight: 800; color: #8b5cf6;", "24/7"),
                  tags$p("Support Available", style = "color: #94a3b8; margin: 5px 0 0 0; font-size: 14px;")
              )
          )
      ),

      # Feature cards
      div(style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap; padding: 0 40px 40px 40px; max-width: 1200px; margin: 0 auto;",
          div(style = "background: rgba(255,255,255,0.1); border-radius: 16px; padding: 30px; flex: 1; min-width: 250px; max-width: 320px; backdrop-filter: blur(10px); border: 1px solid rgba(255,255,255,0.15);",
              tags$div(style = "font-size: 36px; margin-bottom: 15px;", icon("headset")),
              tags$h3("Case Management", style = "color: white; font-size: 20px; font-weight: 600; margin-bottom: 10px;"),
              tags$p("Log, track, and resolve teacher queries efficiently across all regions.", style = "color: #94a3b8; font-size: 14px; line-height: 1.6;")
          ),
          div(style = "background: rgba(255,255,255,0.1); border-radius: 16px; padding: 30px; flex: 1; min-width: 250px; max-width: 320px; backdrop-filter: blur(10px); border: 1px solid rgba(255,255,255,0.15);",
              tags$div(style = "font-size: 36px; margin-bottom: 15px;", icon("chart-line")),
              tags$h3("Real-Time Analytics", style = "color: white; font-size: 20px; font-weight: 600; margin-bottom: 10px;"),
              tags$p("Public dashboards showing regional performance, SLA monitoring, and trend analysis.", style = "color: #94a3b8; font-size: 14px; line-height: 1.6;")
          ),
          div(style = "background: rgba(255,255,255,0.1); border-radius: 16px; padding: 30px; flex: 1; min-width: 250px; max-width: 320px; backdrop-filter: blur(10px); border: 1px solid rgba(255,255,255,0.15);",
              tags$div(style = "font-size: 36px; margin-bottom: 15px;", icon("shield-alt")),
              tags$h3("Role-Based Access", style = "color: white; font-size: 20px; font-weight: 600; margin-bottom: 10px;"),
              tags$p("Secure access with regional controls. National staff see all; regional staff see their cases.", style = "color: #94a3b8; font-size: 14px; line-height: 1.6;")
          )
      ),

      # How It Works Section
      div(style = "background: rgba(255,255,255,0.05); padding: 50px 40px; margin-bottom: 0;",
          div(style = "max-width: 1000px; margin: 0 auto;",
              tags$h3("How It Works", style = "color: white; text-align: center; margin-bottom: 40px; font-size: 28px; font-weight: 600;"),
              div(style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 30px;",
                  div(style = "flex: 1; min-width: 200px; text-align: center;",
                      div(style = "width: 60px; height: 60px; background: #f59e0b; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px auto;",
                          tags$span("1", style = "color: white; font-size: 24px; font-weight: 700;")
                      ),
                      tags$h4("Report Issue", style = "color: white; font-size: 18px; margin-bottom: 10px;"),
                      tags$p("Teacher contacts regional helpline with their query or concern", style = "color: #94a3b8; font-size: 14px;")
                  ),
                  div(style = "flex: 1; min-width: 200px; text-align: center;",
                      div(style = "width: 60px; height: 60px; background: #3b82f6; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px auto;",
                          tags$span("2", style = "color: white; font-size: 24px; font-weight: 700;")
                      ),
                      tags$h4("Case Logged", style = "color: white; font-size: 18px; margin-bottom: 10px;"),
                      tags$p("Regional coordinator logs the case in the system with all details", style = "color: #94a3b8; font-size: 14px;")
                  ),
                  div(style = "flex: 1; min-width: 200px; text-align: center;",
                      div(style = "width: 60px; height: 60px; background: #8b5cf6; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px auto;",
                          tags$span("3", style = "color: white; font-size: 24px; font-weight: 700;")
                      ),
                      tags$h4("Track & Resolve", style = "color: white; font-size: 18px; margin-bottom: 10px;"),
                      tags$p("Case is tracked, escalated if needed, and resolved with follow-ups", style = "color: #94a3b8; font-size: 14px;")
                  ),
                  div(style = "flex: 1; min-width: 200px; text-align: center;",
                      div(style = "width: 60px; height: 60px; background: #10b981; border-radius: 50%; display: flex; align-items: center; justify-content: center; margin: 0 auto 15px auto;",
                          tags$span("4", style = "color: white; font-size: 24px; font-weight: 700;")
                      ),
                      tags$h4("Teacher Notified", style = "color: white; font-size: 18px; margin-bottom: 10px;"),
                      tags$p("Teacher receives updates and resolution notification", style = "color: #94a3b8; font-size: 14px;")
                  )
              )
          )
      ),

      # Contact Section
      div(style = "padding: 50px 40px; background: rgba(0,0,0,0.15);",
          div(style = "max-width: 1000px; margin: 0 auto; text-align: center;",
              tags$h3("Contact Your Regional Helpline", style = "color: white; margin-bottom: 30px; font-size: 24px; font-weight: 600;"),
              tags$p("Each region has a dedicated support coordinator. Contact your regional office for assistance.",
                     style = "color: #94a3b8; margin-bottom: 30px; font-size: 16px;"),
              div(style = "display: flex; justify-content: center; gap: 40px; flex-wrap: wrap;",
                  div(style = "text-align: center;",
                      icon("envelope", style = "font-size: 28px; color: #f59e0b; margin-bottom: 10px;"),
                      tags$p("Email", style = "color: white; font-weight: 600; margin-bottom: 5px;"),
                      tags$p("enquiry.[region]region@gmail.com", style = "color: #94a3b8; font-size: 14px;")
                  ),
                  div(style = "text-align: center;",
                      icon("clock", style = "font-size: 28px; color: #3b82f6; margin-bottom: 10px;"),
                      tags$p("Hours", style = "color: white; font-weight: 600; margin-bottom: 5px;"),
                      tags$p("Monday - Friday: 8am - 5pm", style = "color: #94a3b8; font-size: 14px;")
                  ),
                  div(style = "text-align: center;",
                      icon("map-marker-alt", style = "font-size: 28px; color: #10b981; margin-bottom: 10px;"),
                      tags$p("Coverage", style = "color: white; font-weight: 600; margin-bottom: 5px;"),
                      tags$p("All 16 Regions of Ghana", style = "color: #94a3b8; font-size: 14px;")
                  )
              )
          )
      ),

      # Footer
      div(style = "text-align: center; padding: 30px; border-top: 1px solid rgba(255,255,255,0.1);",
          tags$p("Ghana Education Service - Ministry of Education", style = "color: #64748b; font-size: 13px; margin: 0 0 5px 0;"),
          tags$p(paste0("Teacher Support Helpline System ", format(Sys.Date(), "%Y")), style = "color: #475569; font-size: 12px; margin: 0;")
      )
  ),
  
  # ========================================
  # LOGIN OVERLAY
  # ========================================
  hidden(
    div(id = "login_overlay",
        style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 100000; background: linear-gradient(135deg, #0f172a 0%, #1e3a8a 50%, #1e40af 100%); display: flex; align-items: center; justify-content: center;",
        
        div(style = "background: white; border-radius: 16px; padding: 40px; width: 420px; max-width: 90%; box-shadow: 0 25px 50px rgba(0,0,0,0.3);",
            
            # Back button
            div(style = "margin-bottom: 20px;",
                actionButton("login_back_btn", icon("arrow-left"), label = " Back to Home",
                             class = "btn btn-link", style = "color: #6b7280; padding: 0; font-size: 14px; text-decoration: none;")
            ),
            
            div(style = "text-align: center; margin-bottom: 30px;",
                tags$div(icon("user-shield"), style = "font-size: 48px; color: #1e3a8a; margin-bottom: 15px;"),
                tags$h2("Staff Login", style = "color: #1e3a8a; font-weight: 700; margin: 0 0 5px 0;"),
                tags$p("Sign in with your GES email address", style = "color: #6b7280; margin: 0; font-size: 14px;")
            ),
            
            div(
              textInput("login_email", "Email Address", placeholder = "enquiry.region@gmail.com",
                        width = "100%"),
              passwordInput("login_password", "Password", placeholder = "Enter your password",
                            width = "100%"),
              br(),
              actionButton("login_btn", "Sign In", class = "btn btn-primary btn-block",
                           style = "width: 100%; padding: 12px; font-size: 16px; font-weight: 600; background: #1e3a8a; border-color: #1e3a8a; border-radius: 8px;"),
              br(), br(),
              div(style = "text-align: center;",
                  tags$span(id = "login_msg_display", style = "color: #dc2626; font-weight: 500;",
                            textOutput("login_msg", inline = TRUE))
              )
            )
        )
    )
  ),

  # ========================================
  # PASSWORD CHANGE MODAL OVERLAY
  # ========================================
  hidden(
    div(id = "password_change_overlay",
        style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 100001; background: rgba(0,0,0,0.7); display: flex; align-items: center; justify-content: center;",

        div(style = "background: white; border-radius: 16px; padding: 40px; width: 450px; max-width: 90%; box-shadow: 0 25px 50px rgba(0,0,0,0.3);",

            div(style = "text-align: center; margin-bottom: 25px;",
                tags$div(icon("key"), style = "font-size: 48px; color: #f59e0b; margin-bottom: 15px;"),
                tags$h2("Change Your Password", style = "color: #1e3a8a; font-weight: 700; margin: 0 0 10px 0;"),
                uiOutput("password_change_subtitle")
            ),

            div(
              passwordInput("current_password", "Current Password", placeholder = "Enter current password", width = "100%"),
              passwordInput("new_password", "New Password", placeholder = "Enter new password (min 8 characters)", width = "100%"),
              passwordInput("confirm_password", "Confirm New Password", placeholder = "Re-enter new password", width = "100%"),

              # Password requirements hint
              div(style = "background: #f0f9ff; padding: 12px; border-radius: 8px; margin: 15px 0;",
                  tags$p(style = "margin: 0 0 8px 0; font-weight: 600; color: #1e3a8a; font-size: 13px;", "Password Requirements:"),
                  tags$ul(style = "margin: 0; padding-left: 20px; color: #64748b; font-size: 12px;",
                    tags$li("At least 8 characters long"),
                    tags$li("Contains at least one uppercase letter"),
                    tags$li("Contains at least one lowercase letter"),
                    tags$li("Contains at least one number")
                  )
              ),

              div(style = "display: flex; gap: 10px; margin-top: 20px;",
                  actionButton("change_password_btn", "Change Password",
                               class = "btn btn-primary",
                               style = "flex: 1; padding: 12px; font-size: 16px; font-weight: 600; background: #1e3a8a; border-color: #1e3a8a; border-radius: 8px;"),
                  uiOutput("skip_password_change_ui")
              ),

              br(),
              div(style = "text-align: center;",
                  tags$span(id = "password_change_msg_display", style = "color: #dc2626; font-weight: 500;",
                            textOutput("password_change_msg", inline = TRUE))
              )
            )
        )
    )
  ),

  # ========================================
  # MAIN DASHBOARD (hidden until login or analytics)
  # ========================================
  hidden(
    div(id = "main_app",
        dashboardPage(
          dashboardHeader(
            title = "GES Teacher Support",
            titleWidth = 200,
            # Horizontal Navigation Menu
            tags$li(class = "dropdown nav-menu-container",
                    uiOutput("header_nav_menu")
            ),
            # Quick Case Lookup in header
            tags$li(class = "dropdown quick-lookup-header",
                    uiOutput("header_quick_search")
            ),
            # User info and logout in header
            tags$li(class = "dropdown user-menu-header",
                    uiOutput("user_info_header")
            )
          ),

          # Hidden sidebar with menu for tab switching (clean menu)
          dashboardSidebar(
            collapsed = TRUE,
            disable = FALSE,
            sidebarMenu(
              id = "sidebar_menu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("New Case", tabName = "new_case", icon = icon("plus")),
              menuItem("All Cases", tabName = "all_cases", icon = icon("list-alt")),
              menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"))
            )
          )
          ,
          
          dashboardBody(
            use_theme(mytheme),
            
            # Hidden input for tab navigation
            shinyjs::hidden(textInput("current_tab", "", value = "dashboard")),

            # Enhanced CSS for modern styling and case details
            tags$head(
              # Load Font Awesome
              tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
              tags$style(HTML("
              /* ============================================ */
              /* HORIZONTAL NAVIGATION STYLING */
              /* ============================================ */

              /* Header styling */
              .main-header {
                max-height: none !important;
              }

              .main-header .navbar {
                background: linear-gradient(135deg, #1e3a8a 0%, #2c5aa0 100%) !important;
                margin-left: 200px !important;
                min-height: 60px !important;
              }

              .main-header .logo {
                background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%) !important;
                width: 200px !important;
                font-size: 16px !important;
                font-weight: 700 !important;
                height: 60px !important;
                line-height: 60px !important;
                padding: 0 10px !important;
              }

              /* Hide sidebar and its toggle completely */
              .sidebar-toggle {
                display: none !important;
              }

              .main-sidebar {
                display: none !important;
                width: 0 !important;
              }

              .left-side, .main-sidebar {
                width: 0 !important;
                min-width: 0 !important;
              }

              /* Content wrapper - full width without sidebar */
              .content-wrapper, .right-side, .main-footer {
                margin-left: 0 !important;
                background-color: #f8fafc;
              }

              body.sidebar-collapse .content-wrapper,
              body.sidebar-collapse .right-side,
              body.sidebar-collapse .main-footer {
                margin-left: 0 !important;
              }

              /* Navigation menu container */
              .nav-menu-container {
                display: flex !important;
                align-items: center !important;
                padding: 0 !important;
                margin: 0 !important;
              }

              .nav-menu-container > a {
                display: none !important;
              }

              /* Horizontal navigation buttons */
              .header-nav-menu {
                display: flex !important;
                align-items: center !important;
                gap: 5px !important;
                padding: 0 10px !important;
              }

              .nav-btn {
                display: inline-flex !important;
                align-items: center !important;
                padding: 12px 20px !important;
                font-size: 15px !important;
                font-weight: 600 !important;
                color: white !important;
                background: rgba(255, 255, 255, 0.1) !important;
                border: none !important;
                border-radius: 8px !important;
                cursor: pointer !important;
                transition: all 0.3s ease !important;
                text-decoration: none !important;
                white-space: nowrap !important;
                margin: 5px 3px !important;
              }

              .nav-btn:hover {
                background: rgba(255, 255, 255, 0.25) !important;
                transform: translateY(-2px) !important;
                box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2) !important;
              }

              .nav-btn.active {
                background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%) !important;
                box-shadow: 0 4px 12px rgba(245, 158, 11, 0.4) !important;
              }

              .nav-btn i {
                margin-right: 8px !important;
                font-size: 16px !important;
              }

              /* Quick lookup in header */
              .quick-lookup-header {
                display: flex !important;
                align-items: center !important;
                padding: 0 15px !important;
              }

              .quick-lookup-header > a {
                display: none !important;
              }

              .header-quick-search {
                display: flex !important;
                align-items: center !important;
                gap: 8px !important;
                background: rgba(255, 255, 255, 0.1) !important;
                padding: 6px 12px !important;
                border-radius: 8px !important;
              }

              .header-quick-search input {
                background: rgba(255, 255, 255, 0.15) !important;
                border: 1px solid rgba(255, 255, 255, 0.3) !important;
                color: white !important;
                padding: 8px 12px !important;
                border-radius: 6px !important;
                width: 160px !important;
                font-size: 14px !important;
              }

              .header-quick-search input::placeholder {
                color: rgba(255, 255, 255, 0.6) !important;
              }

              .header-quick-search input:focus {
                outline: none !important;
                border-color: #f59e0b !important;
                background: rgba(255, 255, 255, 0.25) !important;
              }

              .header-quick-search .btn {
                background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%) !important;
                border: none !important;
                color: white !important;
                padding: 8px 16px !important;
                border-radius: 6px !important;
                font-weight: 600 !important;
                font-size: 13px !important;
                cursor: pointer !important;
                transition: all 0.3s ease !important;
              }

              .header-quick-search .btn:hover {
                background: linear-gradient(135deg, #d97706 0%, #b45309 100%) !important;
                transform: translateY(-1px) !important;
              }

              /* User menu styling */
              .user-menu-header {
                padding: 0 15px !important;
              }

              .user-menu-header .user-info-box {
                display: flex !important;
                align-items: center !important;
                gap: 10px !important;
              }

              .user-menu-header .user-details {
                text-align: right !important;
                line-height: 1.3 !important;
              }

              .user-menu-header .user-name {
                color: white !important;
                font-weight: 600 !important;
                font-size: 14px !important;
              }

              .user-menu-header .user-role {
                color: rgba(255, 255, 255, 0.7) !important;
                font-size: 12px !important;
              }

              .user-menu-header .btn-logout {
                background: rgba(220, 38, 38, 0.8) !important;
                border: none !important;
                color: white !important;
                padding: 8px 16px !important;
                border-radius: 6px !important;
                font-weight: 600 !important;
                font-size: 13px !important;
                transition: all 0.3s ease !important;
              }

              .user-menu-header .btn-logout:hover {
                background: rgba(220, 38, 38, 1) !important;
              }

              /* ============================================ */
              /* END HORIZONTAL NAVIGATION STYLING */
              /* ============================================ */

              .content-wrapper, .right-side {
                background-color: #f8fafc;
              }

              .info-box {
                border-radius: 8px;
                box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
                border: none;
                margin-bottom: 20px;
              }

              .box {
                border-radius: 8px;
                box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
                border: none;
              }

              .btn-primary {
                background-color: #2563eb;
                border-color: #2563eb;
              }

              .case-code {
                font-weight: bold;
                color: #1e3a8a;
                cursor: pointer;
              }

              .case-code:hover {
                text-decoration: underline;
              }

              .priority-high { color: #dc2626; font-weight: bold; }
              .priority-medium { color: #ea580c; font-weight: bold; }
              .priority-low { color: #16a085; font-weight: bold; }
              .priority-urgent { color: #7c2d12; font-weight: bold; }

              .badge {
                padding: 4px 8px;
                border-radius: 4px;
                font-size: 11px;
                font-weight: bold;
              }

              .status-new { background-color: #3b82f6; color: white; }
              .status-in-progress { background-color: #f59e0b; color: white; }
              .status-waiting-on-teacher { background-color: #8b5cf6; color: white; }
              .status-escalated { background-color: #dc2626; color: white; }
              .status-resolved { background-color: #16a085; color: white; }
              .status-closed { background-color: #6b7280; color: white; }

              /* Escalation Popup Styles */
              .escalation-popup {
                position: fixed;
                top: 50%;
                left: 50%;
                transform: translate(-50%, -50%);
                background: linear-gradient(135deg, #dc2626 0%, #b91c1c 100%);
                color: white;
                padding: 30px;
                border-radius: 12px;
                box-shadow: 0 20px 50px rgba(0,0,0,0.3);
                z-index: 10000;
                min-width: 400px;
                max-width: 600px;
                animation: escalationPulse 0.5s ease-out;
              }

              @keyframes escalationPulse {
                0% { transform: translate(-50%, -50%) scale(0.8); opacity: 0; }
                50% { transform: translate(-50%, -50%) scale(1.05); }
                100% { transform: translate(-50%, -50%) scale(1); opacity: 1; }
              }

              .escalation-popup h3 {
                margin-top: 0;
                display: flex;
                align-items: center;
                gap: 10px;
              }

              .escalation-popup .close-btn {
                position: absolute;
                top: 10px;
                right: 10px;
                background: rgba(255,255,255,0.2);
                border: none;
                color: white;
                width: 30px;
                height: 30px;
                border-radius: 50%;
                cursor: pointer;
              }

              .escalation-popup .close-btn:hover {
                background: rgba(255,255,255,0.3);
              }

              .escalation-overlay {
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background: rgba(0,0,0,0.5);
                z-index: 9999;
              }

              /* Bulk Operations Styles */
              .bulk-actions-bar {
                background: linear-gradient(135deg, #1e3a8a 0%, #2c5aa0 100%);
                padding: 15px 20px;
                border-radius: 8px;
                margin-bottom: 15px;
                display: flex;
                align-items: center;
                gap: 15px;
                flex-wrap: wrap;
              }

              .bulk-actions-bar .selected-count {
                color: white;
                font-weight: 600;
                font-size: 14px;
              }

              .bulk-actions-bar .btn {
                background: rgba(255,255,255,0.2);
                border: 1px solid rgba(255,255,255,0.3);
                color: white;
              }

              .bulk-actions-bar .btn:hover {
                background: rgba(255,255,255,0.3);
              }

              /* Follow-up Calendar Styles */
              .follow-up-card {
                background: white;
                border-radius: 8px;
                padding: 15px;
                margin-bottom: 10px;
                border-left: 4px solid #3b82f6;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
              }

              .follow-up-card.overdue {
                border-left-color: #dc2626;
                background: #fef2f2;
              }

              .follow-up-card.due-today {
                border-left-color: #f59e0b;
                background: #fffbeb;
              }

              /* Template Card Styles */
              .template-card {
                background: white;
                border-radius: 8px;
                padding: 15px;
                margin-bottom: 10px;
                border: 1px solid #e5e7eb;
                cursor: pointer;
                transition: all 0.2s ease;
              }

              .template-card:hover {
                border-color: #3b82f6;
                box-shadow: 0 4px 12px rgba(59, 130, 246, 0.15);
              }

              .template-card.selected {
                border-color: #3b82f6;
                background: #eff6ff;
              }

              .template-category-badge {
                display: inline-block;
                padding: 2px 8px;
                border-radius: 4px;
                font-size: 11px;
                font-weight: 600;
                text-transform: uppercase;
              }

              .template-category-payroll { background: #dbeafe; color: #1e40af; }
              .template-category-welfare { background: #fee2e2; color: #991b1b; }
              .template-category-transfer { background: #d1fae5; color: #065f46; }
              .template-category-general { background: #e5e7eb; color: #374151; }
              .template-category-escalation { background: #fef3c7; color: #92400e; }

              .sla-overdue { color: #dc2626; font-weight: bold; }
              .sla-due-soon { color: #ea580c; font-weight: bold; }
              .sla-on-track { color: #16a085; }

              .case-details-header {
                background: linear-gradient(135deg, #1e3a8a 0%, #2c5aa0 100%);
                color: white;
                padding: 20px;
                border-radius: 8px 8px 0 0;
                margin: -15px -15px 20px -15px;
              }

              .case-info-grid {
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
                gap: 20px;
                margin-bottom: 20px;
              }

              .info-card {
                background: white;
                padding: 15px;
                border-radius: 8px;
                border-left: 4px solid #2563eb;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
              }

              .info-label {
                font-weight: bold;
                color: #374151;
                font-size: 12px;
                text-transform: uppercase;
                letter-spacing: 0.5px;
                margin-bottom: 5px;
              }

              .info-value {
                color: #1f2937;
                font-size: 14px;
              }

              .timeline-item {
                border-left: 3px solid #e5e7eb;
                padding-left: 15px;
                margin-bottom: 15px;
                position: relative;
              }

              .timeline-item:before {
                content: '';
                position: absolute;
                left: -6px;
                top: 5px;
                width: 9px;
                height: 9px;
                border-radius: 50%;
                background-color: #3b82f6;
              }

              .timeline-content {
                background: white;
                padding: 12px;
                border-radius: 6px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.1);
              }

              .timeline-header {
                display: flex;
                justify-content: space-between;
                align-items: center;
                margin-bottom: 5px;
              }

              .timeline-action {
                font-weight: bold;
                color: #1e3a8a;
              }

              .timeline-date {
                font-size: 11px;
                color: #6b7280;
              }

              .timeline-text {
                font-size: 13px;
                color: #374151;
                line-height: 1.4;
              }

              .modal-xl {
                width: 95%;
                max-width: 1200px;
              }

              .user-header-info {
                color: white;
                padding: 10px 15px;
                display: flex;
                align-items: center;
                gap: 10px;
              }
              .user-header-info .user-name {
                font-weight: 600;
                font-size: 13px;
              }
              .user-header-info .user-role {
                font-size: 11px;
                opacity: 0.8;
              }

              

              /* Old sidebar styles removed - sidebar is now hidden */

            "))
            ),
            
            # Case Details Modal
            tags$div(id = "caseDetailsModal", class = "modal fade", tabindex = "-1", role = "dialog",
                     tags$div(class = "modal-dialog modal-xl", role = "document",
                              tags$div(class = "modal-content",
                                       tags$div(class = "modal-body", style = "padding: 0;",
                                                uiOutput("case_details_content")
                                       ),
                                       tags$div(class = "modal-footer",
                                                actionButton("closeCaseDetails", "Close", class = "btn btn-default")
                                       )
                              )
                     )
            ),

            # Escalation Popup Modal (shows when case is escalated)
            hidden(
              tags$div(id = "escalationPopupOverlay", class = "escalation-overlay",
                onclick = "Shiny.setInputValue('close_escalation_popup', Math.random())"
              )
            ),
            hidden(
              tags$div(id = "escalationPopup", class = "escalation-popup",
                tags$button(class = "close-btn", onclick = "Shiny.setInputValue('close_escalation_popup', Math.random())",
                            icon("times")),
                tags$h3(icon("exclamation-triangle"), " Case Escalated!"),
                uiOutput("escalation_popup_content"),
                hr(),
                tags$p("This case has been sent to the National PRO Office for review."),
                tags$p(tags$strong("Email notification sent to:"), " enquiry.nationalprooffice@gmail.com"),
                br(),
                fluidRow(
                  column(6,
                    actionButton("view_escalated_case", "View Case Details", class = "btn btn-light", style = "width: 100%;")
                  ),
                  column(6,
                    actionButton("go_to_escalated_tab", "Go to Escalated Cases", class = "btn btn-light", style = "width: 100%;")
                  )
                )
              )
            ),

            # Bulk Status Update Modal
            tags$div(id = "bulkStatusModal", class = "modal fade", tabindex = "-1", role = "dialog",
                     tags$div(class = "modal-dialog", role = "document",
                              tags$div(class = "modal-content",
                                       tags$div(class = "modal-header",
                                                tags$h4(class = "modal-title", "Bulk Status Update")
                                       ),
                                       tags$div(class = "modal-body",
                                                selectInput("bulk_new_status", "New Status",
                                                            choices = c("New", "In Progress", "Waiting on Teacher", "Escalated", "Resolved", "Closed")),
                                                textAreaInput("bulk_status_notes", "Notes (Optional)",
                                                              placeholder = "Add notes for this bulk update...")
                                       ),
                                       tags$div(class = "modal-footer",
                                                actionButton("confirm_bulk_status", "Update All Selected", class = "btn btn-primary"),
                                                tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancel")
                                       )
                              )
                     )
            ),

            # Bulk Priority Change Modal
            tags$div(id = "bulkPriorityModal", class = "modal fade", tabindex = "-1", role = "dialog",
                     tags$div(class = "modal-dialog", role = "document",
                              tags$div(class = "modal-content",
                                       tags$div(class = "modal-header",
                                                tags$h4(class = "modal-title", "Bulk Priority Change")
                                       ),
                                       tags$div(class = "modal-body",
                                                selectInput("bulk_new_priority", "New Priority",
                                                            choices = c("Low", "Medium", "High", "Urgent"))
                                       ),
                                       tags$div(class = "modal-footer",
                                                actionButton("confirm_bulk_priority", "Update All Selected", class = "btn btn-primary"),
                                                tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancel")
                                       )
                              )
                     )
            ),

            # Escalation Reason Modal (for detailed escalation)
            tags$div(id = "escalationReasonModal", class = "modal fade", tabindex = "-1", role = "dialog",
                     tags$div(class = "modal-dialog", role = "document",
                              tags$div(class = "modal-content",
                                       tags$div(class = "modal-header", style = "background: #dc2626; color: white;",
                                                tags$h4(class = "modal-title", icon("exclamation-triangle"), " Escalate Case")
                                       ),
                                       tags$div(class = "modal-body",
                                                tags$p("You are about to escalate this case to the National PRO Office."),
                                                selectInput("escalation_level", "Escalation Level",
                                                            choices = c("Level 1 - Immediate Supervisor" = "1",
                                                                        "Level 2 - Directorate/Functional Lead" = "2",
                                                                        "Level 3 - Executive/TMTC Review" = "3",
                                                                        "Emergency - Immediate Action Required" = "emergency")),
                                                textAreaInput("escalation_reason_text", "Escalation Reason *",
                                                              placeholder = "Explain why this case needs escalation...",
                                                              height = "100px"),
                                                checkboxInput("notify_national_office", "Send email notification to National PRO Office", value = TRUE)
                                       ),
                                       tags$div(class = "modal-footer",
                                                actionButton("confirm_escalation_with_reason", "Escalate Now", class = "btn btn-danger"),
                                                tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancel")
                                       )
                              )
                     )
            ),

            tabItems(
              # Dashboard Tab with sub-tabs
              tabItem(
                tabName = "dashboard",
                tabsetPanel(
                  # Case Summary Tab
                  tabPanel(
                    title = "Case Summary",
                    br(),
                    # Refresh Bar at top
                    fluidRow(
                      column(12,
                        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; padding: 10px 15px; background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%); border-radius: 8px;",
                            div(style = "color: white;",
                                icon("chart-line", style = "margin-right: 8px;"),
                                tags$span("Case Summary Dashboard", style = "font-weight: 600; font-size: 16px;"),
                                tags$small(textOutput("last_refresh_time", inline = TRUE), style = "margin-left: 15px; opacity: 0.8;")
                            ),
                            actionButton("refresh_case_summary", "Refresh Data",
                                        class = "btn btn-light",
                                        icon = icon("sync"),
                                        style = "font-weight: 500;")
                        )
                      )
                    ),
                    # Follow-up Alert Widget
                    uiOutput("followup_alert_widget"),
                    # Row 1: Primary KPIs
                    fluidRow(
                      infoBoxOutput("total_cases"),
                      infoBoxOutput("new_cases"),
                      infoBoxOutput("in_progress_cases"),
                      infoBoxOutput("escalated_cases")
                    ),
                    # Row 2: Secondary KPIs
                    fluidRow(
                      infoBoxOutput("waiting_cases"),
                      infoBoxOutput("resolved_cases"),
                      infoBoxOutput("closed_cases"),
                      infoBoxOutput("overdue_cases_kpi")
                    ),
                    
                    fluidRow(
                      box(
                        title = "Performance Metrics", status = "primary", solidHeader = TRUE,
                        width = 6, height = 300,
                        tableOutput("performance_metrics")
                      ),
                      
                      box(
                        title = "Cases by Priority", status = "primary", solidHeader = TRUE,
                        width = 6, height = 300,
                        withSpinner(plotlyOutput("priority_chart", height = "250px"))
                      )
                    ),
                    
                    fluidRow(
                      box(
                        title = "Recent Cases", status = "info", solidHeader = TRUE,
                        width = 12,
                        fluidRow(
                          column(12, style = "text-align: right; margin-bottom: 10px;",
                                 tags$small(textOutput("auto_refresh_status", inline = TRUE), style = "color: #6b7280;"),
                                 actionButton("manual_refresh_dashboard", "Refresh Now", class = "btn-xs btn-info", style = "margin-left: 10px;")
                          )
                        ),
                        withSpinner(DT::dataTableOutput("recent_cases_table"))
                      )
                    )
                  ),
                  
                  # Detail of Cases Tab
                  tabPanel(
                    title = "Detail of Cases",
                    br(),
                    fluidRow(
                      box(
                        title = "My Cases", status = "primary", solidHeader = TRUE,
                        width = 12,

                        fluidRow(
                          column(4,
                                 selectInput("my_status_filter", "Status Filter",
                                             choices = c("All", "New", "In Progress", "Waiting on Teacher", "Escalated", "Resolved", "Closed"))
                          ),
                          column(4,
                                 uiOutput("my_region_filter_ui")
                          ),
                          column(4,
                                 actionButton("refresh_my_cases", "Refresh", class = "btn-info",
                                              style = "margin-top: 25px;")
                          )
                        ),

                        withSpinner(DT::dataTableOutput("my_cases_table"))
                      )
                    )
                  ),

                  # Follow-ups Tab
                  tabPanel(
                    title = "Follow-ups",
                    icon = icon("calendar-check"),
                    br(),
                    # Summary stats row
                    fluidRow(
                      valueBoxOutput("followup_overdue_box", width = 3),
                      valueBoxOutput("followup_today_box", width = 3),
                      valueBoxOutput("followup_week_box", width = 3),
                      valueBoxOutput("followup_total_box", width = 3)
                    ),

                    fluidRow(
                      # Pending Follow-ups
                      box(
                        title = "Pending Follow-ups", status = "warning", solidHeader = TRUE,
                        width = 8,
                        fluidRow(
                          column(3,
                            selectInput("followup_urgency_filter", "Filter by Urgency",
                                        choices = c("All" = "", "Overdue", "Due Today", "Upcoming"))
                          ),
                          column(3,
                            uiOutput("followup_region_filter_ui")
                          ),
                          column(3,
                            actionButton("refresh_followups", "Refresh", class = "btn-warning",
                                         style = "margin-top: 25px;", icon = icon("sync"))
                          ),
                          column(3,
                            downloadButton("export_followups", "Export to Excel",
                                           class = "btn-success", style = "margin-top: 25px;")
                          )
                        ),
                        hr(),
                        withSpinner(DT::dataTableOutput("pending_followups_table"))
                      ),

                      # Schedule New Follow-up
                      box(
                        title = "Schedule Follow-up", status = "info", solidHeader = TRUE,
                        width = 4,
                        textInput("followup_case_search", "Case Code or Teacher Name",
                                  placeholder = "Enter to search..."),
                        dateInput("followup_date", "Follow-up Date", value = Sys.Date() + 3),
                        textAreaInput("followup_notes", "Notes",
                                      placeholder = "What needs to be followed up...",
                                      height = "80px"),
                        actionButton("schedule_followup", "Schedule Follow-up",
                                     class = "btn-info btn-block",
                                     icon = icon("calendar-plus"))
                      )
                    )
                  ),

                  # Response Guide Tab (Call Scripts)
                  tabPanel(
                    title = "Response Guide",
                    icon = icon("comments"),
                    br(),
                    fluidRow(
                      # Quick Responses Column
                      box(
                        title = "Quick Responses", status = "success", solidHeader = TRUE,
                        width = 4,
                        p("Click to copy common phrases:", style = "color: #6b7280; font-size: 13px;"),

                        h5("Acknowledgements", style = "margin-top: 15px; color: #1e3a8a;"),
                        actionButton("qr_ack_received", "Case Received", class = "btn-sm btn-outline-primary", style = "margin: 2px;"),
                        actionButton("qr_ack_escalated", "Case Escalated", class = "btn-sm btn-outline-primary", style = "margin: 2px;"),
                        actionButton("qr_ack_processing", "Being Processed", class = "btn-sm btn-outline-primary", style = "margin: 2px;"),

                        h5("Status Updates", style = "margin-top: 15px; color: #1e3a8a;"),
                        actionButton("qr_status_pending", "Pending Info", class = "btn-sm btn-outline-warning", style = "margin: 2px;"),
                        actionButton("qr_status_followup", "Follow-up Set", class = "btn-sm btn-outline-warning", style = "margin: 2px;"),
                        actionButton("qr_status_resolved", "Case Resolved", class = "btn-sm btn-outline-success", style = "margin: 2px;"),

                        h5("Professional Phrases", style = "margin-top: 15px; color: #1e3a8a;"),
                        actionButton("qr_phrase_understand", "I understand...", class = "btn-sm btn-outline-secondary", style = "margin: 2px;"),
                        actionButton("qr_phrase_patience", "Thank you for patience...", class = "btn-sm btn-outline-secondary", style = "margin: 2px;"),
                        actionButton("qr_phrase_assist", "Let me assist...", class = "btn-sm btn-outline-secondary", style = "margin: 2px;"),

                        h5("Closings", style = "margin-top: 15px; color: #1e3a8a;"),
                        actionButton("qr_close_help", "Anything else?", class = "btn-sm btn-outline-info", style = "margin: 2px;"),
                        actionButton("qr_close_contact", "Feel free to contact...", class = "btn-sm btn-outline-info", style = "margin: 2px;"),
                        actionButton("qr_close_reference", "Your reference number...", class = "btn-sm btn-outline-info", style = "margin: 2px;"),

                        hr(),
                        h5("Copied Text:"),
                        verbatimTextOutput("quick_response_text", placeholder = TRUE)
                      ),

                      # Response Templates Column
                      box(
                        title = "Response Templates", status = "primary", solidHeader = TRUE,
                        width = 8,
                        fluidRow(
                          column(4,
                            selectInput("template_category_filter", "Category",
                                        choices = c("All" = "", "Payroll", "CPD/Licensing", "ICT", "Transfer",
                                                    "Welfare", "General", "Escalation"))
                          ),
                          column(6,
                            textInput("template_search", "Search Templates",
                                      placeholder = "Search by name or content...")
                          ),
                          column(2,
                            actionButton("refresh_templates", "Refresh", class = "btn-primary",
                                         style = "margin-top: 25px;", icon = icon("sync"))
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(5,
                            h5("Available Templates"),
                            div(style = "max-height: 350px; overflow-y: auto;",
                              uiOutput("templates_list")
                            )
                          ),
                          column(7,
                            h5("Preview"),
                            wellPanel(style = "min-height: 200px; background: #f8fafc;",
                              uiOutput("template_preview")
                            ),
                            actionButton("use_template", "Copy Template",
                                         class = "btn-primary", icon = icon("copy"))
                          )
                        )
                      )
                    )
                  ),

                  # Escalated Cases Tab (Only for National PRO Office)
                  tabPanel(
                    title = uiOutput("escalated_tab_title"),
                    value = "escalated_panel",
                    br(),
                    uiOutput("escalated_cases_panel")
                  )
                )
              ),
              
              # New Case Tab (unchanged from original)
              tabItem(
                tabName = "new_case",
                fluidRow(
                  box(
                    title = "Log New Teacher Support Case", status = "primary", solidHeader = TRUE,
                    width = 12,
                    
                    h4("Teacher/Caller Information", style = "color: #1e3a8a; margin-bottom: 15px;"),
                    
                    fluidRow(
                      column(6,
                             textInput("teacher_name", "Teacher/Caller Name *", 
                                       placeholder = "Full name of person calling"),
                             textInput("teacher_phone", "Contact Number *", 
                                       placeholder = "+233XXXXXXXXX or 0XXXXXXXXX"),
                             textInput("teacher_staff_id", "Staff ID", 
                                       placeholder = "Teacher staff ID (if available)")
                      ),
                      column(6,
                             uiOutput("region_select_ui"),
                             textInput("district", "District", placeholder = "District name"),
                             textInput("school_name", "School Name", placeholder = "Name of school")
                      )
                    ),
                    
                    hr(),
                    
                    h4("Case Details", style = "color: #1e3a8a; margin-bottom: 15px;"),
                    
                    fluidRow(
                      column(6,
                             uiOutput("channel_select_ui"),
                             uiOutput("category_select_ui"),
                             uiOutput("subcategory_select_ui")
                      ),
                      column(6,
                             selectInput("priority", "Priority Level *",
                                         choices = c("Low" = "Low", "Medium" = "Medium", "High" = "High", "Urgent" = "Urgent"),
                                         selected = "Medium"),
                             checkboxInput("consent_contact", "Teacher agrees to be contacted on this number", value = TRUE)
                      )
                    ),
                    
                    fluidRow(
                      column(12,
                             textAreaInput("case_summary", "Case Summary *", 
                                           placeholder = "Brief summary of the issue (minimum 20 characters)...",
                                           height = "100px"),
                             textAreaInput("case_description", "Detailed Description", 
                                           placeholder = "Provide detailed information about the issue, including any relevant background, what the teacher has tried, and any specific questions they have...",
                                           height = "120px")
                      )
                    ),
                    
                    hr(),
                    
                    fluidRow(
                      column(12, align = "center",
                             actionButton("save_case", "Create Case", 
                                          class = "btn-primary btn-lg", 
                                          style = "margin: 10px; padding: 10px 30px;"),
                             actionButton("clear_form", "Clear Form", 
                                          class = "btn-default btn-lg",
                                          style = "margin: 10px; padding: 10px 30px;")
                      )
                    )
                  )
                )
              ),
              
              # All Cases Tab
              tabItem(
                tabName = "all_cases",
                fluidRow(
                  box(
                    title = "All Cases", status = "primary", solidHeader = TRUE,
                    width = 12,
                    
                    # Row 1: Search and status/region/category filters
                    fluidRow(
                      column(4,
                             textInput("all_search_text", "Search (Name, Phone, Staff ID, Case Code, School)",
                                       placeholder = "Type to search...")
                      ),
                      column(2,
                             selectInput("all_status_filter", "Status",
                                         choices = c("All", "New", "In Progress", "Waiting on Teacher", "Escalated", "Resolved", "Closed"))
                      ),
                      column(2,
                             uiOutput("all_region_filter_ui")
                      ),
                      column(2,
                             uiOutput("all_category_filter_ui")
                      ),
                      column(2,
                             actionButton("refresh_all_cases", "Refresh", class = "btn-info",
                                          style = "margin-top: 25px; width: 100%;")
                      )
                    ),
                    
                    # Row 2: Date range filters
                    fluidRow(
                      column(3,
                             dateInput("all_date_from", "From Date", value = NULL, format = "yyyy-mm-dd")
                      ),
                      column(3,
                             dateInput("all_date_to", "To Date", value = NULL, format = "yyyy-mm-dd")
                      ),
                      column(3,
                             actionButton("clear_all_filters", "Clear All Filters", class = "btn-default",
                                          style = "margin-top: 25px;")
                      ),
                      column(3,
                             div(style = "margin-top: 25px; text-align: right;",
                                 downloadButton("export_cases_csv", "Export CSV", class = "btn-sm btn-success")
                             )
                      )
                    ),
                    
                    hr(),

                    # Bulk Actions Bar (hidden by default)
                    hidden(
                      div(id = "bulk_actions_bar", class = "bulk-actions-bar",
                        span(class = "selected-count", textOutput("bulk_selected_count", inline = TRUE)),
                        actionButton("bulk_update_status", "Update Status", icon = icon("edit"), class = "btn-sm"),
                        actionButton("bulk_change_priority", "Change Priority", icon = icon("flag"), class = "btn-sm"),
                        actionButton("bulk_assign", "Assign", icon = icon("user-plus"), class = "btn-sm"),
                        actionButton("bulk_escalate", "Escalate All", icon = icon("exclamation-triangle"), class = "btn-sm btn-danger"),
                        actionButton("bulk_clear_selection", "Clear Selection", icon = icon("times"), class = "btn-sm")
                      )
                    ),

                    withSpinner(DT::dataTableOutput("all_cases_table"))
                  )
                )
              ),

              # Analytics Tab
              tabItem(
                tabName = "analytics",
                
                # Top KPIs
                fluidRow(
                  infoBoxOutput("analytics_total"),
                  infoBoxOutput("analytics_resolved"),
                  infoBoxOutput("analytics_resolution_rate"),
                  infoBoxOutput("analytics_avg_time")
                ),
                
                br(),
                
                tabsetPanel(
                  id = "analytics_tabs",
                  
                  # 1) Regional Performance
                  tabPanel(
                    "Regional Performance",
                    fluidRow(
                      box(
                        title = "Resolution Rate by Region",
                        status = "primary", solidHeader = TRUE,
                        width = 8, height = 420,
                        withSpinner(plotlyOutput("region_resolution_chart", height = "360px"))
                      ),
                      box(
                        title = "Backlog by Region",
                        status = "info", solidHeader = TRUE,
                        width = 4, height = 420,
                        withSpinner(plotlyOutput("region_backlog_chart", height = "360px"))
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Regional Performance Table",
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        withSpinner(DTOutput("region_table"))
                      )
                    )
                  ),
                  
                  # 2) Category Trends
                  tabPanel(
                    "Category Trends",
                    fluidRow(
                      box(
                        title = "Top Categories Over Time",
                        status = "primary", solidHeader = TRUE,
                        width = 8, height = 420,
                        withSpinner(plotlyOutput("category_trend_chart", height = "360px"))
                      ),
                      box(
                        title = "Current Month Movement",
                        status = "warning", solidHeader = TRUE,
                        width = 4, height = 420,
                        withSpinner(DTOutput("category_movement_table"))
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Category Trend Table (All)",
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        withSpinner(DTOutput("category_trend_table"))
                      )
                    )
                  ),
                  
                  # 3) SLA Monitoring
                  tabPanel(
                    "SLA Monitoring",
                    fluidRow(
                      box(
                        title = "SLA Overview",
                        status = "primary", solidHeader = TRUE,
                        width = 6, height = 360,
                        withSpinner(plotlyOutput("sla_overview_chart", height = "300px"))
                      ),
                      box(
                        title = "SLA by Region",
                        status = "info", solidHeader = TRUE,
                        width = 6, height = 360,
                        withSpinner(plotlyOutput("sla_region_chart", height = "300px"))
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Overdue Cases",
                        status = "danger", solidHeader = TRUE,
                        width = 12,
                        withSpinner(DTOutput("sla_overdue_table"))
                      )
                    )
                  ),
                  
                  # 4) Time Series
                  tabPanel(
                    "Time Series",
                    fluidRow(
                      box(
                        title = "Monthly Created vs Resolved",
                        status = "primary", solidHeader = TRUE,
                        width = 8, height = 420,
                        withSpinner(plotlyOutput("monthly_created_resolved_chart", height = "360px"))
                      ),
                      box(
                        title = "Average Resolution Time (Hours)",
                        status = "info", solidHeader = TRUE,
                        width = 4, height = 420,
                        withSpinner(plotlyOutput("monthly_avg_resolution_chart", height = "360px"))
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Monthly Trend Table",
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        withSpinner(DTOutput("monthly_trend_table"))
                      )
                    )
                  ),
                  
                  # 5) Exports
                  tabPanel(
                    "Exports",
                    fluidRow(
                      box(
                        title = "Export Reports",
                        status = "primary", solidHeader = TRUE,
                        width = 12,
                        p("Download the latest analytics outputs for reporting and briefings."),
                        br(),
                        downloadButton("export_excel", "Download Excel", class = "btn-primary"),
                        tags$span(style = "padding-left: 10px;"),
                        downloadButton("export_pdf", "Download PDF", class = "btn-secondary"),
                        br(), br(),
                        tags$small("Note: PDF export may require additional server setup (R Markdown/LaTeX). If unavailable, we can export HTML instead.")
                      )
                    )
                  )
                )
              )
              ####
            )
          )
        ) # end dashboardPage
    ) # end div#main_app
  ) # end hidden
) # end tagList (ui)

# Server Logic with Enhanced Case Management
server <- function(input, output, session) {
  
  # ========================================
  # Authentication & Page State Management
  # ========================================
  rv <- reactiveValues(
    logged_in = FALSE,
    user = NULL,
    page_state = "landing",  # "landing", "login", "analytics", "dashboard"
    last_escalated_case = NULL,  # Store escalated case for popup
    quick_response_text = "",  # Store quick response text for templates
    first_time_login = FALSE,  # Track if this is user's first login
    show_password_change = FALSE  # Track if password change modal should show
  )
  
  # --- Landing page button handlers ---
  observeEvent(input$landing_login_btn, { rv$page_state <- "login" })
  observeEvent(input$hero_login_btn, { rv$page_state <- "login" })
  observeEvent(input$landing_analytics_btn, { rv$page_state <- "analytics" })
  observeEvent(input$hero_analytics_btn, { rv$page_state <- "analytics" })
  observeEvent(input$login_back_btn, {
    rv$page_state <- "landing"
    output$login_msg <- renderText("")
  })

  # --- Landing page live statistics ---
  output$landing_total_cases <- renderUI({
    tryCatch({
      count <- dbGetQuery(pool, "SELECT COUNT(*) as count FROM tickets")$count
      tags$span(format(count, big.mark = ","))
    }, error = function(e) {
      tags$span("--")
    })
  })

  output$landing_resolved_cases <- renderUI({
    tryCatch({
      count <- dbGetQuery(pool, "SELECT COUNT(*) as count FROM tickets WHERE status IN ('Resolved', 'Closed')")$count
      tags$span(format(count, big.mark = ","))
    }, error = function(e) {
      tags$span("--")
    })
  })
  
  # --- Observe page state changes and show/hide overlays ---
  observeEvent(rv$page_state, {
    state <- rv$page_state
    
    if (state == "landing") {
      shinyjs::show("landing_overlay")
      shinyjs::hide("login_overlay")
      shinyjs::hide("main_app")
    } else if (state == "login") {
      shinyjs::hide("landing_overlay")
      shinyjs::show("login_overlay")
      shinyjs::hide("main_app")
    } else if (state == "analytics") {
      shinyjs::hide("landing_overlay")
      shinyjs::hide("login_overlay")
      shinyjs::show("main_app")
      # Navigate to analytics tab with delay to ensure menu is rendered
      shinyjs::delay(100, updateTabItems(session, "sidebar_menu", selected = "analytics"))
    } else if (state == "dashboard") {
      shinyjs::hide("landing_overlay")
      shinyjs::hide("login_overlay")
      shinyjs::show("main_app")
      # Navigate to dashboard tab with delay to ensure menu is rendered
      shinyjs::delay(100, updateTabItems(session, "sidebar_menu", selected = "dashboard"))
    }
  })
  
  # --- Login handler ---
  get_user_by_email <- function(conn, email) {
    DBI::dbGetQuery(conn, "
      SELECT user_id, full_name, email, role, region_id, is_active, password_hash
      FROM users
      WHERE LOWER(email) = LOWER(?)
      LIMIT 1
    ", params = list(email))
  }
  
  is_allowlisted <- function(conn, email) {
    out <- DBI::dbGetQuery(conn, "
      SELECT email, is_active
      FROM login_allowlist
      WHERE LOWER(email) = LOWER(?)
      LIMIT 1
    ", params = list(email))
    nrow(out) == 1 && isTRUE(out$is_active[1] == 1)
  }
  
  can_see_all_regions <- function(role) {
    role %in% c("National Admin", "National Resolver")
  }
  
  # Log activity helper
  log_activity <- function(conn, user_id, action, details = NULL) {
    tryCatch({
      DBI::dbExecute(conn,
                     "INSERT INTO login_audit (email, user_id, success, reason)
         VALUES ((SELECT email FROM users WHERE user_id = ?), ?, 1, ?)",
                     params = list(user_id, user_id, paste0("ACTION: ", action, if (!is.null(details)) paste0(" | ", details) else ""))
      )
    }, error = function(e) {
      # Silently fail on activity logging errors
    })
  }
  
  observeEvent(input$login_btn, {
    req(input$login_email, input$login_password)
    email <- trimws(tolower(input$login_email))
    pwd <- input$login_password
    
    tryCatch({
      poolWithTransaction(pool, function(conn) {
        
        if (!is_allowlisted(conn, email)) {
          try(DBI::dbExecute(conn,
                             "INSERT INTO login_audit (email, success, reason) VALUES (?, 0, ?)",
                             params = list(email, "Email not allowlisted or inactive")
          ), silent = TRUE)
          output$login_msg <- renderText("Access denied. Email not authorized.")
          return()
        }
        
        u <- get_user_by_email(conn, email)
        if (nrow(u) == 0 || !isTRUE(u$is_active[1] == 1)) {
          try(DBI::dbExecute(conn,
                             "INSERT INTO login_audit (email, success, reason) VALUES (?, 0, ?)",
                             params = list(email, "User missing or inactive")
          ), silent = TRUE)
          output$login_msg <- renderText("Account not available.")
          return()
        }
        
        ok <- FALSE
        try({ ok <- bcrypt::checkpw(pwd, u$password_hash[1]) }, silent = TRUE)
        
        if (!isTRUE(ok)) {
          try(DBI::dbExecute(conn,
                             "INSERT INTO login_audit (email, user_id, success, reason) VALUES (?, ?, 0, ?)",
                             params = list(email, u$user_id[1], "Invalid password")
          ), silent = TRUE)
          output$login_msg <- renderText("Invalid credentials.")
          return()
        }
        
        # Successful login
        try(DBI::dbExecute(conn,
                           "INSERT INTO login_audit (email, user_id, success, reason) VALUES (?, ?, 1, 'Login successful')",
                           params = list(email, u$user_id[1])
        ), silent = TRUE)

        # Check if this is a first-time login (last_login is NULL)
        is_first_login <- is.null(u$last_login[1]) || is.na(u$last_login[1])

        # Update last_login
        try(DBI::dbExecute(conn,
                           "UPDATE users SET last_login = NOW() WHERE user_id = ?",
                           params = list(u$user_id[1])
        ), silent = TRUE)

        rv$logged_in <- TRUE
        rv$user <- u[1, ]
        rv$first_time_login <- is_first_login
        output$login_msg <- renderText("")

        # If first-time login, show password change modal
        if (is_first_login) {
          rv$show_password_change <- TRUE
          rv$page_state <- "dashboard"  # Go to dashboard but show overlay
          shinyjs::show("password_change_overlay")
          shinyjs::hide("login_overlay")
          shinyjs::show("main_app")
          showNotification("Welcome! Please change your password for security.", type = "warning", duration = 8)
        } else {
          rv$page_state <- "dashboard"
        }

        # Check for overdue follow-ups and show alert after login (only if not first time)
        if (!is_first_login) {
          tryCatch({
            user_region <- if (can_see_all_regions(u$role[1])) NULL else u$region_id[1]

            overdue_query <- "
              SELECT COUNT(*) as count FROM follow_ups f
              JOIN tickets t ON f.ticket_id = t.ticket_id
              WHERE f.status = 'Pending' AND f.follow_up_date < CURDATE()"

            if (!is.null(user_region)) {
              overdue_query <- paste0(overdue_query, " AND t.region_id = ", user_region)
            }

            overdue_count <- dbGetQuery(conn, overdue_query)$count

            if (overdue_count > 0) {
              showNotification(
                paste0("You have ", overdue_count, " overdue follow-up(s) that need attention!"),
                type = "warning",
                duration = 10
              )
            }
          }, error = function(e) {})
        }
      })
    }, error = function(e) {
      output$login_msg <- renderText("Connection error. Please try again.")
    })
  })
  
  # --- Logout handler ---
  observeEvent(input$logout_btn, {
    if (!is.null(rv$user)) {
      tryCatch({
        log_activity(pool, rv$user$user_id, "Logout")
      }, error = function(e) {})
    }
    rv$logged_in <- FALSE
    rv$user <- NULL
    rv$first_time_login <- FALSE
    rv$show_password_change <- FALSE
    rv$page_state <- "landing"
    updateTextInput(session, "login_email", value = "")
    updateTextInput(session, "login_password", value = "")
    shinyjs::hide("password_change_overlay")
  })

  # ========================================
  # PASSWORD CHANGE HANDLERS
  # ========================================

  # Password change subtitle (different message for first-time vs voluntary)
  output$password_change_subtitle <- renderUI({
    if (isTRUE(rv$first_time_login)) {
      tags$p("For security, please change your default password.", style = "color: #6b7280; margin: 0; font-size: 14px;")
    } else {
      tags$p("Enter your current password and choose a new one.", style = "color: #6b7280; margin: 0; font-size: 14px;")
    }
  })

  # Skip button UI (only show for first-time login, with warning)
  output$skip_password_change_ui <- renderUI({
    if (isTRUE(rv$first_time_login)) {
      actionButton("skip_password_change", "Skip for Now",
                   class = "btn btn-outline-secondary",
                   style = "padding: 12px 20px; font-size: 14px; border-radius: 8px;")
    } else {
      actionButton("cancel_password_change", "Cancel",
                   class = "btn btn-outline-secondary",
                   style = "padding: 12px 20px; font-size: 14px; border-radius: 8px;")
    }
  })

  # Open password change from header button
  observeEvent(input$open_change_password, {
    rv$show_password_change <- TRUE
    rv$first_time_login <- FALSE  # This is a voluntary change
    shinyjs::show("password_change_overlay")
    updateTextInput(session, "current_password", value = "")
    updateTextInput(session, "new_password", value = "")
    updateTextInput(session, "confirm_password", value = "")
    output$password_change_msg <- renderText("")
  })

  # Skip password change (first-time login only)
  observeEvent(input$skip_password_change, {
    rv$show_password_change <- FALSE
    rv$first_time_login <- FALSE
    shinyjs::hide("password_change_overlay")
    showNotification("You can change your password anytime from the key icon in the header.", type = "message", duration = 6)
  })

  # Cancel password change (voluntary change)
  observeEvent(input$cancel_password_change, {
    rv$show_password_change <- FALSE
    shinyjs::hide("password_change_overlay")
    updateTextInput(session, "current_password", value = "")
    updateTextInput(session, "new_password", value = "")
    updateTextInput(session, "confirm_password", value = "")
    output$password_change_msg <- renderText("")
  })

  # Password validation function
  validate_password <- function(password) {
    if (nchar(password) < 8) {
      return(list(valid = FALSE, msg = "Password must be at least 8 characters long."))
    }
    if (!grepl("[A-Z]", password)) {
      return(list(valid = FALSE, msg = "Password must contain at least one uppercase letter."))
    }
    if (!grepl("[a-z]", password)) {
      return(list(valid = FALSE, msg = "Password must contain at least one lowercase letter."))
    }
    if (!grepl("[0-9]", password)) {
      return(list(valid = FALSE, msg = "Password must contain at least one number."))
    }
    return(list(valid = TRUE, msg = ""))
  }

  # Change password handler
  observeEvent(input$change_password_btn, {
    req(rv$user)

    current_pwd <- input$current_password
    new_pwd <- input$new_password
    confirm_pwd <- input$confirm_password

    # Validate inputs
    if (is.null(current_pwd) || current_pwd == "") {
      output$password_change_msg <- renderText("Please enter your current password.")
      return()
    }

    if (is.null(new_pwd) || new_pwd == "") {
      output$password_change_msg <- renderText("Please enter a new password.")
      return()
    }

    if (is.null(confirm_pwd) || confirm_pwd == "") {
      output$password_change_msg <- renderText("Please confirm your new password.")
      return()
    }

    if (new_pwd != confirm_pwd) {
      output$password_change_msg <- renderText("New passwords do not match.")
      return()
    }

    if (new_pwd == current_pwd) {
      output$password_change_msg <- renderText("New password must be different from current password.")
      return()
    }

    # Validate password strength
    validation <- validate_password(new_pwd)
    if (!validation$valid) {
      output$password_change_msg <- renderText(validation$msg)
      return()
    }

    # Verify current password
    tryCatch({
      conn <- poolCheckout(pool)
      on.exit(poolReturn(conn))

      user_data <- dbGetQuery(conn, "SELECT password_hash FROM users WHERE user_id = ?",
                              params = list(rv$user$user_id))

      if (nrow(user_data) == 0) {
        output$password_change_msg <- renderText("User not found.")
        return()
      }

      # Check current password
      ok <- FALSE
      try({ ok <- bcrypt::checkpw(current_pwd, user_data$password_hash[1]) }, silent = TRUE)

      if (!isTRUE(ok)) {
        output$password_change_msg <- renderText("Current password is incorrect.")
        return()
      }

      # Hash new password and update
      new_hash <- bcrypt::hashpw(new_pwd)

      dbExecute(conn, "UPDATE users SET password_hash = ? WHERE user_id = ?",
                params = list(new_hash, rv$user$user_id))

      # Log the password change
      try(dbExecute(conn,
                    "INSERT INTO login_audit (email, user_id, success, reason) VALUES (?, ?, 1, 'Password changed')",
                    params = list(rv$user$email, rv$user$user_id)
      ), silent = TRUE)

      # Close the modal and show success
      rv$show_password_change <- FALSE
      rv$first_time_login <- FALSE
      shinyjs::hide("password_change_overlay")
      updateTextInput(session, "current_password", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_password", value = "")
      output$password_change_msg <- renderText("")

      showNotification("Password changed successfully!", type = "message", duration = 5)

    }, error = function(e) {
      output$password_change_msg <- renderText(paste("Error:", e$message))
    })
  })

  # --- Back to landing from analytics (for non-logged-in users) ---
  observeEvent(input$back_to_landing_btn, {
    rv$page_state <- "landing"
  })
  
  # --- Auth reactive output (for conditionalPanel if needed) ---
  output$auth <- reactive({ isTRUE(rv$logged_in) })
  outputOptions(output, "auth", suspendWhenHidden = FALSE)
  
  # --- User info in header ---
  output$user_info_header <- renderUI({
    if (isTRUE(rv$logged_in) && !is.null(rv$user)) {
      div(class = "user-header-info",
          div(
            div(class = "user-name", rv$user$full_name),
            div(class = "user-role", rv$user$role)
          ),
          div(style = "display: flex; gap: 8px;",
              actionButton("open_change_password", icon("key"), label = NULL,
                           class = "btn btn-sm", title = "Change Password",
                           style = "background: rgba(255,255,255,0.2); color: white; border: 1px solid rgba(255,255,255,0.3); border-radius: 4px; padding: 5px 10px;"),
              actionButton("logout_btn", "Logout", class = "btn btn-sm",
                           style = "background: rgba(255,255,255,0.2); color: white; border: 1px solid rgba(255,255,255,0.3); border-radius: 4px;")
          )
      )
    } else {
      div(class = "user-header-info",
          actionButton("header_login_btn", "Staff Login", class = "btn btn-sm",
                       style = "background: #f59e0b; color: #0f172a; border: none; border-radius: 4px; font-weight: 600;"),
          actionButton("back_to_landing_btn", "Home", class = "btn btn-sm",
                       style = "background: rgba(255,255,255,0.2); color: white; border: 1px solid rgba(255,255,255,0.3); border-radius: 4px;")
      )
    }
  })
  
  observeEvent(input$header_login_btn, { rv$page_state <- "login" })

  # --- Horizontal Navigation Menu in Header ---
  output$header_nav_menu <- renderUI({
    current <- input$current_tab
    if (is.null(current)) current <- "dashboard"

    if (isTRUE(rv$logged_in)) {
      # Clean navigation - Dashboard, New Case, All Cases, Analytics only
      div(class = "header-nav-menu",
          actionButton("nav_dashboard",
                       tagList(icon("dashboard"), "Dashboard"),
                       class = paste("nav-btn", if(current == "dashboard") "active" else "")),
          actionButton("nav_new_case",
                       tagList(icon("plus"), "New Case"),
                       class = paste("nav-btn", if(current == "new_case") "active" else "")),
          actionButton("nav_all_cases",
                       tagList(icon("list-alt"), "All Cases"),
                       class = paste("nav-btn", if(current == "all_cases") "active" else "")),
          actionButton("nav_analytics",
                       tagList(icon("chart-bar"), "Analytics"),
                       class = paste("nav-btn", if(current == "analytics") "active" else ""))
      )
    } else {
      # Non-logged-in users see only analytics
      div(class = "header-nav-menu",
          actionButton("nav_analytics",
                       tagList(icon("chart-bar"), "Analytics"),
                       class = "nav-btn active")
      )
    }
  })

  # --- Navigation Button Observers ---
  observeEvent(input$nav_dashboard, {
    updateTextInput(session, "current_tab", value = "dashboard")
    shinydashboard::updateTabItems(session, "sidebar_menu", selected = "dashboard")
  })

  observeEvent(input$nav_new_case, {
    updateTextInput(session, "current_tab", value = "new_case")
    shinydashboard::updateTabItems(session, "sidebar_menu", selected = "new_case")
  })

  observeEvent(input$nav_all_cases, {
    updateTextInput(session, "current_tab", value = "all_cases")
    shinydashboard::updateTabItems(session, "sidebar_menu", selected = "all_cases")
  })

  observeEvent(input$nav_analytics, {
    updateTextInput(session, "current_tab", value = "analytics")
    shinydashboard::updateTabItems(session, "sidebar_menu", selected = "analytics")
  })

  # Sync current_tab when sidebar_menu changes (for any external changes)
  observeEvent(input$sidebar_menu, {
    updateTextInput(session, "current_tab", value = input$sidebar_menu)
  })

  # --- Quick Case Lookup in Header ---
  output$header_quick_search <- renderUI({
    if (isTRUE(rv$logged_in)) {
      div(class = "header-quick-search",
          tags$input(
            type = "text",
            id = "quick_case_code",
            placeholder = "Case Code",
            class = "form-control"
          ),
          actionButton("quick_case_btn", "Find", class = "btn")
      )
    }
  })

  # --- Legacy Sidebar Menu (kept for compatibility) ---
  output$sidebar_menu_items <- renderUI({
    if (isTRUE(rv$logged_in)) {
      tagList(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
        menuItem("New Case", tabName = "new_case", icon = icon("plus")),
        menuItem("All Cases", tabName = "all_cases", icon = icon("list-alt")),
        menuItem("Analytics", tabName = "analytics", icon = icon("bar-chart"))
      )
    } else {
      tagList(
        menuItem("Analytics", tabName = "analytics", icon = icon("bar-chart"), selected = TRUE)
      )
    }
  })
  
  # --- Quick Case Lookup (only for logged-in users) ---
  output$sidebar_quick_search <- renderUI({
    tagList(
      textInput(
        "quick_case_code",
        NULL,
        placeholder = "Enter Case Code",
        width = "100%"
      ),
      actionButton(
        "quick_case_btn",
        "Find Case",
        class = "btn-primary btn-block"
      )
    )
  })
  
  
  # --- Helper: Get user's allowed region_id for filtering ---
  user_region_id <- reactive({
    if (!isTRUE(rv$logged_in) || is.null(rv$user)) return(NULL)
    if (can_see_all_regions(rv$user$role)) return(NULL)  # NULL = see all
    return(rv$user$region_id)
  })
  
  # ========================================
  # Reactive data (existing)
  # ========================================
  categories <- reactive({
    get_categories(con())
  })
  
  regions <- reactive({
    get_regions(con())
  })
  
  channels <- reactive({
    get_channels(con())
  })
  
  subcategories <- reactive({
    req(input$category_id)
    get_subcategories(con(), input$category_id)
  })
  
  # NEW: Selected case reactive for details view
  selected_case_id <- reactiveVal(NULL)
  
  # UI outputs - region select respects user's assigned region
  output$region_select_ui <- renderUI({
    regions_df <- regions()
    forced_region <- user_region_id()
    
    if (!is.null(forced_region)) {
      # Regional users can only create cases for their region
      region_row <- regions_df[regions_df$region_id == forced_region, ]
      selectInput("region_select", "Region *",
                  choices = setNames(region_row$region_id, region_row$region_name),
                  selected = forced_region)
    } else {
      # National staff can select any region
      default_sel <- if (!is.null(rv$user) && !is.null(rv$user$region_id)) rv$user$region_id else 1
      selectInput("region_select", "Region *",
                  choices = setNames(regions_df$region_id, regions_df$region_name),
                  selected = default_sel)
    }
  })
  
  output$channel_select_ui <- renderUI({
    channels_df <- channels()
    selectInput("channel", "Contact Channel *",
                choices = setNames(channels_df$channel_id, channels_df$channel_name),
                selected = 1)
  })
  
  output$category_select_ui <- renderUI({
    cats <- categories()
    if (nrow(cats) > 0) {
      selectInput("category_id", "Issue Category *",
                  choices = c("Select category..." = "", setNames(cats$category_id, cats$category_name)))
    } else {
      selectInput("category_id", "Issue Category *",
                  choices = c("Loading categories..." = ""))
    }
  })
  
  output$subcategory_select_ui <- renderUI({
    subcats <- subcategories()
    if (nrow(subcats) > 0) {
      selectInput("subcategory_id", "Issue Subcategory",
                  choices = c("Select subcategory..." = "", setNames(subcats$subcategory_id, subcats$subcategory_name)))
    } else if (!is.null(input$category_id) && input$category_id != "") {
      selectInput("subcategory_id", "Issue Subcategory",
                  choices = c("No subcategories available" = ""))
    } else {
      selectInput("subcategory_id", "Issue Subcategory",
                  choices = c("Please select category first" = ""))
    }
  })
  
  # Filter UI outputs - region-restricted based on user role
  output$my_region_filter_ui <- renderUI({
    regions_df <- regions()
    forced_region <- user_region_id()
    
    if (!is.null(forced_region)) {
      # Regional users can only see their own region
      region_row <- regions_df[regions_df$region_id == forced_region, ]
      selectInput("my_region_filter", "Region Filter",
                  choices = setNames(region_row$region_id, region_row$region_name),
                  selected = forced_region)
    } else {
      selectInput("my_region_filter", "Region Filter",
                  choices = c("All Regions" = 0, setNames(regions_df$region_id, regions_df$region_name)))
    }
  })
  
  output$all_region_filter_ui <- renderUI({
    regions_df <- regions()
    forced_region <- user_region_id()
    
    if (!is.null(forced_region)) {
      region_row <- regions_df[regions_df$region_id == forced_region, ]
      selectInput("all_region_filter", "Region Filter",
                  choices = setNames(region_row$region_id, region_row$region_name),
                  selected = forced_region)
    } else {
      selectInput("all_region_filter", "Region Filter",
                  choices = c("All Regions" = 0, setNames(regions_df$region_id, regions_df$region_name)))
    }
  })
  
  output$all_category_filter_ui <- renderUI({
    cats <- categories()
    selectInput("all_category_filter", "Category Filter",
                choices = c("All Categories" = 0, setNames(cats$category_id, cats$category_name)))
  })
  
  # Auto-refresh: invalidate dashboard data every 5 minutes
  auto_refresh_timer <- reactiveTimer(300000)  # 300,000 ms = 5 minutes
  dashboard_stats_invalidator <- reactiveVal(0)
  
  # Dashboard KPIs (auto-refreshing) - filtered by user's region
  dashboard_stats <- reactive({
    auto_refresh_timer()
    dashboard_stats_invalidator()
    forced_region <- user_region_id()
    get_dashboard_stats(con(), region_id = forced_region)
  })
  
  output$total_cases <- renderInfoBox({
    stats <- dashboard_stats()
    total <- sum(stats$status$count, na.rm = TRUE)
    
    infoBox(
      title = "Total Cases",
      value = total,
      icon = icon("clipboard-list"),
      color = "blue"
    )
  })
  
  output$new_cases <- renderInfoBox({
    stats <- dashboard_stats()
    new_count <- sum(stats$status$count[stats$status$status == "New"], na.rm = TRUE)
    
    infoBox(
      title = "New Cases",
      value = new_count,
      icon = icon("plus"),
      color = "yellow"
    )
  })
  
  output$in_progress_cases <- renderInfoBox({
    stats <- dashboard_stats()
    progress_count <- sum(stats$status$count[stats$status$status == "In Progress"], na.rm = TRUE)
    
    infoBox(
      title = "In Progress",
      value = progress_count,
      icon = icon("cog"),
      color = "orange"
    )
  })
  
  output$escalated_cases <- renderInfoBox({
    stats <- dashboard_stats()
    escalated_count <- sum(stats$status$count[stats$status$status == "Escalated"], na.rm = TRUE)
    
    infoBox(
      title = "Escalated",
      value = escalated_count,
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$waiting_cases <- renderInfoBox({
    stats <- dashboard_stats()
    waiting_count <- sum(stats$status$count[stats$status$status == "Waiting on Teacher"], na.rm = TRUE)
    infoBox(
      title = "Waiting on Teacher",
      value = waiting_count,
      icon = icon("hourglass-half"),
      color = "purple"
    )
  })
  
  output$resolved_cases <- renderInfoBox({
    stats <- dashboard_stats()
    resolved_count <- sum(stats$status$count[stats$status$status == "Resolved"], na.rm = TRUE)
    infoBox(
      title = "Resolved",
      value = resolved_count,
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$closed_cases <- renderInfoBox({
    stats <- dashboard_stats()
    closed_count <- sum(stats$status$count[stats$status$status == "Closed"], na.rm = TRUE)
    infoBox(
      title = "Closed",
      value = closed_count,
      icon = icon("archive"),
      color = "black"
    )
  })
  
  output$overdue_cases_kpi <- renderInfoBox({
    sla <- tryCatch({
      get_sla_overview(con())
    }, error = function(e) {
      list(summary = data.frame(overdue = 0))
    })
    overdue_count <- if (nrow(sla$summary) > 0) sla$summary$overdue[1] else 0
    infoBox(
      title = "SLA Overdue",
      value = overdue_count,
      icon = icon("exclamation-circle"),
      color = "maroon"
    )
  })
  
  output$auto_refresh_status <- renderText({
    auto_refresh_timer()
    paste("Auto-refreshes every 5 min | Last:", format(Sys.time(), "%H:%M:%S"))
  })
  
  observeEvent(input$manual_refresh_dashboard, {
    dashboard_stats_invalidator(dashboard_stats_invalidator() + 1)
  })

  # Last refresh time tracker
  last_refresh <- reactiveVal(Sys.time())

  # New prominent refresh button handler
  observeEvent(input$refresh_case_summary, {
    dashboard_stats_invalidator(dashboard_stats_invalidator() + 1)
    last_refresh(Sys.time())
    showNotification("Dashboard data refreshed!", type = "message", duration = 3)
  })

  # Display last refresh time
  output$last_refresh_time <- renderText({
    refresh_time <- last_refresh()
    paste("Last updated:", format(refresh_time, "%H:%M:%S"))
  })
  
  # Performance metrics table (existing)
  output$performance_metrics <- renderTable({
    stats <- dashboard_stats()
    avg_data <- stats$averages
    
    if (nrow(avg_data) > 0) {
      avg_hours <- if (length(avg_data$avg_resolution_hours) > 0 && !is.na(avg_data$avg_resolution_hours[1])) {
        round(avg_data$avg_resolution_hours[1], 1)
      } else {
        0
      }
      
      esc_rate <- if (length(avg_data$escalation_rate) > 0 && !is.na(avg_data$escalation_rate[1])) {
        round(avg_data$escalation_rate[1], 1)
      } else {
        0
      }
      
      data.frame(
        Metric = c("Avg Resolution Time", "Escalation Rate"),
        Value = c(paste(avg_hours, "hours"), paste(esc_rate, "%"))
      )
    } else {
      data.frame(Metric = c("Avg Resolution Time", "Escalation Rate"), 
                 Value = c("0 hours", "0 %"))
    }
  })
  
  # Save new case (existing)
  observeEvent(input$save_case, {
    req(input$teacher_name, input$teacher_phone, input$category_id, input$priority, input$case_summary)
    
    if (input$category_id == "") {
      showNotification("Please select a category", type = "warning")
      return()
    }
    
    if (nchar(input$case_summary) < 20) {
      showNotification("Case summary must be at least 20 characters", type = "warning")
      return()
    }
    
    sub_id <- if (!is.null(input$subcategory_id) && input$subcategory_id != "") {
      input$subcategory_id
    } else {
      NULL
    }
    
    success <- insert_ticket(
      con = con(),
      region_id = input$region_select %or% 1,
      channel_id = input$channel %or% 1,
      teacher_name = input$teacher_name,
      teacher_phone = input$teacher_phone,
      teacher_staff_id = input$teacher_staff_id,
      school_name = input$school_name,
      district = input$district,
      category_id = as.integer(input$category_id),
      subcategory_id = if (!is.null(sub_id)) as.integer(sub_id) else NULL,
      priority = input$priority,
      summary = input$case_summary,
      description = input$case_description
    )
    
    if (success) {
      uid <- current_user_id()
      tryCatch({ log_activity(pool, uid, "Create Case", paste("Teacher:", input$teacher_name)) }, error = function(e) {})
      updateTextInput(session, "teacher_name", value = "")
      updateTextInput(session, "teacher_phone", value = "")
      updateTextInput(session, "teacher_staff_id", value = "")
      updateTextInput(session, "school_name", value = "")
      updateTextInput(session, "district", value = "")
      updateTextAreaInput(session, "case_summary", value = "")
      updateTextAreaInput(session, "case_description", value = "")
      updateSelectInput(session, "category_id", selected = "")
      updateSelectInput(session, "priority", selected = "Medium")
    }
  })
  
  # Clear form (existing)
  observeEvent(input$clear_form, {
    updateTextInput(session, "teacher_name", value = "")
    updateTextInput(session, "teacher_phone", value = "")
    updateTextInput(session, "teacher_staff_id", value = "")
    updateTextInput(session, "school_name", value = "")
    updateTextInput(session, "district", value = "")
    updateTextAreaInput(session, "case_summary", value = "")
    updateTextAreaInput(session, "case_description", value = "")
    updateSelectInput(session, "category_id", selected = "")
    updateSelectInput(session, "priority", selected = "Medium")
  })
  
  # Data tables with role-based region access control
  recent_cases_data <- reactive({
    forced_region <- user_region_id()
    fetch_tickets(con(), region_id = forced_region, limit = 10)
  })
  
  my_cases_data <- reactive({
    input$refresh_my_cases
    status_val <- if(is.null(input$my_status_filter) || input$my_status_filter == "All") {
      NULL
    } else {
      input$my_status_filter
    }
    # Enforce region restriction: regional users always filtered to their region
    forced_region <- user_region_id()
    region_filter <- if (!is.null(forced_region)) forced_region else input$my_region_filter
    fetch_tickets(con(), region_id = region_filter, status_filter = status_val)
  })
  
  all_cases_data <- reactive({
    input$refresh_all_cases
    status_val <- if(is.null(input$all_status_filter) || input$all_status_filter == "All") {
      NULL
    } else {
      input$all_status_filter
    }
    cat_val <- if(is.null(input$all_category_filter) || input$all_category_filter == "0") {
      NULL
    } else {
      input$all_category_filter
    }
    search_val <- if(is.null(input$all_search_text) || input$all_search_text == "") {
      NULL
    } else {
      input$all_search_text
    }
    date_from_val <- if(is.null(input$all_date_from) || is.na(input$all_date_from)) NULL else input$all_date_from
    date_to_val <- if(is.null(input$all_date_to) || is.na(input$all_date_to)) NULL else input$all_date_to
    
    # Enforce region restriction: regional users always filtered to their region
    forced_region <- user_region_id()
    region_filter <- if (!is.null(forced_region)) forced_region else input$all_region_filter
    
    fetch_tickets(con(), region_id = region_filter, status_filter = status_val,
                  category_id = cat_val, search_text = search_val,
                  date_from = date_from_val, date_to = date_to_val)
  })
  
  # Clear All Filters handler
  observeEvent(input$clear_all_filters, {
    updateTextInput(session, "all_search_text", value = "")
    updateSelectInput(session, "all_status_filter", selected = "All")
    updateSelectInput(session, "all_region_filter", selected = "0")
    updateSelectInput(session, "all_category_filter", selected = "0")
    updateDateInput(session, "all_date_from", value = NA)
    updateDateInput(session, "all_date_to", value = NA)
  })
  
  # CSV Export for All Cases
  output$export_cases_csv <- downloadHandler(
    filename = function() paste0("cases_export_", Sys.Date(), ".csv"),
    content = function(file) {
      data <- all_cases_data()
      if (nrow(data) > 0) {
        export_data <- data %>%
          select(case_code, created_at, teacher_name, teacher_phone, teacher_staff_id,
                 school_name, district, category_name, priority, status, summary, hours_open)
        write.csv(export_data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No data to export"), file, row.names = FALSE)
      }
    }
  )
  
  # ENHANCED: Render data tables with clickable case codes
  output$recent_cases_table <- DT::renderDataTable({
    data <- recent_cases_data()
    if (nrow(data) == 0) return(data.frame(Message = "No recent cases"))
    
    display_data <- data %>%
      select(ticket_id, case_code, created_at, teacher_name, category_name, priority, status) %>%
      mutate(
        created_at = format(as.POSIXct(created_at), "%Y-%m-%d %H:%M"),
        case_code = paste0('<span class="case-code" onclick="Shiny.setInputValue(\'view_case_id\', ', ticket_id, ');">', case_code, '</span>'),
        priority = paste0('<span class="priority-', tolower(priority), '">', priority, '</span>'),
        status = paste0('<span class="badge status-', tolower(gsub(" ", "-", status)), '">', status, '</span>')
      ) %>%
      select(-ticket_id)
    
    DT::datatable(display_data,
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    dom = 't'
                  ),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Case", "Created", "Teacher", "Category", "Priority", "Status")
    )
  })
  
  output$my_cases_table <- DT::renderDataTable({
    data <- my_cases_data()
    if (nrow(data) == 0) return(data.frame(Message = "No cases found"))
    
    display_data <- data %>%
      select(ticket_id, case_code, created_at, teacher_name, school_name, category_name, priority, status, hours_open) %>%
      mutate(
        created_at = format(as.POSIXct(created_at), "%Y-%m-%d %H:%M"),
        case_code = paste0('<span class="case-code" onclick="Shiny.setInputValue(\'view_case_id\', ', ticket_id, ');">', case_code, '</span>'),
        priority = paste0('<span class="priority-', tolower(priority), '">', priority, '</span>'),
        status = paste0('<span class="badge status-', tolower(gsub(" ", "-", status)), '">', status, '</span>'),
        hours_open = paste(round(as.numeric(hours_open)), "hours")
      ) %>%
      select(-ticket_id)
    
    DT::datatable(display_data,
                  options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    order = list(list(1, 'desc'))
                  ),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Case", "Created", "Teacher", "School", "Category", "Priority", "Status", "Age")
    )
  })
  
  output$all_cases_table <- DT::renderDataTable({
    data <- all_cases_data()
    if (nrow(data) == 0) return(data.frame(Message = "No cases found"))
    
    display_data <- data %>%
      select(ticket_id, case_code, created_at, teacher_name, school_name, district, category_name, priority, status) %>%
      mutate(
        created_at = format(as.POSIXct(created_at), "%Y-%m-%d %H:%M"),
        case_code = paste0('<span class="case-code" onclick="Shiny.setInputValue(\'view_case_id\', ', ticket_id, ');">', case_code, '</span>'),
        priority = paste0('<span class="priority-', tolower(priority), '">', priority, '</span>'),
        status = paste0('<span class="badge status-', tolower(gsub(" ", "-", status)), '">', status, '</span>')
      ) %>%
      select(-ticket_id)
    
    DT::datatable(display_data,
                  options = list(
                    pageLength = 20,
                    scrollX = TRUE,
                    order = list(list(1, 'desc'))
                  ),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Case", "Created", "Teacher", "School", "District", "Category", "Priority", "Status")
    )
  })
  
  # Quick Case Lookup from sidebar
  observeEvent(input$quick_search_btn, {
    req(input$quick_search_code)
    search_code <- trimws(input$quick_search_code)
    if (nchar(search_code) < 3) {
      showNotification("Please enter at least 3 characters of the case code", type = "warning")
      return()
    }
    
    tryCatch({
      result <- dbGetQuery(con(), "SELECT ticket_id FROM tickets WHERE case_code LIKE ? LIMIT 1",
                           params = list(paste0("%", search_code, "%")))
      if (nrow(result) > 0) {
        selected_case_id(result$ticket_id[1])
        runjs("$('#caseDetailsModal').modal('show');")
        updateTextInput(session, "quick_search_code", value = "")
      } else {
        showNotification("No case found matching that code", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Search error:", e$message), type = "error")
    })
  })
  
  # Case Details Modal Logic - with region access control
  observeEvent(input$view_case_id, {
    if (!is.null(input$view_case_id)) {
      cat("Opening case details for ticket_id:", input$view_case_id, "\n")

      # Check region access for regional users
      forced_region <- user_region_id()
      if (!is.null(forced_region)) {
        # Verify case belongs to user's region
        case_region <- tryCatch({
          dbGetQuery(con(), "SELECT region_id FROM tickets WHERE ticket_id = ?",
                     params = list(input$view_case_id))
        }, error = function(e) data.frame())

        if (nrow(case_region) == 0 || case_region$region_id[1] != forced_region) {
          showNotification("Access denied: This case is not in your region.", type = "error")
          return()
        }
      }

      selected_case_id(input$view_case_id)
      runjs("$('#caseDetailsModal').modal('show');")
    }
  })
  
  # NEW: Render Case Details Content with improved error handling
  output$case_details_content <- renderUI({
    req(selected_case_id())

    case_data <- get_case_details(con(), selected_case_id())
    actions_data <- get_case_actions(con(), selected_case_id())
    followups_data <- get_case_followups(con(), selected_case_id())

    if (is.null(case_data)) {
      return(div(h3("Case not found"), class = "text-center"))
    }

    # Safe helper function for handling NULL/NA values
    safe_value <- function(x, default = "Not provided") {
      if (is.null(x) || is.na(x) || x == "") default else as.character(x)
    }
    
    tagList(
      # Case Header with Summary
      div(class = "case-details-header",
          fluidRow(
            column(8,
                   h2(safe_value(case_data$case_code, "Unknown Case"), style = "margin: 0; font-weight: bold;"),
                   h4("Case Summary:", style = "margin: 10px 0 5px 0; font-weight: bold; opacity: 0.9;"),
                   p(safe_value(case_data$summary, "No summary available"), style = "margin: 0; font-size: 15px; opacity: 0.9; line-height: 1.4;")
            ),
            column(4, style = "text-align: right;",
                   tags$span(safe_value(case_data$status, "Unknown"), 
                             class = paste("badge", paste0("status-", tolower(gsub(" ", "-", safe_value(case_data$status, "unknown")))), sep = " "),
                             style = "font-size: 14px; padding: 6px 12px;"),
                   p(paste("Priority:", safe_value(case_data$priority, "Unknown")), style = "margin: 10px 0 0 0; font-size: 14px;")
            )
          )
      ),
      
      # Case Information Grid
      div(class = "case-info-grid",
          # Teacher Information
          div(class = "info-card",
              h4("Teacher Information", style = "color: #1e3a8a; margin-bottom: 15px;"),
              div(class = "info-label", "Name"),
              div(class = "info-value", safe_value(case_data$teacher_name)),
              br(),
              div(class = "info-label", "Phone"),
              div(class = "info-value", safe_value(case_data$teacher_phone)),
              br(),
              div(class = "info-label", "Staff ID"),
              div(class = "info-value", safe_value(case_data$teacher_staff_id))
          ),
          
          # School Information  
          div(class = "info-card",
              h4("School Information", style = "color: #1e3a8a; margin-bottom: 15px;"),
              div(class = "info-label", "School"),
              div(class = "info-value", safe_value(case_data$school_name)),
              br(),
              div(class = "info-label", "District"),
              div(class = "info-value", safe_value(case_data$district)),
              br(),
              div(class = "info-label", "Region"),
              div(class = "info-value", safe_value(case_data$region_name))
          ),
          
          # Case Classification
          div(class = "info-card",
              h4("Classification", style = "color: #1e3a8a; margin-bottom: 15px;"),
              div(class = "info-label", "Category"),
              div(class = "info-value", safe_value(case_data$category_name)),
              br(),
              div(class = "info-label", "Subcategory"),
              div(class = "info-value", safe_value(case_data$subcategory_name, "None")),
              br(),
              div(class = "info-label", "Channel"),
              div(class = "info-value", safe_value(case_data$channel_name))
          ),
          
          # Timing Information
          div(class = "info-card",
              h4("Timing", style = "color: #1e3a8a; margin-bottom: 15px;"),
              div(class = "info-label", "Created"),
              div(class = "info-value", 
                  if (!is.null(case_data$created_at) && !is.na(case_data$created_at)) {
                    format(as.POSIXct(case_data$created_at), "%Y-%m-%d %H:%M")
                  } else {
                    "Unknown"
                  }),
              br(),
              div(class = "info-label", "Age"),
              div(class = "info-value", 
                  if (!is.null(case_data$hours_open) && !is.na(case_data$hours_open)) {
                    paste(round(as.numeric(case_data$hours_open)), "hours")
                  } else {
                    "Unknown"
                  }),
              br(),
              div(class = "info-label", "SLA Status"),
              div(class = paste("info-value sla", tolower(gsub(" ", "-", safe_value(case_data$sla_status, "unknown"))), sep = "-"), 
                  safe_value(case_data$sla_status, "Unknown"))
          )
      ),
      
      # Case Description Section - Always display
      div(class = "info-card", style = "margin-bottom: 20px;",
          h4("Case Summary & Description", style = "color: #1e3a8a; margin-bottom: 15px;"),
          
          # Case Summary
          div(style = "margin-bottom: 15px;",
              div(class = "info-label", "CASE SUMMARY"),
              div(style = "padding: 12px; background: #f0f9ff; border-radius: 6px; border-left: 4px solid #3b82f6;",
                  p(safe_value(case_data$summary, "No summary available"), 
                    style = "line-height: 1.5; margin: 0; font-size: 14px; color: #1e40af; font-weight: 500;")
              )
          ),
          
          # Detailed Description
          div(
            div(class = "info-label", "DETAILED DESCRIPTION"),
            if (!is.null(case_data$description) && case_data$description != "" && !is.na(case_data$description)) {
              div(style = "padding: 12px; background: #f8fafc; border-radius: 6px; border-left: 4px solid #6366f1;",
                  p(case_data$description, style = "line-height: 1.5; margin: 0; font-size: 14px; color: #374151;")
              )
            } else {
              div(style = "padding: 12px; background: #f9fafb; border-radius: 6px; border-left: 4px solid #d1d5db;",
                  p("No detailed description provided for this case.", 
                    style = "line-height: 1.5; margin: 0; font-size: 14px; color: #6b7280; font-style: italic;")
              )
            }
          )
      ),

      # Follow-up History Section
      div(class = "info-card", style = "margin-bottom: 20px;",
          h4("Follow-up History", style = "color: #1e3a8a; margin-bottom: 15px;"),
          if (!is.null(followups_data) && nrow(followups_data) > 0) {
            div(
              lapply(1:nrow(followups_data), function(i) {
                fu <- followups_data[i, ]
                status_color <- switch(safe_value(fu$urgency, "Upcoming"),
                  "Completed" = "#10b981",
                  "Overdue" = "#dc2626",
                  "Due Today" = "#f59e0b",
                  "#3b82f6"
                )
                div(style = "padding: 12px; margin-bottom: 10px; background: #f8fafc; border-radius: 6px; border-left: 4px solid; border-left-color: inherit;",
                    style = paste0("border-left-color: ", status_color, ";"),
                    fluidRow(
                      column(8,
                        div(style = "font-weight: 600; color: #1e3a8a;",
                            paste("Follow-up:", format(as.Date(fu$follow_up_date), "%Y-%m-%d"))),
                        if (!is.null(fu$follow_up_notes) && fu$follow_up_notes != "" && !is.na(fu$follow_up_notes)) {
                          div(style = "margin-top: 5px; color: #374151; font-size: 13px;", fu$follow_up_notes)
                        },
                        div(style = "margin-top: 5px; font-size: 11px; color: #6b7280;",
                            paste("Scheduled by:", safe_value(fu$created_by_name, "Unknown")))
                      ),
                      column(4, style = "text-align: right;",
                        tags$span(safe_value(fu$urgency, "Upcoming"),
                                  style = paste0("display: inline-block; padding: 4px 10px; border-radius: 4px; font-size: 12px; font-weight: 500; color: white; background: ", status_color, ";")),
                        if (safe_value(fu$urgency, "") == "Completed" && !is.null(fu$completed_by_name) && !is.na(fu$completed_by_name)) {
                          div(style = "margin-top: 5px; font-size: 11px; color: #6b7280;",
                              paste("Completed by:", fu$completed_by_name))
                        }
                      )
                    )
                )
              })
            )
          } else {
            div(style = "padding: 20px; text-align: center; color: #6b7280; font-style: italic;",
                "No follow-ups scheduled for this case.")
          }
      ),

      # Action Buttons
      fluidRow(
        column(12,
               div(style = "text-align: center; margin: 20px 0;",
                   actionButton("update_status_btn", "Update Status", class = "btn btn-primary", style = "margin: 5px;"),
                   actionButton("add_note_btn", "Add Note", class = "btn btn-info", style = "margin: 5px;"),
                   actionButton("escalate_case_btn", "Escalate", class = "btn btn-warning", style = "margin: 5px;"),
                   if (!is.null(case_data$status) && case_data$status == "New") {
                     actionButton("start_progress_btn", "Start Working", class = "btn btn-success", style = "margin: 5px;")
                   },
                   if (!is.null(case_data$status) && case_data$status %in% c("Resolved", "Closed")) {
                     actionButton("reopen_case_btn", "Reopen Case", class = "btn btn-danger", style = "margin: 5px;")
                   },
                   if (!is.null(case_data$status) && case_data$status %in% c("In Progress", "Waiting on Teacher", "Escalated")) {
                     actionButton("resolve_with_rating_btn", "Resolve Case", class = "btn btn-success", style = "margin: 5px;")
                   }
               )
        )
      ),
      
      # Action Timeline
      div(class = "info-card",
          h4("Case Timeline", style = "color: #1e3a8a; margin-bottom: 20px;"),
          if (!is.null(actions_data) && nrow(actions_data) > 0) {
            div(
              lapply(1:nrow(actions_data), function(i) {
                action <- actions_data[i, ]
                div(class = "timeline-item",
                    div(class = "timeline-content",
                        div(class = "timeline-header",
                            span(class = "timeline-action", safe_value(action$action_label, "Unknown Action")),
                            span(class = "timeline-date", 
                                 if (!is.null(action$action_at) && !is.na(action$action_at)) {
                                   format(as.POSIXct(action$action_at), "%Y-%m-%d %H:%M")
                                 } else {
                                   "Unknown time"
                                 })
                        ),
                        if (!is.null(action$action_text) && action$action_text != "" && !is.na(action$action_text)) {
                          div(class = "timeline-text", action$action_text)
                        },
                        if (!is.null(action$action_by_name) && action$action_by_name != "" && !is.na(action$action_by_name)) {
                          div(style = "margin-top: 8px; font-size: 11px; color: #6b7280;", 
                              paste("by", action$action_by_name))
                        }
                    )
                )
              })
            )
          } else {
            div("No actions recorded yet.", style = "color: #6b7280; font-style: italic;")
          }
      ),
      
      # Hidden sections for status update and note addition - controlled by JavaScript
      div(id = "statusUpdateSection", style = "display: none; margin-top: 20px; padding: 20px; background: #f8fafc; border-radius: 8px; border: 1px solid #e5e7eb;",
          h4("Update Case Status", style = "color: #1e3a8a; margin-bottom: 15px;"),
          fluidRow(
            column(6,
                   selectInput("new_status", "New Status",
                               choices = c("New", "In Progress", "Waiting on Teacher", "Escalated", "Resolved", "Closed"),
                               selected = safe_value(case_data$status, "New"))
            ),
            column(6,
                   textAreaInput("status_notes", "Notes (Optional)", placeholder = "Add any notes about this status change...", height = "100px")
            )
          ),
          div(style = "text-align: center;",
              actionButton("confirm_status_update", "Update Status", class = "btn btn-primary"),
              actionButton("cancel_status_update", "Cancel", class = "btn btn-default", style = "margin-left: 10px;")
          )
      ),
      
      # Add Note Section - controlled by JavaScript
      div(id = "addNoteSection", style = "display: none; margin-top: 20px; padding: 20px; background: #f8fafc; border-radius: 8px; border: 1px solid #e5e7eb;",
          h4("Add Note", style = "color: #1e3a8a; margin-bottom: 15px;"),
          textAreaInput("note_text", "Note", placeholder = "Enter your note here...", height = "120px"),
          div(style = "text-align: center;",
              actionButton("confirm_add_note", "Add Note", class = "btn btn-info"),
              actionButton("cancel_add_note", "Cancel", class = "btn btn-default", style = "margin-left: 10px;")
          )
      ),
      
      # Resolve with Satisfaction Rating Section
      div(id = "resolveRatingSection", style = "display: none; margin-top: 20px; padding: 20px; background: #f0fdf4; border-radius: 8px; border: 1px solid #bbf7d0;",
          h4("Resolve Case with Satisfaction Rating", style = "color: #16a085; margin-bottom: 15px;"),
          fluidRow(
            column(6,
                   textAreaInput("resolution_notes", "Resolution Notes *",
                                 placeholder = "Describe how the case was resolved...", height = "100px")
            ),
            column(6,
                   sliderInput("satisfaction_rating", "Teacher Satisfaction (1-5)",
                               min = 1, max = 5, value = 3, step = 1, ticks = TRUE),
                   tags$small("1 = Very Dissatisfied, 3 = Neutral, 5 = Very Satisfied", style = "color: #6b7280;"),
                   br(), br(),
                   textAreaInput("satisfaction_feedback", "Teacher Feedback (Optional)",
                                 placeholder = "Any feedback from the teacher...", height = "60px")
            )
          ),
          div(style = "text-align: center;",
              actionButton("confirm_resolve_rating", "Resolve Case", class = "btn btn-success"),
              actionButton("cancel_resolve_rating", "Cancel", class = "btn btn-default", style = "margin-left: 10px;")
          )
      ),
      
      # Reopen Case Section
      div(id = "reopenCaseSection", style = "display: none; margin-top: 20px; padding: 20px; background: #fef2f2; border-radius: 8px; border: 1px solid #fecaca;",
          h4("Reopen Case", style = "color: #dc2626; margin-bottom: 15px;"),
          textAreaInput("reopen_reason", "Reason for Reopening *",
                        placeholder = "Why does this case need to be reopened?", height = "100px"),
          div(style = "text-align: center;",
              actionButton("confirm_reopen_case", "Reopen Case", class = "btn btn-danger"),
              actionButton("cancel_reopen_case", "Cancel", class = "btn btn-default", style = "margin-left: 10px;")
          )
      )
    )
  })
  
  # NEW: Status Update Handlers with JavaScript control
  observeEvent(input$update_status_btn, {
    runjs("$('#statusUpdateSection').show();")
  })
  
  observeEvent(input$add_note_btn, {
    runjs("$('#addNoteSection').show();")
  })
  
  # Helper to get current user ID (defaults to 1 if not logged in)
  current_user_id <- reactive({
    if (isTRUE(rv$logged_in) && !is.null(rv$user)) rv$user$user_id else 1
  })
  
  observeEvent(input$start_progress_btn, {
    if (!is.null(selected_case_id())) {
      uid <- current_user_id()
      success <- update_case_status(con(), selected_case_id(), "In Progress", "Case work started", uid)
      if (success) {
        tryCatch({ log_activity(pool, uid, "Start Progress", paste("Case ID:", selected_case_id())) }, error = function(e) {})
        runjs("$('#caseDetailsModal').modal('hide');")
        showNotification("Case status updated to In Progress", type = "message")
        shinyjs::click("refresh_my_cases")
        shinyjs::click("refresh_all_cases")
      }
    }
  })
  
  observeEvent(input$confirm_status_update, {
    if (!is.null(input$new_status) && !is.null(selected_case_id())) {
      uid <- current_user_id()
      success <- update_case_status(con(), selected_case_id(), input$new_status,
                                    if(!is.null(input$status_notes) && input$status_notes != "") input$status_notes else NULL,
                                    uid)
      if (success) {
        tryCatch({ log_activity(pool, uid, paste("Status Update:", input$new_status), paste("Case ID:", selected_case_id())) }, error = function(e) {})
        runjs("$('#statusUpdateSection').hide();")
        updateTextAreaInput(session, "status_notes", value = "")
        runjs("$('#caseDetailsModal').modal('hide');")
        showNotification(paste("Case status updated to:", input$new_status), type = "message")
        shinyjs::click("refresh_my_cases")
        shinyjs::click("refresh_all_cases")
      }
    } else {
      showNotification("Please select a valid status", type = "warning")
    }
  })
  
  observeEvent(input$cancel_status_update, {
    runjs("$('#statusUpdateSection').hide();")
    updateSelectInput(session, "new_status", selected = "")
    updateTextAreaInput(session, "status_notes", value = "")
  })
  
  # Add Note Handlers
  observeEvent(input$confirm_add_note, {
    if (!is.null(input$note_text) && nchar(input$note_text) >= 5) {
      if (!is.null(selected_case_id())) {
        uid <- current_user_id()
        success <- add_case_note(con(), selected_case_id(), input$note_text, uid)
        if (success) {
          tryCatch({ log_activity(pool, uid, "Add Note", paste("Case ID:", selected_case_id())) }, error = function(e) {})
          updateTextAreaInput(session, "note_text", value = "")
          runjs("$('#addNoteSection').hide();")
          runjs("$('#caseDetailsModal').modal('hide');")
        }
      }
    } else {
      showNotification("Note must be at least 5 characters", type = "warning")
    }
  })
  
  observeEvent(input$cancel_add_note, {
    runjs("$('#addNoteSection').hide();")
    updateTextAreaInput(session, "note_text", value = "")
  })
  
  # Escalate case handler - Shows escalation reason modal
  observeEvent(input$escalate_case_btn, {
    if (!is.null(selected_case_id())) {
      runjs("$('#caseDetailsModal').modal('hide');")
      runjs("$('#escalationReasonModal').modal('show');")
    }
  })

  # Confirm escalation with reason
  observeEvent(input$confirm_escalation_with_reason, {
    req(selected_case_id())
    req(input$escalation_reason_text)

    if (nchar(trimws(input$escalation_reason_text)) < 10) {
      showNotification("Please provide a detailed escalation reason (at least 10 characters)", type = "warning")
      return()
    }

    uid <- current_user_id()
    reason <- input$escalation_reason_text
    level <- input$escalation_level

    # Update case status and add escalation reason
    success <- tryCatch({
      # Update ticket status
      dbExecute(con(),
        "UPDATE tickets SET status = 'Escalated', escalated_at = NOW(), escalation_reason = ? WHERE ticket_id = ?",
        params = list(reason, selected_case_id()))

      # Log the action
      add_case_note(con(), selected_case_id(), paste("ESCALATED (Level", level, "):", reason), uid)

      # Insert into escalations table
      national_pro_user <- dbGetQuery(con(), "SELECT user_id FROM users WHERE email = 'enquiry.nationalprooffice@gmail.com' LIMIT 1")
      if (nrow(national_pro_user) > 0) {
        dbExecute(con(),
          "INSERT INTO escalations (ticket_id, escalated_by_user_id, escalated_to_user_id, escalation_reason, escalation_method) VALUES (?, ?, ?, ?, 'System')",
          params = list(selected_case_id(), uid, national_pro_user$user_id[1], reason))
      }

      TRUE
    }, error = function(e) {
      showNotification(paste("Escalation error:", e$message), type = "error")
      FALSE
    })

    if (success) {
      tryCatch({ log_activity(pool, uid, "Escalate Case", paste("Case ID:", selected_case_id(), "Level:", level)) }, error = function(e) {})

      # Close the reason modal and show escalation popup
      runjs("$('#escalationReasonModal').modal('hide');")

      # Store escalated case info for popup
      rv$last_escalated_case <- get_case_details(con(), selected_case_id())

      # Show escalation popup
      shinyjs::show("escalationPopupOverlay")
      shinyjs::show("escalationPopup")

      showNotification("Case escalated successfully - National PRO Office notified", type = "warning")
      shinyjs::click("refresh_my_cases")
      shinyjs::click("refresh_all_cases")
    }
  })
  
  # Resolve with satisfaction rating handlers
  observeEvent(input$resolve_with_rating_btn, {
    runjs("$('#resolveRatingSection').show();")
  })
  
  observeEvent(input$confirm_resolve_rating, {
    if (is.null(input$resolution_notes) || nchar(trimws(input$resolution_notes)) < 5) {
      showNotification("Resolution notes must be at least 5 characters", type = "warning")
      return()
    }
    if (!is.null(selected_case_id())) {
      uid <- current_user_id()
      # Update status to Resolved
      success <- update_case_status(con(), selected_case_id(), "Resolved",
                                    input$resolution_notes, uid)
      if (success) {
        # Save satisfaction rating
        tryCatch({
          dbExecute(con(),
                    "UPDATE tickets SET satisfaction_rating = ?, satisfaction_feedback = ? WHERE ticket_id = ?",
                    params = list(
                      as.integer(input$satisfaction_rating),
                      if (!is.null(input$satisfaction_feedback) && input$satisfaction_feedback != "") input$satisfaction_feedback else NULL,
                      selected_case_id()
                    ))
        }, error = function(e) {
          showNotification(paste("Resolved but failed to save rating:", e$message), type = "warning")
        })
        
        tryCatch({ log_activity(pool, uid, "Resolve Case", paste("Case ID:", selected_case_id())) }, error = function(e) {})
        runjs("$('#resolveRatingSection').hide();")
        runjs("$('#caseDetailsModal').modal('hide');")
        updateTextAreaInput(session, "resolution_notes", value = "")
        updateTextAreaInput(session, "satisfaction_feedback", value = "")
        showNotification("Case resolved with satisfaction rating saved", type = "message")
        shinyjs::click("refresh_my_cases")
        shinyjs::click("refresh_all_cases")
      }
    }
  })
  
  observeEvent(input$cancel_resolve_rating, {
    runjs("$('#resolveRatingSection').hide();")
    updateTextAreaInput(session, "resolution_notes", value = "")
    updateTextAreaInput(session, "satisfaction_feedback", value = "")
  })
  
  # Reopen case handlers
  observeEvent(input$reopen_case_btn, {
    runjs("$('#reopenCaseSection').show();")
  })
  
  observeEvent(input$confirm_reopen_case, {
    if (is.null(input$reopen_reason) || nchar(trimws(input$reopen_reason)) < 5) {
      showNotification("Reopen reason must be at least 5 characters", type = "warning")
      return()
    }
    if (!is.null(selected_case_id())) {
      uid <- current_user_id()
      success <- update_case_status(con(), selected_case_id(), "In Progress",
                                    paste("Case reopened:", input$reopen_reason), uid)
      if (success) {
        # Clear resolved/closed timestamps
        tryCatch({
          dbExecute(con(),
                    "UPDATE tickets SET resolved_at = NULL, closed_at = NULL, satisfaction_rating = NULL WHERE ticket_id = ?",
                    params = list(selected_case_id()))
        }, error = function(e) {
          showNotification(paste("Reopened but failed to clear timestamps:", e$message), type = "warning")
        })
        
        tryCatch({ log_activity(pool, uid, "Reopen Case", paste("Case ID:", selected_case_id())) }, error = function(e) {})
        runjs("$('#reopenCaseSection').hide();")
        runjs("$('#caseDetailsModal').modal('hide');")
        updateTextAreaInput(session, "reopen_reason", value = "")
        showNotification("Case reopened successfully", type = "message")
        shinyjs::click("refresh_my_cases")
        shinyjs::click("refresh_all_cases")
      }
    }
  })
  
  observeEvent(input$cancel_reopen_case, {
    runjs("$('#reopenCaseSection').hide();")
    updateTextAreaInput(session, "reopen_reason", value = "")
  })
  
  # Close case details modal
  observeEvent(input$closeCaseDetails, {
    runjs("$('#caseDetailsModal').modal('hide');")
    selected_case_id(NULL)
    # Clear any open sections
    runjs("$('#statusUpdateSection').hide();")
    runjs("$('#addNoteSection').hide();")
    runjs("$('#resolveRatingSection').hide();")
    runjs("$('#reopenCaseSection').hide();")
    updateTextAreaInput(session, "status_notes", value = "")
    updateTextAreaInput(session, "note_text", value = "")
    updateTextAreaInput(session, "resolution_notes", value = "")
    updateTextAreaInput(session, "satisfaction_feedback", value = "")
    updateTextAreaInput(session, "reopen_reason", value = "")
  })
  
  # Charts (existing)
  output$priority_chart <- renderPlotly({
    stats <- dashboard_stats()
    if (nrow(stats$priority) == 0) {
      return(plotly_empty())
    }
    
    p <- plot_ly(stats$priority, x = ~priority, y = ~count, type = 'bar',
                 marker = list(color = c("#16a085", "#f59e0b", "#dc2626", "#7c2d12"))) %>%
      layout(title = "", xaxis = list(title = ""), yaxis = list(title = "Count"))
    
    p
  })
  
  output$category_chart <- renderPlotly({
    stats <- dashboard_stats()
    if (nrow(stats$category) == 0) {
      return(plotly_empty())
    }
    
    top_cats <- head(stats$category, 5)
    
    p <- plot_ly(top_cats, x = ~count, y = ~reorder(category_name, count), type = 'bar',
                 orientation = 'h', marker = list(color = "#2563eb")) %>%
      layout(title = "", xaxis = list(title = "Count"), yaxis = list(title = ""))
    
    p
  })
  
  # Analytics outputs (existing)
  analytics_stats <- reactive({
    get_dashboard_stats(con())
  })
  
  output$analytics_total <- renderInfoBox({
    stats <- analytics_stats()
    total <- sum(stats$status$count, na.rm = TRUE)
    infoBox("Total Cases", total, icon = icon("list"), color = "blue")
  })
  
  output$analytics_resolved <- renderInfoBox({
    stats <- analytics_stats()
    resolved <- sum(stats$status$count[stats$status$status %in% c("Resolved", "Closed")], na.rm = TRUE)
    infoBox("Resolved", resolved, icon = icon("check"), color = "green")
  })
  
  output$analytics_resolution_rate <- renderInfoBox({
    stats <- analytics_stats()
    total <- sum(stats$status$count, na.rm = TRUE)
    resolved <- sum(stats$status$count[stats$status$status %in% c("Resolved", "Closed")], na.rm = TRUE)
    rate <- if(total > 0) round((resolved / total) * 100, 1) else 0
    infoBox("Resolution Rate", paste0(rate, "%"), icon = icon("percentage"), color = "purple")
  })
  
  output$analytics_avg_time <- renderInfoBox({
    stats <- analytics_stats()
    avg_hours <- if (length(stats$averages$avg_resolution_hours) > 0 && !is.na(stats$averages$avg_resolution_hours[1])) {
      round(stats$averages$avg_resolution_hours[1], 1)
    } else {
      0
    }
    infoBox("Avg Resolution", paste(avg_hours, "hours"), icon = icon("clock"), color = "orange")
  })
  
  
  # ----------------------------
  # Advanced Analytics reactives
  # ----------------------------
  regional_perf <- reactive({
    req(con())
    get_regional_performance(con(), months_back = 12)
  })
  
  category_trends <- reactive({
    req(con())
    get_category_trends(con(), months_back = 12)
  })
  
  monthly_series <- reactive({
    req(con())
    get_time_series(con(), months_back = 18)
  })
  
  sla_data <- reactive({
    req(con())
    get_sla_overview(con())
  })
  
  # ----------------------------
  # Regional Performance outputs
  # ----------------------------
  output$region_resolution_chart <- renderPlotly({
    df <- regional_perf()
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(
      df,
      x = ~reorder(region_name, resolution_rate),
      y = ~resolution_rate,
      type = "bar"
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Resolution rate (%)"))
  })
  
  output$region_backlog_chart <- renderPlotly({
    df <- regional_perf()
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(
      df,
      x = ~reorder(region_name, open_cases),
      y = ~open_cases,
      type = "bar",
      orientation = "v"
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Open cases"))
  })
  
  output$region_table <- renderDT({
    df <- regional_perf()
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 16, autoWidth = TRUE)
    )
  })
  
  # ----------------------------
  # Category Trend outputs
  # ----------------------------
  output$category_trend_table <- renderDT({
    df <- category_trends()
    datatable(df, rownames = FALSE, options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  output$category_trend_chart <- renderPlotly({
    df <- category_trends()
    if (nrow(df) == 0) return(plotly_empty())
    
    # Top 5 categories overall
    top5 <- df |>
      dplyr::group_by(category_name) |>
      dplyr::summarise(total = sum(cases), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(total)) |>
      dplyr::slice_head(n = 5)
    
    df2 <- df |>
      dplyr::inner_join(top5, by = "category_name") |>
      dplyr::arrange(ym)
    
    plot_ly(
      df2,
      x = ~ym,
      y = ~cases,
      color = ~category_name,
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Cases"))
  })
  
  output$category_movement_table <- renderDT({
    df <- category_trends()
    if (nrow(df) == 0) return(datatable(data.frame()))
    
    # Compare latest month to previous month
    months <- sort(unique(df$ym))
    if (length(months) < 2) return(datatable(data.frame()))
    
    m1 <- months[length(months)]      # latest
    m0 <- months[length(months) - 1]  # previous
    
    cur <- df[df$ym == m1, c("category_name", "cases")]
    prev <- df[df$ym == m0, c("category_name", "cases")]
    names(cur)[2] <- "cases_current"
    names(prev)[2] <- "cases_prev"
    
    movement <- dplyr::full_join(cur, prev, by = "category_name") |>
      dplyr::mutate(
        cases_current = dplyr::coalesce(cases_current, 0L),
        cases_prev    = dplyr::coalesce(cases_prev, 0L),
        change        = cases_current - cases_prev,
        pct_change    = dplyr::if_else(cases_prev > 0, round((change / cases_prev) * 100, 1), NA_real_)
      ) |>
      dplyr::arrange(dplyr::desc(change)) |>
      dplyr::slice_head(n = 10)
    
    datatable(movement, rownames = FALSE, options = list(dom = "t", autoWidth = TRUE))
  })
  
  # ----------------------------
  # SLA outputs
  # ----------------------------
  output$sla_overview_chart <- renderPlotly({
    s <- sla_data()$summary
    if (nrow(s) == 0) return(plotly_empty())
    
    df <- data.frame(
      bucket = c("On track", "Due soon (4h)", "Overdue"),
      count  = c(s$on_track[1], s$due_soon[1], s$overdue[1])
    )
    
    plot_ly(df, labels = ~bucket, values = ~count, type = "pie") %>%
      layout(showlegend = TRUE)
  })
  
  output$sla_region_chart <- renderPlotly({
    df <- sla_data()$by_region
    if (nrow(df) == 0) return(plotly_empty())
    
    # stacked bars: on_track, due_soon, overdue
    plot_ly(df, x = ~region_name, y = ~on_track, type = "bar", name = "On track") %>%
      add_trace(y = ~due_soon, name = "Due soon") %>%
      add_trace(y = ~overdue, name = "Overdue") %>%
      layout(barmode = "stack", xaxis = list(title = ""), yaxis = list(title = "Open cases"))
  })
  
  output$sla_overdue_table <- renderDT({
    df <- sla_data()$overdue
    datatable(df, rownames = FALSE, options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  # ----------------------------
  # Time series outputs
  # ----------------------------
  output$monthly_created_resolved_chart <- renderPlotly({
    df <- monthly_series()
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, x = ~ym, y = ~created, type = "scatter", mode = "lines+markers", name = "Created") %>%
      add_trace(y = ~resolved, name = "Resolved") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Cases"))
  })
  
  output$monthly_avg_resolution_chart <- renderPlotly({
    df <- monthly_series()
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, x = ~ym, y = ~avg_resolution_hours, type = "bar") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Avg resolution (hours)"))
  })
  
  output$monthly_trend_table <- renderDT({
    df <- monthly_series()
    datatable(df, rownames = FALSE, options = list(pageLength = 24, autoWidth = TRUE))
  })
  
  
  output$export_excel <- downloadHandler(
    filename = function() paste0("helpline_analytics_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()

      addWorksheet(wb, "Regional Performance")
      writeData(wb, "Regional Performance", regional_perf())

      addWorksheet(wb, "Category Trends")
      writeData(wb, "Category Trends", category_trends())

      addWorksheet(wb, "SLA Summary")
      writeData(wb, "SLA Summary", sla_data()$summary)
      writeData(wb, "SLA Summary", sla_data()$by_region, startRow = 5)

      addWorksheet(wb, "SLA Overdue")
      writeData(wb, "SLA Overdue", sla_data()$overdue)

      addWorksheet(wb, "Time Series")
      writeData(wb, "Time Series", monthly_series())

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )


  # ========================================
  # ESCALATION POPUP HANDLERS
  # ========================================

  # Render escalation popup content
  output$escalation_popup_content <- renderUI({
    case_data <- rv$last_escalated_case
    if (is.null(case_data)) return(NULL)

    tagList(
      tags$p(tags$strong("Case Code:"), " ", case_data$case_code),
      tags$p(tags$strong("Teacher:"), " ", case_data$teacher_name),
      tags$p(tags$strong("Region:"), " ", case_data$region_name),
      tags$p(tags$strong("Category:"), " ", case_data$category_name),
      tags$p(tags$strong("Priority:"), " ", case_data$priority)
    )
  })

  # Close escalation popup
  observeEvent(input$close_escalation_popup, {
    shinyjs::hide("escalationPopupOverlay")
    shinyjs::hide("escalationPopup")
    rv$last_escalated_case <- NULL
  })

  # View escalated case from popup
  observeEvent(input$view_escalated_case, {
    shinyjs::hide("escalationPopupOverlay")
    shinyjs::hide("escalationPopup")
    if (!is.null(rv$last_escalated_case)) {
      selected_case_id(rv$last_escalated_case$ticket_id)
      runjs("$('#caseDetailsModal').modal('show');")
    }
  })

  # Go to escalated cases tab
  observeEvent(input$go_to_escalated_tab, {
    shinyjs::hide("escalationPopupOverlay")
    shinyjs::hide("escalationPopup")
    rv$last_escalated_case <- NULL
    updateTabItems(session, "sidebar_menu", "escalated_cases")
  })


  # ========================================
  # ESCALATED CASES TAB HANDLERS
  # ========================================

  # Check if user is National PRO Office
  is_national_pro <- reactive({
    if (!isTRUE(rv$logged_in) || is.null(rv$user)) return(FALSE)
    rv$user$email == "enquiry.nationalprooffice@gmail.com" || rv$user$role == "National Admin"
  })

  # Escalated tab title - only shows for National PRO Office
  output$escalated_tab_title <- renderUI({
    if (is_national_pro()) {
      tagList(icon("exclamation-triangle"), " Escalated Cases")
    } else {
      NULL
    }
  })

  # Escalated cases panel - full content for National PRO Office, message for others
  output$escalated_cases_panel <- renderUI({
    if (!is_national_pro()) {
      return(
        div(style = "text-align: center; padding: 50px;",
          icon("lock", style = "font-size: 48px; color: #6b7280;"),
          h4("Access Restricted", style = "color: #374151; margin-top: 20px;"),
          p("Escalated cases are managed by the National PRO Office.", style = "color: #6b7280;"),
          p("If you need to view escalated cases, please contact: ", tags$strong("enquiry.nationalprooffice@gmail.com"))
        )
      )
    }

    # Full escalated cases panel for National PRO Office
    tagList(
      # Summary stats
      fluidRow(
        valueBoxOutput("esc_total_box", width = 3),
        valueBoxOutput("esc_pending_box", width = 3),
        valueBoxOutput("esc_today_box", width = 3),
        valueBoxOutput("esc_critical_box", width = 3)
      ),

      fluidRow(
        box(
          title = "Escalated Cases - National Review Queue",
          status = "danger", solidHeader = TRUE,
          width = 12,

          fluidRow(
            column(3,
              selectInput("esc_region_filter", "Region",
                          choices = c("All Regions" = ""))
            ),
            column(3,
              selectInput("esc_priority_filter", "Priority",
                          choices = c("All" = "", "Urgent", "High", "Medium", "Low"))
            ),
            column(3,
              selectInput("esc_category_filter", "Category",
                          choices = c("All Categories" = ""))
            ),
            column(3,
              actionButton("refresh_escalated", "Refresh", class = "btn-danger",
                           style = "margin-top: 25px; width: 100%;", icon = icon("sync"))
            )
          ),

          hr(),
          withSpinner(DT::dataTableOutput("escalated_cases_table")),

          hr(),
          p(tags$small("All escalations are sent to enquiry.nationalprooffice@gmail.com", style = "color: #6b7280;"))
        )
      )
    )
  })

  # Escalated cases valueBox outputs
  output$esc_total_box <- renderValueBox({
    valueBox(nrow(escalated_cases_data()), "Total Escalated", icon = icon("exclamation-circle"), color = "red")
  })

  output$esc_pending_box <- renderValueBox({
    valueBox(sum(escalated_cases_data()$status == "Escalated", na.rm = TRUE), "Pending Review", icon = icon("clock"), color = "yellow")
  })

  output$esc_today_box <- renderValueBox({
    data <- escalated_cases_data()
    count <- if (nrow(data) == 0) 0 else sum(as.Date(data$escalated_at) == Sys.Date(), na.rm = TRUE)
    valueBox(count, "Today", icon = icon("calendar-day"), color = "orange")
  })

  output$esc_critical_box <- renderValueBox({
    data <- escalated_cases_data()
    count <- if (nrow(data) == 0) 0 else sum(data$priority %in% c("High", "Urgent"), na.rm = TRUE)
    valueBox(count, "Critical", icon = icon("fire"), color = "maroon")
  })

  # Reactive data for escalated cases
  escalated_cases_data <- reactive({
    input$refresh_escalated  # Dependency for refresh

    tryCatch({
      base_query <- "
        SELECT t.ticket_id, t.case_code, t.escalated_at, t.created_at,
               t.teacher_name, t.teacher_phone, t.teacher_staff_id,
               t.school_name, t.district, r.region_name,
               c.category_name, t.priority, t.status, t.summary,
               t.escalation_reason,
               TIMESTAMPDIFF(HOUR, t.escalated_at, NOW()) as hours_since_escalation
        FROM tickets t
        LEFT JOIN regions r ON t.region_id = r.region_id
        LEFT JOIN issue_categories c ON t.category_id = c.category_id
        WHERE t.status = 'Escalated'
      "

      params <- list()

      # Apply filters
      if (!is.null(input$esc_region_filter) && input$esc_region_filter != "") {
        base_query <- paste0(base_query, " AND r.region_name = ?")
        params <- c(params, input$esc_region_filter)
      }

      if (!is.null(input$esc_priority_filter) && input$esc_priority_filter != "") {
        base_query <- paste0(base_query, " AND t.priority = ?")
        params <- c(params, input$esc_priority_filter)
      }

      if (!is.null(input$esc_category_filter) && input$esc_category_filter != "") {
        base_query <- paste0(base_query, " AND c.category_name = ?")
        params <- c(params, input$esc_category_filter)
      }

      base_query <- paste0(base_query, " ORDER BY t.escalated_at DESC")

      if (length(params) > 0) {
        dbGetQuery(con(), base_query, params = params)
      } else {
        dbGetQuery(con(), base_query)
      }
    }, error = function(e) {
      data.frame()
    })
  })

  # Escalated cases counts
  output$escalated_total_count <- renderText({
    nrow(escalated_cases_data())
  })

  output$escalated_pending_count <- renderText({
    sum(escalated_cases_data()$status == "Escalated", na.rm = TRUE)
  })

  output$escalated_today_count <- renderText({
    data <- escalated_cases_data()
    if (nrow(data) == 0) return("0")
    sum(as.Date(data$escalated_at) == Sys.Date(), na.rm = TRUE)
  })

  output$escalated_critical_count <- renderText({
    data <- escalated_cases_data()
    if (nrow(data) == 0) return("0")
    sum(data$priority %in% c("High", "Urgent"), na.rm = TRUE)
  })

  # Escalated cases table
  output$escalated_cases_table <- DT::renderDataTable({
    data <- escalated_cases_data()
    if (nrow(data) == 0) return(datatable(data.frame(Message = "No escalated cases")))

    display_data <- data %>%
      mutate(
        escalated_at = format(as.POSIXct(escalated_at), "%Y-%m-%d %H:%M"),
        case_code = paste0('<span class="case-code" onclick="Shiny.setInputValue(\'view_case_id\', ', ticket_id, ');">', case_code, '</span>'),
        priority = paste0('<span class="priority-', tolower(priority), '">', priority, '</span>'),
        hours_since_escalation = paste(hours_since_escalation, "hours ago")
      ) %>%
      select(case_code, escalated_at, teacher_name, region_name, category_name, priority, hours_since_escalation, summary)

    datatable(display_data,
              options = list(pageLength = 15, scrollX = TRUE, order = list(list(1, 'desc'))),
              escape = FALSE, rownames = FALSE,
              selection = 'single',
              colnames = c("Case", "Escalated At", "Teacher", "Region", "Category", "Priority", "Time Since", "Summary"))
  })

  # Update region filter choices for escalated cases
  observe({
    regions <- get_regions(con())
    updateSelectInput(session, "esc_region_filter",
                      choices = c("All Regions" = "", regions$region_name))
  })

  # Update category filter choices for escalated cases
  observe({
    categories <- get_categories(con())
    updateSelectInput(session, "esc_category_filter",
                      choices = c("All Categories" = "", categories$category_name))
  })


  # ========================================
  # FOLLOW-UP SCHEDULING HANDLERS
  # ========================================

  # Follow-up Alert Widget for Dashboard
  output$followup_alert_widget <- renderUI({
    data <- follow_ups_data()
    if (nrow(data) == 0) return(NULL)

    overdue_count <- sum(data$urgency == "Overdue", na.rm = TRUE)
    today_count <- sum(data$urgency == "Due Today", na.rm = TRUE)

    if (overdue_count == 0 && today_count == 0) return(NULL)

    # Build alert message
    alert_parts <- c()
    if (overdue_count > 0) {
      alert_parts <- c(alert_parts, paste0("<strong>", overdue_count, " overdue</strong>"))
    }
    if (today_count > 0) {
      alert_parts <- c(alert_parts, paste0("<strong>", today_count, " due today</strong>"))
    }

    alert_class <- if (overdue_count > 0) "alert-danger" else "alert-warning"

    fluidRow(
      column(12,
        div(class = paste("alert", alert_class), style = "margin-bottom: 15px; display: flex; align-items: center; justify-content: space-between;",
            div(
              icon("bell", style = "margin-right: 10px;"),
              HTML(paste0("Follow-up Alert: You have ", paste(alert_parts, collapse = " and "), " follow-ups that need attention."))
            ),
            actionButton("go_to_followups", "View Follow-ups", class = "btn btn-sm btn-outline-dark")
        )
      )
    )
  })

  # Navigate to Follow-ups tab when alert button clicked
  observeEvent(input$go_to_followups, {
    updateTabsetPanel(session, "main_tabs", selected = "Follow-ups")
  })

  # Reactive data for follow-ups - filtered by user's region
  follow_ups_data <- reactive({
    input$refresh_followups  # Dependency
    forced_region <- user_region_id()  # Get user's region restriction

    tryCatch({
      # Build query with optional region filter
      base_query <- "
        SELECT f.follow_up_id, f.ticket_id, t.case_code, t.teacher_name,
               t.teacher_phone, t.status as ticket_status, r.region_name,
               r.region_id, c.category_name, f.follow_up_date, f.follow_up_notes,
               f.status as follow_up_status,
               CASE
                 WHEN f.follow_up_date < CURDATE() THEN 'Overdue'
                 WHEN f.follow_up_date = CURDATE() THEN 'Due Today'
                 ELSE 'Upcoming'
               END as urgency
        FROM follow_ups f
        JOIN tickets t ON f.ticket_id = t.ticket_id
        LEFT JOIN regions r ON t.region_id = r.region_id
        LEFT JOIN issue_categories c ON t.category_id = c.category_id
        WHERE f.status = 'Pending'"

      # Add region filter for regional users
      if (!is.null(forced_region)) {
        base_query <- paste0(base_query, " AND t.region_id = ", forced_region)
      }

      base_query <- paste0(base_query, " ORDER BY f.follow_up_date ASC")

      dbGetQuery(con(), base_query)
    }, error = function(e) {
      data.frame()
    })
  })

  # Follow-up counts (text outputs)
  output$followup_overdue_count <- renderText({
    data <- follow_ups_data()
    if (nrow(data) == 0) return("0")
    sum(data$urgency == "Overdue", na.rm = TRUE)
  })

  output$followup_today_count <- renderText({
    data <- follow_ups_data()
    if (nrow(data) == 0) return("0")
    sum(data$urgency == "Due Today", na.rm = TRUE)
  })

  output$followup_week_count <- renderText({
    data <- follow_ups_data()
    if (nrow(data) == 0) return("0")
    sum(as.Date(data$follow_up_date) <= Sys.Date() + 7 & as.Date(data$follow_up_date) >= Sys.Date(), na.rm = TRUE)
  })

  output$followup_total_pending <- renderText({
    nrow(follow_ups_data())
  })

  # Follow-up valueBox outputs (for Dashboard tab)
  output$followup_overdue_box <- renderValueBox({
    data <- follow_ups_data()
    count <- if (nrow(data) == 0) 0 else sum(data$urgency == "Overdue", na.rm = TRUE)
    valueBox(count, "Overdue", icon = icon("exclamation-triangle"), color = "red")
  })

  output$followup_today_box <- renderValueBox({
    data <- follow_ups_data()
    count <- if (nrow(data) == 0) 0 else sum(data$urgency == "Due Today", na.rm = TRUE)
    valueBox(count, "Due Today", icon = icon("calendar-day"), color = "yellow")
  })

  output$followup_week_box <- renderValueBox({
    data <- follow_ups_data()
    count <- if (nrow(data) == 0) 0 else sum(as.Date(data$follow_up_date) <= Sys.Date() + 7 & as.Date(data$follow_up_date) >= Sys.Date(), na.rm = TRUE)
    valueBox(count, "This Week", icon = icon("calendar-week"), color = "blue")
  })

  output$followup_total_box <- renderValueBox({
    valueBox(nrow(follow_ups_data()), "Total Pending", icon = icon("clock"), color = "purple")
  })

  # Follow-up region filter UI - region-restricted based on user role
  output$followup_region_filter_ui <- renderUI({
    regions_df <- get_regions(con())
    forced_region <- user_region_id()

    if (!is.null(forced_region)) {
      # Regional users can only see their own region - no dropdown needed
      region_row <- regions_df[regions_df$region_id == forced_region, ]
      selectInput("followup_region_filter", "Region",
                  choices = setNames(region_row$region_name, region_row$region_name),
                  selected = region_row$region_name)
    } else {
      # National staff can filter by any region
      selectInput("followup_region_filter", "Region",
                  choices = c("All Regions" = "", regions_df$region_name))
    }
  })

  # Pending follow-ups table with complete action
  output$pending_followups_table <- DT::renderDataTable({
    data <- follow_ups_data()
    if (nrow(data) == 0) return(datatable(data.frame(Message = "No pending follow-ups")))

    # Apply filters
    if (!is.null(input$followup_urgency_filter) && input$followup_urgency_filter != "") {
      data <- data %>% filter(urgency == input$followup_urgency_filter)
    }

    if (!is.null(input$followup_region_filter) && input$followup_region_filter != "") {
      data <- data %>% filter(region_name == input$followup_region_filter)
    }

    display_data <- data %>%
      mutate(
        case_code = paste0('<span class="case-code" onclick="Shiny.setInputValue(\'view_case_id\', ', ticket_id, ');">', case_code, '</span>'),
        urgency = case_when(
          urgency == "Overdue" ~ '<span class="badge" style="background:#dc2626;color:white;">Overdue</span>',
          urgency == "Due Today" ~ '<span class="badge" style="background:#f59e0b;color:white;">Due Today</span>',
          TRUE ~ '<span class="badge" style="background:#3b82f6;color:white;">Upcoming</span>'
        ),
        actions = paste0(
          '<button class="btn btn-success btn-xs" onclick="Shiny.setInputValue(\'complete_followup_id\', ',
          follow_up_id, ', {priority: \'event\'});" title="Mark as Complete">',
          '<i class="fa fa-check"></i> Complete</button>'
        )
      ) %>%
      select(case_code, teacher_name, region_name, follow_up_date, urgency, follow_up_notes, actions)

    datatable(display_data,
              options = list(pageLength = 15, scrollX = TRUE, columnDefs = list(list(width = '100px', targets = 6))),
              escape = FALSE, rownames = FALSE,
              colnames = c("Case", "Teacher", "Region", "Follow-up Date", "Status", "Notes", "Actions"))
  })

  # Export follow-ups to Excel
  output$export_followups <- downloadHandler(
    filename = function() {
      paste0("follow_ups_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      data <- follow_ups_data()

      if (nrow(data) == 0) {
        # Create empty workbook with message
        wb <- createWorkbook()
        addWorksheet(wb, "Follow-ups")
        writeData(wb, "Follow-ups", data.frame(Message = "No follow-ups found"))
        saveWorkbook(wb, file, overwrite = TRUE)
        return()
      }

      # Apply current filters
      if (!is.null(input$followup_urgency_filter) && input$followup_urgency_filter != "") {
        data <- data %>% filter(urgency == input$followup_urgency_filter)
      }
      if (!is.null(input$followup_region_filter) && input$followup_region_filter != "") {
        data <- data %>% filter(region_name == input$followup_region_filter)
      }

      # Prepare export data
      export_data <- data %>%
        select(case_code, teacher_name, teacher_phone, region_name,
               category_name, follow_up_date, urgency, follow_up_notes, ticket_status) %>%
        rename(
          `Case Code` = case_code,
          `Teacher Name` = teacher_name,
          `Phone` = teacher_phone,
          `Region` = region_name,
          `Category` = category_name,
          `Follow-up Date` = follow_up_date,
          `Status` = urgency,
          `Notes` = follow_up_notes,
          `Case Status` = ticket_status
        )

      # Create workbook with styling
      wb <- createWorkbook()
      addWorksheet(wb, "Follow-ups")

      # Header style
      headerStyle <- createStyle(
        fontSize = 12, fontColour = "#FFFFFF", halign = "center",
        fgFill = "#1e3a8a", border = "TopBottomLeftRight",
        textDecoration = "bold"
      )

      # Write data
      writeData(wb, "Follow-ups", export_data, headerStyle = headerStyle)

      # Conditional formatting for urgency
      overdueStyle <- createStyle(fontColour = "#dc2626", textDecoration = "bold")
      todayStyle <- createStyle(fontColour = "#f59e0b", textDecoration = "bold")

      # Auto width columns
      setColWidths(wb, "Follow-ups", cols = 1:ncol(export_data), widths = "auto")

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Mark follow-up as complete handler
  observeEvent(input$complete_followup_id, {
    req(input$complete_followup_id)

    tryCatch({
      # Update follow-up status to Completed
      dbExecute(con(), "
        UPDATE follow_ups
        SET status = 'Completed',
            completed_at = NOW(),
            completed_by_user_id = ?
        WHERE follow_up_id = ?
      ", params = list(current_user_id(), input$complete_followup_id))

      # Log the action in ticket_actions
      followup_info <- dbGetQuery(con(),
        "SELECT ticket_id FROM follow_ups WHERE follow_up_id = ?",
        params = list(input$complete_followup_id))

      if (nrow(followup_info) > 0) {
        dbExecute(con(), "
          INSERT INTO ticket_actions (ticket_id, action_type, action_by_user_id, notes)
          VALUES (?, 'note', ?, 'Follow-up completed')
        ", params = list(followup_info$ticket_id[1], current_user_id()))
      }

      showNotification("Follow-up marked as complete!", type = "message")

      # Refresh the follow-ups data
      shinyjs::click("refresh_followups")
    }, error = function(e) {
      showNotification(paste("Error completing follow-up:", e$message), type = "error")
    })
  })

  # Schedule follow-up handler - restricted to user's region
  observeEvent(input$schedule_followup, {
    req(input$followup_case_search)
    req(input$followup_date)

    forced_region <- user_region_id()

    # Find the case - restricted to user's region for regional users
    case <- tryCatch({
      if (!is.null(forced_region)) {
        # Regional users can only schedule follow-ups for cases in their region
        dbGetQuery(con(),
          "SELECT ticket_id, case_code FROM tickets
           WHERE (case_code LIKE ? OR teacher_name LIKE ?) AND region_id = ? LIMIT 1",
          params = list(paste0("%", input$followup_case_search, "%"),
                       paste0("%", input$followup_case_search, "%"),
                       forced_region))
      } else {
        # National users can schedule follow-ups for any case
        dbGetQuery(con(),
          "SELECT ticket_id, case_code FROM tickets
           WHERE case_code LIKE ? OR teacher_name LIKE ? LIMIT 1",
          params = list(paste0("%", input$followup_case_search, "%"),
                       paste0("%", input$followup_case_search, "%")))
      }
    }, error = function(e) data.frame())

    if (nrow(case) == 0) {
      if (!is.null(forced_region)) {
        showNotification("Case not found in your region. Please enter a valid case code or teacher name.", type = "error")
      } else {
        showNotification("Case not found. Please enter a valid case code or teacher name.", type = "error")
      }
      return()
    }

    # Insert follow-up
    tryCatch({
      dbExecute(con(), "
        INSERT INTO follow_ups (ticket_id, follow_up_date, follow_up_notes, created_by_user_id)
        VALUES (?, ?, ?, ?)
      ", params = list(case$ticket_id[1], input$followup_date, input$followup_notes, current_user_id()))

      # Update ticket's next follow-up date
      dbExecute(con(), "UPDATE tickets SET next_follow_up_date = ? WHERE ticket_id = ?",
                params = list(input$followup_date, case$ticket_id[1]))

      showNotification(paste("Follow-up scheduled for", case$case_code[1], "on", input$followup_date), type = "message")
      updateTextInput(session, "followup_case_search", value = "")
      updateTextAreaInput(session, "followup_notes", value = "")
    }, error = function(e) {
      showNotification(paste("Error scheduling follow-up:", e$message), type = "error")
    })
  })


  # ========================================
  # CASE TEMPLATES HANDLERS
  # ========================================

  # Reactive data for templates
  templates_data <- reactive({
    input$refresh_templates  # Dependency

    tryCatch({
      base_query <- "SELECT * FROM case_templates WHERE is_active = 1"

      if (!is.null(input$template_category_filter) && input$template_category_filter != "") {
        base_query <- paste0(base_query, " AND template_category = '", input$template_category_filter, "'")
      }

      if (!is.null(input$template_search) && input$template_search != "") {
        search_term <- paste0("%", input$template_search, "%")
        base_query <- paste0(base_query, " AND (template_name LIKE '", search_term, "' OR template_body LIKE '", search_term, "')")
      }

      base_query <- paste0(base_query, " ORDER BY template_category, template_name")
      dbGetQuery(con(), base_query)
    }, error = function(e) {
      # Return default templates if table doesn't exist
      data.frame(
        template_id = 1:3,
        template_name = c("Case Acknowledgement", "Case Resolved", "Escalation Notice"),
        template_category = c("General", "General", "Escalation"),
        template_subject = c("Acknowledgement", "Resolved", "Escalated"),
        template_body = c(
          "GES Helpline: Your request has been received. Reference: {case_code}",
          "GES Helpline: Your case {case_code} has been resolved.",
          "GES Helpline: Your case {case_code} has been escalated for review."
        )
      )
    })
  })

  # Render templates list
  output$templates_list <- renderUI({
    data <- templates_data()
    if (nrow(data) == 0) return(p("No templates found"))

    template_cards <- lapply(1:nrow(data), function(i) {
      t <- data[i, ]
      cat_class <- tolower(gsub("/", "-", t$template_category))

      div(class = "template-card",
          onclick = paste0("Shiny.setInputValue('selected_template_id', ", t$template_id, ", {priority: 'event'})"),
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              tags$strong(t$template_name),
              span(class = paste0("template-category-badge template-category-", cat_class), t$template_category)
          ),
          tags$small(t$template_subject, style = "color: #6b7280;")
      )
    })

    do.call(tagList, template_cards)
  })

  # Selected template reactive
  selected_template <- reactive({
    req(input$selected_template_id)
    data <- templates_data()
    data[data$template_id == input$selected_template_id, ]
  })

  # Render template preview
  output$template_preview <- renderUI({
    if (is.null(input$selected_template_id)) {
      return(p("Select a template to preview", style = "color: #6b7280; text-align: center;"))
    }

    t <- selected_template()
    if (nrow(t) == 0) return(NULL)

    tagList(
      h5(t$template_name[1]),
      tags$strong("Subject: "), t$template_subject[1],
      hr(),
      tags$pre(style = "white-space: pre-wrap; background: #f8fafc; padding: 15px; border-radius: 8px;",
               t$template_body[1])
    )
  })

  # Quick response text
  output$quick_response_text <- renderText({
    rv$quick_response_text
  })

  # Quick response buttons
  observeEvent(input$qr_ack_received, {
    rv$quick_response_text <- "GES Helpline: Your request has been received and logged. You will receive an update within 24-48 hours. Thank you."
  })

  observeEvent(input$qr_ack_escalated, {
    rv$quick_response_text <- "GES Helpline: Your case has been escalated to the appropriate office for review. You will be contacted within 4 working hours."
  })

  observeEvent(input$qr_ack_processing, {
    rv$quick_response_text <- "GES Helpline: Your case is currently being processed. We will update you on progress."
  })

  observeEvent(input$qr_status_pending, {
    rv$quick_response_text <- "We require additional information to proceed with your case. Please provide the requested details at your earliest convenience."
  })

  observeEvent(input$qr_status_followup, {
    rv$quick_response_text <- "A follow-up has been scheduled for your case. We will contact you on the scheduled date with an update."
  })

  observeEvent(input$qr_status_resolved, {
    rv$quick_response_text <- "GES Helpline: Your case has been resolved. If you need further assistance, please contact us."
  })

  observeEvent(input$qr_phrase_understand, {
    rv$quick_response_text <- "I understand your concern. Let me help you resolve this matter."
  })

  observeEvent(input$qr_phrase_patience, {
    rv$quick_response_text <- "Thank you for your patience. We are working to address your concern as quickly as possible."
  })

  observeEvent(input$qr_phrase_assist, {
    rv$quick_response_text <- "Let me assist you with this matter. I will need a few details to proceed."
  })

  observeEvent(input$qr_close_help, {
    rv$quick_response_text <- "Is there anything else I can help you with today?"
  })

  observeEvent(input$qr_close_contact, {
    rv$quick_response_text <- "Please feel free to contact the GES Teacher Helpline if you have any further questions."
  })

  observeEvent(input$qr_close_reference, {
    rv$quick_response_text <- "Your reference number is [CASE_CODE]. Please keep this for future follow-up."
  })

  # Save new template
  observeEvent(input$save_new_template, {
    req(input$new_template_name)
    req(input$new_template_category)
    req(input$new_template_body)

    tryCatch({
      dbExecute(con(), "
        INSERT INTO case_templates (template_name, template_category, template_subject, template_body)
        VALUES (?, ?, ?, ?)
      ", params = list(input$new_template_name, input$new_template_category,
                       input$new_template_subject, input$new_template_body))

      showNotification("Template saved successfully", type = "message")
      updateTextInput(session, "new_template_name", value = "")
      updateTextInput(session, "new_template_subject", value = "")
      updateTextAreaInput(session, "new_template_body", value = "")
    }, error = function(e) {
      showNotification(paste("Error saving template:", e$message), type = "error")
    })
  })

  # Admin templates table
  output$admin_templates_table <- DT::renderDataTable({
    data <- templates_data()
    if (nrow(data) == 0) return(datatable(data.frame(Message = "No templates")))

    display_data <- data %>%
      select(template_id, template_name, template_category, template_subject)

    datatable(display_data,
              options = list(pageLength = 10, scrollX = TRUE),
              selection = 'single',
              rownames = FALSE,
              colnames = c("ID", "Name", "Category", "Subject"))
  })


  # ========================================
  # BULK OPERATIONS HANDLERS
  # ========================================

  # Track selected cases for bulk operations
  bulk_selected_cases <- reactiveVal(c())

  # Show/hide bulk actions bar based on selection
  observe({
    selected <- input$all_cases_table_rows_selected
    if (length(selected) > 0) {
      shinyjs::show("bulk_actions_bar")
      bulk_selected_cases(selected)
    } else {
      shinyjs::hide("bulk_actions_bar")
      bulk_selected_cases(c())
    }
  })

  # Selected count display
  output$bulk_selected_count <- renderText({
    paste(length(bulk_selected_cases()), "cases selected")
  })

  # Bulk update status button
  observeEvent(input$bulk_update_status, {
    if (length(bulk_selected_cases()) > 0) {
      runjs("$('#bulkStatusModal').modal('show');")
    }
  })

  # Confirm bulk status update
  observeEvent(input$confirm_bulk_status, {
    selected <- bulk_selected_cases()
    if (length(selected) == 0) return()

    data <- all_cases_data()
    ticket_ids <- data$ticket_id[selected]

    success_count <- 0
    for (tid in ticket_ids) {
      tryCatch({
        update_case_status(con(), tid, input$bulk_new_status, input$bulk_status_notes, current_user_id())
        success_count <- success_count + 1
      }, error = function(e) {})
    }

    runjs("$('#bulkStatusModal').modal('hide');")
    showNotification(paste("Updated", success_count, "of", length(ticket_ids), "cases"), type = "message")
    shinyjs::click("refresh_all_cases")
  })

  # Bulk change priority button
  observeEvent(input$bulk_change_priority, {
    if (length(bulk_selected_cases()) > 0) {
      runjs("$('#bulkPriorityModal').modal('show');")
    }
  })

  # Confirm bulk priority change
  observeEvent(input$confirm_bulk_priority, {
    selected <- bulk_selected_cases()
    if (length(selected) == 0) return()

    data <- all_cases_data()
    ticket_ids <- data$ticket_id[selected]

    success_count <- 0
    for (tid in ticket_ids) {
      tryCatch({
        dbExecute(con(), "UPDATE tickets SET priority = ? WHERE ticket_id = ?",
                  params = list(input$bulk_new_priority, tid))
        success_count <- success_count + 1
      }, error = function(e) {})
    }

    runjs("$('#bulkPriorityModal').modal('hide');")
    showNotification(paste("Updated priority for", success_count, "cases"), type = "message")
    shinyjs::click("refresh_all_cases")
  })

  # Bulk escalate all
  observeEvent(input$bulk_escalate, {
    selected <- bulk_selected_cases()
    if (length(selected) == 0) return()

    data <- all_cases_data()
    ticket_ids <- data$ticket_id[selected]

    success_count <- 0
    for (tid in ticket_ids) {
      tryCatch({
        dbExecute(con(), "UPDATE tickets SET status = 'Escalated', escalated_at = NOW() WHERE ticket_id = ?",
                  params = list(tid))
        add_case_note(con(), tid, "Bulk escalated for national review", current_user_id())
        success_count <- success_count + 1
      }, error = function(e) {})
    }

    showNotification(paste("Escalated", success_count, "cases"), type = "warning")
    shinyjs::click("refresh_all_cases")
  })

  # Clear bulk selection
  observeEvent(input$bulk_clear_selection, {
    bulk_selected_cases(c())
    shinyjs::hide("bulk_actions_bar")
  })


}

# Run the application

shinyApp(ui = ui, server = server)

