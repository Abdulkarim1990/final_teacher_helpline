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
    db_user <- Sys.getenv("DB_USER", " ")
    db_password <- Sys.getenv("DB_PASSWORD", " ")
    
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
pool <- create_db_pool()

# Ensure pool is closed when app stops
onStop(function() {
  if (!is.null(pool)) {
    poolClose(pool)
  }
})

# Enhanced connection helper with better error handling


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
      
      # Feature cards
      div(style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap; padding: 0 40px 60px 40px; max-width: 1200px; margin: 0 auto;",
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
      
      # Footer
      div(style = "text-align: center; padding: 30px; border-top: 1px solid rgba(255,255,255,0.1);",
          tags$p("Ghana Education Service - Ministry of Education", style = "color: #64748b; font-size: 13px; margin: 0;")
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
  # MAIN DASHBOARD (hidden until login or analytics)
  # ========================================
  hidden(
    div(id = "main_app",
        dashboardPage(
          dashboardHeader(
            title = "GES Teacher Support Helpline",
            tags$li(class = "dropdown",
                    tags$style(HTML("
                    .main-header .navbar {background-color: #1e3a8a !important;}
                    .main-header .logo {background-color: #0f172a !important;}
                    .content-wrapper {background-color: #f8fafc;}
                  "))
            ),
            # User info and logout in header
            tags$li(class = "dropdown",
                    uiOutput("user_info_header")
            )
          ),
          
          dashboardSidebar(
            sidebarMenu(
              id = "sidebar_menu",
              
              # These menu items are conditionally shown via server-side rendering
              uiOutput("sidebar_menu_items")
            ),
            
            # Quick Case Lookup in sidebar (only visible when logged in)
            uiOutput("sidebar_quick_search")
          ),
          
          dashboardBody(
            use_theme(mytheme),
            
            # Enhanced CSS for modern styling and case details
            tags$head(
              tags$style(HTML("
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

              /* Enhanced Sidebar Menu Styling */
              .sidebar-menu > li > a {
                padding: 15px 20px !important;
                font-size: 15px !important;
                font-weight: 500 !important;
                border-left: 4px solid transparent;
                transition: all 0.2s ease;
              }

              .sidebar-menu > li > a:hover {
                background-color: #2c5aa0 !important;
                border-left: 4px solid #f59e0b;
              }

              .sidebar-menu > li.active > a {
                background-color: #2c5aa0 !important;
                border-left: 4px solid #f59e0b;
              }

              .sidebar-menu > li > a > i,
              .sidebar-menu > li > a > .fa,
              .sidebar-menu > li > a > .fas,
              .sidebar-menu > li > a > .far {
                font-size: 18px !important;
                width: 28px !important;
                margin-right: 12px !important;
              }

              .sidebar-menu > li > a > span {
                font-size: 15px !important;
              }

              /* Sidebar header styling */
              .main-sidebar {
                padding-top: 10px;
              }

              .sidebar-menu {
                padding: 10px 0;
              }

              /* Quick Case Lookup styling */
              .sidebar .form-control {
                background-color: rgba(255, 255, 255, 0.15) !important;
                border: 1px solid rgba(255, 255, 255, 0.3) !important;
                color: white !important;
                padding: 10px 12px !important;
                font-size: 14px !important;
                border-radius: 6px !important;
              }

              .sidebar .form-control::placeholder {
                color: rgba(255, 255, 255, 0.6) !important;
              }

              .sidebar .form-control:focus {
                background-color: rgba(255, 255, 255, 0.25) !important;
                border-color: #f59e0b !important;
                outline: none !important;
                box-shadow: 0 0 0 2px rgba(245, 158, 11, 0.2) !important;
              }

              .sidebar h5 {
                font-size: 14px !important;
                font-weight: 600 !important;
                text-transform: uppercase;
                letter-spacing: 0.5px;
              }

              .sidebar hr {
                border-color: rgba(255, 255, 255, 0.2);
                margin: 15px 0;
              }

              /* Logo area styling */
              .main-header .logo {
                font-size: 16px !important;
                font-weight: 600 !important;
                padding: 0 15px !important;
              }
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
            
            tabItems(
              # Dashboard Tab with sub-tabs
              tabItem(
                tabName = "dashboard",
                tabsetPanel(
                  # Case Summary Tab
                  tabPanel(
                    title = "Case Summary",
                    br(),
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
                    withSpinner(DT::dataTableOutput("all_cases_table"))
                  )
                )
              ),
              
              # Analytics Tab (unchanged from original)
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
    page_state = "landing"  # "landing", "login", "analytics", "dashboard"
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
        
        # Update last_login
        try(DBI::dbExecute(conn,
                           "UPDATE users SET last_login = NOW() WHERE user_id = ?",
                           params = list(u$user_id[1])
        ), silent = TRUE)
        
        rv$logged_in <- TRUE
        rv$user <- u[1, ]
        rv$page_state <- "dashboard"
        output$login_msg <- renderText("")
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
    rv$page_state <- "landing"
    updateTextInput(session, "login_email", value = "")
    updateTextInput(session, "login_password", value = "")
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
          actionButton("logout_btn", "Logout", class = "btn btn-sm",
                       style = "background: rgba(255,255,255,0.2); color: white; border: 1px solid rgba(255,255,255,0.3); border-radius: 4px;")
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
  
  # --- Dynamic Sidebar Menu ---
  output$sidebar_menu_items <- renderUI({
    if (isTRUE(rv$logged_in)) {
      # Logged-in users see all menu items - Dashboard selected by default
      tagList(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"), selected = TRUE),
        menuItem("New Case", tabName = "new_case", icon = icon("plus-circle")),
        menuItem("All Cases", tabName = "all_cases", icon = icon("list")),
        menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"))
      )
    } else {
      # Non-logged-in users (analytics-only) see only analytics
      tagList(
        menuItem("Analytics", tabName = "analytics", icon = icon("chart-bar"), selected = TRUE)
      )
    }
  })
  
  # --- Quick Case Lookup (only for logged-in users) ---
  output$sidebar_quick_search <- renderUI({
    if (isTRUE(rv$logged_in)) {
      tagList(
        hr(),
        div(style = "padding: 10px 15px;",
            h5("Quick Case Lookup", style = "color: #ffffff; margin-bottom: 10px;"),
            textInput("quick_search_code", NULL, placeholder = "Enter Case Code..."),
            actionButton("quick_search_btn", "Find Case", class = "btn-sm btn-info", style = "width: 100%;")
        )
      )
    }
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
  
  # Case Details Modal Logic
  observeEvent(input$view_case_id, {
    if (!is.null(input$view_case_id)) {
      cat("Opening case details for ticket_id:", input$view_case_id, "\n")
      selected_case_id(input$view_case_id)
      runjs("$('#caseDetailsModal').modal('show');")
    }
  })
  
  # NEW: Render Case Details Content with improved error handling
  output$case_details_content <- renderUI({
    req(selected_case_id())
    
    case_data <- get_case_details(con(), selected_case_id())
    actions_data <- get_case_actions(con(), selected_case_id())
    
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
  
  # Escalate case handler
  observeEvent(input$escalate_case_btn, {
    if (!is.null(selected_case_id())) {
      uid <- current_user_id()
      success <- update_case_status(con(), selected_case_id(), "Escalated", "Case escalated for further review", uid)
      if (success) {
        tryCatch({ log_activity(pool, uid, "Escalate Case", paste("Case ID:", selected_case_id())) }, error = function(e) {})
        runjs("$('#caseDetailsModal').modal('hide');")
        showNotification("Case escalated successfully", type = "warning")
        shinyjs::click("refresh_my_cases")
        shinyjs::click("refresh_all_cases")
      }
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
  
  
  
}

# Run the application

shinyApp(ui = ui, server = server)

