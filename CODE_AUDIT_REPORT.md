# Code Audit Report: GES Teacher Support Helpline

**Date:** 2026-02-14
**Auditor:** Senior Developer / System Designer / Code Auditor
**Application:** GES Teacher Support Helpline & Query Tracking System
**Stack:** R/Shiny + MariaDB, deployed via Docker/Heroku

---

## Executive Summary

This is a well-intentioned, feature-rich case management application for the Ghana Education Service. It has good foundational security practices (parameterized queries, bcrypt, rate limiting, XSS escaping). However, there are **critical architectural issues**, **security gaps**, **maintainability concerns**, and **data integrity risks** that need to be addressed before this can be considered production-hardened.

**Severity Legend:** CRITICAL | HIGH | MEDIUM | LOW

---

## 1. CRITICAL: Architecture & Maintainability

### 1.1 Monolithic Single-File Application (CRITICAL)

**File:** `app.R` -- **5,713 lines in a single file**

This is the single most pressing issue. The entire application -- UI, server logic, database queries, authentication, business logic, HTML/CSS -- lives in one file. This creates:

- **Impossible code reviews:** Changes to any feature risk unintended side effects.
- **No separation of concerns:** UI, business logic, and data access are interleaved.
- **Onboarding difficulty:** New developers cannot reason about the system.
- **Merge conflict hell:** Multiple developers cannot work on different features simultaneously.

**Recommendation:** Refactor into Shiny modules:
```
app.R                          # Entry point only (~50 lines)
R/
  mod_landing_page.R           # Landing page module
  mod_auth.R                   # Authentication module
  mod_dashboard.R              # Dashboard module
  mod_case_entry.R             # Case entry (full + quick)
  mod_case_details.R           # Case details modal
  mod_all_cases.R              # Case listing/search
  mod_analytics.R              # Analytics module
  mod_escalation.R             # Escalation workflow
  mod_followups.R              # Follow-up management
  mod_templates.R              # Response templates
  utils/
    db_helpers.R               # Database query functions
    auth_helpers.R             # Authentication utilities
    escape_helpers.R           # XSS prevention utilities
    ui_helpers.R               # Shared UI components
```

### 1.2 Duplicated Code Throughout (HIGH)

Multiple patterns are repeated extensively:

- **Region fallback data** (hardcoded list of 16 regions) appears 3 times (`app.R:370-401`).
- **Entry mode backwards-compatibility check** (`if (!"entry_mode" %in% names(data)) data$entry_mode <- "full"`) appears 3 times.
- **HTML table rendering with XSS escaping** is copy-pasted across `recent_cases_table`, `my_cases_table`, `all_cases_table`, `escalated_cases_table`, and `pending_followups_table` -- nearly identical blocks.
- **Follow-up region-filtered queries** are duplicated with only the WHERE clause differing.

**Recommendation:** Extract shared rendering logic into helper functions. Use a single `render_cases_datatable()` function parameterized by columns and options.

### 1.3 Duplicate Library Import (LOW)

**File:** `app.R:19` and `app.R:27` -- `library(dplyr)` is loaded twice.

---

## 2. CRITICAL: Security Issues

### 2.1 Plaintext Password in SQL Seed File (CRITICAL)

**File:** `teacher_query_7.sql:442`
```sql
('Mary Administrator', 'admin@ges.gov.gh', 'National Admin', NULL, 'game1990');
```

A National Admin account has a **plaintext password** stored directly in the SQL seed file. This is not a bcrypt hash -- it's the literal string `game1990`. If this INSERT was ever executed, bcrypt verification would fail (it would never match), but the password itself is exposed in version control.

**Recommendation:** Remove this line entirely or replace with a proper bcrypt hash. Audit git history to verify this was never committed with real credentials.

### 2.2 Default Password Shared Across All Accounts (HIGH)

**File:** `teacher_query_7.sql:227`
```sql
-- Default password for all accounts: 'password' (bcrypt hash below)
```

All 18 staff accounts share the **same bcrypt hash** (`$2a$12$.0q2ipj...`). A comment explicitly states the default password is `password`. While the app prompts for password change on first login, users can **skip this** (`app.R:2886-2890`).

**Recommendation:**
- Remove the ability to skip password change on first login, or at minimum enforce it within 7 days.
- Generate unique initial passwords per user and distribute them securely.
- Remove the plaintext password comment from the SQL file.

### 2.3 No CSRF Protection (HIGH)

Shiny applications are inherently somewhat protected by WebSocket communication, but the application uses `Shiny.setInputValue()` calls from inline JavaScript extensively. There is no CSRF token validation. Any malicious script injected into the page could trigger actions.

**Recommendation:** Consider adding a session token that is validated on sensitive operations (status changes, escalations, bulk updates).

### 2.4 Rate Limiting is Per-Session, Not Per-IP (MEDIUM)

**File:** `app.R:2498-2541`

Rate limiting for login attempts uses `reactiveVal(list())` which is scoped to a single Shiny session. An attacker can simply open a new browser tab/session to bypass rate limiting entirely.

**Recommendation:** Move rate limiting to a persistent store (database or Redis). The `login_audit` table already tracks attempts per email -- use it for rate limiting:
```sql
SELECT COUNT(*) FROM login_audit
WHERE email = ? AND success = 0
AND attempted_at > DATE_SUB(NOW(), INTERVAL 15 MINUTE)
```

### 2.5 Session Timeout Relies on Client-Side Keepalive (MEDIUM)

**File:** `app.R:2477-2480`

A keepalive fires every 25 seconds (`invalidateLater(25000)`), which means the session timeout check at `app.R:2559` (every 60 seconds) will always see recent activity as long as the browser tab is open, even if the user is idle. The `observe` at line 2550-2556 triggers on **any** input change, but the keepalive is also an input change.

**Recommendation:** The activity tracker should only count explicit user interactions (button clicks, form changes), not the keepalive ping.

### 2.6 Hardcoded Email Address for National PRO Office (MEDIUM)

**File:** `app.R:1753`, `app.R:4396`, `app.R:4856`

The email `enquiry.nationalprooffice@gmail.com` is hardcoded in multiple places for escalation routing. This should be configurable via environment variable or database setting.

### 2.7 No Content Security Policy Headers (MEDIUM)

The application loads Font Awesome from an external CDN (`cdnjs.cloudflare.com`) at `app.R:1206` without any CSP headers. This creates a supply chain risk if the CDN is compromised.

**Recommendation:** Add CSP headers via `shiny-server.conf` or serve Font Awesome locally.

### 2.8 Database Name Exposed in SQL File (LOW)

**File:** `teacher_query_7.sql:7`
```sql
USE oqhm7raqbkfdrtqz;
```

The actual production database name is committed to source control.

---

## 3. HIGH: Database Design Issues

### 3.1 SQL File is Not Idempotent (HIGH)

**File:** `teacher_query_7.sql`

The SQL file contains:
- `CREATE TABLE IF NOT EXISTS` (idempotent) -- good
- `CREATE INDEX` without `IF NOT EXISTS` (lines 279-299) -- will fail on re-run
- `ALTER TABLE tickets ADD COLUMN entry_mode` appears **3 times** (lines 100, 1017, 1032) -- the first is in the CREATE TABLE, the other two are separate ALTERs that will fail on re-run
- `CREATE TRIGGER` without `DROP TRIGGER IF EXISTS` -- will fail on re-run
- Multiple duplicate `INSERT IGNORE` and `ON DUPLICATE KEY UPDATE` blocks for the same data

**Recommendation:** Use a proper migration system. At minimum, wrap ALTERs and CREATEs in conditional checks, and remove duplicate statements.

### 3.2 No Foreign Key on `created_by_user_id` for Ticket Actions During Escalation (MEDIUM)

**File:** `app.R:4386-4401`

The escalation flow uses `dbExecute` directly instead of the `EscalateTicket` stored procedure that was specifically designed for this purpose with proper transaction handling. The stored procedure at `teacher_query_7.sql:562-594` wraps the operation in a transaction, but the app code does not.

**Recommendation:** Use the stored procedure via `CALL EscalateTicket(?, ?, ?, ?)` instead of manual queries.

### 3.3 Inconsistent Use of Connection Pool (MEDIUM)

The codebase uses three different patterns to interact with the database:
1. `con()` which returns the pool directly (`app.R:224-234`)
2. `poolCheckout(con)` / `poolReturn()` in `insert_ticket` (`app.R:693-694`)
3. `poolWithTransaction(pool, ...)` in `update_case_status` (`app.R:565`)

The `con()` helper function is misleading -- it returns the pool object, not a connection. Some functions pass `con()` as `pool_conn` while others pass it as `con`. This inconsistency increases the risk of connection leaks.

**Recommendation:** Standardize on `pool` for read operations (the pool handles checkout/return automatically) and `poolWithTransaction(pool, ...)` for write operations.

### 3.4 No Database Migration Strategy (MEDIUM)

The SQL file has grown organically with appended ALTER TABLE statements and duplicate blocks. There's no versioning, no rollback capability, and no way to know which statements have been applied to a given environment.

**Recommendation:** Adopt a migration tool or at minimum maintain numbered migration files (`001_initial_schema.sql`, `002_add_templates.sql`, etc.).

### 3.5 Dashboard Statistics View References Another View (LOW)

**File:** `teacher_query_7.sql:549`

The `dashboard_stats` view references the `sla_monitoring` view in a LEFT JOIN. Nested view references can cause performance issues with MySQL/MariaDB's view implementation (which materializes views as temporary tables).

---

## 4. HIGH: Performance & Scalability

### 4.1 N+1 Query Pattern in Bulk Operations (HIGH)

**File:** `app.R:5638-5643`, `app.R:5666-5677`, `app.R:5688-5695`

Bulk status update, priority change, and escalation all loop through selected ticket IDs one at a time:
```r
for (tid in ticket_ids) {
  update_case_status(con(), tid, ...)
}
```

Each iteration executes a full transaction (checkout connection, begin transaction, UPDATE, INSERT action log, commit, return connection). For 50 selected cases, this is 50 separate transactions.

**Recommendation:** Use a single batch UPDATE statement:
```sql
UPDATE tickets SET status = ? WHERE ticket_id IN (?, ?, ?, ...)
```

### 4.2 Full Table Scan on Every Dashboard Load (MEDIUM)

**File:** `app.R:808-834`

`get_dashboard_stats()` fetches **all tickets** from the database and aggregates them in R:
```r
raw_data <- dbGetQuery(con, main_query)
status_data <- raw_data %>% group_by(status) %>% summarise(count = n())
```

As the ticket count grows, this will become increasingly slow and memory-intensive. The database already has views (`dashboard_stats`) designed for this purpose.

**Recommendation:** Use the `dashboard_stats` database view, or at minimum, perform aggregation in SQL.

### 4.3 `reactiveValuesToList(input)` Triggers on Every Input Change (MEDIUM)

**File:** `app.R:2552`

```r
observe({
  reactiveValuesToList(input)
  if (isTRUE(rv$logged_in)) {
    rv$last_activity <- Sys.time()
  }
})
```

This observer fires on **every single input change** across the entire application, which is extremely expensive. With multiple DT tables, dropdowns, and inputs, this creates thousands of unnecessary reactive updates.

**Recommendation:** Track activity on specific meaningful interactions (button clicks, form submissions) rather than all inputs.

### 4.4 No Pagination for Database Queries (MEDIUM)

**File:** `app.R:786`

`fetch_tickets` uses `LIMIT 200` as a hard cap, but this means up to 200 rows are always fetched. As the system grows, consider server-side pagination with DT's `server = TRUE` option.

### 4.5 Auto-Refresh Timer Fires for All Users (LOW)

**File:** `app.R:3341`

`reactiveTimer(300000)` fires every 5 minutes for every connected user, even if they're on a tab that doesn't use the data. Combined with the keepalive, this means every idle browser tab triggers database queries every 5 minutes.

---

## 5. MEDIUM: Code Quality Issues

### 5.1 Inconsistent Error Handling (MEDIUM)

Some functions show notifications on error:
```r
showNotification(paste("Error loading categories:", e$message), type = "error")
```

Others silently fail:
```r
}, error = function(e) {
  return(data.frame())
})
```

And some log errors to the console:
```r
message("Connection test failed: ", e$message)
```

**Recommendation:** Establish a consistent error handling strategy. Consider a centralized `handle_db_error()` function.

### 5.2 Magic Numbers and Strings Throughout (MEDIUM)

- `LIMIT 200` appears in multiple queries
- Region ID `1` is used as a default in several places
- Status strings like `"New"`, `"In Progress"`, `"Escalated"` are repeated ~50 times as raw strings
- User ID `1` is the fallback for `current_user_id()` (`app.R:4297`)

**Recommendation:** Define constants at the top of the file:
```r
STATUS_NEW <- "New"
STATUS_IN_PROGRESS <- "In Progress"
DEFAULT_QUERY_LIMIT <- 200
```

### 5.3 The `con()` Helper is Misleading (MEDIUM)

**File:** `app.R:224-234`

```r
con <- function() {
  if (is.null(pool)) return(NULL)
  tryCatch({ pool }, error = function(e) { return(NULL) })
}
```

This function is named `con` (implying "connection") but returns the pool. The `tryCatch` around simply returning a variable does nothing useful. Functions that receive this are sometimes named `con`, sometimes `pool_conn`, creating confusion about what they receive.

### 5.4 `cat()` Debug Statements Left in Production Code (LOW)

**File:** `app.R:3972`
```r
cat("Opening case details for ticket_id:", input$view_case_id, "\n")
```

Debug output should not be in production code.

### 5.5 Unused/Orphaned Code (LOW)

- `output$sidebar_menu_items` and `output$sidebar_quick_search` (`app.R:3132-3162`) render sidebar content for a sidebar that is hidden via CSS.
- The `USE_LOCAL_DB` variable (`app.R:205`) is defined but never used in the connection logic.
- `add_case_action()` function (`app.R:535-553`) uses `showNotification` which won't work when called outside a reactive context.

---

## 6. MEDIUM: Frontend / UX Issues

### 6.1 Massive Inline CSS (MEDIUM)

**File:** `app.R:1207-1722`

Over **500 lines of CSS** are embedded inline in the R code via `tags$style(HTML(...))`. This makes both the CSS and the R code harder to maintain.

**Recommendation:** Move CSS to a `www/styles.css` file and reference it with `tags$link(rel = "stylesheet", href = "styles.css")`.

### 6.2 Inline JavaScript in HTML Attributes (MEDIUM)

Multiple places use inline `onclick` handlers:
```r
onclick = "Shiny.setInputValue('view_case_id', ...)"
```

This tightly couples the UI to specific Shiny input IDs and makes it harder to refactor.

### 6.3 External CDN Dependency (LOW)

**File:** `app.R:1206`

Font Awesome is loaded from `cdnjs.cloudflare.com`. If the CDN is unavailable (common in some African regions with network instability), icons will not render. Consider bundling it locally.

### 6.4 No Mobile Responsiveness Testing (LOW)

The landing page uses flexbox with `min-width: 180px` cards, but the dashboard uses `shinydashboard` which is not inherently mobile-friendly. The horizontal navigation menu may overflow on smaller screens.

---

## 7. MEDIUM: Deployment & Operations

### 7.1 Dockerfile Missing Health Check (MEDIUM)

**File:** `Dockerfile`

No `HEALTHCHECK` instruction is defined. Container orchestrators (Docker Compose, Kubernetes) cannot determine if the app is actually healthy.

**Recommendation:**
```dockerfile
HEALTHCHECK --interval=30s --timeout=10s --retries=3 \
  CMD curl -f http://localhost:3838/ || exit 1
```

### 7.2 Dockerfile Missing Non-Root User Directive (MEDIUM)

The Dockerfile sets file ownership to `shiny:shiny` but doesn't explicitly set `USER shiny`. The Shiny Server itself drops privileges, but for Docker best practices, add `USER shiny` before `EXPOSE`.

### 7.3 No Logging Infrastructure (MEDIUM)

The application uses `message()` for logging, which goes to stderr. There's no structured logging, log levels, or log rotation. In production, critical events like failed logins, escalations, and errors should be logged in a queryable format.

### 7.4 No `.dockerignore` File (LOW)

Without a `.dockerignore`, the `COPY . .` directive copies unnecessary files (`.git/`, `renv/library/`, etc.) into the Docker image, increasing build time and image size.

### 7.5 No Automated Tests (LOW)

There are zero test files. For a production application handling sensitive teacher data, at minimum there should be:
- Unit tests for `escape_html()`, `validate_password()`, helper functions
- Integration tests for `insert_ticket()`, `update_case_status()`
- Authentication flow tests

---

## 8. LOW: Data Quality Issues

### 8.1 Teacher Phone Number Not Validated (LOW)

**File:** `app.R:3503`

The form requires `teacher_phone` for full entries but performs no format validation. Ghana phone numbers follow specific patterns (+233XXXXXXXXX or 0XXXXXXXXX), and the placeholder suggests this, but no server-side validation enforces it.

### 8.2 Case Code Generation Has a Theoretical Race Condition (LOW)

**File:** `app.R:711-715`

```r
ticket_id <- as.integer(dbGetQuery(db_conn, "SELECT LAST_INSERT_ID() AS id")$id[1])
generated_code <- sprintf("GES-%d-%06d", as.integer(format(Sys.Date(), "%Y")), ticket_id)
dbExecute(db_conn, "UPDATE tickets SET case_code = ? WHERE ticket_id = ?", ...)
```

While using `LAST_INSERT_ID()` on a checked-out connection is correct, the UPDATE is not within the same transaction as the INSERT. If the UPDATE fails, you have a ticket with a NULL case_code. The trigger approach or using the database to generate the code would be more robust.

---

## Summary of Priorities

| Priority | Count | Key Items |
|----------|-------|-----------|
| CRITICAL | 3 | Monolithic file, plaintext password in SQL, shared default passwords |
| HIGH | 6 | N+1 bulk queries, per-session rate limiting, SQL file not idempotent, CSRF, duplicated code, inconsistent pool usage |
| MEDIUM | 15 | Full table scans, inline CSS, no CSP, no health check, no migration strategy, session timeout bug, etc. |
| LOW | 10 | Duplicate import, debug statements, no tests, no .dockerignore, etc. |

---

## Recommended Action Plan

### Phase 1: Security Hardening (Immediate)
1. Remove plaintext password from `teacher_query_7.sql:442`
2. Remove default password comment from `teacher_query_7.sql:227`
3. Move rate limiting to the database (use `login_audit` table)
4. Fix session timeout to not count keepalive as activity
5. Make escalation target email configurable

### Phase 2: Data Integrity (Short-term)
1. Clean up the SQL file -- remove duplicate ALTER/INSERT blocks
2. Use stored procedures for escalation/resolution
3. Standardize database connection patterns
4. Add phone number validation

### Phase 3: Architecture (Medium-term)
1. Refactor into Shiny modules (biggest ROI for maintainability)
2. Extract CSS into separate file
3. Extract database helpers into separate files
4. Add a `.dockerignore` and Docker `HEALTHCHECK`

### Phase 4: Performance & Quality (Long-term)
1. Move dashboard aggregation to SQL
2. Replace bulk operation loops with batch SQL
3. Add server-side DT pagination
4. Add automated tests
5. Add structured logging
