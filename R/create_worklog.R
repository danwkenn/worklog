#' Create a worklog database for storing work information.
#' @param path Path to create db
#' @export
create_worklog <- function(path = "worklog.db") {

# Create SQLite database
db_path <- path
con <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = db_path)

# Create contacts table
DBI::dbExecute(con, "
  CREATE TABLE contacts (
    id INTEGER PRIMARY KEY,
    title TEXT,
    firstname TEXT,
    surname TEXT,
    fullname TEXT,
    other_info TEXT
  )
")

# Create clients table
DBI::dbExecute(con, "
  CREATE TABLE clients (
    id INTEGER PRIMARY KEY,
    name TEXT,
    short_name TEXT,
    long_name TEXT,
    other_info TEXT
  )
")

# Create position table
DBI::dbExecute(con, "
  CREATE TABLE positions (
    id INTEGER PRIMARY KEY,
    contact_id INTEGER,
    client_id INTEGER,
    name TEXT,
    start_date TEXT,
    end_date TEXT,
    other_info TEXT
  )
")

# Create addresses table
DBI::dbExecute(con, "
  CREATE TABLE addresses (
    id INTEGER PRIMARY KEY,
    contact_id INTEGER,
    position_id INTEGER,
    type TEXT,
    value TEXT
  )
")

# Create projects table
DBI::dbExecute(con, "
  CREATE TABLE projects (
    id INTEGER PRIMARY KEY,
    client_id INTEGER,
    name TEXT,
    description TEXT,
    billing_scheme_id INTEGER,
    other_info TEXT
  )
")

# Create tasks table
DBI::dbExecute(con, "
  CREATE TABLE tasks (
    id INTEGER PRIMARY KEY,
    project_id INTEGER,
    name TEXT,
    description TEXT,
    completion_time NUMERIC,
    status TEXT
  )
")

# Create task_groups table
DBI::dbExecute(con, "
  CREATE TABLE task_groups (
    id INTEGER PRIMARY KEY,
    project_id INTEGER,
    name TEXT,
    description TEXT
  )
")

# Create tasks table
DBI::dbExecute(con, "
  CREATE TABLE task_group_allocation (
    id INTEGER PRIMARY KEY,
    task_id INTEGER,
    task_group_id TEXT
  )
")

# Billing Scheme table
DBI::dbExecute(con, "
  CREATE TABLE billing_scheme (
    id INTEGER PRIMARY KEY,
    name TEXT,
    description TEXT,
    rate_period TEXT,
    rate_value REAL,
    currency TEXT
  )
")

message("Worklog created at `", path, "`")
# Close the database connection
DBI::dbDisconnect(con)

invisible(TRUE)
}

function() {
  unlink("test.db")
  create_worklog("test.db")
  path <- "test.db"
  db <- get_db(path)
  DBI::dbListTables(db)
}