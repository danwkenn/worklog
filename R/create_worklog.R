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
    shortname TEXT,
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

message("Worklog created at `", path, "`")
# Close the database connection
DBI::dbDisconnect(con)

invisible(TRUE)
}

function() {
  create_worklog("test.db")
  db <- "test.db"
}