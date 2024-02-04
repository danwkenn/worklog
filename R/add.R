#' Internal function for DB.
#' @param db data-base
get_db <- function(db) {
  if (is.null(db)) {
    db <- Sys.getenv("WORKLOG_DB_PATH")
  }

  if (is.character(db)) {
    db <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  }
  db
}

#' Internal function for new ID creation
#' @param db data-base
#' @param table table to create ID for
get_unique_id <- function(db, table) {
  query <- paste(
    "SELECT COUNT(*) AS N FROM",
    table)
  row_count <- DBI::dbGetQuery(db, query)$N
  id <- row_count + 1
  id
}

#' Convert other info into character form
#' @param other_info List to convert to JSON
convert_other_info <- function(other_info) {
  jsonlite::toJSON(other_info)
}

#' Add a contact to the contacts table.
#' @param title Title (Mr/Ms) of contact
#' @param firstname First name
#' @param surname Last name/surname
#' @param fullname Full name including title, firstname, surname
#' @param other_info Other information.
#' @param db Database file.
#' @export
add_contact <- function(
  title,
  firstname,
  surname,
  fullname,
  other_info = NULL,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "contacts")
  other_info <- convert_other_info(other_info)
  # Insert data into the contacts table
  new_contact <- data.frame(
    id = id,
    title = title,
    firstname = firstname,
    surname = surname,
    fullname = fullname,
    other_info = as.character(other_info))

  DBI::dbWriteTable(
    db,
    "contacts",
    new_contact,
    append = TRUE,
    row.names = FALSE)

  message("Contact added successfully with id `", id, "`")
  invisible(TRUE)
}

#' Add a client to the clients table.
#' @param name Name of the client
#' @param short_name Short name of client
#' @param long_name Long name of client
#' @param other_info Other information.
#' @param db Database file.
#' @export
add_client <- function(
  name,
  short_name,
  long_name,
  other_info = NULL,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "clients")
  other_info <- convert_other_info(other_info)
  # Insert data into the contacts table
  new_contact <- data.frame(
    id = id,
    name = name,
    short_name = short_name,
    long_name = long_name,
    other_info = as.character(other_info))

  DBI::dbWriteTable(
    db,
    "clients",
    new_contact,
    append = TRUE,
    row.names = FALSE)

  message("Client added successfully with id `", id, "`")
  invisible(TRUE)
}

#' Add a position to the positions table.
#' @param contact_id Id of contact
#' @param client_id id of client
#' @param name name of the position.
#' @param start_date Initial date of the position
#' @param end_date End date of the position
#' @param other_info Other information.
#' @param db Database file.
#' @export
add_position <- function(
  contact_id,
  client_id,
  name,
  start_date = NA_character_,
  end_date = NA_character_,
  other_info = NULL,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "positions")
  other_info <- convert_other_info(other_info)

  # Insert data into the contacts table
  new_position <- data.frame(
    id = id,
    contact_id = contact_id,
    client_id = client_id,
    name = name,
    start_date = as.character(start_date),
    end_date = as.character(end_date),
    other_info = as.character(other_info))

  DBI::dbWriteTable(
    db,
    "positions",
    new_position,
    append = TRUE,
    row.names = FALSE)

  message("position added successfully with id `", id, "`")
  invisible(TRUE)
}

#' Add a address to the addresses table.
#' @param contact_id Id of contact
#' @param position_id id of client
#' @param type name of the address type.
#' @param value value of address.
#' @param db Database file.
#' @export
add_address <- function(
  contact_id,
  position_id,
  type,
  value,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "addresses")
  # Insert data into the contacts table
  new_address <- data.frame(
    id = id,
    contact_id = contact_id,
    position_id = position_id,
    type = type,
    value = value)

  DBI::dbWriteTable(
    db,
    "address",
    new_address,
    append = TRUE,
    row.names = FALSE)

  message("address added successfully with id `", id, "`")
  invisible(TRUE)
}


#' Add a address to the addresses table.
#' @param name Name of the scheme
#' @param description Description
#' @param rate_period unit where rate is applied
#' @param rate_value  bill per unit
#' @param currency currency billing is in.
#' @param db Database file.
#' @export
add_billing_scheme <- function(
  name,
  description,
  rate_period,
  rate_value,
  currency,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "billing_schemes")
  # Insert data into the contacts table
  new_row <- data.frame(
    id = id,
    name = name,
    description = description,
    rate_period = rate_period,
    rate_value = rate_value,
    currency = currency)

  DBI::dbWriteTable(
    db,
    "billing_schemes",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("billing scheme added successfully with id `", id, "`")
  invisible(TRUE)
}

#' Add a project to the projects table.
#' @param client_id Client ID
#' @param name Name of project
#' @param description Description of project
#' @param billing_scheme_id ID for billing scheme.
#' @param other_info Other info about projects.
#' @param db Database file.
#' @export
add_project <- function(
  client_id,
  name,
  description,
  billing_scheme_id,
  other_info,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "projects")
  other_info <- convert_other_info(other_info)

  # Insert data into the projects table
  new_row <- data.frame(
    id = id,
  client_id = client_id,
  name = name,
  description = description,
  billing_scheme_id = billing_scheme_id,
  other_info = other_info
   )

  DBI::dbWriteTable(
    db,
    "projects",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("projects added successfully with id `", id, "`")
  invisible(TRUE)
}



#' Add a task to the tasks table.
#' @param project_id Project ID
#' @param name Name of task
#' @param description Description of task
#' @param completion_time time taken to complete
#' @param status Not Started, In progress, Finished
#' @param db Database file.
#' @export
add_task <- function(
  project_id,
  name,
  description,
  completion_time,
  status,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "tasks")
  # Insert data into the contacts table
  new_row <- data.frame(
    id = id,
project_id = project_id,
name = name,
description = description,
completion_time = completion_time,
status = status
   )

  DBI::dbWriteTable(
    db,
    "tasks",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("tasks added successfully with id `", id, "`")
  invisible(TRUE)
}



#' Add a task to the task_groups table.
#' @param project_id Project ID
#' @param name Name of task group
#' @param description Description of task group
#' @param db Database file.
#' @export
add_task_group <- function(
  project_id,
  name,
  description,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "task_groups")
  # Insert data into the contacts table
  new_row <- data.frame(
    id = id,
project_id = project_id,
name = name,
description = description
   )

  DBI::dbWriteTable(
    db,
    "task_groups",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("task_group added successfully with id `", id, "`")
  invisible(TRUE)
}



#' Add a task to the addresses table.
#' @param task_id Task ID
#' @param task_group_id Group ID.
#' @param order_value Value to order tasks by
#' @param db Database file.
#' @export
add_task_group_allocation <- function(
  task_id,
  task_group_id,
  order_value,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "task_group_allocations")
  # Insert data into the contacts table
  new_row <- data.frame(
    id = id,
    task_id = task_id,
    task_group_id = task_group_id,
   )

  DBI::dbWriteTable(
    db,
    "task_group_allocations",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("task_group_allocation added successfully with id `", id, "`")
  invisible(TRUE)
}

#' Add a task to the addresses table.
#' @param issue_date Date to issue.
#' @param due_period_unit Period unit to use.
#' @param due_period_value Period length
#' @param sent Sent?
#' @param paid Paid?
#' @param other_info Other info to add
#' @param db Database file.
#' @export
add_invoice <- function(
  issue_date,
  due_period_unit,
  due_period_value,
  sent,
  paid,
  other_info,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "task_group_allocations")
  other_info <- convert_other_info(other_info)


  # Insert data into the contacts table
  new_row <- data.frame(
    id = id,
    issue_date = issue_date,
    due_period_unit = due_period_unit,
    due_period_value = due_period_value,
    sent = sent,
    paid = paid,
    other_info = other_info
   )

  DBI::dbWriteTable(
    db,
    "task_group_allocations",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("task_group_allocation added successfully with id `", id, "`")
  invisible(TRUE)
}

#' Add a task group to the invoice allocation table.
#' @param invoice_id Invoice ID.
#' @param task_group_id Task Group ID.
#' @param db Database file.
#' @export
add_invoice_task_group_allocation <- function(
  invoice_id,
  task_group_id,
  db = NULL
) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  # Get ID:
  id <- get_unique_id(db, "invoice_task_group_allocations")
  # Insert data into the contacts table
  new_row <- data.frame(
    id = id,
    invoice_id = invoice_id,
    task_group_id = task_group_id,
   )

  DBI::dbWriteTable(
    db,
    "invoice_task_group_allocation",
    new_row,
    append = TRUE,
    row.names = FALSE)

  message("task_group_allocation added successfully with id `", id, "`")
  invisible(TRUE)
}
