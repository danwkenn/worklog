#' Create an XLSX from database
#' @param path Path to save XLSX to.
#' @param db data-base
#' @export
create_xlsx_from_db <- function(
  path,
  db = NULL) {

  if (is.null(db)) {
    on.exit(DBI::dbDisconnect(db))
  }
  if (is.character(db)) {
    on.exit(DBI::dbDisconnect(db))
  }

  db <- get_db(db)

  tables <- DBI::dbListTables(db)

  wb <- openxlsx::createWorkbook()

  for (tab in tables) {
    tab_data <- DBI::dbReadTable(
      conn = db,
      name = tab)
    openxlsx::addWorksheet(
      wb,
      tab
    )
    openxlsx::writeData(
      wb,
      tab,
      tab_data
    )
  }
  openxlsx::saveWorkbook(
    wb,
    path,
    overwrite = FALSE
  )
  return(path)
}
