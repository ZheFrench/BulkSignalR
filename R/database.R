#' Fetch the database from internet.
#'
#' Fetch LR database from remote location.
#'
#' @param onRequest logical True if you force
#' download again. This will overwrite
#' pre-existing database. Default is True.
#' @param verbose Logical TRUE/FALSE
#' @return Returns `NULL`, invisibly. 
#' 
#' @import httr
#' @importFrom Biobase testBioCConnection
#' @importFrom cli cli_alert_danger cli_alert
#' @export
#' @examples
#' print("createDatabase")
#' if(FALSE){
#'  createDatabase()
#' }
createDatabase <- function(onRequest = TRUE, verbose = FALSE) {
    # Default directory
    cacheDir <- get("BulkSignalR_CACHEDIR")
    databaseCacheDir <- paste(cacheDir, "database", sep = "/")
    url <- get("BulkSignalR_DB_URL")
    # databaseFilePath <- paste(databaseCacheDir
    #    ,basename(url)
    #    ,sep = "/")
    www <- Biobase::testBioCConnection()
    if (!www & 
        !file.exists(databaseCacheDir)) {
        cli::cli_alert_danger("Your internet connection is off :")
        stop(
        "- Remote database can't be downloaded."
        )   
    }

    if (!www & 
        onRequest) {
        cli::cli_alert_danger("Your internet connection is off :")
        stop(
        "- Remote database can't be downloaded.\n"
        )   
    }

    if (!file.exists(databaseCacheDir) | onRequest) {
        # isDownloaded <- .downloadDatabase(url,databaseFilePath)
        # if(!isDownloaded)
        # stop("Ligand-Receptor database was not downloaded successfully.")
        #cacheVersion()
        .cacheAdd(
            fpath = url,
            cacheDir = databaseCacheDir,
            resourceName = basename(url),
            verbose = verbose,
            download = TRUE
        )

    }
    cacheVersion(dir="database")

    # fc <- BiocFileCache::BiocFileCache(databaseCacheDir,ask = FALSE)
    # cacheHits <- bfcquery(bfc,query="LRdb",field="rname")
    # rid <- cacheHits$rid
    # message(rid)
    # if(file.exists(bfc[[rid]])) {

    #    connexionObject <- DBI::dbCanConnect(RSQLite::SQLite(),
    # databaseFilePath)

    #    .checkDatabaseValidity(connexionObject=connexionObject)

    # }

    return(invisible(NULL))
}


#' Fetch the database from internet.
#'
#' Fetch LR database from remote location.
#'
#' @param url File URL.
#' @param databaseFilePath Path to database file.
#' @return Returns logical True/False. 
#' 
#' @import httr
#' @importFrom cli col_cyan
#' @keywords internal
.downloadDatabase <- function(url, databaseFilePath) {
    cli::col_cyan("Download Ligand-Receptor database...\n")

    isValid <- TRUE
    httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    response <- httr::GET(
        url,
        httr::write_disk(databaseFilePath, overwrite = TRUE),
        httr::progress()
    )

    if (httr::http_error(response)) {
        unlink(databaseFilePath)
        httr::warn_for_status(response, paste("find data at", url))
        isValid <- FALSE
    }
    return(isValid)
}

#' Check validity of database
#'
#' Control connexion is ok.
#'
#' @param connexionObject DBI::dbCanConnect object
#' to test if it's a valid connexion.
#' @return Returns `NULL`, invisibly. 
#' 
#' @import DBI RSQLite
#' @importFrom cli cli_alert_danger
#' @keywords internal
.checkDatabaseValidity <- function(connexionObject) {
    # check file is a database
    tryCatch(connexionObject,
        warning = function(e) {
            cli::cli_alert_danger("Ligand-Receptor database is corrupted.\n")
            stop("The file provided is not a valid database.")
        }
    )
    return(invisible(NULL))
}
