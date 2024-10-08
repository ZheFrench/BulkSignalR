####################################################
###     Generic Hidden Cache Functions           ###
####################################################

#' Add cache for resources & database.
#'
#' Add cache for resources (pathways, or PWC network)
#' downloaded from the web or local database .
#' This part is handled with BiocFileCache.
#'
#' @param fpath    Path to file on the web or local system.
#' @param resourceName   Ressource name.
#' @param cacheDir  Absolute path to cache directory.
#' @param verbose   Default FALSE
#' @param download   Logical(TRUE/FALSE) Default TRUE for download.
#' @return Returns `NULL`, invisibly. 
#' 
#' @import BiocFileCache
#' @import httr
#' @keywords internal
.cacheAdd <- function(fpath, cacheDir, resourceName,
    verbose = FALSE, download = TRUE) {
    if (!dir.exists(cacheDir)) {
        dir.create(cacheDir, recursive = TRUE)
    }

    dir <- basename(cacheDir)

    bfc <- BiocFileCache::BiocFileCache(cacheDir, ask = FALSE)

    # safeguard
    cacheHits <- BiocFileCache::bfcquery(bfc, 
        query = resourceName, field = "rname")
    if (nrow(cacheHits) >= 1) {
        cli::cli_alert_danger("Multiple cache results found for {.var {dir}}.")
        stop("Please clear your cache with `cacheClear()`!", "\n")
    }

    config <- httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

    # if fname="exact" remove the unique identifier
    BiocFileCache::bfcadd(bfc, rname = resourceName,
        config = config, fpath = fpath, download = download)
    
    cli::cli_alert_info("{.val {resourceName}} added to cache with success.")

    if (verbose) {
        cli::cli_alert("{.path {BiocFileCache::bfccache(bfc)}}")
        BiocFileCache::bfcinfo(bfc)
        message("")
    }

    return(invisible(NULL))
}

#' Check existence of a record in the cache.
#'
#' Check if the cache record exists or not, by passing
#' to the function an associated keyword
#' related to the resource we are looking for.
#'
#' @param bfc Object of class BiocFileCache, created by a call to
#' BiocFileCache::BiocFileCache()
#' @param resourceName keyword associated to a specific resource name.
#'
#' @keywords internal
#' @return logical This function returns TRUE if a record with
#' the requested keyword already  exists in the file cache,
#'  otherwise returns FALSE.
.cacheCheckIn <- function(bfc, resourceName) {
    cacheHits <- BiocFileCache::bfcquery(bfc, 
        query = resourceName, field = "rname")
    as.logical(nrow(cacheHits))
}

####################################################
###     Generic Public Cache Functions           ###
####################################################

#' Delete cache content.
#'
#' Delete the content of cache directory.
#'
#' @param dir Directory to remove. Can be only 'resources' or 'database'.
#' @return Returns `NULL`, invisibly. 
#' 
#' @importFrom BiocFileCache removebfc
#' @importFrom cli cli_alert_danger cli_alert
#' @export
#' @examples
#' print("cacheClear")
cacheClear <- function(dir = c("both", "resources", "database")) {
    dir <- match.arg(dir)

    if (!dir %in% c("resources", "database", "both")) {
        stop("Only `resources`, `database` or `both` cache can be cleared.")
    }

    if (dir == "both") {
        cacheClear(dir = "resources")
        cacheClear(dir = "database")
        return(invisible(NULL))
    }

    cacheDir <- get("BulkSignalR_CACHEDIR")

    cacheDir <- paste(cacheDir, dir, sep = "/")

    # safeguard
    if (!dir.exists(cacheDir)) {
        cli::cli_alert("BulkSignalR cache for {.val {dir}} doesn't exist.")
        return(invisible(NULL))
    }

    bfc <- BiocFileCache::BiocFileCache(cacheDir, ask = FALSE)
    BiocFileCache::removebfc(bfc, ask = FALSE)

    # dir.create(resourcesCacheDir)
    cli::cli_alert("BulkSignalR cache {.val {dir}} has been deleted.\n")
    message(
        "- Location: ", cacheDir, "\n",
        "- No. of files: 0", "\n"
    )

    return(invisible(NULL))
}

#' Get cache content informations..
#'
#' Get cache content informations for specific cache dir.
#'
#' @param dir Directory to remove in order to clean the cache.
#' Can be only 'resources', 'database' or 'both'.
#' @return Returns `NULL`, invisibly. 

#' @importFrom BiocFileCache BiocFileCache bfcinfo
#' @importFrom cli cli_alert_danger cli_alert
#' @export
#' @examples
#' print("cacheInfo")
cacheInfo <- function(dir = c("both", "resources", "database")) {
    dir <- match.arg(dir)

    if (!dir %in% c("resources", "database", "both")) {
        stop("Only `resources`, `database` or `both` cache can be cleared.")
    }

    if (dir == "both") {
        cacheInfo(dir = "resources")
        cacheInfo(dir = "database")
        return(invisible(NULL))
    }

    cacheDir <- get("BulkSignalR_CACHEDIR")

    cacheDir <- paste(cacheDir, dir, sep = "/")

    # safeguard
    if (!dir.exists(cacheDir)) {
        cli::cli_alert("BulkSignalR {.val {dir}} cache uninitialized.")
        message("- Location: ", cacheDir, "\n")
        return(invisible(NULL))
    }

    bfc <- BiocFileCache::BiocFileCache(cacheDir, ask = FALSE)
    files <- BiocFileCache::bfcinfo(bfc)$rpath

    if (length(files) == 0) {
        cli::cli_alert("BulkSignalR {.val {dir}} cache uninitialized.")
        message("- Location: ", cacheDir, "\n",
            "- No. of files: ", length(files), "\n"
        )
        return(invisible(NULL))
    } else {
        total_size <- sum(file.size(files))
        size_obj <- structure(total_size, class = "object_size")
        # print(total_size)
        # print(size_obj)

        cli::cli_alert("BulkSignalR {.val {dir}} cache :")
        message(
            "- Location:  ", cacheDir, " \n",
            "- No. of files:  ", length(files), "\n",
            "- Total size:  ", format(size_obj, units = "auto"), "\n"
        )
        listing <- paste0(files, "\n")
        message(listing)
    }

    return(invisible(NULL))
}


#' Check remote files ressources are changed.
#'
#' Check to see if some ressources has
#' has been updated.
#'
#' @param dir Directory for which you want to check Version.
#' Can be only 'resources', 'database' or 'both'.
#'
#' @importFrom cli cli_alert_danger cli_alert cli_alert_info
#' @importFrom cli cli_inform
#' @import BiocFileCache httr
#' @importFrom Biobase testBioCConnection
#' @return Returns `NULL`, invisibly. 
#'
#' @export
#' @examples
#' print("cacheVersion")
cacheVersion <- function(dir = c("both", "resources", "database")) {
    dir <- match.arg(dir)

    if (!dir %in% c("resources", "database", "both")) {
        stop("Only `resources`, `database` or `both` are valid keywords.")
    }

    if (dir == "both") {
        cacheVersion(dir = "resources")
        cacheVersion(dir = "database")
        return(invisible(NULL))
    }

    cacheDir <- get("BulkSignalR_CACHEDIR")
    cacheDir <- paste(cacheDir, dir, sep = "/")

    if (!dir.exists(cacheDir)) {
        cli::cli_alert_danger("BulkSignalR {.val {dir}} cache uninitialized.")
        stop("- Location: ", cacheDir, "\n")    
    }

    config <- httr::set_config(config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

    word <- ifelse(dir == "resources", "have", "has")
    word2 <- ifelse(dir == "resources", "are", "is")

    if (Biobase::testBioCConnection()) {

        bfc <- BiocFileCache::BiocFileCache(cacheDir, ask = FALSE)

        if (any(BiocFileCache::bfcneedsupdate(bfc))) {
            cli::cli_alert("Remote {.val {dir}} {word} been updated.\n")
            mess_info <- paste0("To update locally,",
                " clear your cache with cacheClear({.var {dir}})\n")
            cli::cli_alert_info(mess_info)
            return(invisible(NULL))
        } else {
            cli::cli_inform("Local {.val {dir}} {word2} up to date.\n",
            class = "packageStartupMessage")
            packageStartupMessage("")
        }
    } 
    else {
        mess_info <- paste0("Your internet connection is off:",
        " remote update of {.val {dir}} won't be checked.")
        cli::cli_alert_info(mess_info)
    }
    return(invisible(NULL))
}

####################################################
###     Check / Read RDS From Cache              ###
####################################################

#' Read RDS from the cache.
#'
#' Access  resources (pathways, or PathwayCommons network
#' from \url{https://www.pathwaycommons.org/})
#' stored in the cache.
#'
#' @param bfc Object of class BiocFileCache, created by a call to
#' BiocFileCache::BiocFileCache()
#' @param resourceName keyword associated to a specific resourceName
#' @return Returns BiocFileCache::bfcquery object or FALSE
#' 
#' @importFrom cli cli_alert_danger cli_alert cli_alert_info
#' @import BiocFileCache
#' @keywords internal
.readRDSFromCache <- function(bfc, resourceName) {
    cacheHits <- BiocFileCache::bfcquery(bfc, query = resourceName,
        field = "rname")
    if (nrow(cacheHits) == 0) {
        cli::cli_alert_danger("No cache result found.\n")
        stop()
    } else if (nrow(cacheHits) > 1) {
        cli::cli_alert_danger("Multiple cache results found.\n")
        stop("Please, clear your cache! See cacheClear() function.")
    } else {
        test <- tryCatch(is.list(infoRDS(cacheHits$rpath[1])),
            error = function(e) {
                return(FALSE)
            }
        )
        if (test) {
            rid <- cacheHits$rid
            result <- readRDS(bfc[[rid]])
            return(result)
        }
    }
    # if not RDS
    return(FALSE)
}

#' Check valid RDS cache file.
#'
#' This function checks if a cache entry is a valid RDS file.
#' Returns TRUE if the cache entry is valid, FALSE otherwise.
#' In the case of an invalid file the cache entry and file are
#' deleted.
#'
#' @param bfc Object of class BiocFileCache, created by a call to
#' BiocFileCache::BiocFileCache()
#' @param resourceName keyword associated to a specific resource name.
#' @return logical TRUE/FALSE
#' 
#' @importFrom cli cli_alert_danger cli_alert cli_alert_info
#' @importFrom BiocFileCache bfcremove bfcquery
#' @keywords internal
.checkRDSFromCache <- function(bfc, resourceName) {
    cacheHits <- BiocFileCache::bfcquery(bfc, query = resourceName,
        field = "rname")
    if (nrow(cacheHits) == 0) {
        cli::cli_alert_danger("No cache result found.\n")
        stop()
    } else if (nrow(cacheHits) > 1) {
        cli::cli_alert_danger("Multiple cache results found.\n")
        stop("Please, clear your cache! See cacheClear() function.")
    } else {
        test <- tryCatch(is.list(infoRDS(cacheHits$rpath[1])),
            error = function(e) {
                return(FALSE)
            }
        )
        if (!test) {
            BiocFileCache::bfcremove(bfc, cacheHits$rid[1])
        }
        return(test)
    }

    return(FALSE)
}