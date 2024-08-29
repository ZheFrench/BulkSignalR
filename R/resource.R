####################################################
###     Create / Get Ressources                  ###
####################################################

#' Create all resources.
#'
#' Create cache for all resources (pathways, or PWC network)
#' downloaded from the web when library is first loaded.
#' This part is handled with BiocFileCache.
#' Otherwise datatabase, is handled by another process
#' not relying on BiocFileCache instance.
#'
#' @param onRequest logical True if you force
#' download again. This will overwrite
#' pre-existing database. Default is True.
#' @param verbose Default is FALSE
#'
#' @export
#' @examples
#' if (FALSE) {
#'     createResources()
#' }
createResources <- function(onRequest = TRUE, verbose = FALSE) {
    cacheDir <- get("BulkSignalR_CACHEDIR")
    resourcesCacheDir <- paste(cacheDir, "resources", sep = "/")

    # Do it once, onLoad
    if (!dir.exists(resourcesCacheDir) | onRequest) {
        .cacheAdd(fpath = BulkSignalR_GO_URL, cacheDir = resourcesCacheDir, resourceName = "GO-BP", verbose = verbose)
        .cacheAdd(fpath = BulkSignalR_Reactome_URL, cacheDir = resourcesCacheDir, resourceName = "Reactome", verbose = verbose)
        .cacheAdd(fpath = BulkSignalR_Network_URL, cacheDir = resourcesCacheDir, resourceName = "Network", verbose = verbose)
    }

    return(invisible(NULL))
}



#' Get ressource from the cache.
#'
#' Get  resources (pathways, or PathwayCommons network
#' from \url{https://www.pathwaycommons.org/})
#' stored in the cache.
#'
#' @param resourceName   Ressource name.
#' @param cache   True/False. Defautlt is False
#' If True, you will use environment variables.
#'
#' @importFrom cli cli_alert_danger
#' @export
#' @examples
#' reactome <- getResource(resourceName = "Reactome")
getResource <- function(resourceName = NULL, cache = FALSE) {
    if (!resourceName %in% c("GO-BP", "Reactome", "Network")) {
        cli::cli_alert_danger(
            ".val {GO-BP, Reactome & Network} are the only keywords alllowed.\n")
        stop()
    }

    if (cache == TRUE) {
        cacheDir <- get("BulkSignalR_CACHEDIR")
        resourcesCacheDir <- paste(cacheDir, "resources", sep = "/")

        # safeguard
        if (!dir.exists(resourcesCacheDir)) {
            cli::cli_alert_danger("Resources repository doesn't exist.\n")
            stop()
        }

        bfc <- BiocFileCache::BiocFileCache(resourcesCacheDir, ask = FALSE)

        dataframe <- .readRDSFromCache(bfc = bfc, resourceName = resourceName)

        # Due to the fact React and Go are organized differently
        if (resourceName == "Reactome") {
            if (!all(c("Reactome ID", "Gene name", "Reactome name") %in% colnames(dataframe))) {
                cli::cli_alert_danger("Colnames of raw data are not well defined.\n")
                stop()
            }
            dataframe <- dataframe[, c("Reactome name", "Gene name", "Reactome ID")]
        }

        if (resourceName == "GO-BP") {
            if (!all(c("GO ID", "Gene name", "GO name") %in% colnames(dataframe))) {
                cli::cli_alert_danger("Colnames of raw data are not well defined.\n")
                stop()
            }
            dataframe <- dataframe[, c("GO ID", "Gene name", "GO name")]
        }

        if (resourceName == "Network") {
            if (!all(c("a.gn", "type", "b.gn") %in% colnames(dataframe))) {
                cli::cli_alert_danger("Colnames of raw data are not well defined.\n")
                stop()
            }
            dataframe <- dataframe[, c("a.gn", "type", "b.gn")]
        }
    } else if (cache == FALSE) {
        if (resourceName == "Reactome") {
            dataframe <- get("reactome")
        }

        if (resourceName == "GO-BP") {
            dataframe <- get("gobp")
        }

        if (resourceName == "Network") {
            dataframe <- get("Network")
        }
    }

    return(dataframe)
}

####################################################
###     / Parse / Format / Import Ressources     ###
####################################################

#' Import Network from your own
#'
#' Network is a dataframe that gives relation between
#' genes. It's composed of 3 columns annoted as
#' follows :
#'
#' a.gn : Gene Symbol 1
#' type : controls-expression-of
#' b.gn : Gene Symbol 2
#'
#' When the user provide his own network
#' 'type' should be set to 'controls-expression-of'.
#'
#' @param network   Network dataframe is defined with 3 columns
#' a.gn, b.gn & type. 'a.gn' & 'b.gn' should be gene symbols
#' of gene interactions. 'type'  should be set as
#' 'controls-expression-of' when user provide
#' his own file.
#'
#' @return NULL
#'
#' @importFrom cli cli_alert_info
#' @export
#' @examples
#' print("resetNetwork")
#' if (FALSE) {
#'     resetNetwork(network)
#' }
#'
resetNetwork <- function(network) {
    if (!all(c("a.gn", "type", "b.gn") %in% colnames(network))) {
        stop("Column names of network should be defined as a.gn, type & b.gn.")
    }

    message("")
    cli::cli_alert_info("New resource defined for {.val Network}.\n")

    assign("Network", network, envir = as.environment(get("nameEnv")))

    return(invisible(NULL))
} # resetNetwork


#' Import pathways from a file to dataframe
#'
#' Pathways are defined in Reactome and
#' GoBP databases.
#' Those can be updated using
#' json files from
#' the Human Molecular Signatures Database (MSigDB)
#' at \url{https://www.gsea-msigdb.org/}
#'
#' \code{resetDownstreamPathways} is a function
#' we provide to user to refresh REACTOME
#' and GO-BP content included in BulkSignalR.
#'
#' Gmt file format also can be imported.
#'
#' @param file    Path to file.
#' @param fileType    Default is Json.
#' Other options are gmt or txt files.
#' @param resourceName    Two options "GO-BP" or "REACTOME".
#'
#' @return NULL
#'
#' @importFrom cli cli_alert_info
#' @export
#' @examples
#' print("resetPathwaysFromFile")
#' if (FALSE) {
#'     resetPathwaysFromFile(file, "GO-BP")
#' }
#'
resetPathwaysFromFile <- function(file,
                                  fileType = c("json", "gmt", "txt"),
                                  resourceName = NULL) {
    fileType <- match.arg(fileType)

    if (!resourceName %in% c("GO-BP", "Reactome")) {
        stop("GO-BP and Reactome are the only keywords alllowed.")
    }

    if (!file.exists(file)) {
        stop("This file doesn't exist.")
    }

    if (fileType == "json") {
        db <- .formatPathwaysFromJson(
            file = file,
            resourceName = resourceName
        )
    } else if (fileType == "gmt") {
        db <- .formatPathwaysFromGmt(
            file = file,
            resourceName = resourceName
        )
    } else if (fileType == "txt") {
        db <- .formatPathwaysFromTxt(
            file = file,
            resourceName = resourceName
        )
    } else {
        stop("File format accepted are `json` , `gmt` or `txt` only.")
    }

    message("")
    cli::cli_alert_info("New resource defined for {.val {resourceName}}.\n")
    message(utils::head(db))


    if (resourceName == "Reactome") {
        assign("reactome", db, envir = as.environment(get("nameEnv")))
    }
    if (resourceName == "GO-BP") {
        assign("gobp", db, envir = as.environment(get("nameEnv")))
    }


    return(invisible(NULL))
} # resetPathwaysFromFile

#' Read dataframe from txt file
#'
#' @param file    Path to a tabular file.
#' @param resourceName    Two options "GO-BP"  "REACTOME".
#'
#' @return Dataframe with pathwayID, geneName and pathwayName
#'
.formatPathwaysFromTxt <- function(
    file,
    resourceName = NULL) {
    db <- utils::read.csv(file,
        stringsAsFactors = FALSE, sep = "\t", check.names = FALSE
    )

    if (resourceName == "Reactome") {
        if (!all(c("Reactome ID", "Gene name", "Reactome name") %in% names(db))) {
            cli::cli_alert_danger("Colnames should be defined with specific names.\n")
            stop("Three columns must defined as 'Reactome ID','Gene name','Reactome name'.")
        }
    } else if (resourceName == "GO-BP") {
        if (!all(c("GO ID", "Gene name", "GO name") %in% names(db))) {
            cli::cli_alert_danger("Colnames should be defined with specific names.\n")
            stop("Three columns must defined as 'GO ID','Gene name','GO name'.")
        }
    } else {
        cli::cli_alert_danger("Resource name is not well defined.\n")
        stop("")
    }

    return(db)
} # .formatPathwaysFromTxt


#' Format dataframe according to json input
#'
#' @param file    Path to file.
#' @param resourceName    Two options "GO-BP" or "REACTOME".
#'
#' @return Dataframe with pathwayID, geneName and pathwayName
#'
#' @importFrom foreach %do% %dopar%
#' @import doParallel
#' @import jsonlite
.formatPathwaysFromJson <- function(
    file,
    resourceName = NULL) {
    data <- jsonlite::read_json(file, simplifyVector = TRUE)
    indexPathway <- NULL

    if (!all(c("exactSource", "geneSymbols") %in% names(data[[1]]))) {
        cli::cli_alert_danger("Json format is invalid.\n")
        stop("Json must follow the standard from The Molecular Signatures Database (MSigDB).")
    }

    db <- foreach::foreach(
        indexPathway = seq_len(length(data)),
        .combine = "rbind"
    ) %dopar% {
        data.frame(
            pathwayID = rep(data[[indexPathway]]$exactSource, length(data[[indexPathway]]$exactSource)),
            geneName = unlist(data[[indexPathway]]$geneSymbols)[seq_len(length(unlist(data[[indexPathway]]$geneSymbols)))],
            pathwayName = rep(names(data)[[indexPathway]], length(names(data)[[indexPathway]]))
        )
    }

    # Due to the fact React and Go are organized differently
    if (resourceName == "Reactome") {
        names(db) <- c("Reactome ID", "Gene name", "Reactome name")
    }

    if (resourceName == "GO-BP") {
        names(db) <- c("GO ID", "Gene name", "GO name")
    }

    return(db)
} # .formatPathwaysFromJson

#' Transform gmt file to dataframe
#'
#' We note discrepancy between format available
#' over internet.
#'
#' Here we consider a valid gmt file format defined
#' on each lines as follows :
#' First is Pathway name,
#' Then comes the ID,
#' Finally you will find genes symbols
#' according to the pathway defined on the line.
#'
#' You can find an example here.
#' - For Reactome. (Directly from their website)
#' \url{https://reactome.org/download/current/ReactomePathways.gmt.zip}
#' Note that you need to unzip the file to read the content.

#' The code is inspired from read.gmt function
#' from the gsa R package.
#'
#' @param file    Path to GMT file
#' @param resourceName   Two options "GO-BP" or "REACTOME"
#'
#' @return Dataframe with pathwayID, geneName and pathwayName
#'
#' @importFrom foreach %do% %dopar%
#' @import doParallel
.formatPathwaysFromGmt <- function(
    file,
    resourceName = NULL) {
    read1 <- scan(file,
        what = list("", ""), sep = "\t",
        quote = NULL, fill = TRUE, flush = TRUE, multi.line = FALSE
    )

    read1 <- stats::setNames(read1, c("Description", "ID"))
    read1 <- read1[c("ID", "Description")]

    geneset.ids <- read1[1][[1]]
    geneset.descriptions <- read1[2][[1]]

    read2 <- scan(file, what = "", sep = "\t", quote = NULL)
    read2 <- read2[read2 != ""]

    nn <- length(geneset.ids)
    n <- length(read2)
    ox <- rep(NA, nn)

    # Compute indice of pathway
    ii <- 1
    for (i in seq_len(nn)) {
        while ((read2[ii] != geneset.descriptions[i]) | (read2[ii + 1] != geneset.ids[i])) {
            ii <- ii + 1
        }
        ox[i] <- ii
        ii <- ii + 1
    }

    genesets <- vector("list", nn)

    for (i in seq_len(nn - 1)) {
        i1 <- ox[i] + 2
        i2 <- ox[i + 1] - 1
        geneset.descriptions[i] <- read2[ox[i] + 1]
        geneset.ids[i] <- read2[ox[i]]
        genesets[[i]] <- read2[i1:i2]
    }

    geneset.ids[nn] <- read2[ox[nn]]
    geneset.descriptions[nn] <- read2[ox[nn] + 1]
    genesets[[nn]] <- read2[(ox[nn] + 2):n]

    data <- list(
        geneset.ids = geneset.ids,
        geneset.descriptions = geneset.descriptions,
        genesets = genesets
    )

    dataframeFromGmt <- foreach::foreach(
        i = seq_len(length(data$geneset.ids)),
        .combine = "rbind"
    ) %dopar% {
        data.frame(
            a = rep(data$geneset.ids[[i]], length(data$genesets[[i]])),
            b = data$genesets[[i]][seq_len(length(data$genesets[[i]]))],
            c = rep(data$geneset.descriptions[[i]], length(data$genesets[[i]]))
        )
    }

    # Due to the fact React and Go are organized differently
    if (resourceName == "Reactome") {
        names(dataframeFromGmt) <- c("Reactome name", "Gene name", "Reactome ID")
        dataframeFromGmt <- dataframeFromGmt[c("Reactome ID", "Gene name", "Reactome name")]
    }

    if (resourceName == "GO-BP") {
        names(dataframeFromGmt) <- c("GO name", "Gene name", "GO ID")
        dataframeFromGmt <- dataframeFromGmt[c("GO ID", "Gene name", "GO name")]
    }

    return(dataframeFromGmt)
} # .formatPathwaysFromGmt
