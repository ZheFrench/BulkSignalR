.onLoad <- function(...) {
    ############################
    ##        Database        ##
    ############################

    # handle directory creation over different OS
    # same as tools::R_user_dir("BulkSignalR", which="cache")
    cacheDir <- rappdirs::user_cache_dir("R/BulkSignalR")

    nameEnv <- "SignalR-Env"
    myEnv <- new.env(parent = emptyenv()) # globalenv() #emptyenv() #baseenv()
    attach(myEnv, name = nameEnv)
    assign("nameEnv", nameEnv, envir = as.environment(nameEnv))

    assign("BulkSignalR_CACHEDIR", cacheDir, envir = as.environment(nameEnv))

    url <- "https://partage-dev.montp.inserm.fr:9192"
    urlDatabase <- paste0(url,
        "/CBSB/SignalR/database/SignalR.db")
    assign("BulkSignalR_DB_URL", urlDatabase, envir = as.environment(nameEnv))

    createDatabase(onRequest = FALSE)

    LRdb <- getInteractions()

    assign("LRdb", LRdb, envir = as.environment(nameEnv))

    ################################
    ##   Resource Cache Files   ###
    ################################
    urlGo <- paste0(url,
        "/CBSB/SignalR/resources/gobp.rds")
    urlReactome <- paste0(url,
        "/CBSB/SignalR/resources/reactome.rds")
    urlNetwork <- paste0(url,
        "/CBSB/SignalR/resources/Network.rds")

    assign("BulkSignalR_GO_URL", urlGo, 
        envir = as.environment(nameEnv))
    assign("BulkSignalR_Reactome_URL", urlReactome, 
        envir = as.environment(nameEnv))
    assign("BulkSignalR_Network_URL", urlNetwork, 
        envir = as.environment(nameEnv))

    createResources(onRequest = FALSE)

    reactome <- getResource(resourceName = "Reactome", cache = TRUE)
    gobp <- getResource(resourceName = "GO-BP", cache = TRUE)
    Network <- getResource(resourceName = "Network", cache = TRUE)

    assign("reactome", reactome, envir = as.environment(nameEnv))
    assign("gobp", gobp, envir = as.environment(nameEnv))
    assign("Network", Network, envir = as.environment(nameEnv))
}

.onAttach <- function(libname, pkgname) {
    startupMsg <- r"{
    ################################
    ### Welcome to BulkSignalR ! ###
    ################################
}"

    packageStartupMessage(startupMsg)
}
