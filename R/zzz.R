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
        "/CBSB/SignalR/database/SignalR_09.2024.db")
    assign("BulkSignalR_DB_URL", urlDatabase, envir = as.environment(nameEnv))

    createDatabase(onRequest = FALSE)

    BulkSignalR_LRdb <- getInteractions()

    assign("BulkSignalR_LRdb", BulkSignalR_LRdb,
    envir = as.environment(nameEnv))

    ################################
    ##   Resource Cache Files   ###
    ################################
    urlGo <- paste0(url,
        "/CBSB/SignalR/resources/gobp_09.2024.rds")
    urlReactome <- paste0(url,
        "/CBSB/SignalR/resources/reactome_09.2024.rds")
    urlNetwork <- paste0(url,
        "/CBSB/SignalR/resources/rawData/C10_M50/Network_09.2024.rds")

    assign("BulkSignalR_GO_URL", urlGo, 
        envir = as.environment(nameEnv))
    assign("BulkSignalR_Reactome_URL", urlReactome, 
        envir = as.environment(nameEnv))
    assign("BulkSignalR_Network_URL", urlNetwork, 
        envir = as.environment(nameEnv))

    createResources(onRequest = FALSE)

    BulkSignalR_Reactome <- getResource(resourceName = "Reactome",
        cache = TRUE)
    BulkSignalR_Gobp <- getResource(resourceName = "GO-BP",
        cache = TRUE)
    BulkSignalR_Network <- getResource(resourceName = "Network",
        cache = TRUE)

    assign("BulkSignalR_Reactome", BulkSignalR_Reactome, 
        envir = as.environment(nameEnv))
    assign("BulkSignalR_Gobp", BulkSignalR_Gobp, 
        envir = as.environment(nameEnv))
    assign("BulkSignalR_Network", BulkSignalR_Network, 
        envir = as.environment(nameEnv))
}

.onAttach <- function(libname, pkgname) {
    startupMsg <- r"{
    ################################
    ### Welcome to BulkSignalR ! ###
    ################################
}"

    packageStartupMessage(startupMsg)
}
