.onLoad <- function(...) {
    
    ############################
    ##        Database        ##
    ############################

    # handle directory creation over different OS
    # same as tools::R_user_dir("BulkSignalR", which="cache")
    cacheDir <- rappdirs::user_cache_dir("R/BulkSignalR")

    Sys.setenv("BulkSignalR_CACHEDIR" = cacheDir)
    
    urlDatabase <- "https://partage-dev.montp.inserm.fr:9192/CBSB/SignalR/database/SignalR.db"
    Sys.setenv("BulkSignalR_DB_URL" = urlDatabase)

    createDatabase(onRequest=FALSE)
    LRdb <- getInteractions()

    myEnv <- new.env(parent = emptyenv())#globalenv() #emptyenv() #baseenv()
    attach(myEnv, name="LRdbEnv")
    assign("LRdb", LRdb, envir = as.environment("LRdbEnv"))

    ################################
    ##   Resource Cache Files   ###  
    ################################

    urlGo       <- "https://partage-dev.montp.inserm.fr:9192/CBSB/SignalR/resources/gobp.rds"
    urlReactome <- "https://partage-dev.montp.inserm.fr:9192/CBSB/SignalR/resources/reactome.rds"
    #urlNetwork <- "https://partage-dev.montp.inserm.fr:9192/CBSB/SignalR/resources/PwC_ReactomeKEGG.rds"
    urlNetwork <- "https://partage-dev.montp.inserm.fr:9192/CBSB/SignalR/resources/Network.rds"

    Sys.setenv("BulkSignalR_GO_URL" = urlGo)
    Sys.setenv("BulkSignalR_Reactome_URL" = urlReactome)
    Sys.setenv("BulkSignalR_Network_URL" = urlNetwork)

    createResources(onRequest=FALSE)
    reactome <- getResource(resourceName="Reactome")
    gobp  <- getResource(resourceName="GO-BP")
    Network  <- getResource(resourceName="Network")

    assign("reactome", reactome, envir = as.environment("LRdbEnv"))
    assign("gobp", gobp, envir = as.environment("LRdbEnv"))
    assign("Network", Network, envir = as.environment("LRdbEnv"))


}

.onAttach <- function(libname, pkgname){

startupMsg <- r"{
 .----------------.  .----------------.  .----------------.  .----------------.                                                             
| .--------------. || .--------------. || .--------------. || .--------------. |                                                            
| |   ______     | || | _____  _____ | || |   _____      | || |  ___  ____   | |                                                            
| |  |_   _ \    | || ||_   _||_   _|| || |  |_   _|     | || | |_  ||_  _|  | |                                                            
| |    | |_) |   | || |  | |    | |  | || |    | |       | || |   | |_/ /    | |                                                            
| |    |  __'.   | || |  | '    ' |  | || |    | |   _   | || |   |  __'.    | |                                                            
| |   _| |__) |  | || |   \ `--' /   | || |   _| |__/ |  | || |  _| |  \ \_  | |                                                            
| |  |_______/   | || |    `.__.'    | || |  |________|  | || | |____||____| | |                                                            
| |              | || |              | || |              | || |              | |                                                            
| '--------------' || '--------------' || '--------------' || '--------------' |                                                            
 '----------------'  '----------------'  '----------------'  '----------------'                                                             
 .----------------.  .----------------.  .----------------.  .-----------------. .----------------.  .----------------.  .----------------. 
| .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. || .--------------. |
| |    _______   | || |     _____    | || |    ______    | || | ____  _____  | || |      __      | || |   _____      | || |  _______     | |
| |   /  ___  |  | || |    |_   _|   | || |  .' ___  |   | || ||_   \|_   _| | || |     /  \     | || |  |_   _|     | || | |_   __ \    | |
| |  |  (__ \_|  | || |      | |     | || | / .'   \_|   | || |  |   \ | |   | || |    / /\ \    | || |    | |       | || |   | |__) |   | |
| |   '.___`-.   | || |      | |     | || | | |    ____  | || |  | |\ \| |   | || |   / ____ \   | || |    | |   _   | || |   |  __ /    | |
| |  |`\____) |  | || |     _| |_    | || | \ `.___]  _| | || | _| |_\   |_  | || | _/ /    \ \_ | || |   _| |__/ |  | || |  _| |  \ \_  | |
| |  |_______.'  | || |    |_____|   | || |  `._____.'   | || ||_____|\____| | || ||____|  |____|| || |  |________|  | || | |____| |___| | |
| |              | || |              | || |              | || |              | || |              | || |              | || |              | |
| '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' || '--------------' |
 '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------'  '----------------' 
}"

packageStartupMessage(startupMsg)

}