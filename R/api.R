#' Retrieve LR complexes
#'
#' Fetch LR complexes from database and
#' and return a dataframe
#'
#' @param idRelease integer id version Release
#' Default is NULL so last version is selected.
#'
#' @import DBI RSQLite BiocFileCache
#' @importFrom cli cli_alert_danger cli_alert cli_abort
#' @importFrom dplyr left_join group_by mutate distinct
#' @export
#' @examples
#' print("getComplexes")
getComplexes <- function(idRelease=NULL) { 


    cacheDir <-  get("BulkSignalR_CACHEDIR")
    url      <-  get("BulkSignalR_DB_URL")

    name <- stoichiometry <- pool.stoichiometry <- NULL

    # Retrieve database from cache
    cacheDir <- paste(cacheDir,"database",sep="/")
    bfc <- BiocFileCache::BiocFileCache(cacheDir,ask = FALSE)
    databaseFilePath <- BiocFileCache::bfcrpath(bfc, rnames=basename(url))

    BulkSignalRCon <- DBI::dbConnect(RSQLite::SQLite(), databaseFilePath)
    if(is.null(idRelease))
        release <- DBI::dbGetQuery(BulkSignalRCon, 'SELECT id FROM Release ORDER BY id DESC LIMIT 1')
 
    else {
        release <- DBI::dbGetQuery(BulkSignalRCon, 'SELECT id FROM Release WHERE id = ?',
        , params = list(idRelease))
    }
 
    if(nrow(release)==0)
        cat(cli::cli_abort("ID Release {idRelease} doesn't exist.","\n"))

     complexes <-  DBI::dbGetQuery(BulkSignalRCon, 'SELECT 
      Clex.name,
      Clex.description,
      Clex.size,
      CC.stoichiometry,
      Clex.sources,
      Clex.pmids 
      FROM Complex as Clex  
      inner join Component as Comp
      inner join Complex_Component as CC
      on Comp.id = CC."id.component_fk" AND  Clex.id = CC."id.complex_fk" 
      where Comp."id.release_fk" = ?;' ,release$id);

    if(nrow(complexes)==0)
        cat(cli::cli_alert("No complexes found for ID Release {release$id}.","\n"))


    complexes <- complexes %>% 
    dplyr::group_by(name) %>% 
    dplyr::mutate(pool.stoichiometry = paste0(stoichiometry, collapse = ",")) %>% 
    dplyr::distinct(name, pool.stoichiometry, .keep_all = TRUE)

    complexes$stoichiometry <- NULL
    colnames(complexes)[which(colnames(complexes)=="pool.stoichiometry")] <- 'stoichiometry'
    complexes <- as.data.frame(complexes[,c("name","description","size","stoichiometry","sources","pmids")])

    DBI::dbDisconnect(BulkSignalRCon)

    return(invisible(complexes))

}

#' Retrieve LR interactions.
#'
#' Fetch LR interactions from database and
#' and return a dataframe
#'
#' @param idRelease integer id version Release
#' Default is NULL so last version is selected.
#'
#' @import DBI RSQLite BiocFileCache 
#' @importFrom cli cli_alert_danger cli_alert cli_abort
#' @importFrom dplyr left_join
#' @export
#' @examples
#' print("getInteractions")
#' getInteractions()
getInteractions <- function(idRelease=NULL) { 


    cacheDir <- get("BulkSignalR_CACHEDIR")
    url      <- get("BulkSignalR_DB_URL")

    # Retrieve database from cache
    cacheDir <- paste(cacheDir,"database",sep="/")
    bfc <- BiocFileCache::BiocFileCache(cacheDir,ask = FALSE)
    databaseFilePath <- BiocFileCache::bfcrpath(bfc, rnames=basename(url))

    BulkSignalRCon <- DBI::dbConnect(RSQLite::SQLite(), databaseFilePath)

    if(is.null(idRelease))
        release <- DBI::dbGetQuery(BulkSignalRCon, 'SELECT id FROM Release ORDER BY id DESC LIMIT 1')
 
    else {
        release <- DBI::dbGetQuery(BulkSignalRCon, 'SELECT id FROM Release WHERE id = ?',
        , params = list(idRelease))
    }
 
    if(nrow(release)==0)
        cat(cli::cli_abort("ID Release {idRelease} doesn't exist.","\n"))

  pairsReference <- DBI::dbGetQuery(BulkSignalRCon, 
  'SELECT 
    Inter."id.ligand_fk", 
    Inter."id.receptor_fk",
    Inter."sources",
    Inter."pmids"
    FROM Interaction as Inter 
    inner join Component as Comp 
    on Comp."id" = Inter."id.receptor_fk" 
    where Comp."id.release_fk" = ?'
    ,params = list(release$id))


    receptors <-  DBI::dbGetQuery(BulkSignalRCon, 
    'SELECT 
      DISTINCT(Comp."id"),
      Comp."name", 
      Comp."type", 
      Comp."description"
      FROM Component as Comp 
      inner join Interaction as Inter
      on Comp."id" = Inter."id.receptor_fk"  
      where Comp."id.release_fk" = ?'
      ,params = list(release$id))

    ligands <-  DBI::dbGetQuery(BulkSignalRCon, 
    'SELECT 
      DISTINCT(Comp."id"),
      Comp."name", 
      Comp."type", 
      Comp."description"
      FROM Component as Comp 
      inner join Interaction as Inter
      on Comp."id" = Inter."id.ligand_fk" 
      WHERE Comp."id.release_fk" = ?'
      ,params = list(release$id))
 

    colnames(ligands)[which(colnames(ligands)=="description")] <- "d.ligand"
    colnames(receptors)[which(colnames(receptors)=="description")] <- "d.receptor"

    # id   name  description
    colnames(receptors)[1] <- "id.receptor_fk"
    colnames(ligands)[1] <- "id.ligand_fk"

    colnames(receptors)[2] <- "receptor"
    colnames(ligands)[2]   <- "ligand"

    pairsReference <- dplyr::left_join(pairsReference,receptors,by ="id.receptor_fk")
    pairsReference <- dplyr::left_join(pairsReference,ligands,by ="id.ligand_fk")

    pairsReference <- pairsReference[,c("ligand","receptor","d.ligand","d.receptor","sources","pmids")]

    DBI::dbDisconnect(BulkSignalRCon)

    return(invisible(pairsReference))
}
