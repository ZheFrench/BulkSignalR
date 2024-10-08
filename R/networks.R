#' Generate a ligand-receptor network
#'
#' Generate a ligand-receptor network from a ligand-receptor table.
#'
#' @param bsrinf        A BSRInference object.
#' @param pval.thres    P-value threshold.
#' @param qval.thres    Q-value threshold.
#' @param node.size     Default node size in the network.
#' @param red.pairs     A data frame with columns L (ligands) and R
#' (receptors) that restrict LR pairs to those listed.
#' @return An \code{igraph} object featuring the ligand-receptor network.
#' Default colors and node sizes are assigned,
#' which can be changed afterwards if necessary.
#' @import igraph
#' @export
#' @examples
#' print("getLRNetwork")
#' if(FALSE){
#' data(bsrinf.example, package = "BulkSignalR")
#' 
#' gLR <- getLRNetwork(bsrinf.example, qval.thres = 1e-4)
#' # plot(gLR)
#' # write.graph(gLR, file="SDC-LR-network.graphml", format="graphml")
#' }
#' @importFrom methods is
getLRNetwork <- function(bsrinf, pval.thres = NULL, qval.thres = NULL,
    node.size = 5, red.pairs = NULL) {
    if (!is(bsrinf, "BSRInference")) {
        stop("bsrinf must be a BSRInference object")
    }
    if (is.null(pval.thres) && is.null(qval.thres)) {
        stop("Either a P- or a Q-value threshold must be provided")
    }
    ipar <- infParam(bsrinf)
    if (ipar$ligand.reduced || ipar$receptor.reduced) {
        stop(
            "Does not work with inferences already reduced to the ",
            "ligands or the receptors"
        )
    }

    # get unique LR pairs with required statistical significance
    bsrinf <- reduceToBestPathway(bsrinf)
    pairs <- LRinter(bsrinf)
    if (!is.null(pval.thres)) {
        pairs <- pairs[pairs$pval <= pval.thres, ]
    } else {
        pairs <- pairs[pairs$qval <= qval.thres, ]
    }
    # different pathways for the same LR pair can have the same P-value
    ref <- paste(pairs$L, pairs$R, sep = "||")
    dup <- duplicated(ref)
    pairs <- pairs[!dup, ]

    # LR pairs restricted to a provided list
    if (!is.null(red.pairs)) {
        ref <- paste(pairs$L, pairs$R, sep = "||")
        restricted <- paste(red.pairs$L, red.pairs$R, sep = "||")
        pairs <- pairs[ref %in% restricted, ]
    }

    # generate the igraph object
    g <- igraph::graph_from_data_frame(pairs[, c("L", "R")], directed = TRUE)
    g.names <- igraph::vertex_attr(g, "name")
    g <- igraph::set_vertex_attr(g, name = "size", value = node.size)
    g <- igraph::set_vertex_attr(g, name = "label", value = g.names)
    g.types <- stats::setNames(rep("ligand", length(g.names)), g.names)
    g.types[g.names %in% pairs$R] <- "receptor"
    g <- igraph::set_vertex_attr(g, name = "node.type",
        value = g.types[g.names])
    g.colors <- rep("green", length(g.names))
    g.colors[g.names %in% pairs$R] <- "red"
    g <- igraph::set_vertex_attr(g, name = "color", value = g.colors)
    g <- igraph::set_edge_attr(g, name = "edge.type", value = "LR")
    el <- igraph::as_edgelist(g)
    pval <- NULL
    qval <- NULL
    corr <- NULL
    for (i in seq_len(nrow(el))) {
        j <- which(pairs$L == el[i, 1] & pairs$R == el[i, 2])
        pval <- c(pval, pairs$pval[j])
        qval <- c(qval, pairs$qval[j])
        corr <- c(corr, pairs$LR.corr[j])
    }
    g <- igraph::set_edge_attr(g, name = "corr", value = corr)
    g <- igraph::set_edge_attr(g, name = "pval", value = pval)
    g <- igraph::set_edge_attr(g, name = "log10.pval", value = -log10(pval))
    g <- igraph::set_edge_attr(g, name = "qval", value = qval)
    g <- igraph::set_edge_attr(g, name = "log10.qval", value = -log10(qval))
} # getLRNetwork


#' Internal function to generate a ligand-receptor-downstream signaling network
#'
#' @param pairs     A ligand-receptor table such as output by
#' \code{LRinter(BSRInference)}.
#' @param pw              A table defining the reference pathways.
#' @param t.genes    Target gene list such as output by
#' \code{tGenes(BSRInference)}.
#' @param tg.corr    Target gene correlation list (with the receptor) such as
#' output by \code{tgCorr(BSRInference)}.
#' @param id.col Column index or name in \code{pw} for the pathway IDs.
#' @param gene.col Column index or name in \code{pw} for the gene symbols.
#' @param min.cor Minimum correlation required for the target genes.
#' @param tg.pval Target gene P-value list such as returned by
#' \code{ tgPval(BSRInferenceComp)}.
#' @param max.pval     Maximum (regulation) P-value for target genes in case
#' a BSRInferenceComp object is used 
#' to generate the network.
#' @param min.logFC     Minimum logFC required for the target genes in case
#' a BSRInferenceComp object is used to generate the network.
#' @param pos.targets   A logical imposing that all the network targets must
#' display positive correlation or logFC a BSRInferenceComp object is used to
#' generate the network.
#' @param neg.targets   A logical imposing that all the network targets must
#' display negative correlation or logFC a BSRInferenceComp object is used to
#' generate the network. Correlations must be
#' <= -min.cor or logFC <= - min.logFC
#' with this option activated.
#'
#' @return An \code{igraph} object featuring the ligand-receptor-downstream
#' signaling network. Default colors and node sizes are assigned. In case
#' the \code{max.pval} parameter is set, it is assumed that \code{tg.pval}
#' is set as well and downstream signaling genes are selected by their
#' P-values in the comparison of clusters of samples.
#'
#' @importFrom foreach %do% %dopar%
#' @keywords internal
.edgesLRIntracell <- function(pairs, pw, t.genes, tg.corr, id.col, gene.col,
    min.cor = 0.25, pos.targets = FALSE,
    neg.targets = FALSE, tg.pval = NULL,
    max.pval = NULL, tg.logFC = NULL, min.logFC = 0) {
    # local binding
    i <- NULL
    
    directed.int <- c(
        "controls-transport-of",
        "catalysis-precedes",
        "positively-controls-expression-of",
        "negatively-controls-expression-of",
        "regulates-transcription-of",
        "controls-dephospho-of",
        "controls-phospho-of",
        "regulates-dephospho-of",
        "regulates-phospho-of",
        "controls-state-change-of",
        "controls-state-of-by-metab"
    )


    arcs <- foreach::foreach(i = seq_len(nrow(pairs)), .combine = rbind) %do% {
        # arcs <- NULL
        # for (i in 1:nrow(pairs)) {
        r <- pairs$R[i]
        p <- pairs$pw.id[i]
        tg <- t.genes[[i]]
        if (is.null(max.pval) && is.null(min.logFC)) {
            # selection on correlations
            corr <- tg.corr[[i]]
            if (pos.targets) {
                targets <- tg[corr >= min.cor]
            } else if (neg.targets) {
                targets <- tg[corr <= -min.cor]
            } else {
                targets <- tg[abs(corr) >= min.cor]
            }
        } else {
            # selection on P-values and logFC
            pval <- tg.pval[[i]]
            logFC <- tg.logFC[[i]]
            if (pos.targets) {
                targets <- tg[pval <= max.pval & logFC >= min.logFC]
            } else if (neg.targets) {
                targets <- tg[pval <= max.pval & logFC <= -min.logFC]
            } else {
                targets <- tg[pval <= max.pval & abs(logFC) >= min.logFC]
            }
        }

        # build a hybrid directed/undirected graph
        a.iter <- data.frame(
            from = pairs$L[i], to = r, edge.type = "LR",
            stringsAsFactors = FALSE
        )
        genes.in.pw <- pw[pw[[id.col]] == p, gene.col]
        int <- BulkSignalR_Network[
            BulkSignalR_Network$a.gn %in% genes.in.pw &
                BulkSignalR_Network$b.gn %in% genes.in.pw,
        ]
        directed <- int$type %in% directed.int
        ret <- int[!directed, c("b.gn", "a.gn")]
        names(ret) <- c("a.gn", "b.gn")
        d.int <- unique(rbind(int[, c("a.gn", "b.gn")], ret))
        g <- igraph::graph_from_data_frame(d.int, directed = TRUE)
        targets <- intersect(targets, c(d.int$a.gn, d.int$b.gn))

        # keep shortest paths from the receptor to the targets only
        if ((r %in% d.int$a.gn || r %in% d.int$b.gn) && length(targets) > 0) {
            paths <- try(igraph::shortest_paths(g,
                from = igraph::V(g)[igraph::V(g)$name == r],
                to = igraph::V(g)[igraph::V(g)$name %in% targets],
                output = "vpath"
            ), silent = TRUE)
            if (inherits(paths, "try-error")) {
                paths <- NULL
            }
            if (is.null(paths)){
                stop("No directed graph found by igraph")
            }
            if (length(paths$vpath) > 0) {
                for (j in seq_len(length(paths$vpath))) {
                    vertices <- paths$vpath[[j]]
                    if (length(vertices) > 0) {
                        for (k in 2:length(vertices)) {
                            from <- igraph::V(g)$name[vertices[k - 1]]
                            to <- igraph::V(g)$name[vertices[k]]
                            edge.type <- BulkSignalR_Network[
                                BulkSignalR_Network$a.gn == from &
                                    BulkSignalR_Network$b.gn == to,
                                "type"
                            ][1]
                            a.iter <- rbind(a.iter, data.frame(
                                from = from, to = to,
                                edge.type = edge.type,
                                stringsAsFactors = FALSE
                            ))
                        }
                    }
                }
            }
        }
        # arcs <- rbind(arcs, unique(a.iter))
        unique(a.iter)
    }

    unique(arcs)
} # .edgesLRIntracell


#' Generate a ligand-receptor-downstream signaling network
#'
#' Generate a ligand-receptor network from a BSRInference object and add
#' the shortest paths from the receptors to correlated
#' target genes following Reactome and KEGG pathways.
#'
#' @param bsrinf        A BSRInference or BSRInference Comp object.
#' @param pval.thres    P-value LR interaction threshold.
#' @param qval.thres    Q-value LR interaction threshold.
#' @param min.cor       Minimum correlation required for the target genes.
#' @param max.pval      Maximum P-value required for the target genes in case
#'   a BSRInferenceComp object is provided.
#' @param min.logFC     Minimum logFC required for the target genes in case
#'   a BSRInferenceComp object is provided.
#' @param pos.targets   A logical imposing that all the network targets must
#'   display positive correlation or logFC in case of a BSRInferenceComp object.
#' @param neg.targets   A logical imposing that all the network targets must
#'   display negative correlation or logFC in case of a BSRInferenceComp object.
#'   Correlations must be <= -min.cor or logFC <= - min.logFC
#'   with this option activated.
#' @param restrict.pw   A vector of pathway IDs to which receptor downstream
#' signaling is restricted.
#' @param node.size     Default node size in the network.
#' @return An \code{igraph} object featuring the ligand-receptor-downstream
#' signaling network. Default colors and node sizes are assigned,
#' which can be changed afterwards if necessary.
#'
#' The target genes to which the \code{min.cor} correlation is imposed are
#' those listed in \code{tGenes(bsrinf)}, correlations are in
#' \code{tgCorr(bsrinf)}.
#' The construction of shortest paths from the receptors to those selected
#' targets adds other genes, which were either some targets with too low
#' correlation or genes along the shortest paths to reach the selected targets.
#' @import igraph
#' @importFrom methods is
#' @export
#' @examples
#' print("getLRIntracellNetwork")
#' if(FALSE){
#' data(bsrinf.example, package = "BulkSignalR")
#' 
#' bsrinf.redBP <- reduceToBestPathway(bsrinf.example)
#'
#' pairs <- LRinter(bsrinf.redBP)
#' top <- unique(pairs[pairs$pval < 1e-20, c("pw.id", "pw.name")])
#'
#' gLRintra.res <- getLRIntracellNetwork(bsrinf.redBP,
#' qval.thres = 0.01,
#' restrict.pw = top$pw.id
#' )
#'
#' # write.graph(gLRintra, file="SDC-LR-intracellular-network.reduced.graphml",
#' # format="graphml")
#' }
getLRIntracellNetwork <- function(bsrinf, pval.thres = NULL, qval.thres = NULL,
    min.cor = 0.25, max.pval = NULL, min.logFC = NULL,
    pos.targets = FALSE, neg.targets = FALSE,
    restrict.pw = NULL, node.size = 5) {
    comp.obj <- is(bsrinf, "BSRInferenceComp")
    if (!is(bsrinf, "BSRInference") && !comp.obj) {
        stop("bsrinf must be a BSRInference or BSRInferenceComp object")
    }
    if (!is.null(max.pval) && !comp.obj) {
        stop("max.pval can only be specified for a BSRInferenceComp object")
    }
    if (!is.null(min.logFC) && !comp.obj) {
        stop("min.logFC can only be specified for a BSRInferenceComp object")
    }
    if (is.null(pval.thres) && is.null(qval.thres)) {
        stop("Either a P- or a Q-value threshold must be provided")
    }
    ipar <- infParam(bsrinf)
    if (ipar$ligand.reduced || ipar$receptor.reduced) {
        stop(
            "Does not work with inferences already reduced to the ",
            "ligands or the receptors"
        )
    }
    if (neg.targets && pos.targets) {
        stop("neg.targets and pos.targets cannot be TRUE simultaneously")
    }

    # get unique LR pairs with required statistical significance
    pairs <- LRinter(bsrinf)
    t.genes <- tGenes(bsrinf)
    tg.corr <- tgCorr(bsrinf)
    if (!is.null(pval.thres)) {
        good <- pairs$pval <= pval.thres
    } else {
        good <- pairs$qval <= qval.thres
    }
    pairs <- pairs[good, ]
    t.genes <- t.genes[good]
    tg.corr <- tg.corr[good]
    if (comp.obj) {
        tg.pval <- tgPval(bsrinf)[good]
        tg.logFC <- tgLogFC(bsrinf)[good]
    }

    pool <- unique(c(pairs$L, pairs$R))
    all.edges <- NULL

    # reactome pathways
    i.react <- grep("^R-", pairs$pw.id)
    pairs.react <- pairs[i.react, ]
    t.genes.react <- t.genes[i.react]
    tg.corr.react <- tg.corr[i.react]
    if (comp.obj) {
        tg.pval.react <- tg.pval[i.react]
        tg.logFC.react <- tg.logFC[i.react]
    }
    if (!is.null(restrict.pw)) {
        i.react <- which(pairs.react$pw.id %in% restrict.pw)
        pairs.react <- pairs.react[i.react, ]
        t.genes.react <- t.genes.react[i.react]
        tg.corr.react <- tg.corr.react[i.react]
        if (comp.obj) {
            tg.pval.react <- tg.pval.react[i.react]
            tg.logFC.react <- tg.logFC.react[i.react]
        }
    }
    if (nrow(pairs.react) > 0) {
        ids <- unique(BulkSignalR_Reactome[
            BulkSignalR_Reactome$`Gene name` %in% pool, "Reactome ID"])
        react <- BulkSignalR_Reactome[
            BulkSignalR_Reactome$`Reactome ID` %in% ids, ]
        if (!is.null(restrict.pw)) {
            react <- BulkSignalR_Reactome[
            BulkSignalR_Reactome$`Reactome ID` %in% restrict.pw, ]
        }
        all.edges <- .edgesLRIntracell(
            pairs.react, react, t.genes.react,
            tg.corr.react, "Reactome ID", "Gene name", min.cor,
            pos.targets, neg.targets, tg.pval.react, max.pval,
            tg.logFC.react, min.logFC
        )
    }

    # GOBP
    i.go <- grep("^GO:", pairs$pw.id)
    pairs.go <- pairs[i.go, ]
    t.genes.go <- t.genes[i.go]
    tg.corr.go <- tg.corr[i.go]
    if (comp.obj) {
        tg.pval.go <- tg.pval[i.go]
        tg.logFC.go <- tg.logFC[i.go]
    }
    if (!is.null(restrict.pw)) {
        i.go <- which(pairs.go$pw.id %in% restrict.pw)
        pairs.go <- pairs.go[i.go, ]
        t.genes.go <- t.genes.go[i.go]
        tg.corr.go <- tg.corr.go[i.go]
        if (comp.obj) {
            tg.pval.go <- tg.pval.go[i.go]
            tg.logFC.go <- tg.logFC.go[i.go]
        }
    }
    if (nrow(pairs.go) > 0) {
        ids <- unique(BulkSignalR_Gobp[
            BulkSignalR_Gobp$`Gene name` %in% pool, "GO ID"])
        go <- BulkSignalR_Gobp[
        BulkSignalR_Gobp$`GO ID` %in% ids, ]
        if (!is.null(restrict.pw)) {
            go <- go[go$`GO ID` %in% restrict.pw, ]
        }
        all.edges <- unique(rbind(
            all.edges,
            .edgesLRIntracell(
                pairs.go, go, t.genes.go,
                tg.corr.go, "GO ID", "Gene name", min.cor,
                pos.targets, neg.targets, tg.pval.go, max.pval,
                tg.logFC.go, min.logFC
            )
        ))
    }

    # generate igraph object -------------------

    # some LR interactions are also given as in-complex-with interactions,
    # get rid of them
    ref <- paste(all.edges[[1]], all.edges[[2]], sep = "||")
    dup <- ref[duplicated(ref)]
    all.edges <- all.edges[!(ref %in% dup) | 
    (ref %in% dup & all.edges$edge.type == "LR"), ]
    ref <- paste(all.edges[[1]], all.edges[[2]], sep = "||")
    duppl <- duplicated(ref)
    if (sum(duppl) > 0) {
        all.edges <- all.edges[!duppl, ]
    } # some duplicates remain, eliminate

    # build graph
    directed.int <- c(
        "controls-transport-of",
        "catalysis-precedes",
        "positively-controls-expression-of",
        "negatively-controls-expression-of",
        "regulates-transcription-of",
        "controls-dephospho-of",
        "controls-phospho-of",
        "regulates-dephospho-of",
        "regulates-phospho-of",
        "controls-state-change-of",
        "controls-state-of-by-metabo",
        "LR"
    )

    directed <- all.edges$edge.type %in% directed.int
    ret <- all.edges[!directed, ]
    from <- ret$from
    ret$from <- ret$to
    ret$to <- from
    d.int <- unique(rbind(all.edges, ret))
    g <- igraph::graph_from_data_frame(d.int, directed = TRUE)

    g.names <- igraph::vertex_attr(g, "name")
    g <- igraph::set_vertex_attr(g, name = "size", value = node.size)
    g <- igraph::set_vertex_attr(g, name = "label", value = g.names)
    g.types <- stats::setNames(rep("downstream.gene", length(g.names)), g.names)
    g.types[g.names %in% pairs$R] <- "receptor"
    g.types[g.names %in% pairs$L] <- "ligand"
    g <- igraph::set_vertex_attr(g, name = "node.type",
        value = g.types[g.names])
    g.colors <- rep("gray80", length(g.names))
    g.colors[g.names %in% pairs$R] <- "red"
    g.colors[g.names %in% pairs$L] <- "green"
    g <- igraph::set_vertex_attr(g, name = "color", value = g.colors)
} # getLRIntracellNetwork
