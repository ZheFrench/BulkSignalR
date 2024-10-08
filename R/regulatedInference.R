#' Get regulated ligand-receptor pairs.
#'
#' Internal function to return all the pairs of ligands and receptors
#' having both a P-value below a given threshold.
#'
#' @param ds              A BSRDataModel object
#' @param cc              A BSRClusterComp object.
#' @param scc             An optional BSRClusterComp object in case ligand
#'   regulation was assessed in a separate cluster comparison.
#' @param max.pval        The maximum P-value imposed to both the ligand
#' and the receptor.
#' @param min.logFC       The maximum log2 fold-change allowed for
#'   both the receptor and the ligand.
#' @param neg.receptors     A logical indicating whether receptors are only
#'   allowed to be upregulated (FALSE), or up- and downregulated (TRUE).
#' @param restrict.genes  A list of gene symbols that restricts ligands and
#'   receptors.
#'
#' @return A data frame containing putative ligand-receptor pairs along
#'   with the product of their respective P-values. This table is the first step
#'   of a ligand-receptor analysis.
#'
#' @details The \code{restrict.genes} parameter is used for special cases where
#'   LRdb must be further restricted to a subset.
#'   The putative ligand-receptor pairs has 6 columns : R, L, LR.pval, corr,
#'   L.logFC, and R.logFC. 
#'   Note that correlations are currently set to 1 to avoid
#'   lengthy computations with scRNA-seq data and multiple cell
#'   populations.
#'
#' @importFrom methods is
#' @importFrom foreach %do% %dopar%
#' @import doParallel
#' @keywords internal
.getRegulatedLR <- function(ds, cc, scc = NULL, max.pval = 0.01, min.logFC = 1,
                            neg.receptors = FALSE, restrict.genes = NULL) {
    # local binding
    i <- NULL

    if ((max.pval <= 0) || (max.pval > 1)) {
        stop("max.pval must lie in ]0;1]")
    }
    if (min.logFC <= 0) {
        stop("min.logFC must be > 0")
    }
    if (!is(ds, "BSRDataModelComp")) {
        stop("ds must be an object of class BSRDataModelComp")
    }
    if (!is(cc, "BSRClusterComp")) {
        stop("cc must be an object of class BSRClusterComp")
    }
    if (!is.null(scc) && !is(scc, "BSRClusterComp")) {
        stop("scc must be an object of class BSRClusterComp")
    }

    lrgenes <- intersect(c(
        BulkSignalR_LRdb$ligand,
        BulkSignalR_LRdb$receptor
    ), rownames(stats(cc)))
    if (!is.null(restrict.genes)) {
        lrgenes <- intersect(lrgenes, restrict.genes)
    }

    # # compute all the correlations at once
    # if (is.null(scc)){
    #   corlr <- stats::cor(t(ncounts(ds)[lrgenes,
    # c(colA(cc),colB(cc))]), method = "spearman")
    # }
    # else {
    #   corlr <- stats::cor(t(ncounts(ds)[lrgenes,
    # c(colA(cc),colA(scc))]), method = "spearman")
    # }
    # get the pairs
    R.stats <- stats(cc)
    if (is.null(scc)) {
        L.stats <- stats(cc)
    } else {
        L.stats <- stats(scc)
    }
    pairs <- NULL
    for (i in seq_len(nrow(BulkSignalR_LRdb))) {
        L <- BulkSignalR_LRdb$ligand[i]
        R <- BulkSignalR_LRdb$receptor[i]
        if (L %in% lrgenes && R %in% lrgenes) {
            pL <- L.stats[L, "pval"]
            pR <- R.stats[R, "pval"]
            if (pL <= max.pval && pR <= max.pval) {
                fcL <- L.stats[L, "logFC"]
                fcR <- R.stats[R, "logFC"]
                if (fcL >= min.logFC &&
                    ((neg.receptors && abs(fcR) >= min.logFC) || 
                        fcR >= min.logFC)) {
                    pairs <- rbind(
                        pairs,
                        data.frame(
                            L = L, R = R,
                            LR.pval = pL * pR, corr = 1, # corlr[L, R],
                            L.logFC = fcL, R.logFC = fcR,
                            stringsAsFactors = FALSE
                        )
                    )
                }
            }
        }
    }

    if (is.null(pairs)) {
        stop("Dataframe `pairs` from `.getRegulatedLR` is NULL.")
    }

    pairs
} # .getRegulatedLR


#' Internal function to check receptor signaling downstream
#'
#' @param lr              A data frame as returned by
#'   \code{.getRegulatedLR()}.
#' @param pw              A table defining the reference pathways.
#' @param pw.size         A named vector with pathway sizes (names are pathway
#'   IDs).
#' @param rncounts        A matrix of normalized read counts with at
#'   least all the ligands, receptors, and genes in the reference pathways.
#' @param stats           A data.frame with a column 'pval' and
#' \code{rownames(stats)} assigned to gene symbols. Rows must at least
#'   include all the ligands, receptors, and genes in the reference pathways.
#' @param id.col          Column index or name in \code{pw} for the pathway IDs.
#' @param gene.col        Column index or name in \code{pw} for the gene
#'   symbols.
#' @param pw.col          Column index or name in \code{pw} for the pathway
#'   names.
#' @param min.positive    Minimum number of target genes to be found in a given
#'   pathway.
#' @param min.t.logFC     The minimum log2 fold-change allowed for
#'   targets in case pos.targets or neg.targets are used.
#' @param pos.targets   A logical imposing that all the network targets must
#'   display positive logFC, i.e. logFC >= min.t.logFC.
#' @param neg.targets   A logical imposing that all the network targets must
#'   display negative logFC, i.e. logFC <= - min.t.logFC.
#' @param with.complex    A logical indicating whether receptor co-complex
#'   members should be included in the target genes.
#'
#' @return A table reporting all the ligand-receptor pairs provided in \code{lr}
#'   along with the pathways found and data about target gene regulation
#'   P-values. Note that correlations are currently set to 1 to avoid
#'   lengthy computations with scRNA-seq data and multiple cell
#'   populations.
#'
#' @importFrom foreach %do% %dopar%
#' @keywords internal
.downstreamRegulatedSignaling <- function(lr, pw, pw.size, rncounts, stats,
    id.col, gene.col, pw.col,
    min.positive, pos.targets = FALSE,
    neg.targets = FALSE, min.t.logFC = 0.5,
    with.complex = TRUE) {
    if (!is.data.frame(stats)) {
        stop("stats must be a data.frame")
    }
    if (!("pval" %in% names(stats))) {
        stop("stats must contain a column named 'pval'")
    }
    if (neg.targets && pos.targets) {
        stop("neg.targets and pos.targets cannot be TRUE simultaneously")
    }
    if (min.t.logFC <= 0) {
        stop("min.t.logFC must be >0")
    }

    # local binding
    r <- p <- pl <- id <- NULL

    # define interaction types
    control.int <- c(
        "positively-controls-expression-of",
        "negatively-controls-expression-of",
        "regulates-transcription-of"
    )

    incomplex.int <- c("in-complex-with", "interacts-with")

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

    if (with.complex) {
        correlated.int <- union(control.int, incomplex.int)
    } else {
        correlated.int <- control.int
    }

    # # compute downstream correlations
    # corrg <- stats::cor(t(rncounts), method = "spearman")
    #
    # # the global computation above is faster than restricted to the receptors
    # corrg <- corrg[unique(lr$R), ]

    # check each putative LR pair, loop over the receptors
    reg.proc <- foreach::foreach(r = unique(lr$R), .combine = rbind) %do% {
        # reg.proc <- NULL
        # for (r in unique(lr$putative.pairs$R)){

        # loop over the pathways containing the receptor r
        pa <- intersect(pw[pw[[gene.col]] == r, id.col], names(pw.size))
        if (length(pa) > 0) {
            receptor.ligands <- unique(lr$L[lr$R == r])
            best.2nd <- foreach::foreach(p = pa, .combine = rbind) %do% {
                # best.2nd <- NULL
                # for (p in pa){
                int <- BulkSignalR_Network[
                    BulkSignalR_Network$a.gn %in% 
                    pw[pw[[id.col]] == p, gene.col] &
                        BulkSignalR_Network$b.gn %in% 
                        pw[pw[[id.col]] == p, gene.col],
                ]

                # extract the target genes of receptor r
                if (r %in% int$a.gn || r %in% int$b.gn) {
                    # double the undirected interactions 
                    # and generate a directed graph
                    directed <- int$type %in% directed.int
                    ret <- int[!directed, c("a.gn", "b.gn")]
                    from <- ret$a.gn
                    ret$a.gn <- ret$b.gn
                    ret$b.gn <- from
                    d.int <- unique(rbind(int[, c("a.gn", "b.gn")], ret))
                    g <- igraph::graph_from_data_frame(d.int, directed = TRUE)

                    # putative targets in the pathway
                    target.genes <- setdiff(
                        c(
                            int[int$type %in% correlated.int & 
                            int$a.gn == r, "b.gn"],
                            int[int$type %in% correlated.int &
                            int$b.gn == r, "a.gn"],
                            int[int$type %in% directed.int, "b.gn"]
                        ),
                        r
                    )

                    # reduce putative to reachable from the receptor
                    sp <- igraph::distances(g, r, target.genes)
                    target.genes <- colnames(sp)[!is.infinite(sp[r, ])]

                    # eliminate ligands of the receptor if present
                    # and intersect with detected genes in case use.full.network
                    # parameter was set to TRUE in 
                    # .checkRegulatedReceptorSignaling
                    target.genes <- intersect(
                        rownames(stats),
                        setdiff(target.genes, receptor.ligands)
                    )

                    # check fold-change direction if required
                    if (pos.targets || neg.targets) {
                        lfc <- stats[target.genes, "logFC"]
                        if (pos.targets) {
                            target.genes <- target.genes[lfc >= min.t.logFC]
                        } else {
                            target.genes <- target.genes[lfc <= -min.t.logFC]
                        }
                    }

                    if (length(target.genes) >= min.positive) {
                        # if all conditions are met, list all target genes with
                        # their regulation P-values in a data frame
                        # row. Target genes are sorted wrt P-values
                        # in decreasing order to
                        # keep the compatibility with correlation analysis,
                        # where the most significant values are at the end.
                        pv <- stats[target.genes, "pval"]
                        o <- order(pv, decreasing = TRUE)
                        pv <- pv[o]
                        lfc <- stats[target.genes, "logFC"]
                        lfc <- lfc[o]
                        expr <- stats[target.genes, "expr"]
                        expr <- expr[o]
                        target.genes <- target.genes[o]
                        c <- rep(1, length(target.genes))
                        data.frame(
                            pathway = p, 
                            target.pval = paste(pv, collapse = ";"),
                            target.genes = paste(target.genes, collapse = ";"),
                            target.corr = paste(c, collapse = ";"),
                            target.logFC = paste(lfc, collapse = ";"),
                            target.expr = paste(expr, collapse = ";"),
                            len = length(c), stringsAsFactors = FALSE
                        )
                    } else {
                        NULL
                    }
                } else {
                    NULL
                }
            }
            if (!is.null(best.2nd)) {
                # one or several pathways containing the receptor r were found,
                # combine them in |-separated strings
                data.frame(
                    R = r, pathways = paste(best.2nd$pathway, collapse = "|"),
                    target.pval = paste(best.2nd$target.pval, collapse = "|"),
                    target.genes = paste(best.2nd$target.genes, collapse = "|"),
                    target.corr = paste(best.2nd$target.corr, collapse = "|"),
                    target.logFC = paste(best.2nd$target.logFC, collapse = "|"),
                    target.expr = paste(best.2nd$target.expr, collapse = "|"),
                    len = paste(best.2nd$len, collapse = "|"),
                    stringsAsFactors = FALSE
                )
            } else {
                NULL
            }
        } else {
            NULL
        }
    }

    # combine LR pair correlations with R-target gene correlations
    rownames(reg.proc) <- reg.proc$R
    conf.pairs <- lr[lr$R %in% reg.proc$R, ]
    conf.pairs$pwid <- reg.proc[conf.pairs$R, "pathways"]
    conf.pairs$target.pval <- reg.proc[conf.pairs$R, "target.pval"]
    conf.pairs$len <- reg.proc[conf.pairs$R, "len"]
    conf.pairs$target.genes <- reg.proc[conf.pairs$R, "target.genes"]
    conf.pairs$target.corr <- reg.proc[conf.pairs$R, "target.corr"]
    conf.pairs$target.logFC <- reg.proc[conf.pairs$R, "target.logFC"]
    conf.pairs$target.expr <- reg.proc[conf.pairs$R, "target.expr"]
    pw.name <- unique(pw[, c(id.col, pw.col)])
    pw2name <- stats::setNames(pw.name[[2]], pw.name[[1]])
    conf.pairs$pwname <- foreach::foreach(pl = conf.pairs$pwid,
        .combine = c) %do% {
        paste(
            foreach::foreach(
                id = unlist(strsplit(pl, "\\|")),
                .combine = c
            ) %do% {
                pw2name[id]
            },
            collapse = "|"
        )
    }

    conf.pairs[, c(
        "L", "R", "LR.pval", "corr", "L.logFC", "R.logFC",
        "pwid", "pwname", "len", "target.genes",
        "target.pval", "target.logFC", "target.corr",
        "target.expr"
    )]
} # .downstreamRegulatedSignaling


#' Internal function to check receptor signaling downstream
#'
#' Assess the existence of concomitant regulations between a receptor,
#' part of a ligand-receptor pair, and
#' genes coding for proteins forming a complex with the receptor or genes
#' regulated by the receptor downstream signaling.
#'
#' @param ds              A BSRDataModel object
#' @param cc              A BSRClusterComp object.
#' @param lr              A table as returned by \code{.getRegulatedLR()}.
#' @param ds              An optional BSRDataModel object.
#' @param reference       Which pathway reference should be used ("REACTOME"
#'   for Reactome, "GOBP" for GO Biological Process,
#'   or "REACTOME-GOBP" for both).
#' @param max.pw.size     Maximum pathway size to consider from the pathway
#'   reference.
#' @param min.pw.size     Minimum pathway size to consider from the pathway
#'   reference.
#' @param min.positive    Minimum number of target genes to be found in a given
#'   pathway.
#' @param restrict.pw     A list of pathway IDs to restrict the application of
#'   the function.
#' @param with.complex    A logical indicating whether receptor co-complex
#'   members should be included in the target genes.
#' @param min.t.logFC     The minimum log2 fold-change allowed for
#'   targets in case pos.targets or neg.targets are used.
#' @param pos.targets   A logical imposing that all the network targets must
#'   display positive logFC, i.e. logFC >= min.t.logFC.
#' @param neg.targets   A logical imposing that all the network targets must
#'   display negative logFC, i.e. logFC <= - min.t.logFC.
#' @param use.full.network  A logical to avoid limiting the reference network
#'   to the detected genes and use the whole reference network.
#' @return A data frame extending \code{lr} content with the pathways found to
#' contain the receptors and data about receptor target gene regulations
#' Strings in semi-colon-separated format are used to report
#' target genes and their regulation P-values in the
#' data frame. The target genes are sorted according to the P-values in
#' decreasing order.
#'
#' In case \code{ds} is set, then correlations between the receptor and
#' target genes will be computed for documentation or additional use. The
#' row names of \code{stats(cc)} and \code{ncounts(ds)} must match
#' exactly (not necessarily in the same order). The same is true for
#' \code{stats(scc)} in case \code{scc} is provided.
#'
#' In a pathway of the reference, i.e., a Reactome pathway or the genes of a
#' GOBP term, the target genes are the
#' genes coding for proteins forming a complex with the receptor and the
#' genes in the pathway downstream the receptor,
#' which are given as regulated by the pathway. If \code{with.complex} is
#' set to \code{FALSE}, then only the
#' regulated genes are considered. Participation to a complex and being
#' regulated as well as the pathway directed topologies
#' are defined by Reactome and KEGG pathways as provided by PathwayCommons.
#'
#' The maximum pathway size is used to limit the redundancy inherent to GOBP
#' and Reactome. The minimum pathway size is
#' used to avoid overspecific, noninformative results.
#'
#' @importFrom methods is
#' @keywords internal
.checkRegulatedReceptorSignaling <- function(ds, cc, lr,
    reference = c("REACTOME-GOBP", "REACTOME", "GOBP"),
    pos.targets = FALSE, neg.targets = FALSE, min.t.logFC = 0.5,
    use.full.network = FALSE,
    max.pw.size = 400, min.pw.size = 5,
    min.positive = 2, restrict.pw = NULL,
    with.complex = TRUE) {
    
    if (!is(cc, "BSRClusterComp")) {
        stop("cc must be a BSRClusterComp object")
    }
    if (!is(ds, "BSRDataModelComp")) {
        stop("ds must be a BSRDataModelComp object")
    }
    if (!all(rownames(stats(cc)) %in% rownames(ncounts(ds)))) {
        stop("The row names of stats(cc) and ncounts(ds) must match exactly")
    }
    if (neg.targets && pos.targets) {
        stop("neg.targets and pos.targets cannot be TRUE simultaneously")
    }
    if (min.t.logFC <= 0) {
        stop("min.t.logFC must be >0")
    }

    reference <- match.arg(reference)
    results <- list()

    # Reactome pathways
    if (reference %in% c("REACTOME-GOBP", "REACTOME")) {
        pw.size <- table(BulkSignalR_Reactome$`Reactome ID`)
        pw.size <- pw.size[pw.size >= min.pw.size & pw.size <= max.pw.size]
        if (use.full.network) {
            react <- BulkSignalR_Reactome
        } else {
            react <- BulkSignalR_Reactome[
            BulkSignalR_Reactome$`Gene name` %in% rownames(stats(cc)), ]
        }
        if (!is.null(restrict.pw)) {
            react <- react[react$`Reactome ID` %in% restrict.pw, ]
        }
        contains.receptors <- react[react$`Gene name` %in% lr$R, "Reactome ID"]
        pw.size <- pw.size[names(pw.size) %in% contains.receptors]
        corgenes <- intersect(
            rownames(stats(cc)),
            c(lr$R, react[react$`Reactome ID` %in% names(pw.size), "Gene name"])
        )
        results$reactome.pairs <- .downstreamRegulatedSignaling(lr,
            react, pw.size,
            ncounts(ds)[corgenes, c(colA(cc), colB(cc))], stats(cc)[corgenes, ],
            id.col = "Reactome ID", gene.col = "Gene name",
            pw.col = "Reactome name", min.positive = min.positive,
            pos.targets = pos.targets, neg.targets = neg.targets,
            min.t.logFC = min.t.logFC,
            with.complex = with.complex
        )
    }

    # GOBP
    if (reference %in% c("REACTOME-GOBP", "GOBP")) {
        pw.size <- table(BulkSignalR_Gobp$`GO ID`)
        pw.size <- pw.size[pw.size >= min.pw.size & pw.size <= max.pw.size]
        if (use.full.network) {
            go <- BulkSignalR_Gobp
        } else {
            go <- BulkSignalR_Gobp[
            BulkSignalR_Gobp$`Gene name` %in% rownames(stats(cc)), ]
        }
        if (!is.null(restrict.pw)) {
            go <- go[go$`GO ID` %in% restrict.pw, ]
        }
        contains.receptors <- go[go$`Gene name` %in% lr$R, "GO ID"]
        pw.size <- pw.size[names(pw.size) %in% contains.receptors]
        corgenes <- intersect(
            rownames(stats(cc)),
            c(lr$R, go[go$`GO ID` %in% names(pw.size), "Gene name"])
        )
        results$gobp.pairs <- .downstreamRegulatedSignaling(lr, go, pw.size,
            ncounts(ds)[corgenes, c(colA(cc), colB(cc))], stats(cc)[corgenes, ],
            id.col = "GO ID", gene.col = "Gene name", pw.col = "GO name",
            pos.targets = pos.targets, neg.targets = neg.targets,
            min.t.logFC = min.t.logFC,
            min.positive = min.positive, with.complex = with.complex
        )
    }

    # merge
    if (reference == "REACTOME-GOBP") {
        pairs <- unique(rbind(
            results$reactome.pairs[, c(1, 2)],
            results$gobp.pairs[, c(1, 2)]
        ))
        react.keys <- paste(results$reactome.pairs[[1]],
            results$reactome.pairs[[2]],
            sep = "|"
        )
        gobp.keys <- paste(results$gobp.pairs[[1]],
            results$gobp.pairs[[2]],
            sep = "|"
        )
        results$merged.pairs <- rbind(
            results$reactome.pairs,
            results$gobp.pairs[!(gobp.keys %in% react.keys), ]
        )
    } else if (reference == "REACTOME") {
        results$merged.pairs <- results$reactome.pairs
    } else {
        results$merged.pairs <- results$gobp.pairs
    }

    results$merged.pairs
} # .checkRegulatedReceptorSignaling


#' Internal function to assign P-values to LR interactions
#'
#' Estimate the P-value of each ligand-receptor pair based
#' on the data frame output by \code{\link{.checkRegulatedReceptorSignaling}}.
#'
#' @param pairs         A data frame output by
#'   \code{checkRegulatedReceptorSignaling}.
#' @param param         A list containing the statistical model parameters.
#' @param rank.p        A number between 0 and 1 defining the rank of the last
#'   considered target genes.
#' @param fdr.proc      The procedure for adjusting P-values according to
#'   \code{\link[multtest]{mt.rawp2adjp}}.
#'
#' @return A data.frame with the data in \code{pairs} complemented with
#' P-values and adjusted P-values.
#' @keywords internal
.pValuesRegulatedLR <- function(pairs, param, rank.p = 0.75,
                                fdr.proc = c(
                                    "BH", "Bonferroni", "Holm", "Hochberg",
                                    "SidakSS", "SidakSD", "BY", "ABH", "TSBH"
                                )) {
    if (rank.p < 0 || rank.p > 1) {
        stop("rank.p must lie in [0;1]")
    }
    fdr.proc <- match.arg(fdr.proc)
    if (is.null(pairs)) {
        stop("Dataframe `pairs` from `.checkRegulatedReceptorSignaling`",
            " is NULL.")
    }

    # estimate P-values
    res <- NULL
    for (i in seq_len(nrow(pairs))) {
        # all the data related to each pathway containing a given
        # receptor were collapsed separated by |
        # we need to split those pathways
        pwid <- unlist(strsplit(pairs$pwid[i], split = "\\|"))
        pwname <- unlist(strsplit(pairs$pwname[i], split = "\\|"))
        tg <- unlist(strsplit(pairs$target.genes[i], split = "\\|"))
        spval <- unlist(strsplit(pairs$target.pval[i], split = "\\|"))
        slfc <- unlist(strsplit(pairs$target.logFC[i], split = "\\|"))
        spear <- unlist(strsplit(pairs$target.corr[i], split = "\\|"))
        sexpr <- unlist(strsplit(pairs$target.expr[i], split = "\\|"))
        len <- as.numeric(unlist(strsplit(pairs$len[i], split = "\\|")))

        # get the LR correlation P-value
        p.lr <- pairs$LR.pval[i]

        # estimate the target gene correlation P-value based on rank statistics
        # for the individual correlation Gaussian model
        for (k in seq_len(length(len))) {
            spvals <- as.numeric(strsplit(spval[k], split = ";")[[1]])
            spears <- as.numeric(strsplit(spear[k], split = ";")[[1]])
            r <- min(max(1, trunc(rank.p * len[k])), len[k])
            rank.pval <- spvals[r]
            rank.corr <- spears[r]
            # r-1 P-values are > rank.pval, prob to have r-1 or less
            # P-values > rank.pval is given by a binomial with success rate
            # equal to the probability to get a P-value > rank.pval, i.e.,
            # 1-rank.pval. If rank.pval is low (i.e., highly significant),
            # it becomes difficult to get as little 
            # as r-1 P-values > rank.pval by chance!
            p.rt <- stats::pbinom(r - 1, len[k], 1 - rank.pval) 
            res <- rbind(res, data.frame(pairs[i, c("L", "R", "LR.pval",
                "corr", "L.logFC", "R.logFC")],
                pw.id = pwid[k], pw.name = pwname[k], rank = r,
                len = len[k], rank.pval = rank.pval,
                rank.corr = rank.corr,
                target.genes = tg[k], target.pval = spval[k],
                target.logFC = slfc[k], target.corr = spear[k],
                target.expr = sexpr[k], pval = p.lr * p.rt,
                stringsAsFactors = FALSE
            ))
        }
    }
    names(res)[4] <- "LR.corr"

    # avoid the impossible
    key <- paste(res$L, res$R, res$pw.id, sep = "||")
    bad <- duplicated(key)
    res <- res[!bad, ]

    # multiple hypothesis correction
    rawp <- res$pval
    adj <- multtest::mt.rawp2adjp(rawp, fdr.proc)
    res$qval <- adj$adjp[order(adj$index), fdr.proc]

    res
} # .pValuesRegulatedLR
