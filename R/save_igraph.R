#' Converting an igraph object to any graph format.
#'
#' @name save_igraph
#' @param igraph object
#' @return None
#' @author Yu Hatakeyama
#' @export
#'

save_igraph <- function(graph, community, file_name=NULL, format="gexf", save_dir=NULL) {
  if (is.null(file_name)) {
    stop("Argument of file_name is needed.")
  }

  if (is.null(save_dir)) {
    save_dir <- getwd()
  }

  com.nums <- as.integer(community$membership)
  V(graph)$CommunityNumber <- com.nums
  V(graph)$color <- rainbow(max(com.nums))[com.nums]

  file_path <- paste0(save_dir, "/", file_name)
  loaded_packages <- search()

  if (("package:rgexf" %in% loaded_packages) && (format == "gexf")) {
    gexf <- igraph.to.gexf(graph)

    sink(file=file_path)
    print(gexf)
    sink()
  } else if (format == "gexf") {
    stop("rgexf package has not been installed.")
  } else {
    write.graph(graph, file=file_path, format=format)
  }
}
