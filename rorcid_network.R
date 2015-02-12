library(igraph)
library(devtools)

install_github('ropensci/rorcid')
library(rorcid)

# The idea is to go into a user and get all their papers, and all the papers of people they've published with.
#  Just replace my ORCiD with your own (you can replace your name too)
orcid.record <- orcid_id(orcid = '0000-0002-2700-4605', profile="works")

get_doi <- function(x){
  #  This pulls the DOIs out of the ORCiD record:
  list.x <- x$'work-external-identifiers.work-external-identifier'
  
  #  We have to catch a few objects with NULL DOI information:
  do.call(rbind.data.frame,lapply(list.x, function(x){
      if(length(x) == 0 | (!'DOI' %in% x[,1])){
        data.frame(value=NA)
      } else{
        data.frame(value = x[which(x[,1] %in% 'DOI'),2])
      }
    }))
}

get_papers <- function(x){
  all.papers <- x[[1]]$works # this is where the papers are.
  papers <- data.frame(title = all.papers$'work-title.title.value',
                       doi   = get_doi(all.papers))
  
  paper.doi <- lapply(1:nrow(papers), function(x){
    if(!is.na(papers[x,2]))return(orcid_doi(dois = papers[x,2], fuzzy = FALSE))
    return(NA)
  })

  your.papers <- lapply(1:length(paper.doi), function(x){
      if(is.na(paper.doi[[x]])){
        data.frame(doi=NA, orcid=NA, name=NA)
      } else {
        data.frame(doi = papers[x,2],
                   orcid = paper.doi[[x]][[1]]$data$'orcid-identifier.path',
                   name = paste(paper.doi[[x]][[1]]$data$'personal-details.given-names.value',
                                paper.doi[[x]][[1]]$data$'personal-details.family-name.value', sep = ' '),
                   stringsAsFactors = FALSE)
      }})
  do.call(rbind.data.frame, your.papers)
  
}

paper.set <- get_papers(orcid.record)

unique.orcids <- unique(paper.set$orcid)

all.colleagues <- list()

for(i in 1:length(unique.orcids)){
  all.colleagues[[i]] <- get_papers(orcid_id(orcid = unique.orcids[i], profile="works"))
}

all.df <- do.call(rbind.data.frame,all.colleagues)
all.df <- na.omit(all.df[!duplicated(all.df),])

all.pairs <- matrix(ncol = length(unique(all.df$name)),
                    nrow = length(unique(all.df$name)),
                    dimnames = list(unique(all.df$name),unique(all.df$name)), 0)

unique.dois <- unique(as.character(all.df$doi))

for(i in 1:length(unique.dois)){
  doi <- unique.dois[i]
  
  all.pairs[all.df$name[all.df$doi %in% doi],all.df$name[all.df$doi %in% doi]] <- 
    all.pairs[all.df$name[all.df$doi %in% doi],all.df$name[all.df$doi %in% doi]] + 1

}

all.pairs <- all.pairs[rowSums(all.pairs)>0, colSums(all.pairs)>0]

diag(all.pairs) <- 0

author.adj <- graph.adjacency(all.pairs, mode = 'undirected', weighted = TRUE)
plot(author.adj, vertex.label.cex = 0.8, edge.width = E(author.adj)$weight)
