## LDA workbench
## created on 2023/03/27
## modified on 2023/03/DD
#
# * set parameters ----
options(stringsAsFactors = F)
options("scipen" = 100, "digits" = 4) # suppress math annotation
#
set.seed(12345)
#
debugged <- F
verbose <- F
# implications
if (debugged) verbose <- T

## * functions ----

source("~/Dropbox/R-world/scripts/personal.R") # provides count.items(..)

# tokenize ----
tokenize.line <- function(l) {
   # l is a line
   return(strsplit(l, " "))
}
# alias
get.bow.of.line <- tokenize.line

# tokenize grossly ----
tokenize.over.lines <- function(L) {
   # L is a list of lines.
   M <- Map(tokenize.line, L)
   return(Reduce(rbind, M, NULL))
}
# alias
get.bow.over.lines <- tokenize.over.lines
#
remove.puncts <- function(w) {
   # w is a word
   require(stringr)
   puncts <- c(",", ";", ":", "!", "\\?", "\\.", "--+") # Crucially, \\x
   for (punct in puncts) w <- str_replace_all(w, punct, "")
   return(w)
}
# gen.ngrams ----
gen.char.ngrams <- function(x, n, filler = "_") {
   x.size <- nchar(x)
   x.char.vec <- unlist(strsplit(x, ""))
   ngrams <- NULL
   for (i in 1:x.size) {
      sub.vec <- x.char.vec[ i:(i+n-1) ] # Crucially, i:(i+n-1)
      if (!is.na(filler)) {
         seg <- sapply(sub.vec,
                       function(y) ifelse(is.na(y), filler, y))
      } else {
         seg <- Filter(function(y) !is.na(y), sub.vec)
      }
      z <- paste(seg, collapse = "")
      ngrams <- c(ngrams, z)
   }
   return(ngrams)
}
#


## variables -----

sampling <- T
remove.stopwords <- T
use.unrooted <- F

## unit type ----
unit.types <- c("word", "ngram")
unit.type <- unit.types[1]
unit.type

## source ----
target.spelling <- T
if (target.spelling) {
   target.dir <- "data"
   files <- Filter(function(x) grepl("grams", x), list.files(target.dir))
   raw.source.fn <- files[1]
} else {
   target.dir <- "texts-English"
   files <- list.files(target.dir)
   raw.source.fn <- files[2]
}
raw.source.fn

## modify par
source.is.japanese <- F
#rm.fn <- "GillSans"
ja.fn <- "HiraKakuPro-W3"
rm.fn <- "Lucida Sans Unicode"
mar.val <- c(5,5,5,5)
par.old <- par
if (source.is.japanese) {
   par(family = eval(ja.fn), xpd = T, mar = mar.val, cex = 0.8)
} else {
   par(family = eval(rm.fn), xpd = T, mar = mar.val, cex = 0.9)
}


## define data name ----
require(stringr)
to.remove <- c("-sl", ".txt", "-combined", "-spell-segment")
d.name <- remove.substrings(raw.source.fn, to.remove)
## the following returns unwanted result
#d.name <- str_replace_all(raw.source.fn, to.delete, "")
d.name

## * read from the file ----
raw.source <- readLines(sprintf("%s/%s",
                                target.dir, raw.source.fn))
str(raw.source)
head(raw.source)

## filter out empty lines ----
raw.source <- Filter(function(x) nchar(x) > 0, raw.source)

## get sentences ----

## get BoWs and normalize them ----
bow <- get.bow.of.line(unlist(raw.source))
bow

## removing punctuations ----
if (debugged) {
   bow1 <- bow[1]
   bow1
   Map(function(w) remove.puncts(w), bow1)
}
bow.clean <- Map(remove.puncts, bow)

## lowering ----
bow <- Map(tolower, bow.clean)
#
str(bow.clean)

## * define data0 ---
if (target.spelling) {
   header <- Map(function(x) x[1], bow.clean)
   bow.clean2 <- Map(function(x) x[2:length(x)], bow.clean)
   data0 <- data.frame(cbind(header, bow.clean=bow.clean2))
} else {
   data0 <- data.frame(cbind(unlist(line), bow.clean)) # Crucially, unlist(..)
}
nrow(data0)
colnames(data0)
if (verbose) View(data0)

nrow(data0)

## * sampling -----
sample.n <- 120
resampling <- T
if (sampling) {
   if (nrow(data0) == sample.n) {
      if (resampling) {
         sampled.id <- sort(sample(1:nrow(data0), sample.n))
         data <- data0[sampled.id, ]
      }
   } else {
      sampled.id <- sort(sample(1:nrow(data0), sample.n))
      data <- data0[sampled.id, ]
   }
}
nrow(data)
str(data)
if (verbose) View(data)
sampled.id

## build the term list -----
#bow.united <- Reduce(function(x, y) c(x, y), data[[2]], NULL)
bow.united <- Reduce(function(x, y) c(x, y), data[[2]], NULL)
bow.united

## take frequencies ----
freq.df <- data.frame(table(bow.united))
str(freq.df)
#
freqRank <- freq.df[order(freq.df$Freq, decreasing = T), ]
head(freqRank, 20)
if (verbose) {
   View(freqRank)
   plot(freqRank$Freq)
}

## filter out stop words ----
#Freq.threshold <- 10
cut.par <- 0.98
freqQuantile <- quantile(freqRank$Freq,
                         probs = c(0, 0.5, cut.par, 1.0))
## remove units gathering frequencies with top 3% of all
Freq.threshold <- as.integer(freqQuantile[3])
Freq.threshold
if (remove.stopwords) {
   freqRank.included <- subset(freqRank,
                               Freq < Freq.threshold)
   unit.set <- freqRank.included[[1]] # Crucially X[[i]]
   freqRank.excluded <- subset(freqRank,
                                Freq >= Freq.threshold)
   discarded.unit.set <- freqRank.excluded[[1]] # Crucially X[[i]]
} else {
   unit.set <- freqRank$unit.bag
}

length(unit.set)
head(unit.set)
tail(unit.set)
length(unit.set)
head(discarded.unit.set)
tail(discarded.unit.set)

## * save freqRank -----
save.units <- T
out.fn1 <- sprintf("unit-included-freq-ranked-%s.txt", unit.type)
out.fn2 <- sprintf("unit-excluded-freq-ranked-%s.txt", unit.type)
if (save.units) {
   write.table(freqRank.included, out.fn1, row.names = F, quote = F)
   write.table(freqRank.excluded, out.fn2, row.names = F, quote = F)
}


## * build a dtm -----
#unit.counts <- lapply(data[[2]],
#                      function(x) count.keys(x, unit.set))
unit.counts <- lapply(data[[2]],
                      function(x) count.items(x, unit.set))
str(unit.counts)
class(unit.counts)

## The following needs to come before colSum, rowSums
unit.dtm <- data.frame(Reduce(rbind, unit.counts, NULL)) # Crucially
if (debugged) {
   class(unit.dtm)
   Map(class, unit.dtm)
}
View(unit.dtm)

## check for validity ----
table(colSums(unit.dtm))
table(rowSums(unit.dtm))
unit.dtm[rowSums(unit.dtm) == 0, ]

## add header
header <- data[1]
nrow(header)
nrow(unit.dtm)
unit.dtm.x <- cbind(unit.dtm, header)
if (verbose) View(unit.dtm.x)
head(unit.dtm.x)
class(unit.dtm.x)
##
bad.rows <- unit.dtm.x[rowSums(unit.dtm.x[-length(unit.dtm.x)])==0, ]
bad.rows$header

## filter 0-sum rows -----
## 0-sum rows are generated when high-frequency items are removed!
unit.dtm.x <- unit.dtm.x[!unit.dtm.x$header %in% bad.rows$header, ]
nrow(unit.dtm.x)

## re-check ----
table(rowSums(unit.dtm.x[-length(unit.dtm.x)]))

header <- unit.dtm.x[length(unit.dtm.x)]
unit.dtm <- unit.dtm.x[-length(unit.dtm.x)]

## assign row names ----
#rename.rows <- F
#if (rename.rows) {
#   header.char.n <- 30 # the number of the initial characters for header
#   old.names <- data[1]
#   length(old.names)
#   rownames(unit.dtm) <- substr(old.names,
#                             1, header.char.n)
#}

if (verbose) View(unit.dtm)
head(unit.dtm)

## * run LDA ----
require(topicmodels)

max.n.topics <- 15 # 50 seems too many

## The following suffers NAs
#lda.Gibbs <- LDA(df, method = "Gibbs", k = max.n.topics)
lda <- LDA(unit.dtm, k = max.n.topics)
str(lda)
#
n.topics <- topics(lda)
n.topics # list of the number of topics covered by docs

## set constants ----
n.docs <- lda@Dim[1]
n.docs
n.units <- lda@Dim[2]
n.units

## set title ----
if (sampling) {
   if (remove.stopwords) {
      main.text <- sprintf(
         "Clustering filtered LDA (n.top = %d) for sample %d docs in %s",
         max.n.topics, n.docs, d.name)  
   } else {
      main.text <- sprintf(
         "Clustering unfiltered LDA (n.top = %d) for sample %d docs in %s",
         max.n.topics, n.docs, d.name)   
   }
} else {
   if (remove.stopwords) {
      main.text <- sprintf(
         "Clustering filtered LDA (n.top = %d) for all docs in %s",
         max.n.topics, d.name)  
   } else {
      main.text <- sprintf(
         "Clustering unfiltered LDA (n.top = %d) for all docs in %s",
         max.n.topics, d.name)   
   }
}
main.text

## select terms by topic ----

#terms.by.topic <- file()

n.terms <- 15
term.list <- terms(lda, n.terms)
term.list
cat(term.list, file = (con <- file(sprintf("%d-terms-by-%d-topic-%s.txt",
                                           n.terms,
                                max.n.topics, d.name))))

## * clustering documents -----
d.docs <- posterior(lda)$topic # not t(..)
#d.docs

docs.hc <- hclust(dist(d.docs), "ward.D2")
str(docs.hc)

## renew labels
docs.hc.labels <- unlist(data0[docs.hc$labels, ][1]) # Crucially, unlist(..)
#docs.hc.labels
docs.hc$labels <- docs.hc.labels
str(docs.hc)

## plot ----
require(ape)
plot(as.phylo(docs.hc), type = 'fan', cex = 0.8,
     main = main.text,
     sub = sprintf("clustering %d documents", n.docs))
if (use.unrooted) {
   plot(as.phylo(docs.hc), type = 'unrooted', cex = 0.8,
     main = main.text,
     sub = sprintf("clustering %d documents", n.docs))
}
if (verbose) {
   plot(as.dendrogram(docs.hc), horiz = T, cex = 0.3,
        main = main.text,
        sub = sprintf("clustering %d documents", n.docs))
}

## * clustering terms -----
d.terms <- t(posterior(lda)$term)
d.terms

terms.hc <- hclust(dist(d.terms), "ward.D2")

## plot ----
require(ape)
plot(as.phylo(terms.hc), type = 'fan', cex = 0.7,
     main = main.text,
     sub = sprintf("clustering %d units", n.units))
if (use.unrooted) {
   plot(as.phylo(terms.hc), type = 'unrooted', cex = 0.7,
     main = main.text,
     sub = sprintf("clustering %d units", n.units))
}
if (verbose) {
   plot(as.dendrogram(terms.hc), horiz = T, cex = 0.3,
        main = main.text,
        sub = sprintf("clustering %d units", n.units))
}

## LDAvis

require(LDAvis)


### end of script