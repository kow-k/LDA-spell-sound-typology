## LDA workbench
## created on 2023/03/13
## modified on 2023/03/14, 15; 11/17
#

## load packages ----
require(tidyverse)

# * set parameters ----
options(stringsAsFactors = F)
options("scipen" = 100, "digits" = 4) # suppress math annotation
#
set.seed(12345)
#
check <- F
verbose <- F
# implications
if (check) verbose <- T

## modify par
source.is.japanese <- F
#rm.fn <- "GillSans"
ja.fn <- "HiraKakuPro-W3"
rm.fn <- "Lucida Sans Unicode"
mar.val <- c(5,5,5,5)
par.old <- par
if (source.is.japanese) {
   par(family = eval(ja.fn), xpd = T, mar = mar.val, cex = 0.7)
} else {
   par(family = eval(rm.fn), xpd = T, mar = mar.val, cex = 0.8)
}

## * functions ----
source("~/Dropbox/R-world/scripts/personal.R") # provides count.items(..)
source("~/Dropbox/R-world/scripts/ngrams.R") # provides gen_Ngrams(..)

# tokenize ----
tokenize.line <- function(l) {
   # l is a line
   return(strsplit(unlist(l), " "))
}

# alias
get.bot.of.line <- tokenize.line

# tokenize grossly ----
tokenize.over.lines <- function(L) {
   # L is a list of lines.
   M <- Map(tokenize.line, L)
   return(Reduce(rbind, M, NULL))
}
# alias
get.bot.over.lines <- tokenize.over.lines
#
remove.puncts <- function(w) {
   # w is a word
   require(stringr)
   puncts <- c(",", ";", ":", "!", "\\?", "\\.", "--+") # Crucially, \\x
   for (punct in puncts) w <- str_replace_all(w, punct, "")
   return(w)
}

## variables -----

sampling <- T
#remove.stopwords <- F

## source.type ----
source.types <- c("text", "medical", "mixed", "spell", "sound")
(source.type <- source.types[4])

## term size ----
term.sizes <- c("character", "word")
(term.size <- term.sizes[1])

## term type ----
term.types <- c("1gram", "2gram", "3gram", "skippy2gram", "skippy3gram")
(term.type <- term.types[2])

## source selection ----
if (term.size == 'character') {
   target.dir <- "data-words"
   files <- list.files(target.dir)
   source.fn <- files[7] # selects a file
} else {
   target.dir <- "data-texts-English"
   files <- list.files(target.dir)
   source.fn <- files[2] # selects a file
}
source.fn

## * read from the file ----
raw.source <- readLines(sprintf("%s/%s", target.dir, source.fn))

## filter out empty lines ----
raw.source <- Filter(function(x) nchar(x) > 0, raw.source)
head(raw.source)
class(raw.source)

## convert lines into bags-of-1grams first
raw.d <- unlist(raw.source)
bots0 <- Map(function (x) str_split(x, ""), raw.d)

## define data name ----
require(stringr)
to.remove <- c("-sl", ".txt", "-combined", "-spell-segment")
data.name <- str_replace_all(source.fn,
                             paste0(to.remove, collapse = "|"),
                             "") # Works!
data.name

## generate n-grams
if (term.type == '1gram') {
   bots <- bots0
} else if (term.type == '2gram') {
   bots <- Map(gen_2grams, bots0)
} else if (term.type == 'skippy2gram') {
   bots <- Map(gen_skippy2grams, bots0)
} else if (term.type == '3gram') {
   bots <- Map(gen_3grams, bots0)
} else if (term.type == 'skippy3gram') {
   bots <- Map(gen_skippy3grams, bots0)
} else {
   bots <- get.bot.of.line(Map(function (x) str_split(x, " "), raw.d))
}

bots
length(bots)

## build the term list -----
bots.unified <- Reduce(function(x, y) c(x, y), unlist(unname(bots)), NULL)
bots.unified

## take frequencies ----
bot.freq <- table(bots.unified)
bot.freq
hist(bot.freq)

## reorder
bot.freq <- bot.freq[order(bot.freq, decreasing = T)]
bot.freq
head(bot.freq, 10)

## filter out extreme terms ----
#Freq.threshold <- 10
cut.par <- 0.9
freqQuantile <- quantile(bot.freq, probs = c(0, 0.5, cut.par, 1.0))

## remove terms gathering frequencies with top 3% of all
freq.threshold <- as.integer(freqQuantile[3])
freq.threshold
##
filter.bots <- F
if (filter.bots) {
   used.terms <- bot.freq[ bot.freq < freq.threshold ]
   unused.terms <- bot.freq[ bot.freq >= freq.threshold]
} else {
   used.terms <- names(bot.freq)
   unused.terms <- NULL
}
#
head(used.terms)
length(used.terms)

head(unused.terms)
length(unused.terms)

## * save freqRank -----
save.terms <- F
out.fn1 <- sprintf("terms-saved/used-terms-freq-ranked-%s.txt", term.type)
out.fn2 <- sprintf("terms-saved/unused-terms-freq-ranked-%s.txt", term.type)
if (save.terms) {
   write.table(used.terms, out.fn1, row.names = F, quote = F)
   write.table(unused.terms, out.fn2, row.names = F, quote = F)
}


## * build a dtm -----
used.terms
class(used.terms)
class(bots)
term.counts <- lapply(bots,
                      function(x) count.items(unlist(x), used.terms))

term.counts

require(rlist)
dtm <- list.rbind(term.counts)
View(dtm)

## check for validity ----
sum(rowSums(dtm) == 0)
sum(colSums(dtm) == 0)

## remove invalid rows
dtm2 <- dtm[rowSums(dtm) != 0, ]

gap <- nrow(dtm) - nrow(dtm2)
print(sprintf("%s rows are removed", gap))

## replace valid dtm ----
dtm <- dtm2
nrow(dtm)

## LDA tuning

run.tuning <- F
if (run.tuning) {
   require(ldatuning)
   
   print("Checking for optimal number of topics ...")
   
   ## calculate metrics
   metric.set4a <-
      c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")
   metric.set4b <-
      c("Griffiths2004", "Arun2010", "CaoJuan2009", "Deveaud2014")
   metric.set3a <- c("Griffiths2004", "CaoJuan2009", "Deveaud2014")
   metric.set3b <- c("CaoJuan2009", "Arun2010", "Deveaud2014")
   metric.set2  <- c("Griffiths2004", "Deveaud2014")
   ##
   tuning.result = FindTopicsNumber(
      dtm,
      topics = seq(from = 3, to = 90, by = 3),
      metrics = metric.set4a,
      method = "Gibbs",
      control = list(seed = 1234, verbose = T),
      #mc.cores = 2L,
      mc.cores = parallel::detectCores() - 1,
      verbose = TRUE
   )
   #
   View(tuning.result)
   
   ## save results
   tstamp <- str_split(Sys.time()[1:1], " ")
   day_sig <- unlist(tstamp)[1]
   hour_sig <- unlist(tstamp)[2][1]
   hour_sig <- str_c(unlist(str_split(unlist(tstamp), ":")[1])[1:2], collapse = '-')
   
   save.result <- F
   if (save.result) {
      result_fn <- sprintf("tuning-result-%s-%s-%s-%s.csv",
                           source.type,
                           term.type,
                           day_sig,
                           hour_sig)
      print(result_fn)
      write.csv(tuning.result, result_fn, quote = F)
   }
   
   ## visualize the tuning result
   title.val = sprintf("LDA tuner result for %s in %s", term.type, source.type)
   title.val
   FindTopicsNumber_plot(values = tuning.result)
   print(sprintf("%s by character %s", source.type, term.type))
   
   ###
   proceed <- F
   stopifnot(proceed)
}


## * run LDA ----
require(topicmodels)

max.n.topics <- 21

## The following suffers NAs
#lda.Gibbs <- LDA(df, method = "Gibbs", k = max.n.topics)
lda <- LDA(dtm, k = max.n.topics)
str(lda)
#
n.topics <- topics(lda)
n.topics # list of the number of topics covered by docs

## set constants ----
(n.docs <- lda@Dim[1])
(n.terms <- lda@Dim[2])

## select terms by topic ----
n.terms <- 20
term.list <- terms(lda, n.terms)
View(term.list)

## save terms
save.terms <- F
if (save.terms) {
   cat(term.list, file = (con <- file(sprintf("%d-terms-by-%d-topic-%s.txt",
                                           n.terms,
                                           max.n.topics, data.name))))
}

## * clustering documents -----

docs <- posterior(lda)$topic # not t(..)
str(docs)
head(docs)

## * sampling -----

##
doc.sample.n <- 60
sampled.docs <- docs[sample(1:nrow(docs), doc.sample.n), ]
View(sampled.docs)

docs.hc <- hclust(dist(sampled.docs), "ward.D2")
str(docs.hc)

docs.hc$labels <- rownames(sampled.docs)
str(docs.hc)

## coloring
require(clusternor)
docs.xms <- Xmeans(sampled.docs, kmax = 10, min.clust.size = 3) # min.clust.size prevents crash.
docs.xms

## set title ----
if (sampling) {
   if (filter.bots) {
      doc.main.text <- sprintf(
         "Filtered LDA (n.top = %d) for sample %d docs in %s",
         max.n.topics, doc.sample.n, data.name)  
   } else {
      doc.main.text <- sprintf(
         "Unfiltered LDA (n.top = %d) for sample %d docs in %s",
         max.n.topics, doc.sample.n, data.name)   
   }
} else {
   if (filter.bots) {
      doc.main.text <- sprintf(
         "Filtered LDA (n.top = %d) for all docs in %s",
         max.n.topics, data.name)  
   } else {
      doc.main.text <- sprintf(
         "Unfiltered LDA (n.top = %d) for all docs in %s",
         max.n.topics, data.name)   
   }
}
#
doc.main.text


## plot ----
require(ape)

## fan plot
plot(as.phylo(docs.hc), type = 'fan', cex = 0.8,
     col = docs.xms$cluster,
     main = doc.main.text,
     sub = sprintf("clustering (sampled) %d documents", doc.sample.n))

## unrooted
plot(as.phylo(docs.hc), type = 'unrooted', cex = 0.7,
     col = docs.xms$cluster,
     main = doc.main.text,
     sub = sprintf("clustering (sampled) %d documents", doc.sample.n))

## usual tree
plot(as.dendrogram(docs.hc), horiz = T, cex = 0.3,
     col = docs.xms$cluster,
     main = doc.main.text,
     sub = sprintf("clustering (sampled) %d documents", doc.sample.n))


## * clustering terms -----

(terms <- t(posterior(lda)$term))

## * sampling -----
term.sample.n <- 50
sampled.terms <- terms[sample(1:nrow(terms), term.sample.n), ]
View(sampled.terms)


## set title ----
if (sampling) {
   if (filter.bots) {
      term.main.text <- sprintf(
         "Filtered LDA (n.top = %d) for sample %d terms in %s",
         max.n.topics, term.sample.n, data.name)  
   } else {
      term.main.text <- sprintf(
         "Unfiltered LDA (n.top = %d) for sample %d terms in %s",
         max.n.topics, term.sample.n, data.name)   
   }
} else {
   if (filter.bots) {
      term.main.text <- sprintf(
         "Filtered LDA (n.top = %d) for all terms in %s",
         max.n.topics, data.name)  
   } else {
      term.main.text <- sprintf(
         "Unfiltered LDA (n.top = %d) for all terms in %s",
         max.n.topics, data.name)   
   }
}
#
term.main.text

terms.hc <- hclust(dist(sampled.terms), "ward.D2")
str(terms.hc)

terms.hc$labels <- rownames(sampled.terms)
str(terms.hc)

## coloring
require(clusternor)
terms.xms <- Xmeans(sampled.terms, kmax = 10, min.clust.size = 3) # min.clust.size prevents crash.
terms.xms

## plot ----
require(ape)

## fan plot
plot(as.phylo(terms.hc), type = 'fan', cex = 0.7,
     col = terms.xms$cluster,
     main = term.main.text,
     sub = sprintf("clustering (sampled) %d terms", term.sample.n))

## unrooted
plot(as.phylo(terms.hc), type = 'unrooted', cex = 0.6,
     col = terms.xms$cluster,
     main = term.main.text,
     sub = sprintf("clustering (sampled) %d terms", term.sample.n))

## usual tree
plot(as.dendrogram(terms.hc), horiz = T, cex = 0.5,
     col = terms.xms$cluster,
     main = term.main.text,
     sub = sprintf("clustering (sampled) %d terms", term.sample.n))

### LDAvis ----

#data(TwentyNewsgroups, package="LDAvis")

# create the json object, start a local file server, open in default browser
#json <- with(TwentyNewsgroups,
#             createJSON(phi, theta, doc.length, vocab, term.frequency))

json <- createJSON(docs, t(terms), nrow(docs), nrow(terms), dtm)

serVis(json) # press ESC or Ctrl-C to kill

# createJSON() reorders topics in decreasing order of term frequency
RJSONIO::fromJSON(json)$topic.order

### end of script
