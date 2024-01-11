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

## variables -----
sampling <- T
freq.filtering <- F
use.unrooted <- F
use.dendrogram <- F

## unit type ----
unit.types <- c("word", "ngram", "char")
unit.type <- unit.types[3]
unit.type

## source ----
target.dir <- "data"
#
target.German <- T
target.German
target.sound <- T
target.sound
if (target.German) {
   if (target.sound) {
      raw.source.fn <- "German-base-pairs-r1a-sound-originals.txt"
   } else {
      raw.source.fn <- "German-base-pairs-r1a-spell-originals.txt"
   }
} else {
   if (target.sound) {
      raw.source.fn <- "English-base-pairs-r6e-sound-originals.txt"
   } else {
      raw.source.fn <- "English-base-pairs-r6e-spell-originals.txt"
   }
}
raw.source.fn

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
raw.source

## filter out comments ----

source <- Map(function(x) strsplit(x, "#")[1], raw.source)

## filter out duplicates ----

word.raw <- unique(source)
names(word.raw) <- word.raw

## lowering ----
word <- Map(tolower, word.raw)
word

## filter out invalid forms ----
#word <- Filter(function(x) !grepl(c("\\."), x), raw.source)
word <- Filter(function(x) !any(
   grepl("\\.", x), grepl(" ", x), grepl("-", x), grepl("#", x)),
               word)
class(word) # simple list

names(word) <- word
names(word)

## * sampling -----
sample.n <- 100
resampling <- F
if (sampling) {
   if (length(word) == sample.n) {
      if (resampling) {
         sampled.id <- sort(sample(1:length(word), sample.n))
         sampled.id
         word <- word[sampled.id]
      }
   } else {
      sampled.id <- sort(sample(1:length(word), sample.n))
      sampled.id
      word <- word[sampled.id]
   }
}
sampled.id
if (verbose) View(word)
str(word)
length(word)

## build the term list -----

## obtain used characters ----
boch <- Reduce(function(x, y) x <- c(x,y),
                            unlist(Map(function(x) strsplit(x, ""), word)))
boch

## define boch.unit ----
boch.unit <- unique(boch)
boch.unit
length(boch.unit)

## take frequencies ----
boch.freq.df <- data.frame(table(boch))
str(boch.freq.df)
#
boch.freqRank <- boch.freq.df[order(boch.freq.df$Freq, decreasing = T), ]
if (verbose) View(boch.freqRank)
names(boch.freqRank)
head(boch.freqRank, 20)

# plot
main.text <- ifelse(target.German,
                    "Frequncey rank of German sounds",
                    "Frequency rank of English sounds")
vowel.symbols <- c("ə", "a", "ɐ", "ɑ̃", "e" , "ɛ", "i", "ɪ",
                   "ɔ", "o", "u", "ø", "ʏ", "y", "œ", "ː")
col.map <- ifelse(boch.freqRank$boch %in% vowel.symbols,'red','blue')
plot(boch.freqRank$Freq, main = main.text, col= col.map)
text(boch.freqRank$Freq, labels = boch.freqRank$boch, pos = 3)

## filter out too frequent items ----
#Freq.threshold <- 10
cut.par <- 0.98
boch.freqQuantile <- quantile(boch.freqRank$Freq,
                         probs = c(0, 0.5, cut.par, 1.0))
boch.freqQuantile
## remove units gathering frequencies with top 3% of all
boch.Freq.threshold <- as.integer(boch.freqQuantile[3])
boch.Freq.threshold
#
if (freq.filtering) {
   boch.freqRank.included <- subset(boch.freqRank,
                               Freq < Freq.threshold)
   unit.set <- boch.freqRank.included$boch
   boch.freqRank.excluded <- subset(boch.freqRank,
                                Freq >= Freq.threshold)
   discarded.unit.set <- boch.freqRank.excluded$boch
   head(discarded.unit.set)
   tail(discarded.unit.set)
} else {
   unit.set <- boch.freqRank$boch
   boch.freqRank.included <- unit.set
   boch.freqRank.excluded <- NULL
   head(unit.set)
   tail(unit.set)
}
length(unit.set)

## * save freqRank -----
save.units <- F
out.fn1 <- sprintf("unit-included-freq-ranked-%s.txt", unit.type)
out.fn2 <- sprintf("unit-excluded-freq-ranked-%s.txt", unit.type)
if (save.units) {
   write.table(boch.freqRank.included, out.fn1, row.names = F, quote = F)
   write.table(boch.freqRank.excluded, out.fn2, row.names = F, quote = F)
}

## * build a dtm -----
unit.count <- lapply(word,
                      function(x) count.items(unlist(strsplit(x, "")),
                                              unit.set))
if (verbose) View(unit.count)
str(unit.count)
class(unit.count)

## The following needs to come before colSum, rowSums
unit.dtm <- data.frame(Reduce(rbind, unit.count, NULL)) # Crucially
rownames(unit.dtm) = names(word)
View(unit.dtm)
if (debugged) {
   class(unit.dtm)
   Map(class, unit.dtm)
}

## check for validity ----
table(colSums(unit.dtm))
table(rowSums(unit.dtm))
unit.dtm[rowSums(unit.dtm)==0, ]
##
bad.rows <- unit.dtm[rowSums(unit.dtm)==0, ]
bad.rows$header

## filter 0-sum rows -----
## 0-sum rows are generated when high-frequency items are removed!
#unit.dtm <- unit.dtm[!unit.dtm$header %in% bad.rows$doc, ]
#nrow(unit.dtm)

## re-check ----
table(rowSums(unit.dtm))

if (verbose) View(unit.dtm)
head(unit.dtm)

## run LDA-tuner
require(ldatuning)

# 4種類の指標値の算出

metrics.set1 = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014")
metrics.set2 = c("Griffiths2004", "CaoJuan2009", "Deveaud2014")
metrics.set3 = c("Griffiths2004", "CaoJuan2009")

lda.result = FindTopicsNumber(
   unit.dtm,
   topics = seq(from = 5, to = 150, by = 5),
   metrics = metrics.set2,
   method = "Gibbs",
   control = list(seed = 77),
   mc.cores = 2L,
   verbose = TRUE
)

# 結果のグラフ化
ldatuning::FindTopicsNumber_plot(values = lda.result)

## * run LDA ----
require(topicmodels)

set.manually <- T
if (set.manually) {
   max.n.topics <- 15 # 50 seems too many
} else {
   reduct.rate <- 0.2
   max.n.topics <- round(reduct.rate * nrow(unit.dtm))
}
max.n.topics

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
   if (freq.filtering) {
      main.text <- sprintf(
         "Clustering filtered LDA (n.top = %d) for sample %d docs in %s",
         max.n.topics, n.docs, d.name)  
   } else {
      main.text <- sprintf(
         "Clustering unfiltered LDA (n.top = %d) for sample %d docs in %s",
         max.n.topics, n.docs, d.name)   
   }
} else {
   if (freq.filtering) {
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
n.terms <- 20
term.list <- terms(lda, n.terms)
term.list
cat(term.list, file = (con <- file(sprintf("%d-terms-by-%d-topic-%s.txt",
                                           n.terms,
                                max.n.topics, d.name))))


## * clustering terms -----
enc.terms <- t(posterior(lda)$term)
#enc.terms

terms.hc <- hclust(dist(enc.terms), "ward.D2")
str(terms.hc)

## plot ----
require(ape)
# phylogenic
plot(as.phylo(terms.hc), type = 'fan', cex = 0.7,
     main = main.text,
     sub = sprintf("clustering %d units", n.units))
# unrooted
if (use.unrooted) {
   plot(as.phylo(terms.hc), type = 'unrooted', cex = 0.7,
     main = main.text,
     sub = sprintf("clustering %d units", n.units))
}
# dendrogram
if (use.dendrogram) {
   plot(as.dendrogram(terms.hc), horiz = T, cex = 0.3,
        main = main.text,
        sub = sprintf("clustering %d units", n.units))
}

## run tSNE on terms ----
require(Rtsne)
terms.theta.val <- 0.5
#terms.ppl.val <- max.n.topics*0.5 
terms.ppl.val <- round(reduct.rate * nrow(enc.terms))
terms.ppl.val
terms.tsne <- Rtsne(enc.terms, dims = 3, check.duplicates = F,
                  perplexity = terms.ppl.val, theta = terms.theta.val)
str(terms.tsne)

## clustering -----
require(rgl)

term.d <- terms.tsne$Y
summary(term.d)
plot3d(term.d)

max.dist <- apply(term.d, 2, max)
max.dist

require(dbscan)
#eps.val <- 2 # sensitive
eps.val <- mean(unlist(max.dist))/length(max.dist)
eps.val
minPts.val <- 2
terms.clustered <- dbscan(term.d, eps = eps.val, minPts = minPts.val)
terms.clustered

## 2D plot Dim1-Dim2
terms.main.text <- sprintf("tSNE (ppl=%d, θ=%.2f) of terms via LDA of chars in words",
                           terms.ppl.val, terms.theta.val)
plot(terms.tsne$Y[,1], terms.tsne$Y[,2], main = terms.main.text,
     col = terms.clustered$cluster)
text(terms.tsne$Y[,1], terms.tsne$Y[,2], pos = 3, labels = lda@terms)


## 2D plot Dim1-Dim3
plot(terms.tsne$Y[,1], terms.tsne$Y[,3], main = terms.main.text,
     col = terms.clustered$cluster)
text(terms.tsne$Y[,1], terms.tsne$Y[,3], pos = 3, labels = lda@terms)

## 2D plot Dim2-Dim3
plot(terms.tsne$Y[,2], terms.tsne$Y[,3], main = terms.main.text,
     col = terms.clustered$cluster)
text(terms.tsne$Y[,2], terms.tsne$Y[,3], pos = 3, labels = lda@terms)

## 3D plot
require(rgl)
plot3d(terms.tsne$Y[,1], terms.tsne$Y[,2], terms.tsne$Y[,3],
       main = terms.main.text)


## * clustering documents -----
enc.docs <- posterior(lda)$topic # not t(..)
#enc.docs
docs.hc <- hclust(dist(enc.docs), "ward.D2")
str(docs.hc)

## renew labels
docs.hc.labels <- unlist(lda@documents) # Crucially, unlist(..)
docs.hc.labels
docs.hc$labels <- docs.hc.labels
str(docs.hc)

## plot ----
require(ape)
# phylogenic
plot(as.phylo(docs.hc), type = 'fan', cex = 0.7,
     main = main.text,
     sub = sprintf("clustering %d documents", n.docs))
# unrooted
if (use.unrooted) {
   plot(as.phylo(docs.hc), type = 'unrooted', cex = 0.7,
        main = main.text,
        sub = sprintf("clustering %d documents", n.docs))
}
# dengrogram
if (use.dendrogram) {
   plot(as.dendrogram(docs.hc), horiz = T, cex = 0.3,
        main = main.text,
        sub = sprintf("clustering %d documents", n.docs))
}

## run tSNE on docs ----
require(Rtsne)
docs.theta.val <- 0.5
docs.ppl.val <- max.n.topics*0.5
docs.ppl.val
docs.tsne <- Rtsne(enc.docs, dims = 3, check.duplicates = F,
                    perplexity = docs.ppl.val, theta = docs.theta.val)
str(docs.tsne)

## 2D plot Dim1-Dim2
docs.main.text <- sprintf("tSNE (ppl=%d, θ=%.2f) of docs via LDA of chars in words",
                          docs.ppl.val, docs.theta.val)
plot(docs.tsne$Y[,1], docs.tsne$Y[,2], main = docs.main.text)
text(docs.tsne$Y[,1], docs.tsne$Y[,2], pos = 3, cex = 0.5,
     labels = lda@documents)

## 2D plot Dim1-Dim3
plot(docs.tsne$Y[,1], docs.tsne$Y[,3], main = docs.main.text)
text(docs.tsne$Y[,1], docs.tsne$Y[,3], pos = 3, cex = 0.5,
     labels = lda@documents)

## 2D plot Dim2-Dim3
plot(docs.tsne$Y[,2], docs.tsne$Y[,3], main = docs.main.text)
text(docs.tsne$Y[,2], docs.tsne$Y[,3], pos = 3, cex = 0.5,
     labels = lda@documents)

## 3D plot
require(rgl)
plot3d(docs.tsne$Y[,1], docs.tsne$Y[,2], docs.tsne$Y[,3],
       main = main.text)


### end of script