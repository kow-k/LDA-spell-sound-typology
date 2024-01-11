## LDA workbench
## created on 2023/03/27
## modified on 2023/04/02, 03, 04, 05
## 2023/04/18: added handling of German pronunciations
## 2023/04/19: added handling of Swedish, Danish, Norwegian, Irish, Welsh
#
# set parameters ----
options(stringsAsFactors = F)
options("scipen" = 100, "digits" = 4) # suppress math annotation
set.seed(12345)
#
#options(rgl.useNULL = T)
options(rgl.printRglwidget = T) # required by rgl

## * control parameters ----
debugged <- F
verbose <- F
# implications
if (debugged) verbose <- T

# plot parameters ----
save.plot <- F
save.plot3d <- F
plot.width.val <- 500
plot.height.val <- 500
## for png resolution
res.val <- 300 # not effective

# sampling parameters ----
source.sampling <- F
doc.sampling <- T
freq.filtering <- F

# saving parameters ----
save.term.list <- T

# clustering parameters ----
use.unrooted <- T
use.dendrogram <- T
## * select clustering method ----
use.Xmeans <- T
clust.methods <- c("Xmeans", "DBDCAN")
if (use.Xmeans) {
   clust.method <- clust.methods[1]
} else {
   clust.method <- clust.methods[2]
}
clust.method


## classes of characters ----
# vowels
base.v.chars <- c("a", "à", "á", "â", "ã", "ā", "ă", "ȧ", "ä", "ả", "å", "ǎ",
                  "ȁ", "ȃ", "ą", "ạ", "ḁ", "ẚ",
                  "e", "è", "é", "ê", "ẽ", "ē", "ĕ", "ė", "ë", "ẻ", "ě",
                  "i", "ì", "í", "î", "ĩ", "ī", "ĭ", "ı", "ï", "ỉ", "ǐ",
                  "o", "ò", "ó", "ô", "õ", "ō", "ŏ", "ȯ", "ö", "ỏ", "ǒ", "ǒ",
                  "ø", "œ",
                  "y", "ў", "ý",
                  "w",
                  "u", "ù", "ú", "û", "ũ", "ū", "ŭ", "ü", "ủ", "ů", "ű", "ǔ", "ȕ", "ȗ",
                  "а", "е", "и", "й", "о", "у", "ы", "э", "ю", "я", "ѐ", "ё", "є", "і", "ї",
                  "ѝ")
ipa.v.chars <- c("ɑ", "ɐ", "ɒ", "æ", "ᵄ",
                 "e", "ɛ", "ɜ", "ə", "ɛ̯",
                 "i", "ɪ", "ɨ", "ı", "i̯",
                 "o", "ɔ", "ø", "œ", "ɶ", "œ̯", "ɔ̯",
                 "u", "ʉ", "ʊ", "ʌ", "ʏ",
                 "j", 
                 "ː", "ʶ")
v.chars <- c(base.v.chars, ipa.v.chars)
# semi-vowels, or glides
semi.v.chars <- c( "y", "j", "w", "ъ", "ь", "ј" )

   


## modify par
initial.par <- par
mar.val <- c(5,5,5,6)
#rm.fn <- "GillSans"
rm.fn <- "Lucida Sans Unicode"
ja.fn <- "HiraKakuPro-W3"
source.is.japanese <- F

## functions ----
set.par <- function() {
   if (source.is.japanese) {
      par(family = eval(ja.fn), xpd = T, mar = mar.val,
          cex.main = 0.8, pch = 19)
   } else {
      par(family = eval(rm.fn), xpd = T, mar = mar.val,
          cex.main = 0.9, pch = 19)
   }
}
##
get.date.values <- function() {
   # The implicit argument is date()
   x <- unlist(strsplit(date(), " ")) # Crucially, unlist(..) here
   return(list(dayofweek = x[1], month = x[2], week = x[3],
                           day = x[4], time = x[5], year = x[6]))
   }
#
date.values <- get.date.values()
#str(date.values)
#date.values[["month"]]

# define date.signature()
date.signature <- function() {
   d <- get.date.values()
   t0 <- unlist(strsplit(d$time, ":"))[c(1,2)]
   t <- paste(t0, collapse = "") # Crucially, collapse = ... here
   return(paste(d$year, d$month, d$day, t, sep = "-"))
}
date.signature()

## rename.plot()
rename.plot.fn <- function (fn, appendix) {
   x <- unlist(strsplit(fn, "\\."))
   body <- x[1:length(x)-1]
   tail <- x[length(x)]
   y <- paste(paste(body, collapse = "."), appendix, sep = "-")
   z <- paste(y, tail, sep = ".")
   return (z)
}
#rename.plot.fn("x.y.png", "OPG")

## safely.png
gen.png.safely <- function(plot.func, target.file, target.dir) {
   target.path <- paste(target.dir, target.file, sep = "/")
   if ( file.exists(target.path) ) {
      target.path <- rename.plot.fn(target.path, date.signature())
      print(sprintf("created a new plot: %s", target.path))
   }
   #
   png(file = target.path, #res = res.val,
       width = plot.width.val, height = plot.height.val)
   plot.func
   dev.off()
}

## set parameters initially ----
set.par()
main.par <- par # for resume

# save for retain
source("~/Dropbox/R-world/scripts/personal.R") # provides count.items(..)

## * calculation parameters ----

## good (max.n.top, ppl.reduct.rate) pairs:
## (7, 0.33) # produces 4 clusters
## (8, 0.33) # produces 8 clusters: Very good
## (9, 0.3) # produces 7~8 clusters: Good but troubled with DBSCAN
## (9, 0.33) # produces 7~8 clusters: Good but troubled with DBSCAN
## (11, 0.3) # produces nearly 10 clusters, but good resolution

## n.topics strongly affects cluster numbers
#max.n.topics <- 6 # caused a crash?
#max.n.topics <- 7 # fewer the better? leading to 4 clusters 
#max.n.topics <- 8 # sometimes too few
#max.n.topics <- 9 #
max.n.topics <- 10 # best?
#max.n.topics <- 11 # often causes a crash
#max.n.topics <- 12 # sometimes cases a srash
#max.n.topics <- 13 # often causes a crash, too large?
#max.n.topics <- 14 # too large to cause a crash
#max.n.topics <- 15 # too large to cause a crash
#max.n.topics <- 16 # too large to cause a crash 

## perplextiy for tSNE; smaller the better?
## set ppl.val = n.topics * ppl.reduct.rate
#ppl.reduct.rate <- 0.1 # turned out to be too small
#ppl.reduct.rate <- 0.2 # often causes a crash
#ppl.reduct.rate <- 0.3
ppl.reduct.rate <- 0.33 # a bit too large?
#ppl.reduct.rate <- 0.4
#ppl.reduct.rate <- 0.5
#ppl.reduct.rate <- 0.6 # likely to be too large
#ppl.reduct.rate <- 0.7 # likely to be too large


## select source file ----

lang.names <- c("English",
                "French", "Spanish", "Italian", "Portuguese",
                "Dutch", "German",
                "Danish", "Icelandic", "Norwegian", "Swedish",
                "Irish", "Welsh",
                "Romanian", "Czech", "Russian",
                "Hungarian", "Turkish", "Finnish", "Swahili")

## * select language ----
length(lang.names)
lang.name <- lang.names[12]
lang.name
if ( is.na(lang.name) ) stop("Invalid target language")

## source selection ----
target.sounds <- F
target.German <- T
if (target.sounds) {
   require(stringr)
   if (target.German) {
      target.dir <- "~/Dropbox/Resources/1k-most-common-words/pronunciations"
      raw.source.fn <- "German-1k-word-pronuciations.csv"
      raw.source <- data.frame(read.csv(paste(target.dir, raw.source.fn,
                                              sep = "/")))
      pair.data <- data.frame(raw.source[c(2,1)])
      colnames(pair.data) <- c("sound", "spell")
      ## remove stress-markers
      pair.data$sound <- str_replace_all(pair.data$sound, "ˈ", "")
      word.list <- subset(pair.data, sound != "")
   } else {
      target.dir <- "~/Dropbox/Research/Spelling-of-English/base-pairs-r6"
      raw.source.fn <- "base-pairs-bundled-r6d.txt"
      raw.source <- data.frame(read.csv(paste(target.dir, raw.source.fn,
                                              sep = "/")))
      pair.data <- data.frame(raw.source[c(2,3)])
      colnames(pair.data) <- c("sound", "slashed")
      ## remove stress-markers
      pair.data$sound <- str_replace_all(pair.data$sound, "[012]", "")
      word.list <- pair.data
   }
} else {
   target.dir <- "~/Dropbox/Resources/1k-most-common-words"
   raw.source.fn <- "1k-most-common-words-compared-extended.xlsx"
   require(rio)
   raw.source <- import_list(sprintf("%s/%s",
                                     target.dir, raw.source.fn))
   class(raw.source)
   str(raw.source)
   word.list <- data.frame(unname(raw.source)) # Crucially, unname(..)
}

## check
class(word.list)
colnames(word.list)
head(word.list)
str(word.list)

## build a data.frame ----
if (verbose) View(word.list)

## check
if (!target.sounds) {
   Map(function(x) summary(word.list[[x]]), lang.names)
}

## build word ----
if (target.sounds) {
   word <- word.list[[1]]
   if (target.German) {
      lang.name <- "German_sound"
   } else {
      lang.name <- "English_sound"
   }
} else {
   word <- word.list[[lang.name]]
}
word

## filter out empty lines
word <- Filter(function(x) nchar(x) > 0, word)
word

## filter out duplicates
word <- unique(word)
names(word) <- word
word

## strip joints and whitespaces
require(stringr)
word <- Map(function(x) str_replace_all(x, "_", ""), unlist(word))
word <- Map(function(x) str_replace_all(x, " ", ""), unlist(word))
word <- Map(function(x) str_replace_all(x, "-", ""), unlist(word))
word <- Map(function(x) str_replace_all(x, "/", ""), unlist(word))
word <- Map(function(x) str_replace_all(x, "#", ""), unlist(word))
word

## lowering
word <- Map(tolower, word)
word
length(word)

## filter out comments
#source <- Map(function(x) strsplit(x, "#")[1], source)
#source

## filter out invalid forms ----
word <- Filter(function(x) !any(
   grepl("\\.", x), grepl(",", x), grepl(":", x), grepl(";", x)), word)
word
#
head(word)
length(word)

## ** run analysis ----

## * source sampling -----
source.sample.n <- 600
resampling <- F
if (source.sampling) {
   if (length(word) == source.sample.n) {
      if (resampling) {
         sampled.id <- sort(sample(1:length(word), source.sample.n))
         sampled.id
         word <- word[sampled.id]
      }
   } else {
      sampled.id <- sort(sample(1:length(word), source.sample.n))
      sampled.id
      word <- word[sampled.id]
   }
}
if (verbose) View(word)
str(word)
length(word)
if (debugged) edit(word)

## build the term list -----

## obtain used characters ----
system.time(boch <- Reduce(function(x, y) x <- c(x, y),
              unlist(Map(function(x) strsplit(x, ""), word))))
boch
if (debugged) edit(boch)

## define boch.unit ----
boch.unit <- unique(boch)
boch.unit
length(boch.unit)

## take frequencies ----
boch.freq.df <- data.frame(table(boch))
str(boch.freq.df)
#
boch.freqRank <- boch.freq.df[order(boch.freq.df$Freq, decreasing = T), ]
names(boch.freqRank)
head(boch.freqRank, 20)

if (verbose) View(boch.freqRank)


# get character class ----
char.class <- with(boch.freqRank,
                   ifelse(boch %in% v.chars, 2,
                          ifelse(boch %in% semi.v.chars, 3, 1)
                   ))
char.class

## plot freq rank distribution ----

## create directory for freq dist plot ----
freq.plot.dir <- sprintf("plot-freq-chars-ntop%s",
                       max.n.topics)
freq.plot.dir
if ( dir.exists(freq.plot.dir) ) {
   print(sprintf("target dir %s is ready", freq.plot.dir))
} else {
   dir.create(freq.plot.dir)
}
#
plot.out<- function() {
   set.par() # This must be redone.
   plot(log(boch.freqRank$Freq),
     xlab = "Frequency rank",
     col = char.class,
     main = sprintf("Character frequency distribution for (filtered) %d %s words", length(word), lang.name))
   text(log(boch.freqRank$Freq), labels = boch.freqRank$boch, pos = 3)
}
#
save.plot
if (save.plot) {
   ## set target file name ----
   freq.plot.file <- sprintf("plot-freq-%s-chars-ntop%s.png",
                             lang.name, max.n.topics)
   freq.plot.file
   freq.plot.file.path <- paste(freq.plot.dir, freq.plot.file, sep = "/")
   freq.plot.file.path
   if (file.exists(freq.plot.file.path)) {
      freq.plot.file.path <- rename.plot.fn(freq.plot.file.path, date.signature())
      print(sprintf("created a new plot: %s", freq.plot.file.path))
   }
   #
   #set.par()
   png(file = freq.plot.file.path, #res = res.val,
       width = plot.width.val, height = plot.height.val)
   plot.out()
   dev.off()
} else {
   plot.out()
}

## filter out too frequent items ----
cut.val <- 0.99
boch.freqQuantile <- quantile(boch.freqRank$Freq,
                             probs = c(0, 0.5, cut.val, 1.0))
boch.freqQuantile
## remove units gathering frequencies with top 1% of all
boch.Freq.threshold <- as.integer(boch.freqQuantile[3])
boch.Freq.threshold
#
if (freq.filtering) {
   boch.freqRank.included <- subset(boch.freqRank,
                                   Freq < Freq.threshold)
   term.set <- boch.freqRank.included$boch
   boch.freqRank.excluded <- subset(boch.freqRank,
                                   Freq >= Freq.threshold)
   discarded.term.set <- boch.freqRank.excluded$boch
   head(discarded.term.set)
   tail(discarded.term.set)
} else {
   boch.freqRank.included <- boch.freqRank
   boch.freqRank.excluded <- NULL
   term.set <- boch.freqRank$boch
}
#
length(term.set)
head(term.set)
tail(term.set)
length(term.set)

## * save freqRank -----

## create directory for term list ----
term.list.dir <- sprintf("term-list-ntop%s",
                         max.n.topics)
term.list.dir
if ( dir.exists(term.list.dir) ) {
   print(sprintf("target dir %s is ready", term.list.dir))
} else {
   dir.create(term.list.dir)
}
#
out.included <- sprintf("terms-included-freq-ranked-%s.txt", lang.name)
out.excluded <- sprintf("terms-excluded-freq-ranked-%s.txt", lang.name)
#
save.term.list
if (save.term.list) {
   write.table(boch.freqRank.included,
               file = paste(term.list.dir, out.included, sep = "/"),
               row.names = F, quote = F)
   write.table(boch.freqRank.excluded,
               file = paste(term.list.dir, out.excluded, sep = "/"),
               row.names = F, quote = F)
}


## * build dtm ----

if (debugged) {
   View(word)
   View(term.set)
}

## inspection into errors often caused by characters like X_, X.
on.rescue <- F
if (on.rescue) {
   term.count <- lapply(word,
                        function(x) count.keys(unlist(strsplit(x, "")),
                                               term.set))
} else { # proper treatment case
   term.count <- lapply(word,
                        function(x) count.items(unlist(strsplit(x, "")),                                                term.set))
}
## recheck
needs.recheck <- F
if (needs.recheck) {
   term.count <- lapply(Map(function(x) unlist(strsplit(x, "")), word),
                        function(x) count.keys(x, term.set))
   term.count.df <- data.frame(term.count)
   rowSums(term.count.df) == 0
   colnames(term.count.df)[colSums(term.count.df) == 0]
}

str(term.count)
class(term.count)
if (verbose) View(term.count)

## The following needs to come before colSum, rowSums
system.time(dtm <- data.frame(Reduce(rbind, term.count, NULL))) # Crucially
rownames(dtm) = names(word)
if (debugged) {
   class(dtm)
   Map(class, dtm)
}
View(dtm)

## check for validity
table(colSums(dtm))
table(rowSums(dtm))
dtm[rowSums(dtm)==0, ]
##
bad.rows <- dtm[rowSums(dtm)==0, ]
bad.rows$header

## re-check ----
table(rowSums(dtm))

if (verbose) View(dtm)
head(dtm)

## * run LDA ----
require(topicmodels)

LDA.methods <- c("VEM", "Gibbs")
LDA.method <- LDA.methods[1] # Gibbs often causes a trouble
lda <- LDA(dtm, method = LDA.method, k = max.n.topics)
str(lda)
#
n.topics <- topics(lda)
n.topics # list of the number of topics covered by docs

## set constants ----
n.docs <- lda@Dim[1]
n.docs
n.all.docs <- n.docs
n.terms <- lda@Dim[2]
n.terms

## terms selection by topic ----
n.terms.to.select <- 10
term.list <- terms(lda, n.terms.to.select)
term.list

## ** get char encoding -----
terms.enc <- t(posterior(lda)$term)
terms.enc

#
save.term.list
if (save.term.list) {
   term.list.file = paste(term.list.dir, 
                          sprintf("%s-%d-terms-by-%d-topic-%s.txt",
                                  lang.name, n.terms, max.n.topics, lang.name),
                          sep = "/")
   write.table(terms.enc, term.list.file, row.names = T, quote = F)
}

## assign char classes ----
char.class <- ifelse(rownames(terms.enc) %in% v.chars, 2,
                  ifelse(rownames(terms.enc) %in% semi.v.chars, 3, 1)
)

## run tSNE on terms ----
require(Rtsne)

terms.theta.val <- 0.5
terms.ppl.val <- round(max.n.topics * ppl.reduct.rate)
terms.ppl.val
#
terms.tsne <- Rtsne(terms.enc, dims = 3, check_duplicates = F,
                    perplexity = terms.ppl.val,
                    theta = terms.theta.val)
str(terms.tsne)
#
terms.rot <- terms.tsne$Y

## clustering terms ----
if (use.Xmeans) {
   require(clusternor)
   # normalize_input(..) requires Rtsne
   terms.rot.clustered <- Xmeans(normalize_input(terms.rot),
                                 kmax = max.n.topics)
} else {
   require(dbscan)
   terms.eps.val <- 13.0
   terms.minPts.val <- 2
   terms.rot.clustered <- dbscan(terms.rot,
                                 eps = terms.eps.val,
                                 minPts = terms.minPts.val)
}
str(terms.rot.clustered)
terms.cluster <- terms.rot.clustered$cluster
table(terms.cluster)
#
terms.clust.list <- sort(unique(terms.cluster))

## prepare for plot -----
target.unit <- "chars"

## create directory for tSNE plot ----
tSNE.plot.dir <- sprintf(
   "plot-tSNE-%s-ntop%s-%s", target.unit, max.n.topics, clust.method)
tSNE.plot.dir
if ( dir.exists(tSNE.plot.dir) ) {
   print(sprintf("target dir %s is ready", tSNE.plot.dir))
} else {
   dir.create(tSNE.plot.dir)
}

## 2D plot Dim1-Dim2
tsne.terms.main <- sprintf(
   "tSNE (θ=%.2f, ppl=%d) of chars for %s words via LDA (n.top = %d)",
   terms.theta.val, terms.ppl.val, lang.name, max.n.topics)
#
plot.out <- function() {
   set.par() # This must be redone.
   plot(terms.rot[ ,1], terms.rot[ ,2], cex = 0.8,
        #col = char.class,
        col = terms.cluster, main = tsne.terms.main,
        sub = ifelse(use.Xmeans, "",
                     sprintf("DBSCAN eps: %.2f", terms.eps.val)))
   legend("topright", pch ="o", title = "cluster",
          cex = 0.8, inset = c(-0.1,-0.0),
          legend = terms.clust.list, col = terms.clust.list)
   text(terms.rot[ ,1], terms.rot[ ,2], pos = 3, labels = lda@terms)
}
#
save.plot
if (save.plot) {
   ## set target file name
   tSNE.plot.file <- sprintf("plot-tSNE-%s-%s-%s-ntop%s-%s.png",
                             "d1d2", lang.name, target.unit,
                             max.n.topics, clust.method)
   tSNE.plot.file
   gen.png.safely(plot.out(), tSNE.plot.file, tSNE.plot.dir)
} else {
   plot.out()
}

## 2D plot Dim1-Dim3
plot.out <- function() {
   set.par() # This must be redone.
   plot(terms.rot[ ,1], terms.rot[ ,3], cex = 0.8,
        #col = char.class,
        col = terms.cluster, main = tsne.terms.main,
        sub = ifelse(use.Xmeans, "",
                     sprintf("DBSCAN eps: %.2f", terms.eps.val)))
   legend("topright", pch ="o", title = "cluster",
          cex = 0.8, inset = c(-0.10,-0.0),
          legend = terms.clust.list, col = terms.clust.list)
   text(terms.rot[ ,1], terms.rot[ ,3], pos = 3, labels = lda@terms)
}
#
if (save.plot) {
   ## set target file name
   tSNE.plot.file <- sprintf("plot-tSNE-%s-%s-%s-ntop%s-%s.png",
                             "d1d3", lang.name, target.unit,
                             max.n.topics, clust.method)
   tSNE.plot.file
   gen.png.safely(plot.out(), tSNE.plot.file, tSNE.plot.dir)
} else {
   plot.out()
}

## 2D plot Dim2-Dim3
plot.out <- function() {
   set.par() # This must be redone.
   plot(terms.rot[ ,2], terms.rot[ ,3], cex = 0.8,
        #col = char.class,
        col = terms.cluster, main = tsne.terms.main,
        sub = ifelse(use.Xmeans, "",
                     sprintf("DBSCAN eps: %.2f", terms.eps.val)))
   legend("topright", pch ="o", title = "cluster",
          cex = 0.8, inset = c(-0.1,-0.0),
          legend = terms.clust.list, col = terms.clust.list)
   text(terms.rot[ ,2], terms.rot[ ,3], pos = 3, labels = lda@terms)
}
#
if (save.plot) {
   ## set target file name ----
   tSNE.plot.file <- sprintf("plot-tSNE-%s-%s-%s-ntop%s-%s.png",
                             "d2d3", lang.name, target.unit, max.n.topics, clust.method)
   tSNE.plot.file
   gen.png.safely(plot.out(), tSNE.plot.file, tSNE.plot.dir)
} else {
   plot.out()
}

## 3D plot for characters ----

require(rgl)
plot3d.out <- {
   set.par()
   plot3d(terms.rot[ ,1], terms.rot[ ,2], terms.rot[ ,3],
       #col = char.class,
       col = terms.cluster,
       main = tsne.terms.main)
}

## create directory for plot3d ----
tSNE.plot3d.dir <- sprintf("plot3d-tSNE-%s-ntop%s-%s",
                           target.unit, max.n.topics, clust.method)
tSNE.plot3d.dir
if ( dir.exists(tSNE.plot3d.dir) ) {
   print(sprintf("target dir %s is ready", tSNE.plot3d.dir))
} else {
   dir.create(tSNE.plot3d.dir)
}
#
if (save.plot3d) {
   ## set target file name ----
   tSNE.plot3d.file <- sprintf("plot3d-tSNE-%s-%s-ntop%s-%s.png",
                             lang.name, target.unit, max.n.topics, clust.method)
   tSNE.plot3d.file
   tSNE.plot3d.file.path <- paste(tSNE.plot3d.dir, tSNE.plot3d.file, sep = "/")
   tSNE.plot3d.file.path
   if ( file.exists(tSNE.plot3d.file.path) ) {
      tSNE.plot3d.file.path <- rename.plot.fn(tSNE.plot3d.file.path, date.signature())
      print(sprintf("created a new plot: %s", tSNE.plot3d.file.path))
   }
   #writeWebGL(plot3d.out) # obsolete
   rglwidget(tSNE.plot3d.dir, tSNE.plot3d.file)
   #dev.off()
}

## Pause for saving
require(sm)
pause <- function () {
   cat("Pause. Press <Enter> to continue...")
   readline()
   invisible()
}
#pause()

## HC of terms -----
terms.hc <- hclust(dist(terms.enc), "ward.D2")
str(terms.hc)

## set HC titles ----
if (source.sampling) {
   if (freq.filtering) {
      hc.term.main <- sprintf(
         "HC of terms via filtered LDA (n.top=%d) for sample %d words of %s",
         max.n.topics, n.docs, lang.name)  
   } else {
      hc.term.main <- sprintf(
         "HC of terms via unfiltered LDA (n.top=%d) for sample %d words of %s",
         max.n.topics, n.docs, lang.name)   
   }
} else {
   if (freq.filtering) {
      hc.term.main <- sprintf(
         "HC of terms via filtered LDA (n.top=%d) for all %d words of %s",
         max.n.topics, n.all.docs, lang.name)  
   } else {
      hc.term.main <- sprintf(
         "HC of terms via unfiltered LDA (n.top=%d) for all %d words of %s",
         max.n.topics, n.all.docs, lang.name)   
   }
}
hc.term.main

## plot terms hc  ----
require(ape)

## create directory for term HC plot ----
hc.plot.dir <- sprintf("plot-hc-%s-ntop%s-%s",
                         target.unit, max.n.topics, clust.method)
hc.plot.dir
if ( dir.exists(hc.plot.dir) ) {
   print(sprintf("target dir %s is ready", hc.plot.dir))
} else {
   dir.create(hc.plot.dir)
}

# plot phylogenic hc for terms ----
plot.out <- function() {
   set.par() # This must be redone.
   plot(as.phylo(terms.hc), type = 'fan', cex = 1.0,
        cex.main = 1.0,
        main = hc.term.main,
        #col = char.class,
        label.offset = 0.01, tip.color = terms.cluster,
        sub = sprintf("clustered %d characters for %s", n.terms, lang.name))
}
#
if (save.plot) {
   ## set target file name
   hc.plot.file <- sprintf("plot-hc-phylo-%s-%s-ntop%s-%s.png",
                             lang.name, target.unit, max.n.topics, clust.method)
   hc.plot.file
   gen.png.safely(plot.out(), hc.plot.file, hc.plot.dir)
} else {
   plot.out()
}

# plot unrooted hc for terms ----
if (use.unrooted) {
   set.par() # This must be redone.
   plot.out <- function(x) {
      plot(as.phylo(terms.hc), type = 'unrooted', cex = 1.0,
        main = hc.term.main, #col = char.class, # ineffective
        tip.color = terms.cluster,
        sub = sprintf("clustered %d characters for %s", n.terms, lang.name))
   }
   #
   if (save.plot) {
      # set target file name
      hc.plot.file <- sprintf("plot-hc-unrooted-%s-%s-ntop%s-%s.png",
                              lang.name, target.unit, max.n.topics, clust.method)
      hc.plot.file
      gen.png.safely(plot.out(), hc.plot.file, hc.plot.dir)
   } else {
      plot.out()
   }
}

# plot dendrogram hc for terms ----
if (use.dendrogram) {
   plot.out <- function() {
      par(family = eval(rm.fn), xpd = T, mar = c(5,5,5,7),
          cex = 0.8, pch = 19)
      plot(as.dendrogram(terms.hc),
        horiz = T,
        #cex = 1.0,
        #cex.lab = 0.4,
        #col = char.class, # ineffective
        #tip.color = terms.cluster, # ineffective
        #nodePar = list(label.offset = 0.01, tip.color = terms.cluster),
        main = hc.term.main,
        sub = sprintf("clustered %d characters for %s", n.terms, lang.name))
   }
   #
   if (save.plot) {
      ## set target file name
      hc.plot.file <- sprintf("plot-hc-dendro-%s-%s-ntop%s-%s.png",
                              lang.name, target.unit, max.n.topics, clust.method)
      hc.plot.file
      gen.png.safely(plot.out(), hc.plot.file, hc.plot.dir)
   } else {
      plot.out()
   }
}

## ** get word encoding -----

docs.enc <- posterior(lda)$topic # not t(..)
#docs.enc

## doc sampling ----
doc.sample.n <- 110
doc.sampled.id <- sample(1:nrow(docs.enc), doc.sample.n)
docs.enc.sampled <- docs.enc[doc.sampled.id, ]
if (doc.sampling) {
   docs.enc <- docs.enc.sampled
   n.docs <- doc.sample.n
}
str(docs.enc.sampled)
str(docs.enc)

## run tSNE on docs ----
require(Rtsne)
docs.theta.val <- 0.5
docs.ppl.val <- round( max.n.topics * ppl.reduct.rate )
docs.ppl.val
docs.tsne <- Rtsne(docs.enc, dims = 3, check_duplicates = F,
                   perplexity = docs.ppl.val, theta = docs.theta.val)
str(docs.tsne)
#
docs.rot <- docs.tsne$Y
summary(docs.rot)

## docs clustering -----
if (use.Xmeans) {
   require(clusternor)
   # normalize_input(..) requires Rtsne
   docs.rot.clustered <- Xmeans(normalize_input(docs.rot),
                                kmax = max.n.topics)
} else {
   require(dbscan)
   docs.eps.val <- 14.0
   docs.minPts.val <- 2
   docs.rot.clustered <- dbscan(docs.rot,
                                eps = docs.eps.val,
                                minPts = docs.minPts.val)
}
str(docs.rot.clustered)
docs.cluster <- docs.rot.clustered$cluster
table(docs.cluster)
#
docs.clust.list <- sort(unique(docs.cluster))

## set tSNE title -----
if (doc.sampling) {
   tsne.docs.main <- sprintf(
      "tSNE (θ=%.2f, ppl=%d) of sampled %d %s words via LDA (n.top = %d)",
      docs.theta.val, docs.ppl.val, n.docs, lang.name, max.n.topics)
} else {
   tsne.docs.main <- sprintf(
      "tSNE (ppl=%d, θ=%.2f) of %d %s words via LDA (n.top = %d)",
      docs.ppl.val, docs.theta.val, n.docs, lang.name, max.n.topics)
}

## create directory for tSNE plot ----
target.unit <- "words" # Crucially here
tSNE.plot.dir <- sprintf("plot-tSNE-%s-ntop%s-%s",
                       target.unit, max.n.topics, clust.method)
tSNE.plot.dir
if ( dir.exists(tSNE.plot.dir) ) {
   print(sprintf("target dir %s is ready", tSNE.plot.dir))
} else {
   dir.create(tSNE.plot.dir)
}

## 2D plot Dim1-Dim2
#
plot.out <- function() {
   set.par() # This must be redone.
   plot(docs.rot[ ,1], docs.rot[ ,2],  cex = 0.8,
        col = docs.cluster, main = tsne.docs.main,
        sub = ifelse(use.Xmeans, "",
                     sprintf("DBSCAN eps: %.2f", docs.eps.val)))
   legend("topright", pch ="o", title = "cluster",
          cex = 0.8, inset = c(-0.1,-0.0),
          legend = docs.clust.list, col = docs.clust.list)
   text(docs.rot[ ,1], docs.rot[ ,2], pos = 3, cex = 0.6,
        labels = rownames(docs.enc))
}
#
if (save.plot) {
   ## set target file name
   tSNE.plot.file <- sprintf("plot-tSNE-%s-%s-%s-ntop%s-%s.png",
                           "d1d2", lang.name, target.unit, max.n.topics, clust.method)
   gen.png.safely(plot.out(), tSNE.plot.file, tSNE.plot.dir)
} else {
   plot.out()
}

## 2D plot Dim1-Dim3
plot.out <- function() {
   set.par() # This must be redone.
   plot(docs.rot[ ,1], docs.rot[ ,3], cex = 0.8,
        col = docs.cluster, main = tsne.docs.main,
        sub = ifelse(use.Xmeans, "",
                     sprintf("DBSCAN eps: %.2f", docs.eps.val)))
   legend("topright", pch ="o", title = "cluster",
          cex = 0.8, inset = c(-0.1,-0.0),
          legend = docs.clust.list, col = docs.clust.list)
   text(docs.rot[ ,1], docs.rot[ ,3], pos = 3, cex = 0.6,
        labels = rownames(docs.enc))
}
#
if (save.plot) {
   ## set target file name
   tSNE.plot.file <- sprintf("plot-tSNE-%s-%s-%s-ntop%s-%s.png",
                             "d1d3", lang.name, target.unit, max.n.topics, clust.method)
   tSNE.plot.file
   gen.png.safely(plot.out(), tSNE.plot.file, tSNE.plot.dir)
} else {
   plot.out()
}

## 2D plot Dim2-Dim3
plot.out <- function() {
   set.par() # This must be redone.
   plot(docs.rot[ ,2], docs.rot[ ,3], cex = 0.8,
     col = docs.cluster, main = tsne.docs.main,
     sub = ifelse(use.Xmeans, "",
                     sprintf("DBSCAN eps: %.2f", docs.eps.val)))
   legend("topright", pch ="o", title = "cluster",
          cex = 0.8, inset = c(-0.1,-0.0),
          legend = docs.clust.list, col = docs.clust.list)
   text(docs.rot[ ,2], docs.rot[ ,3], pos = 3, cex = 0.6,
        labels = rownames(docs.enc))
}
#
if (save.plot) {
   ## set target file name
   tSNE.plot.file <- sprintf("plot-tSNE-%s-%s-%s-ntop%s-%s.png",
                             "d2d3", lang.name, target.unit, max.n.topics, clust.method)
   tSNE.plot.file
   gen.png.safely(plot.out(), tSNE.plot.file, tSNE.plot.dir)
} else {
   plot.out()
}

## 3D plot for terms ----
require(rgl)
plot3d(docs.rot[ ,1], docs.rot[ ,2], docs.rot[ ,3],
       col = docs.cluster, main = tsne.docs.main)


## ** plot hc for words ----
docs.hc <- hclust(dist(docs.enc), "ward.D2")
str(docs.hc)

## set hc plot titles ----
if (source.sampling) {
   if (freq.filtering) {
      hc.docs.main <- sprintf(
         "HC of words via filtered LDA (n.top=%d) for sampled %d words of %s", max.n.topics, n.docs, lang.name)  
   } else {
      hc.main.text <- sprintf(
         "HC of words via unfiltered LDA (n.top=%d) for sampled %d words of %s", max.n.topics, n.docs, lang.name)   
   }
} else {
   if (freq.filtering) {
      hc.docs.main <- sprintf(
         "HC of words via filtered LDA (n.top=%d) for all %d words of %s",
         max.n.topics, n.all.docs, lang.name)  
   } else {
      hc.docs.main <- sprintf(
         "HC of words via unfiltered LDA (n.top=%d) for all %d words of %s",
         max.n.topics, n.all.docs, lang.name)   
   }
}
hc.docs.main

## plot hc for words ----

## create directory for hc plot for words ----
hc.plot.dir <- sprintf("plot-hc-%s-ntop%s-%s",
                       target.unit, max.n.topics, clust.method)
hc.plot.dir
if ( dir.exists(hc.plot.dir) ) {
   print(sprintf("target dir %s is ready", hc.plot.dir))
} else {
   dir.create(hc.plot.dir)
}


require(ape)

## plot phylogenic tree for words ----
plot.out <- function() {
   set.par() # This must be redone.
   plot(as.phylo(docs.hc), type = 'fan', cex = 0.7,
     label.offset = 0.0002, # needs to very small
     tip.color = docs.cluster,
     main = hc.docs.main,
     sub = sprintf("clustered (sampled) %d words", n.docs))
}
#
if (save.plot) {
   ## set target file name
   hc.plot.file <- sprintf("plot-hc-phylo-%s-%s-ntop%s-%s.png",
                           lang.name, target.unit, max.n.topics, clust.method)
   hc.plot.file
   gen.png.safely(plot.out(), hc.plot.file, hc.plot.dir)
} else {
   plot.out()
}

## plot unrooted tree for words ----
if (use.unrooted) {
   set.par() # This must be redone.
   plot.out <- function() {
      plot(as.phylo(docs.hc), type = 'unrooted', cex = 0.8,
        label.offset = 0.0002, # needs to very small
        tip.color = docs.cluster,
        main = hc.docs.main,
        sub = sprintf("clustered (sampled) %d words", n.docs))
   }
   #
   if (save.plot) {
      ## set target file name
      hc.plot.file <- sprintf("plot-hc-unrooted-%s-%s-ntop%s-%s.png",
                              lang.name, target.unit, max.n.topics, clust.method)
      hc.plot.file
      gen.png.safely(plot.out(), hc.plot.file, hc.plot.dir)
   } else {
      plot.out()
   }
}

## plot dendrogram for docs ----
if (use.dendrogram) {
   plot.out <- function() {
      par(family = eval(rm.fn), xpd = T, mar = c(5,5,5,7),
          cex = 0.4, pch = 19)
      plot(as.dendrogram(docs.hc), horiz = T,
        #tip.color = docs.cluster, # ineffective
        cex.main = 2.0, cex.sub = 2.0, cex.axis = 2.0,
        main = hc.docs.main,
        sub = sprintf("clustered (sampled) %d words", n.docs))
   }
   #
   if (save.plot) {
      ## set target file name
      hc.plot.file <- sprintf("plot-hc-dendro-%s-%s-ntop%s-%s.png",
                              lang.name, target.unit, max.n.topics, clust.method)
      hc.plot.file
      gen.png.safely(plot.out(), hc.plot.file, hc.plot.dir)
   } else {
      plot.out()
   }
   par <- main.par
}
par <- main.par

#if (save.plot) dev.off()


### end of script