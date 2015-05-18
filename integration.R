library(readr)
library(stringr)
tab = read_delim("counts.bed",  delim="\t", col_names=FALSE)
tab$count <-  as.numeric(str_extract(tab$X1, "\\d+"))
tab$chrom <-  str_extract(tab$X1, "([chr|vector]\\w+)")
tab <-  tab[, c(2,3,4,5)]
colnames(tab) <-  c("start", "end", "count", "chrom")

library(ggplot2)

cutoffs <- seq(1, 1000, 10)

z <-  list()
z <-  unlist(Map(function(x) nrow(subset(tab, count >= x)), cutoffs))

qplot(cutoffs, z) +
  scale_x_log10() +
  xlab("cutoff") +
  ylab("# of integration events") +
  theme_bw() +
    theme(text=element_text(family="Gill Sans MT", size=10))

num_per_chrom <- tab %>% group_by(chrom) %>% summarise(number=n())
ggplot(num_per_chrom, aes(chrom, number)) +
  geom_bar(stat='identity', position='dodge') +
  ylab("number of integrations with count > 10") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90),
        text=element_text(family="Gill Sans MT", size=10))

ggplot(subset(tab, chrom ==  "chr1"), aes(start, count)) +
  geom_point(size=1.25) +
  ylab("integrations") +
  xlab("position") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90),
        text=element_text(family="Gill Sans MT", size=10))
