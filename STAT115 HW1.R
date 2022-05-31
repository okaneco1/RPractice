
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install("sva")

BiocManager::valid()

install.packages(c("ggplot2", "dplyr", "tidyr", "HistData", "mvtnorm",
                   "reticulate"))
library(sva)
library(ggplot2)
library(dplyr)
library(reticulate)
library(mvtnorm)
library(HistData)

# r mean
x1 <- c(1:10, 50)
mean(x1)

x1_na <- c(1:10, 50, NA)
mean(x1_na, na.rm = TRUE)
?mean

#r problem 2
x1_na <- c(1:10, 50, NA)
mean(x1_na, na.rm = TRUE)

data("GaltonFamilies")
head(GaltonFamilies)
?GaltonFamilies

#problem 3a
GaltonFamilies[10,"childHeight"]

#problem 3b
sum(with(GaltonFamilies, gender == "male"))

#problem 3c
nrow(GaltonFamilies)

#problem 3d
mean(GaltonFamilies$childHeight[GaltonFamilies$childNum == "1"])

#problem 3e 
mch <- mean(GaltonFamilies$childHeight[GaltonFamilies$gender == "male"])
fch <- mean(GaltonFamilies$childHeight[GaltonFamilies$gender == "female"])
tab <- matrix(data = c(mch, fch),
       nrow = 2, ncol = 1,)
colnames(tab) <- "Average Child Height"
rownames(tab) <- c("Male", "Female")
tab

#problem 3f
mean(GaltonFamilies$children[!duplicated(GaltonFamilies$family)])

#problem 3g
childHeight_cm <- GaltonFamilies$childHeight*2.54
GF.2 <- cbind(GaltonFamilies, childHeight_cm)
head(GF.2)

#problem 4
set.seed(1234)
N <- 25
ngroups <- 100000
sim_data <- data.frame(group = rep(1:ngroups, each = N),
                       X = rnorm(N * ngroups),
                       Y = rnorm(N * ngroups))

sub <- subset(sim_data, group < 5)
sub %>%
  group_by(group) %>%
  summarise(correlation=cor(X,Y)) 

sim_data %>% 
  group_by(group) %>% 
  summarise(cor=cor(X,Y))

cortable <- sim_data %>% 
  group_by(group) %>% 
  summarise(cor=cor(X,Y))

max(cortable$cor)
#max = 0.8014245

hist(cortable$cor)

#problem 5
which.max(cortable$cor) #99655
sub1 <- subset(sim_data, group == 99655)
ggplot(sub1, aes(X,Y)) + 
  geom_point(color = "slateblue", size = 3)

#problem 6
N <- 100
Sigma <- matrix(c(1, 0.75, 0.75, 1), nrow = 2, ncol = 2) * 1.5
means <- list(c(11, 3), c(9, 5), c(7, 7), c(5, 9), c(3, 11))
dat <- lapply(means, function(mu)
  rmvnorm(N, mu, Sigma))
dat <- as.data.frame(Reduce(rbind, dat)) %>%
  mutate(Z = as.character(rep(seq_along(means), each = N)))
names(dat) <- c("X", "Y", "Z")

#problem6a
cor(dat$X,dat$Y) #-0.7362015

#problem6b
ggplot(dat, aes(X,Y))+
  geom_point(color = "slateblue", size = 3) #negative correlation

#problem6c
groupdat <- dat %>% 
  group_by(Z) %>% 
  summarise(cor=cor(X,Y))
groupdat

#problem6d
ggplot(dat, aes(X,Y,col=Z))+
  geom_point(size=3) +
  scale_color_brewer(palette="Set2")
#group-specific correleations are positive.
  

