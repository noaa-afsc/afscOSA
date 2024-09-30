

# install.packages("pak")
# pak::pkg_install("r4ss/r4ss")
library(r4ss)

# other afscOSA package dependencies:
library(ggplot2)
library(cowplot)
library(here)
library(dplyr)
library(reshape2)
library(here)

library(afscOSA)

# create directory for analysis
# out_path <- "test_aicod"
if(!exists("out_path")) out_path = getwd()
if(!dir.exists(out_path)) dir.create(out_path)

# copy all data files to working directory
pkg_path <- find.package('afscOSA')
example_data_files <- list.files(path = file.path(pkg_path, "inst", "examples", "AI_PCOD"))
example_data_files
file.copy(from = file.path(path = file.path(pkg_path, "inst", "examples", "AI_PCOD"),
                           example_data_files),
          to = file.path(file.path(out_path), example_data_files),
          overwrite = TRUE)

setwd(out_path)

sx = 1 # USER INPUT define sex
fleet = c(1,2) # USER INPUT define fleets
model_path <- c("inst/examples/AI_PCOD")

mod <- r4ss::SSgetoutput(dirvec = model_path)

# comps for the fleets defined in "fleet" and "sx"
comps <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16:18)])
comps <- comps[comps$Fleet %in% fleet & comps$Sex %in% sx, ]
comps <- reshape2::melt(comps,id.vars = c('Yr','Fleet','Sex','Bin'))

# effective sample sizes for the fleets defined in "fleet" and "sx"
Neffdf <- as.data.frame(mod[[1]]$lendbase[,c(1,6,13,16,22)])
Neffdf <- Neffdf[Neffdf$Bin == min(Neffdf$Bin),]

# length bins
lens <- sort(unique(comps$Bin))

# fishery (fleet 1) ----

flt <- 1 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Fishery F and Fishery M)
tmp <- comps[comps$Fleet==flt,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out1 <- afscOSA::run_osa(fleet = 'Fishery', index_label = 'Length',
                         obs = obs, exp = exp, Neff = Neff, index = lens, years = yrs)


# AI bottom trawl survey (fleet 2) ----

flt <- 2 # USER INPUT

# this is a 1 sex model but if it had sex structure you would define each sex as
# a separate fleet (e.g., Survey F and Survey M)
tmp <- comps[comps$Fleet==flt,]

# effective sample sizes (vector)
Neff <- Neffdf$effN[Neffdf$Fleet==flt]

# observed values -> put in matrix format (nrow = nyr, ncol = age/length)
obs <- tmp[tmp$variable=='Obs',]
obs <- reshape2::dcast(obs, Yr~Bin, value.var = "value")
yrs <- obs$Yr # years sampled
obs <- as.matrix(obs[,-1])

# expected values -> put in matrix format (nrow = nyr, ncol = age/length
exp <- tmp[tmp$variable=='Exp',]
exp <- reshape2::dcast(exp, Yr~Bin, value.var = "value")
exp <- as.matrix(exp[,-1])

# should all be true!
length(Neff) == length(yrs); length(Neff) == nrow(obs); nrow(obs) == nrow(exp)
ncol(obs);ncol(exp);length(lens)

out2 <- afscOSA::run_osa(fleet = 'AI Trawl Survey', index_label = 'Length',
                         obs = obs, exp = exp, Neff = Neff, index = lens, years = yrs)

# plot results ----
input <- list(out1, out2)
osaplots <- plot_osa(input)
osaplots$bubble
osaplots$qq
osaplots$aggcomp

# old way of plotting ----
ages=seq(1,13,1)
lengths=seq(.5,142.5,by=1)

index=lengths
library(here)
library(maditr)
library(compResidual)
library(cowplot)
library(ggplot2)

model=Dir_M24_1a;lengths=seq(.5,142.5,by=1);fleet=2; sx=1; stck='AI_COD';surv='AI'

mods1<-r4ss::SSgetoutput(dirvec=model)
age<-data.table::data.table(mods1[[1]]$lendbase[,c(1,6,13,16:19)])[Bin%in%lengths & Fleet==fleet &Sex==sx]
age<-data.table::data.table(melt(age,c('Yr','Fleet','Sex','Bin')))
o<-age[variable=='Obs']
o<-maditr::dcast(o,Yr~Bin)
p<-age[variable=='Exp']
p<-maditr::dcast(p,Yr~Bin)
pearson<-age[variable=='Pearson']
pearson<-maditr::dcast(pearson,Yr~Bin)
years<-o$Yr
o <- as.matrix(o[,-1])
p <- as.matrix(p[,-1])
pearson <- as.matrix(pearson[,-1])
Neff<-round(data.table::data.table(mods1[[1]]$lendbase)[Bin==lengths[1]& Fleet==fleet & Sex==sx]$effN)

obs=o
exp=p

res <- resMulti(t(o), t(p))
if(!all(is.finite(res))){
  warning("failed to calculate OSA residuals for ", stock)
  return(NULL)
}
plot(res)
## compare to Pearson side by side
index_label="Lengths"
mat <- t(matrix(res, nrow=nrow(res), ncol=ncol(res)))
dimnames(mat) <- list(year=years, index=index[-1])
reslong <- reshape2::melt(mat, value.name='resid')
g1 <- ggplot(reslong, aes(year, index, size=abs(resid),
                          color=resid>0)) + geom_point() +
  ggtitle(paste0('Model 24.1a OSA w/o ', index_label, ' 1')) + ylim(range(index)) +
  ylab(index_label)+theme_bw()
dimnames(pearson) <- list(year=years, index=index)
pearsonlong <- reshape2::melt(pearson, value.name='resid')
g2 <- ggplot(pearsonlong, aes(year, index, size=abs(resid),
                              color=resid>0)) + geom_point() +
  ggtitle('Pearson') + ylab(index_label)+theme_bw()
print(cowplot::plot_grid(g1,g2, nrow=2))
print(cowplot::plot_grid(g1, nrow=1))

