######## Regressing out nested random (batch) effect ####################

# Set working directory as needed
#setwd("~/FC-Network-Replicability-Effects")

# Install packages
#install.packages('stringr')
#install.packages('lme4')
#install.packages('lmerTest')

# Packages
library(stringr)
library(lme4)
library(lmerTest)

# Load data
load("Data/processed_data.RData")

# Function to extract random effects and prepare them to be in merged df
extract_random_effect <- function(mdl){
  ran_effect <- ranef(mdl)
  
  # Prepping ID:Site
  ran_effect$`ID:site` <- cbind(rownames(ran_effect$`ID:site`), 
                                data.frame(ran_effect$`ID:site`, 
                                           row.names=NULL))
  ran_effect$`ID:site` <- cbind(data.frame(t(data.frame(
    strsplit(ran_effect$`ID:site`$`rownames(ran_effect$\`ID:site\`)`, split = ":"))),
    ran_effect$`ID:site`$X.Intercept.,row.names = NULL))[,c(1,3)]
  colnames(ran_effect$`ID:site`) <- c("ID","ID_site_effect")
  ran_effect$`ID:site`[,1] <- factor(ran_effect$`ID:site`[,1])
  ran_effect$`ID:site`[,2] <- as.numeric(ran_effect$`ID:site`[,2])
  
  # Prepping site
  ran_effect$site = cbind(rownames(ran_effect$site), 
                          data.frame(ran_effect$site, row.names=NULL))
  colnames(ran_effect$site) <- c("site","site_effect")
  ran_effect$site[,1] <- factor(ran_effect$site[,1])
  ran_effect$site[,2] <- as.numeric(ran_effect$site[,2])
  
  return(ran_effect)
}

# Make list of edges in FC network
edges <- c("MPFC.LP_L","MPFC.LP_R","LP_L.LP_R","MPFC.PCC","LP_L.PCC","LP_R.PCC")

### MPFC.LP_L ### 
# Fit model
MPFC.LP_L <- lmer(MPFC.LP_L ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

### MPFC.LP_R ### 
# Fit model
MPFC.LP_R <- lmer(MPFC.LP_R ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

### LP_L.LP_R ### 
# Fit model
LP_L.LP_R <- lmer(LP_L.LP_R ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                  contrasts = list(pipeline = "contr.sum", 
                                   filter = "contr.sum",
                                   atlas = "contr.sum"))

### MPFC.PCC ### 
# Fit model
MPFC.PCC <- lmer(MPFC.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

### LP_L.PCC ### 
# Fit model
LP_L.PCC <- lmer(LP_L.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

### LP_R.PCC ### 
# Fit model
LP_R.PCC <- lmer(LP_R.PCC ~ pipeline*filter*atlas + (1|site/ID), data = df, 
                 contrasts = list(pipeline = "contr.sum", 
                                  filter = "contr.sum",
                                  atlas = "contr.sum"))

# For loop to extract and subtract out random effects from data
for (i in 1:length(edges)){
  
  mdl <- get(edges[i])
  ran_effect <- extract_random_effect(mdl)
  
  edge <- df[,edges[i]]
  dat <- data.frame(df[,1:5],edge)
  
  # Merge dataframes by site and by ID so everything matches up
  dat <- merge(dat,ran_effect$site,by="site")
  dat <- merge(dat,ran_effect$`ID:site`,by="ID")
  
  # Create new variable that is the regressed out version
  dat$edge_regress <- dat$edge-dat$site_effect-dat$ID_site_effect
  
  # Reduce df down
  dat <- dat[,c(1:5,9)] 
  colnames(dat) <- c(colnames(dat)[1:(length(colnames(dat))-1)],edges[i])
  
  assign(edges[i],dat)
}

# Creating final df
dat.final <- data.frame(MPFC.LP_L,MPFC.LP_R[,6],LP_L.LP_R[,6],
                        MPFC.PCC[,6],MPFC.LP_L[,6],MPFC.LP_R[,6])
colnames(dat.final) <- c(colnames(MPFC.LP_L),edges[-1])

# Exporting
save(dat.final, file = "Data/processed_data_ranef.RData")

