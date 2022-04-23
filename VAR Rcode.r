# run entire script at once 
source("TA Session Week 4.R", echo = TRUE)

rm(list=ls())

setwd("//udrive.uw.edu/udrive/rdatta2/Downloads/TA session-Week4")

suppressMessages(library(pkgcond))
suppress_conditions(library(readxl))
suppress_conditions(library(xlsx))
suppress_conditions(library(ggplot2))
suppress_conditions(library(devtools))
suppress_conditions(library(dplyr))
suppress_conditions(library(patchwork))

data1=read_excel("PS2-raw.xlsx",col_names = T)

colnames(data1)=c("DATE","GDP","CPI","UNRATE","FEDFUNDS")

# convert character to date
data1$DATE <- as.Date(data1$DATE, "%d/%m/%Y")

# plot data in levels
GDP_Level=ggplot(data=data1,aes(x=DATE, y=GDP)) +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("GDP")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

CPI_Level=ggplot(data=data1,aes(x=DATE, y=CPI)) +
  geom_line(col="green4") +
  theme_bw() +
  ggtitle("CPI")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))
UNRATE_Level=ggplot(data=data1,aes(x=DATE, y=UNRATE)) +
  geom_line(col="firebrick1") +
  theme_bw() +
  ggtitle("UNRATE")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

FEDFUNDS_Level=ggplot(data=data1,aes(x=DATE, y=FEDFUNDS)) +
  geom_line(col="cornflowerblue") +
  theme_bw() +
  ggtitle("FEDFUNDS")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

panel1=GDP_Level + CPI_Level
panel2=UNRATE_Level + FEDFUNDS_Level
fig_inlevels=panel1 / panel2

fig_inlevels
ggsave("Data in Levels.png", fig_inlevels, width=6, height=6)


# clear screen
cat("\014") 

# HP filter currently commented out
if(0) 
# plot data post hp filter
{suppress_conditions(library(mFilter))

GDP_dt=hpfilter(data1$GDP,freq=1600,type="lambda")
GDP_cycle=GDP_dt$cycle

CPI_cycle=hpfilter(data1$CPI,freq=1600,type="lambda")$cycle

UN_cycle=hpfilter(data1$UNRATE,freq=1600,type="lambda")$cycle

FED_cycle=hpfilter(data1$FEDFUNDS,freq=1600,type="lambda")$cycle

data_cycle=data.frame(data1$DATE,GDP_cycle,CPI_cycle,
                      UN_cycle,FED_cycle)
colnames(data_cycle)=c("DATE","GDP","CPI","UNRATE","FEDFUNDS")

data_cycle$DATE=format.Date(data1$DATE, "%d/%m/%Y")

GDP_cycle=ggplot(data=data1,aes(x=DATE, y=GDP)) +
  geom_hline(yintercept = 0, color="black") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("GDP")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

CPI_cycle=ggplot(data=data1,aes(x=DATE, y=CPI)) +
  geom_hline(yintercept = 0, color="black") +
  geom_line(col="green4") +
  theme_bw() +
  ggtitle("CPI")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

UNRATE_cycle=ggplot(data=data1,aes(x=DATE, y=UNRATE)) +
  geom_hline(yintercept = 0, color="black") +
  geom_line(col="firebrick1") +
  theme_bw() +
  ggtitle("UNRATE")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

FEDFUNDS_cycle=ggplot(data=data1,aes(x=DATE, y=FEDFUNDS)) +
  geom_hline(yintercept = 0, color="black") +
  geom_line(col="cornflowerblue") +
  theme_bw() +
  ggtitle("FEDFUNDS")+
  ylab("Value")+
  xlab("Year") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

panel1=GDP_cycle + CPI_cycle
panel2=UNRATE_cycle + FEDFUNDS_cycle
fig_inlevels=panel1 / panel2

fig_inlevels
ggsave("HP filtered data.png", fig_inlevels, width=6, height=6)


write.xlsx2(x = data_cycle,file = "PS2-HPFiltered.xlsx",row.names = F)

# read hp filtered data for VAR with HP Filter 
data1=read_excel("PS2-HPFiltered.xlsx",col_names = T)

colnames(data1)=c("DATE","GDP","CPI","UNRATE","FEDFUNDS")

}

# collect the data and convert to time series object
suppress_conditions(library(zoo))

data1$DATE= as.yearqtr(x = data1$DATE,format = "%Y:0%q")

GDP=ts(data = data1$GDP,
       start = c(1980, 1), 
       end = c(2020, 4), 
       frequency = 4)

CPI=ts(data = data1$CPI,
       start = c(1980, 1), 
       end = c(2020, 4), 
       frequency = 4)

UNRATE=ts(data = data1$UNRATE,
          start = c(1980, 1), 
          end = c(2020, 4), 
          frequency = 4)

FEDFUNDS=ts(data = data1$FEDFUNDS,
            start = c(1980, 1), 
            end = c(2020, 4), 
            frequency = 4)


VAR_data <- window(ts.union(GDP,CPI,UNRATE,FEDFUNDS),
                   start = c(1980, 1), end = c(2020, 4))

## Estimating VAR( with lag selected by information ordering
suppress_conditions(library(vars))
VAR_sel_crit = VARselect(VAR_data)
VAR_est <-VAR(y = VAR_data,p = VAR_sel_crit$selection[1],type = "none")


rm(list=setdiff(ls(), c("VAR_est","GDP","CPI","UNRATE","FEDFUNDS","VAR_sel_crit")))


# collect and report coefficients in a table
p=VAR_sel_crit$selection[1]
names_model=c("GDP","CPI","UN","FED")
custom_coeff_names=numeric()
for(i in 1:p)
{for(j in 1:length(names_model))
  custom_coeff_names=c(custom_coeff_names,
                       paste(names_model[j],"(-",i,")",sep=""))
}

suppress_conditions(library(texreg))

suppress_conditions(texreg(list(VAR_est$varresult$GDP,
            VAR_est$varresult$CPI,
            VAR_est$varresult$UNRATE,
            VAR_est$varresult$FEDFUNDS),
       dcolumn = F, booktabs = F,
       use.packages = FALSE, label = "Question 2.2", 
       caption = "Estimated Coefficients Matrix $\\widehat{A}$",float.pos = "h",
       digits = 5, leading.zero = T,
       custom.model.names =names_model,
       custom.coef.names=custom_coeff_names,
       no.margin=T,file = "TableOfCoefficients.txt"))


VAR_sum=summary(VAR_est)


#residuals matrix
VAR_resid=data.frame(VAR_sum$varresult$GDP$residuals,
                     VAR_sum$varresult$CPI$residuals,
                     VAR_sum$varresult$UNRATE$residuals,
                     VAR_sum$varresult$FEDFUNDS$residuals)

colnames(VAR_resid)=c("GDP","CPI","UNRATE","FEDFUNDS")

#covariance matrix of residuals
VAR_estcov=data.frame(VAR_sum$covres)
colnames(VAR_estcov)=c("GDP","CPI","UNRATE","FEDFUNDS")
rownames(VAR_estcov)=c("GDP","CPI","UNRATE","FEDFUNDS")

suppress_conditions(library(knitr))

sink("CovarianceMatrix-of-Residuals.txt")
kable(x = VAR_estcov,format = "latex",digits = 3,
      caption ="Covariance matrix of $\\widehat{\\Sigma}$",
      align = 'c', label = "CovMAtrixQuestion2.2",
      booktabs=TRUE)
sink()



#Cholesky Decomposition in lower triangular form
VAR_chol_cov=t(chol(VAR_estcov))
sink("CholeskyDecomposition-of-residuals-matrix.txt")
kable(x = VAR_chol_cov,format = "latex",digits = 3,
      caption ="Cholesky square root of $\\widehat{\\Sigma}$",
      align = 'c', label = "Question2.3",
      booktabs=TRUE)
sink()


###############################################
###  Plot IRFS to FEDFUNDS shock  #############
###############################################


rm(list=setdiff(ls(), c("VAR_est","GDP","CPI","UNRATE","FEDFUNDS","VAR_sel_crit")))

suppress_conditions(source_url("https://raw.githubusercontent.com/anguyen1210/var-tools/master/R/extract_varirf.R"))

# to reproduce results since bootstrap is set to TRUE for generating IRFS
set.seed(0)


irf_all <- irf(VAR_est, impulse = "FEDFUNDS", n.ahead = 25, 
               ortho = TRUE,cumulative = FALSE, 
               boot = TRUE, ci = 0.95, runs = 50)


multiple_varirf <- extract_varirf(irf_all)


asy_gdp <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_gdp, 
             ymin=lower_fedfunds_gdp, 
             ymax=upper_fedfunds_gdp)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("GDP")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_cpi <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_cpi, 
             ymin=lower_fedfunds_cpi, 
             ymax=upper_fedfunds_cpi)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("CPI")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_un <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_unrate, 
             ymin=lower_fedfunds_unrate, 
             ymax=upper_fedfunds_unrate)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("UNRATE")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_fed <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_fedfunds, 
             ymin=lower_fedfunds_fedfunds, 
             ymax=upper_fedfunds_fedfunds)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("FEDFUNDS")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))


p_irf1=asy_gdp + asy_cpi
p_irf2=asy_un + asy_fed
p_irf=p_irf1 / p_irf2

p_irf
ggsave("IRFS to a 1 percent monetary shock.png", p_irf, width=6, height=6)

###############################################
###  Plot IRFS to GDP shock  ###############
###############################################

irf_all <- irf(VAR_est, impulse = "GDP", n.ahead = 25, 
               ortho = TRUE,cumulative = FALSE, 
               boot = TRUE, ci = 0.95, runs = 50)


multiple_varirf <- extract_varirf(irf_all)


asy_gdp <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_gdp_gdp, 
             ymin=lower_gdp_gdp, 
             ymax=upper_gdp_gdp)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="plum1", alpha=.2, color="violetred3", linetype="dashed") +
  geom_line(col="violetred4") +
  theme_bw() +
  ggtitle("GDP")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_cpi <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_gdp_cpi, 
             ymin=lower_gdp_cpi, 
             ymax=upper_gdp_cpi)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="plum1", alpha=.2, color="violetred3", linetype="dashed") +
  geom_line(col="violetred4") +
  theme_bw() +
  ggtitle("CPI")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_un <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_gdp_unrate, 
             ymin=lower_gdp_unrate, 
             ymax=upper_gdp_unrate)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="plum1", alpha=.2, color="violetred3", linetype="dashed") +
  geom_line(col="violetred4") +
  theme_bw() +
  ggtitle("UNRATE")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_fed <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_gdp_fedfunds, 
             ymin=lower_gdp_fedfunds, 
             ymax=upper_gdp_fedfunds)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="plum1", alpha=.2, color="violetred3", linetype="dashed") +
  geom_line(col="violetred4") +
  theme_bw() +
  ggtitle("FEDFUNDS")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))


p_irf1=asy_gdp + asy_cpi
p_irf2=asy_un + asy_fed
p_irf=p_irf1 / p_irf2

p_irf
ggsave("IRFS to a 1 percent GDP shock.png", p_irf, width=6, height=6)

###############################################
###  Plot IRFS to CPI shock  ##################
###############################################

irf_all <- irf(VAR_est, impulse = "CPI", n.ahead = 25, 
               ortho = TRUE,cumulative = FALSE, 
               boot = TRUE, ci = 0.95, runs = 50)


multiple_varirf <- extract_varirf(irf_all)


asy_gdp <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_gdp, 
             ymin=lower_cpi_gdp, 
             ymax=upper_cpi_gdp)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="darksalmon", alpha=.2, color="firebrick", linetype="dashed") +
  geom_line(col="firebrick4") +
  theme_bw() +
  ggtitle("GDP")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_cpi <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_cpi, 
             ymin=lower_cpi_cpi, 
             ymax=upper_cpi_cpi)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="darksalmon", alpha=.2, color="firebrick", linetype="dashed") +
  geom_line(col="firebrick4") +
  theme_bw() +
  ggtitle("CPI")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_un <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_unrate, 
             ymin=lower_cpi_unrate, 
             ymax=upper_cpi_unrate)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="darksalmon", alpha=.2, color="firebrick", linetype="dashed") +
  geom_line(col="firebrick4") +
  theme_bw() +
  ggtitle("UNRATE")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_fed <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_cpi_fedfunds, 
             ymin=lower_cpi_fedfunds, 
             ymax=upper_cpi_fedfunds)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="darksalmon", alpha=.2, color="firebrick", linetype="dashed") +
  geom_line(col="firebrick4") +
  theme_bw() +
  ggtitle("FEDFUNDS")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))


p_irf1=asy_gdp + asy_cpi
p_irf2=asy_un + asy_fed
p_irf=p_irf1 / p_irf2

p_irf
ggsave("IRFS to a 1 percent CPI shock.png", p_irf, width=6, height=6)



###############################################
###  Plot IRFS to UNRATE shock  ###############
###############################################

irf_all <- irf(VAR_est, impulse = "UNRATE", n.ahead = 25, 
               ortho = TRUE,cumulative = FALSE, 
               boot = TRUE, ci = 0.95, runs = 50)


multiple_varirf <- extract_varirf(irf_all)


asy_gdp <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_unrate_gdp, 
             ymin=lower_unrate_gdp, 
             ymax=upper_unrate_gdp)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="turquoise", alpha=.2, color="turquoise3", linetype="dashed") +
  geom_line(col="turquoise4") +
  theme_bw() +
  ggtitle("GDP")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_cpi <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_unrate_cpi, 
             ymin=lower_unrate_cpi, 
             ymax=upper_unrate_cpi)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="turquoise", alpha=.2, color="turquoise3", linetype="dashed") +
  geom_line(col="turquoise4") +
  theme_bw() +
  ggtitle("CPI")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_un <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_unrate_unrate, 
             ymin=lower_unrate_unrate, 
             ymax=upper_unrate_unrate)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="turquoise", alpha=.2, color="turquoise3", linetype="dashed") +
  geom_line(col="turquoise4") +
  theme_bw() +
  ggtitle("UNRATE")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_fed <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_unrate_fedfunds, 
             ymin=lower_unrate_fedfunds, 
             ymax=upper_unrate_fedfunds)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="turquoise", alpha=.2, color="turquoise3", linetype="dashed") +
  geom_line(col="turquoise4") +
  theme_bw() +
  ggtitle("FEDFUNDS")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))


p_irf1=asy_gdp + asy_cpi
p_irf2=asy_un + asy_fed
p_irf=p_irf1 / p_irf2

p_irf
ggsave("IRFS to a 1 percent UNRATE shock.png", p_irf, width=6, height=6)





###############################################
## Is there a structural break around 2000 ####
###############################################

# On 1980-2000
VAR_1980_2000 <- window(ts.union(GDP,CPI,UNRATE,FEDFUNDS),
                        start = c(1980, 1), end = c(1999, 4))
VAR_est1980_2000 <-VAR(y = VAR_1980_2000,p = 4,type = "none")


irf_all <- irf(VAR_est1980_2000, impulse = "FEDFUNDS", n.ahead = 25, 
               ortho = TRUE,cumulative = FALSE, 
               boot = TRUE, ci = 0.95, runs = 50)

multiple_varirf <- extract_varirf(irf_all)


asy_gdp1 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_gdp, 
             ymin=lower_fedfunds_gdp, 
             ymax=upper_fedfunds_gdp)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("GDP-1980-2000")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_cpi1 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_cpi, 
             ymin=lower_fedfunds_cpi, 
             ymax=upper_fedfunds_cpi)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("CPI-1980-2000")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_un1 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_unrate, 
             ymin=lower_fedfunds_unrate, 
             ymax=upper_fedfunds_unrate)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("UNRATE-1980-2000")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_fed1 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_fedfunds, 
             ymin=lower_fedfunds_fedfunds, 
             ymax=upper_fedfunds_fedfunds)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightcyan3", alpha=.2, color="blue3", linetype="dashed") +
  geom_line(col="blueviolet") +
  theme_bw() +
  ggtitle("FEDFUNDS-1980-2000")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))


p_irf1=asy_gdp1 + asy_cpi1
p_irf2=asy_un1 + asy_fed1
p_irf=p_irf1 / p_irf2

p_irf
ggsave("1980-2000-IRFs-to 1 percent monetary shock.png", p_irf, width=6, height=6)



# On 2000-2020
VAR_2000_2020 <- window(ts.union(GDP,CPI,UNRATE,FEDFUNDS),
                        start = c(2000, 1), end = c(2020, 4))

VAR_est2000_2020 <-VAR(y = VAR_2000_2020,p = 4,type = "none")


irf_all <- irf(VAR_est2000_2020, impulse = "FEDFUNDS", n.ahead = 25, 
               ortho = TRUE,cumulative = FALSE, 
               boot = TRUE, ci = 0.95, runs = 50)

multiple_varirf <- extract_varirf(irf_all)


asy_gdp2 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_gdp, 
             ymin=lower_fedfunds_gdp, 
             ymax=upper_fedfunds_gdp)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightgreen", alpha=.2, color="green3", linetype="dashed") +
  geom_line(col="forestgreen") +
  theme_bw() +
  ggtitle("GDP-2000-2020")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_cpi2 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_cpi, 
             ymin=lower_fedfunds_cpi, 
             ymax=upper_fedfunds_cpi)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightgreen", alpha=.2, color="green3", linetype="dashed") +
  geom_line(col="forestgreen") +
  theme_bw() +
  ggtitle("CPI-2000-2020")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_un2 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_unrate, 
             ymin=lower_fedfunds_unrate, 
             ymax=upper_fedfunds_unrate)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightgreen", alpha=.2, color="green3", linetype="dashed") +
  geom_line(col="forestgreen") +
  theme_bw() +
  ggtitle("UNRATE-2000-2020")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))

asy_fed2 <- multiple_varirf %>% 
  ggplot(aes(x=period, y=irf_fedfunds_fedfunds, 
             ymin=lower_fedfunds_fedfunds, 
             ymax=upper_fedfunds_fedfunds)) +
  geom_hline(yintercept = 0, color="black") +
  geom_ribbon(fill="lightgreen", alpha=.2, color="green3", linetype="dashed") +
  geom_line(col="forestgreen") +
  theme_bw() +
  ggtitle("FEDFUNDS-2000-2020")+
  ylab("Value")+
  xlab("Quarter") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        axis.title.y = element_text(size=11))


p_irf1=asy_gdp2 + asy_cpi2
p_irf2=asy_un2 + asy_fed2
p2_irf=p_irf1 / p_irf2

p2_irf
ggsave("2000-2020-IRFs-to 1 percent monetary shock.png", p2_irf, width=6, height=6)


###############################################
##### Changing the ordering ###################
###############################################

VAR_data <- window(ts.union(GDP,UNRATE,CPI,FEDFUNDS),
                   start = c(1980, 1), end = c(2020, 4))

VAR_sel_crit = VARselect(VAR_data)
VAR_est <-VAR(y = VAR_data,p = VAR_sel_crit$selection[1],
              type = "none")


###############################################
##### # check if variables are stationary #####
###############################################


suppressMessages(library(tseries))
sink("Stationarity-using ADF Test.txt")
adf.test(GDP)
cat("\n*********************\n")
adf.test(CPI)
cat("\n*********************\n")
adf.test(UNRATE)
cat("\n*********************\n")
adf.test(FEDFUNDS)
sink()

###############################################
##### Everything else is same #################
###############################################


## In case above url no longer active
################################################################################
# -------------------------------------------------------------------------------
# `extract_varirf()` extracts the impulse reponse vector, along with the upper and 
# lower confidence interval vectors, created by the `irf()` function in the `vars`
# package and puts them into a tidy dataframe that allows for easier 
# impulse-reponse function plotting, particularly with the ggplot2. `extract_varirf()`
# accepts single or multiple 'varirf' list objects created by `irf()`, provided they 
# are created from the same dataset and of the same length. For additional details
# and examples of usage, please consult:
# mentalbreaks.rbind.io/posts/impulse-reponse-plots-with-vars-and-ggplot2
# 
# @anguyen1210
# -------------------------------------------------------------------------------

extract_varirf <- function(...){
  
  varirf_object <- list(...) #list one or more varirf input objects
  
  get_vec_length <- function(list_item){nrow(list_item[[1]][[1]])}
  
  if (!("varirf" %in% mapply(class, varirf_object))){
    stop("this function only accepts 'varirf' class objects")
  }
  
  if (length(unique(mapply(class, varirf_object)))!=1){
    stop("all input items must be 'varirf' class objects")
  }    
  if (length(unique(mapply(get_vec_length, varirf_object)))!=1){
    stop("all irf vectors must have the same length")   
  }  
  
  period <- as.data.frame(0:(nrow(varirf_object[[1]][[1]][[1]])-1)) 
  names(period) <- "period"
  
  for (l in 1:length(varirf_object)){
    for (i in 1:3){
      for (j in 1:dim(varirf_object[[l]][[i]][[1]])[2]){
        for (k in 1:length(varirf_object[[l]][[1]])){
          temp_colname <- paste(names(varirf_object[[l]][i]), #vector type (irf, lower, or upper)
                                names(varirf_object[[l]][[i]])[k], #impulse name
                                colnames(varirf_object[[l]][[i]][[k]])[j], #response name
                                sep = "_")
          
          temp <- as.data.frame(varirf_object[[l]][[i]][[k]][, j]) #extracts the vector
          
          names(temp) <- temp_colname #add the column name (vectortype_impulse_reponse)
          period <- cbind(period, temp) 
        }
        
      }
    }
  }
  names(period) <- tolower(names(period))
  return(period)
}
#######################################################################
#########













