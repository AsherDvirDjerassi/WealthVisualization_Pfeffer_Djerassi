##############################################################################
################################### 1) Set-up ################################
##############################################################################
#############################
##### packages and libs #####
#############################

# install.packages("shinythemes", "highcharter",  "reldist", "coda", "kdensity", "reldist", "plotrix", "directlabels", "sjlabelled")
# detach(package:plyr) #group by does not work if this is operating
library("coda")
library("shinythemes")
library("highcharter")
library("scales")
library("RColorBrewer")
library("ggthemes")
library("ggplot2")
library("grid")
library("markdown")
library("ggExtra")
library("devtools")
library("readxl")   #for reading xlsx files 
library("reldist")  #for computing weighted statistics 
library("dplyr")
library("kableExtra")
library("kdensity")
library("plotrix")
library("reshape2")
library("tidyr")
library("Hmisc")
library("haven") # import dta files into r
library("directlabels") # label ggplot in plot
library("sjlabelled")
library("highcharter") # For highcharter visualization

options(scipen=999) # this remove scientific notation from R output

#############################
######### Load SCF ##########
#############################

scf <- read.csv(file='/Users/asherd/Library/Mobile Documents/com~apple~CloudDocs/0. projects/2. SCF/3. Racial Wealth Gap Vizualization/1. Data /scf.csv', header = TRUE, row.names = 1) 

#############################
##### Load Forbes 400  ######
#############################

forbes_400 <- read.csv("/Users/asherd/Library/Mobile Documents/com~apple~CloudDocs/0. projects/2. SCF/3. Racial Wealth Gap Vizualization/1. Data /forbes_400.csv")

#############################
### merge Forbes & SCF  #####
#############################

# create forbes indicator
forbes_400$Forbes_indicator <- 1
scf$Forbes_indicator <- 0

# keep only networth in 2019 USD
forbes_400$NETWORTH <- forbes_400$NETWORTH_2019
forbes_400$NETWORTH_2019 <- NULL

# race - unknown 
forbes_400$RACE <- NA

# age - 999 - unknown
forbes_400$AGE <- 999

scf_non_forbes <- scf
scf <- dplyr::bind_rows(scf, forbes_400[c("NETWORTH","YEAR","WGT", "RACE", "AGE", "Forbes_indicator")])

# subset 2019
scf_2019_forbes <- subset(scf, YEAR == 2019)



##############################################################################
####################### 2) FIGURE 1 - Wealth Thresholds ######################
##############################################################################

#########################
##### Create Data  ######
#########################

df_quantiles <- data.frame(matrix (nrow = 100, ncol = 0, byrow = 1))
df_quantiles$quantiles <-  c(seq(1, 99, by = 1), "Forbes 400")
df_quantiles$quantiles[100] <- c("Forbes 400")
df_quantiles$Year <- rep(unique(scf_2019_forbes$YEAR))

# forbes
df_quantiles$"Forbes 400" <- 0
df_quantiles$"Forbes 400"[100] <- min(subset(forbes_400$NETWORTH, forbes_400$YEAR == 2019))
df_quantiles$Thresholds <- c(wtd.quantile(subset(scf_2019_forbes$NETWORTH,scf_2019_forbes$Forbes_indicator == 0), seq(.01, .99, by = .01), weight = subset(scf_2019_forbes$WGT,scf_2019_forbes$Forbes_indicator == 0)),NA)

for (raceid in c("Total wealth")){
  for (i in 1:100) {
    df_quantiles[i,raceid] <-  sum(
      subset(scf_2019_forbes$NETWORTH*scf_2019_forbes$WGT, scf_2019_forbes$NETWORTH >= df_quantiles$Thresholds[i] &
               scf_2019_forbes$NETWORTH < df_quantiles$Thresholds[i+1] & scf_2019_forbes$Forbes_indicator == 0)
    )
  }
}


#########################
## Highcharter Options ##
#########################

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 0, thousandsSep = ',')))
lang <- getOption("highcharter.lang")
lang$numericSymbols <- c(" Thousand"," Million"," Billion"," Trillion")
lang$thousandsSep <- ","
options(highcharter.lang = lang)


#########################
#######  Figure 1 #######
#########################

highchart() %>%
  hc_chart(type ="column",
           barBorderWidth = 0) %>%
  hc_title(text = "Figure 1: Wealth thresholds by percentile and the Forbes 400") %>%
  hc_chart(zoomType = "x") %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    groupPadding = 0,
    pointPadding = 0,
    enableMouseTracking = TRUE)) %>% 
  hc_yAxis_multiples(
    list(title = list(text = "Total Wealth in 2019 USD")),
    list(title = list(text = "Total Wealth in 2019 USD"),
         opposite = TRUE)) %>%
  hc_xAxis(categories = df_quantiles$quantiles) %>%
  hc_add_series(data = round(df_quantiles$Thresholds), name = "Wealth thresholds", color = "grey", showInLegend = FALSE, visible = TRUE) %>%
  hc_add_series(data = round(df_quantiles$`Forbes 400`), name = "<p style='font-size:15px; color:black'>Forbes 400 wealth threshold.</p>", color = "red", showInLegend = TRUE, visible = FALSE,  fontSize = '200px') %>%
  hc_xAxis(title = list(text = "Wealth percentiles")) %>%
  hc_legend(
    align = "right",
    verticalAlign = "top"  
    ) %>%
  #hc_legend(title = list(text = ""), enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)


############################################################################### 
######################## 3) FIGURE 2 - Cumulative  ############################
###############################################################################

#############################
#####   Create Data    ######
#############################

#############################
#######   Wide Data   #######
#############################

df_quantiles <- data.frame(matrix (nrow = 21, ncol = 0, byrow = 1))
df_quantiles$quantiles <-  c(seq(.05, 1, by = .05), 21)
df_quantiles$Year <- rep(unique(scf_2019_forbes$YEAR))
df_quantiles$Thresholds <- c(min(scf_2019_forbes$NETWORTH), 
                               wtd.quantile(subset(scf_2019_forbes$NETWORTH,scf_2019_forbes$Forbes_indicator == 0), seq(.05, .95, by = .05), weight = subset(scf_2019_forbes$WGT,scf_2019_forbes$Forbes_indicator == 0)), 
                               max(subset(forbes_400$NETWORTH, forbes_400$YEAR == 2019)))
df_quantiles$White <- NA 
df_quantiles$Black <- NA 
df_quantiles$Other <- NA 
df_quantiles$Hispanic <- NA 

for (raceid in c("White", "Black", "Other", "Hispanic")){
  for (i in 1:20) {
      df_quantiles[i,raceid] <-  sum(
        subset(scf_2019_forbes$NETWORTH*scf_2019_forbes$WGT, scf_2019_forbes$NETWORTH >= df_quantiles$Thresholds[i] &
               scf_2019_forbes$NETWORTH < df_quantiles$Thresholds[i+1] & scf_2019_forbes$RACE == raceid & scf_2019_forbes$Forbes_indicator == 0)
    )
  }
}

df_quantiles$Forbes_400 <- 0
df_quantiles$Forbes_400[20] <- sum(subset(scf_2019_forbes$NETWORTH, scf_2019_forbes$Forbes_indicator ==1))
df_quantiles$Total_Wealth <-  df_quantiles$White + df_quantiles$Black 
df_quantiles <- df_quantiles[-c(21), ] 

df_quantiles$"White wealth if demographically proportional" <- 
  df_quantiles$Total_Wealth*(sum(subset(scf_2019_forbes$WGT, scf_2019_forbes$RACE == "White"))/sum(subset(scf_2019_forbes$WGT, scf_2019_forbes$RACE == "White" | scf_2019_forbes$RACE == "Black")))
df_quantiles$"Black wealth if demographically proportional" <- 
  df_quantiles$Total_Wealth*(sum(subset(scf_2019_forbes$WGT, scf_2019_forbes$RACE == "Black"))/sum(subset(scf_2019_forbes$WGT, scf_2019_forbes$RACE == "White" | scf_2019_forbes$RACE == "Black")))

#########################
###### Long Data  #######
#########################

df_quantiles_long <- 
  df_quantiles %>% 
  gather(var, value, White:"Black wealth if demographically proportional")

df_quantiles_long$White <-
  grepl("White", df_quantiles_long$var)

df_quantiles_long$Race <-
  ifelse(df_quantiles_long$White == TRUE, "White", "Black")

df_quantiles_long$White <- NULL

df2 <- subset(df_quantiles_long, 
              df_quantiles_long$var == "White" |
                df_quantiles_long$var == "Black" |
                df_quantiles_long$var == "White wealth if demographically proportional" |
                df_quantiles_long$var == "Black wealth if demographically proportional")

df2$Ventiles<-  c(rep(1:20, 4))

#########################
## Highcharter Options ##
#########################

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 0, thousandsSep = ',')))
lang <- getOption("highcharter.lang")
lang$numericSymbols <- c(" Thousand"," Million"," Billion"," Trillion")
lang$thousandsSep <- ","
options(highcharter.lang = lang)

#########################
######  Figure A  #######
#########################

highchart() %>%
  hc_title(text = "Figure 2A: Cumulative black and white wealth by quintile") %>%
  hc_subtitle(
    text = "<a href='figure_2b.html'> Let's make wealth equal! Simulate cumulative wealth by quintile as if it were racially proportional. </a>
    <br>",
    align = "left",
    style = list(color = "#2b908f", fontWeight = "bold",  fontSize = '17px')
  ) %>%
  hc_chart(zoomType = "x") %>%
  hc_plotOptions(
    series = list( events = list( 
      legendItemClick = JS("function() {
                           return false; // disable visibility toggle
                           }")
    )),
    column = list(
      dataLabels = list(enabled = FALSE),
      #stacking = "normal",
      grouping = FALSE,
      groupPadding = .2,
      pointPadding = 0,
      pointWidth = 40,
      padding = 0,
      opacity = 1,
      enableMouseTracking = TRUE)) %>% 
  hc_yAxis_multiples(
    list(title = list(text = "Total Wealth in 2019 USD")),
    list(title = list(text = "Total Wealth in 2019 USD"),
         opposite = TRUE)) %>%
  hc_tooltip(crosshairs = TRUE, valueDecimals = round(df2$value), decimalPlaces = 0, valuePrefix = "$") %>%
  hc_add_series(subset(df2, df2$Race == "White" & df2$var == "White"), type = "column", hcaes(x = Ventiles, y = round(value) ),
                edgeColor = "black",
                edgeWidth = 5,
                name = "White net worth - current",
                legendItemClick = FALSE,
                animation = list(duration = 0)) %>%
  hc_add_series(subset(df2, df2$Race == "Black" & df2$var == "Black"), type = "column", hcaes(x = Ventiles, y = round(value)),
                renderTo = 'container',
                edgeColor = "black",
                edgeWidth = 2, 
                name = "Black net worth - current",
                animation = list(duration = 0)) %>%
  hc_colors(c("#002a5b", "#c5050c"))  %>%
  hc_xAxis(title = list(text = "Wealth Quintiles")) %>%
  hc_exporting(enabled = TRUE)

#########################
######  Figure B  #######
#########################

highchart() %>%
  hc_title(text = "Figure 2B: Simulated cumulative wealth by ventiles if each quintile were racially proportional") %>%
  hc_subtitle(
    text = " <a href='figure_2a.html'> Show real black and white cumulative wealth by ventiles </a>
    <br>",
    align = "left",
    style = list(color = "#2b908f", fontWeight = "bold",  fontSize = '17px')
  ) %>%
  hc_chart(zoomType = "x") %>%
  hc_plotOptions(
    series = list( events = list( 
      legendItemClick = JS("function() {
                             return false; // disable visibility toggle
                             }") )),
    column = list(
      dataLabels = list(enabled = FALSE),
      #stacking = "normal",
      grouping = FALSE,
      groupPadding = .5,
      pointPadding = 1,
      pointWidth = 40,
      padding = 1,
      opacity = 1,
      enableMouseTracking = TRUE)) %>% 
  hc_yAxis_multiples(
    list(title = list(text = "Total Wealth in 2019 USD")),
    list(title = list(text = "Total Wealth in 2019 USD"),
         opposite = TRUE)) %>%
  hc_tooltip(crosshairs = TRUE, valueDecimals = round(df2$value), decimalPlaces = 0, valuePrefix = "$") %>%
  hc_tooltip(crosshairs = "black") %>%
  hc_add_series(subset(df2, df2$Race == "White" & df2$var == "White wealth if demographically proportional"),
                type = "column",
                hcaes(x = Ventiles, y = round(value)),
                edgeColor = "black",
                edgeWidth = 2,
                name = "White net worth - demographically proportional",
                animation = list(duration = 0)) %>%
  hc_add_series(subset(df2, df2$Race == "Black" & df2$var == "Black wealth if demographically proportional"), 
                type = "column", hcaes(x = Ventiles, y = round(value)),
                edgeColor = "black",
                edgeWidth = 2,
                name = "Black net worth - demographically proportional",
                animation = list(duration = 700)) %>%
  hc_colors(c("#002a5b", "#c5050c"))  %>%
  hc_xAxis(title = list(text = "Wealth Quintiles")) %>%
  hc_exporting(enabled = TRUE)
