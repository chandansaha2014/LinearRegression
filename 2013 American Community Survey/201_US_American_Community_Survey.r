## US Working Mom study
library(data.table)
cols <- c("PAOC", "ST", "AGEP", "MAR", "ESR", "COW", "SCHL", "WAGP")
pusa <- fread("../data_uscensus/pums/ss13pusa.csv", select = cols)
pusb <- fread("../data_uscensus/pums/ss13pusb.csv", select = cols)


## Subset without childrent
pus <- subset(rbind(pusa, pusb), !is.na(PAOC))


library(ggplot2)
library(dplyr)
library(gridExtra)
library(maps)
#?maps



pus$MAR <- factor(pus$MAR)
levels(pus$MAR) <- c("Married", "Widowed", "Divorced", "Separated", "Never married")

pus$ESR <- factor(pus$ESR)
levels(pus$ESR) <- c("Employed", "Employed, not at work", "Unemployed", "Employed", "Employed, not at work", "Not in labor force")
pus$ESRG <- ifelse(pus$ESR == "Employed", 1, 0)

pus$COW <- factor(pus$COW)
levels(pus$COW) <- c("Private profit", "Private non-profit", "Local government", "State government", "Federal government", "Self-employed", "Self-employed", "Working without pay", "Unemployed")

pus$PAOC <- factor(pus$PAOC)
levels(pus$PAOC) <- c("Children under 6", "Children 6 to 17", "Children under 6 and 6 to 17", "No children")
## Categorise no children with 0 and with children with 1 
pus$PAOCG <- ifelse(pus$PAOC == "No children", 0, 1)

pus$SCHL <- ifelse(pus$SCHL <= 16, 16, pus$SCHL)
pus$SCHL <- ifelse(pus$SCHL >= 17 & pus$SCHL <= 19, 19, pus$SCHL)
pus$SCHL <- factor(pus$SCHL)
levels(pus$SCHL) <- c("High school or lower", "Some college", "Associate's degree", "Bachelor's degree", "Master's degree", "Professional degree", "Doctorate degree")

## Count no of mom 
#cnt_mom = sum(pus, pus$PAOCG == 1)

## All MOMs 
am <- subset(pus, PAOCG == 1)
summary(am)

# Plotting in GgPLOT
ggplot(pus , aes(AGEP , group = PAOC)) + geom_bar(badwidth = 1, aes(color = PAOC , fill = PAOC ) , alpha = 0.3)+
      xlab("Age")+
      ylab("No of Children Count")+
      ggtitle("Women by Age")

prop.table(table(pus$PAOC)) # Check frequency of moms with children in pus table 
#Observation : Roughly a quarter of the women older than 16 were with dependent children, and only 5.8% of them were with little kids younger than 6 years old
prop.table(table(pus$ESR))

by(pus$AGEP, pus$PAOCG, summary)
# Observation : The age distribution of the moms was bell-shaped with a median value of 38 and interquantile range of 13. The median ages of moms with little kids, moms with big kids, and moms with both were 30, 43, and 34 respectively.

## Marital Status 
data <- as.data.frame(prop.table(table(am$AGEP, am$MAR)))
data$margin <- prop.table(table(am$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(am$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Marital Status of the Moms") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))
  
# Marital Status vs Employment Status
chisq.test(table(am$SCHL, am$ESR)) ## significant association b/w marital status and employment status
#chisq.test(table(am$PAOC, am$ESR))
data <- as.data.frame(prop.table(table(am$SCHL,am$ESR), margin = 1))
ggplot(data,aes(x=Var1,y=Freq,group=Var2)) +
  geom_bar(stat = "identity" , aes(colour = Var2 , fill = Var2 ), alpha = 0.3 ) +
  labs(x="Education" , y="Frequency" , title = "Education Vs Employment Status") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1))

## Children Vs Employment Status 
data <- as.data.frame(prop.table(table(am$PAOC,am$ESR), margin = 1))
ggplot(data,aes(x=Var1,y=Freq,group=Var2)) +
  geom_bar(stat = "identity" , aes(colour = Var2 , fill = Var2 ), alpha = 0.3 ) +
  labs(x="Children" , y="Frequency" , title = "Children Vs Employment Status") +
  theme(axis.text.x = element_text(angle = 30 , hjust = 1))

# Field of the Moms's Work
wm <- subset(am, ESR == "Employed" & WAGP > 1000)
data <- as.data.frame(prop.table(table(wm$AGEP, wm$COW)))
data$margin <- prop.table(table(wm$AGEP))
data$height <- data$Freq/data$margin
data$center <- c(0, cumsum(data$margin)[1:length(levels(factor(wm$AGEP))) -1]) + data$margin/2
ggplot(data, aes(center, height)) + 
  geom_bar(stat = "identity", aes(width = margin, fill = Var2), col = "gray", alpha = 0.7) +
  labs(x = "Age", y = "Frequency", title = "Field of the Moms' Work") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), labels=c("16", "27", "31", "33", "36", "38", "41", "43", "46", "50", "93"))

# Wages of the Working Mom 
wm$AGEG <- cut(wm$AGEP, breaks = quantile(wm$AGEP))
ggplot(na.omit(wm), aes(x = AGEG, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = AGEG), alpha = 0.5) + 
  labs(x = "Age Group", y = "Wage on Log10 Scale", title = "Wages vs Age Groups")
## significant increment was shown between the first and the second groups. The wages of working moms in the second, third and fourth age groups remained relatively on the same level.


## Relationship between company and wages 
ggplot(na.omit(wm), aes(x = COW, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = COW), alpha = 0.5) + 
  labs(x = "Field of Work", y = "Wage on Log10 Scale", title = "Wage vs Field of Work") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

## Wages Vs Age

ggplot(subset(wm, AGEP <= 56 & AGEP >= 21), aes(x = factor(AGEP), y = log10(WAGP))) + 
  stat_summary(fun.y=mean, aes(colour = COW), geom="point", size = 3) +
  stat_summary(fun.y=mean, aes(group=COW, colour = COW), geom="line") + 
  labs(x="Age", y="Wage on Log10 Scale", title="Wage vs Age, Grouped by Field of Work") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


## Wages Vs Maritial Status

ggplot(na.omit(wm), aes(x = MAR, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = MAR), alpha = 0.5) + 
  labs(x = "Marital Status", y = "Wage on Log10 Scale", title = "Wage vs Marital Status") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)

## Wage Vs age by Maritial Status
ggplot(subset(wm, AGEP <= 56 & AGEP >= 21), aes(x = factor(AGEP), y = log10(WAGP))) + 
  stat_summary(fun.y=mean, aes(colour = MAR), geom="point", size = 3) +
  stat_summary(fun.y=mean, aes(group=MAR, colour = MAR), geom="line") + 
  labs(x="Age", y="Wage on Log10 Scale", title="Wage vs Age, Grouped by Marital Status") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Wage Vs education by ageg 
ggplot(na.omit(wm), aes(x = SCHL, y = log10(WAGP))) + 
  geom_boxplot(aes(fill = SCHL), alpha = 0.5) + 
  labs(x = "Education", y = "Wage on Log10 Scale", title = "Wage vs Education") +
  stat_summary(fun.y=mean, aes(colour = AGEG), geom="point", size = 5) +
  stat_summary(fun.y=mean, aes(group=AGEG, colour = AGEG), geom="line") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  guides(fill = FALSE)
# Wage Vs Age by Education 

ggplot(subset(wm, AGEP <= 56 & AGEP >= 21), aes(x = factor(AGEP), y = log10(WAGP))) + 
  stat_summary(fun.y=mean, aes(colour = SCHL), geom="point", size = 3) +
  stat_summary(fun.y=mean, aes(group=SCHL, colour = SCHL), geom="line") + 
  labs(x="Age", y="Wage on Log10 Scale", title="Wage vs Age, Grouped by Education") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Working Moms across the States 
data <- as.data.frame(prop.table(table(am$ST, am$ESRG), margin = 1))
data <- subset(data, Var2 == 1)
data$state <- c('alabama','alaska','arizona','arkansas','california','colorado','connecticut','delaware','district of columbia','florida','georgia','hawaii','idaho','illinois','indiana','iowa','kansas','kentucky','louisiana','maine','maryland','massachusetts','michigan','minnesota','mississippi','missouri','montana','nebraska','nevada','new hampshire','new jersey','new mexico','new york','north carolina','north dakota','ohio','oklahoma','oregon','pennsylvania','rhode island','south carolina','south dakota','tennessee','texas','utah','vermont','virginia','washington','west virginia','wisconsin','wyoming')

all_states <- map_data("state")
all_states$freq <- data$Freq[match(all_states$region, data$state)]*100
ggplot(all_states, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill = freq),colour="gray") + 
  ggtitle("Percentage of Working Moms across the States")


head(arrange(data, -Freq), 5) # The States with the highest employment rate of moms
##   Var1 Var2      Freq        state
## 1   46    1 0.7829638 south dakota
## 2   19    1 0.7747981         iowa
## 3   27    1 0.7708950    minnesota
## 4   55    1 0.7561019    wisconsin
## 5   31    1 0.7469008     nebraska

tail(arrange(data, -Freq), 5) # The States with the lowest employment rate of moms
##    Var1 Var2      Freq      state
## 47    2    1 0.6040541     alaska
## 48    4    1 0.6000000    arizona
## 49   35    1 0.5912659 new mexico
## 50    6    1 0.5845201 california
## 51   49    1 0.5580545       utah

## Wages across all states 
wm_grouped <- group_by(wm, ST)
data <- summarise(wm_grouped, wage = mean(WAGP))
data$state <- c('alabama','alaska','arizona','arkansas','california','colorado','connecticut','delaware','district of columbia','florida','georgia','hawaii','idaho','illinois','indiana','iowa','kansas','kentucky','louisiana','maine','maryland','massachusetts','michigan','minnesota','mississippi','missouri','montana','nebraska','nevada','new hampshire','new jersey','new mexico','new york','north carolina','north dakota','ohio','oklahoma','oregon','pennsylvania','rhode island','south carolina','south dakota','tennessee','texas','utah','vermont','virginia','washington','west virginia','wisconsin','wyoming')

all_states <- map_data("state")
all_states$wage <- data$wage[match(all_states$region, data$state)]
ggplot(all_states, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill = wage),colour="gray") + 
  ggtitle("Wage of Working Moms across the States")

head(arrange(data, -wage), 5) # The States with the highest wage of working moms
##   ST     wage                state
## 1 11 76504.32 district of columbia
## 2 24 57865.49             maryland
## 3  9 56082.99          connecticut
## 4 34 55462.22           new jersey
## 5 25 55313.76        massachusetts

tail(arrange(data, -wage), 5) # The States with the lowest wage of workign moms
## Source: local data frame [5 x 3]
## 
##      ST     wage        state
##   (int)    (dbl)        (chr)
## 1    30 31194.74      montana
## 2    46 31161.64 south dakota
## 3    28 30725.38  mississippi
## 4    49 30387.75         utah
## 5    16 28361.46        idaho
