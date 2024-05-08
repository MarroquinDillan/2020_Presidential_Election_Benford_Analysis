library(tidyverse)
library(scales)

setwd("C:/Users/di11m/Downloads/Coursera/Data Analysis/RStudio")

# Read election data
washoe <- read.csv("Election_Results_Washoe_CLEANED.csv")
nevada <- read.csv("Election_Results_NV_CLEANED.csv")


# Sum of votes by candidate: Washoe and NV
bidenElecW <- filter(washoe, Candidate == "BIDEN, JOSEPH R.") #Washoe
  bidenVotesW <- sum(bidenElecW$Votes)
bidenElecN <- filter(nevada, Candidate == "BIDEN, JOSEPH R.") #State
  bidenVotesN <- sum(bidenElecN$Votes)
  
blankElecW <- filter(washoe, Candidate == "BLANKENSHIP, DON")
  blankVotesW <- sum(blankElecW$Votes)
blankElecN <- filter(nevada, Candidate == "BLANKENSHIP, DON")
  blankVotesN <- sum(blankElecN$Votes)
  
jorgElecW <- filter(washoe, Candidate == "JORGENSEN, JO")
  jorgVotesW <- sum(jorgElecW$Votes)
jorgElecN <- filter(nevada, Candidate == "JORGENSEN, JO")
  jorgVotesN <- sum(jorgElecN$Votes)
  
noneElecW <- filter(washoe, Candidate == "None Of These Candidates")
  noneVotesW <- sum(noneElecW$Votes)
noneElecN <- filter(nevada, Candidate == "None Of These Candidates")
  noneVotesN <- sum(noneElecN$Votes)
  
trumpElecW <- filter(washoe, Candidate == "TRUMP, DONALD J.")
  trumpVotesW <- sum(trumpElecW$Votes)
trumpElecN <- filter(nevada, Candidate == "TRUMP, DONALD J.")
  trumpVotesN <- sum(trumpElecN$Votes)


# Vote metrics by candidate: Washoe and NV
votesTotalW <- bidenVotesW+blankVotesW+jorgVotesW+noneVotesW+trumpVotesW
votesTotalN <- bidenVotesN+blankVotesN+jorgVotesN+noneVotesN+trumpVotesN

candidate <- c("BIDEN", "BLANKENSHIP", "JORGENSEN", "NONE", "TRUMP")
votesByCandW <- c(bidenVotesW, blankVotesW, jorgVotesW, noneVotesW, trumpVotesW)
votesByCandN <- c(bidenVotesN, blankVotesN, jorgVotesN, noneVotesN, trumpVotesN)
percentsW <- c(bidenVotesW/votesTotalW, blankVotesW/votesTotalW, jorgVotesW/votesTotalW, noneVotesW/votesTotalW, trumpVotesW/votesTotalW)
percentsN <- c(bidenVotesN/votesTotalN, blankVotesN/votesTotalN, jorgVotesN/votesTotalN, noneVotesN/votesTotalN, trumpVotesN/votesTotalN)

VotesByCandW <- data.frame("Candidate" = candidate,
                           "Votes" = votesByCandW)
VotesByCandN <- data.frame("Candidate" = candidate,
                           "Votes" = votesByCandN)
PercentTotalW <- data.frame("Candidate" = candidate,
                            "Percent" = percentsW)
PercentTotalN <- data.frame("Candidate" = candidate,
                            "Percent" = percentsN)


# Graph of Election Results: Washoe
ElectionGraphW <- 
  ggplot(VotesByCandW,
         aes(x=candidate, y=votesByCandW, fill= candidate))+ 
  geom_col()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  scale_fill_manual(values = c("BIDEN" = "blue",
                               "BLANKENSHIP" = "purple",
                               "JORGENSEN" = "yellow",
                               "NONE" = "black",
                               "TRUMP" = "RED"))+
  labs(title="2020 Presidential Election Results: Washoe County",
       x="Candidate",
       y="Votes")

# Graph of Election Results: NV
ElectionGraphN <- 
  ggplot(VotesByCandN,
         aes(x=candidate, y=votesByCandN, fill= candidate))+ 
  geom_col()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = label_number())+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  scale_fill_manual(values = c("BIDEN" = "blue",
                               "BLANKENSHIP" = "purple",
                               "JORGENSEN" = "yellow",
                               "NONE" = "black",
                               "TRUMP" = "RED"))+
  labs(title="2020 Presidential Election Results: Nevada",
       x="Candidate",
       y="Votes")


# Benford's Law distribution
benlaw <- function(d) log10(1 + 1 / d)  #Formula
digits <- 1:9
BensData <- data.frame(Digits = digits, Percentage = benlaw(digits))

BensGraph <-
  ggplot(BensData,
         aes(x= Digits, y=Percentage)) +
  geom_line(color="red")+
  geom_point(color="red", size=3)+ 
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  labs(title = "Benford's Law",
       x = "Leading Digit",
       y= "Proportion of Occurrence")


# Leading Digits: Washoe
ldOccurW <- c(sum(washoe$LeadDigit == 1),
              sum(washoe$LeadDigit == 2),
              sum(washoe$LeadDigit == 3),
              sum(washoe$LeadDigit == 4),
              sum(washoe$LeadDigit == 5),
              sum(washoe$LeadDigit == 6),
              sum(washoe$LeadDigit == 7),
              sum(washoe$LeadDigit == 8),
              sum(washoe$LeadDigit == 9))

ldTotalW <- sum(ldOccurW)
ldPercentW <- c(ldOccurW[1]/ldTotalW,
                ldOccurW[2]/ldTotalW,
                ldOccurW[3]/ldTotalW,
                ldOccurW[4]/ldTotalW,
                ldOccurW[5]/ldTotalW,
                ldOccurW[6]/ldTotalW,
                ldOccurW[7]/ldTotalW,
                ldOccurW[8]/ldTotalW,
                ldOccurW[9]/ldTotalW)

LdDataW <- data.frame(Digits = digits, Percentage = ldPercentW)

LdGraphW <-
  ggplot(LdDataW,
         aes(x=Digits, y=Percentage))+
  geom_line(color="blue")+
  geom_point(color="blue", size=3)+
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  labs(title = "Leading Digit of Total Votes\nin Washoe Presidential Election",
       x= "Leading Digit",
       y= "Proportion of Occurrence")


# Leading Digits: NV
ldOccur <- c(sum(nevada$LeadDigit == 1),
             sum(nevada$LeadDigit == 2),
             sum(nevada$LeadDigit == 3),
             sum(nevada$LeadDigit == 4),
             sum(nevada$LeadDigit == 5),
             sum(nevada$LeadDigit == 6),
             sum(nevada$LeadDigit == 7),
             sum(nevada$LeadDigit == 8),
             sum(nevada$LeadDigit == 9))

ldTotal <- sum(ldOccur)
ldPercent <- c(ldOccur[1]/ldTotal,
               ldOccur[2]/ldTotal,
               ldOccur[3]/ldTotal,
               ldOccur[4]/ldTotal,
               ldOccur[5]/ldTotal,
               ldOccur[6]/ldTotal,
               ldOccur[7]/ldTotal,
               ldOccur[8]/ldTotal,
               ldOccur[9]/ldTotal)

LdTableN <- data.frame(Digits = digits, Percentage = ldPercent)

LdGraphN <-
  ggplot(LdTableN,
         aes(x=Digits, y=Percentage)) +
  geom_line(color="orange")+
  geom_point(color="orange", size=3)+
  scale_x_continuous(n.breaks = 9) +
  scale_y_continuous(n.breaks = 8) +
  labs(title = "Leading Digit of Total Votes\nin Nevada Presidential Election",
       x= "Leading Digit",
       y= "Proportion of Occurrence")

# Benford's Law comparison: Washoe
TableWB <- data.frame(Digits=digits,
                     Percentage=c(benlaw(digits), ldPercentW),
                     Focus = c(rep("Benford",9), rep("Washoe",9)))

GraphWB <-
  ggplot(TableWB,
         aes(x=Digits, y=Percentage, group=Focus)) + 
  geom_line(aes(color=Focus))+
  geom_point(aes(color=Focus))+
  scale_x_continuous(n.breaks = 9)+
  scale_y_continuous(n.breaks = 8)+
  scale_color_manual(values=c("red", "blue"))+
  labs(title = "Leading Digits of Votes in Washoe v. Benford",
       x= "Leading Digit",
       y= "Proportion of Occurrence")


# Benford's Law comparison: NV
TableNB <- data.frame(Digits=digits,
                     Percentage=c(benlaw(digits), ldPercent),
                     Focus = c(rep("Benford",9), rep("Nevada",9)))

GraphNB <-
  ggplot(TableNB,
         aes(x=Digits, y=Percentage, group=Focus)) + 
  geom_line(aes(color=Focus))+
  geom_point(aes(color=Focus))+
  scale_x_continuous(n.breaks = 9)+
  scale_y_continuous(n.breaks = 8)+
  scale_color_manual(values=c("red", "orange"))+
  labs(title = "Leading Digits of Votes in Nevada v. Benford",
       x= "Leading Digit",
       y= "Proportion of Occurrence")

# Benford's Law comparison: NV, Washoe, Benford
TableAll <- data.frame(Digits = digits, 
                      Percentage = c(benlaw(digits), ldPercentW, ldPercent), 
                      Focus = c(rep("Benford", 9), rep("Washoe", 9), rep("Nevada", 9)))

GraphAll <-
  ggplot(TableAll,
         aes(x=Digits, y=Percentage, group=Focus)) + 
  geom_line(aes(color=Focus))+
  geom_point(aes(color=Focus))+
  scale_x_continuous(n.breaks = 9)+
  scale_y_continuous(n.breaks = 8)+
  scale_color_manual(values=c("red", "orange", "blue"))+
  labs(title = "Washoe and Nevada v. Benford",
       x= "Leading Digit",
       y= "Proportion of Occurrence")


# View csv files
View(washoe)
View(nevada)

# View data tables
View(VotesByCandW)  # votes per candidate
View(VotesByCandN)

View(PercentTotalW)  # percentage of total votes
View(PercentTotalN)

# View graphs
ElectionGraphW      # votes per candidate
ElectionGraphN
BensGraph           # Ben's Law distribution
LdGraphW
LdGraphN
GraphWB             # Ben's Law comparison
GraphNB
GraphAll