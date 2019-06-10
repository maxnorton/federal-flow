# import and restructure period designations
fed <- read.csv('Empowerment_Zones_and_Enterprise_Communities.csv')

#would like this to be a matrix not a bunch of vars, but maybe need to create the vars and reshape after
#fed$designation[i] <- matrix(nrow=2, ncol=7)
#rownames(fed$designation) <- vector("Primary", "Secondary")
#colnames(fed$designation) <- vector("1994-1997, 1998-1999", "2000-2001", "2002-2004", "2005-2009", "2010-2011", "2012-2017")

fed$desig94_97 <- NA
fed$desig98_99 <- NA
fed$desig00_01 <- NA
fed$desig02_04 <- NA
fed$desig05_09 <- NA
fed$desig10_11 <- NA
fed$desig12_17 <- NA

fed$desB94_97 <- NA
fed$desB98_99 <- NA
fed$desB00_01 <- NA
fed$desB02_04 <- NA
fed$desB05_09 <- NA
fed$desB10_11 <- NA
fed$desB12_17 <- NA

for(i in 1:nrow(fed)) {
  
  # Set 94-97
  if (fed$PERIODA[i] == "Atlanta Empowerment Zone" |
      fed$PERIODA[i] == "Empowerment Zone (Round I)") {
        fed$desig94_97[i] <- "EMPZ"
        if (fed$PERIODA[i] == "Atlanta Empowerment Zone") {
          fed$desB94_97[i] <- "ATL"
        } else {
          fed$desB94_97[i] <- "None"
        }
  } else if (fed$PERIODA[i] == "Enhanced Enterprise Community (Round I)" |
             fed$PERIODA[i] == "Enterprise Community (Round I)") {
      fed$desig94_97[i] <- "ENTC" 
      if (fed$PERIODA[i] == "Enhanced Enterprise Community (Round I)") {
        fed$desB94_97[i] <- "ENH"
      } else {
        fed$desB94_97[i] <- "None"
      }
  } else if (fed$PERIODA[i] == "None") {
      fed$desig94_97[i] <- "None"
      fed$desB94_97[i] <- "None"
  }

  # Set 98-99
  if (fed$PERIODB[i] == "DC Enterprise Zone beginning 1-1-1998") {
      fed$desig98_99[i] <- "EMPZ" 
      fed$ desB98_99[i] <- "DC"
  } else {
      fed$desig98_99[i] <- fed$desig94_97[i]
      fed$desB98_99[i] <- fed$desB94_97[i]
  }
  
  if (is.na(fed$desig94_97[i]) |
      is.na(fed$desB94_97[i]) |
      is.na(fed$desig98_99[i]) |
      is.na(fed$desB98_99[i])) {
        print(i)
  }
  
  # Set 00-01
  if (fed$PERIODB[i] == "Atlanta Empowerment Zone" |
      fed$PERIODB[i] == "DC Enterprise Zone beginning 1-1-1998" |
      fed$PERIODB[i] == "Empowerment Zone (Round I)" |
      fed$PERIODB[i] == "Empowerment Zone (Round I) Beginning Jan. 1, 2000" |
      fed$PERIODB[i] == "Empowerment Zone (Round I) Beginning Jan. 1, 2000; Enterprise Community (Round I)" |
      fed$PERIODB[i] == "Empowerment Zone (Round II)" |
      fed$PERIODB[i] == "Empowerment Zone (Round II); Enhanced Enterprise Community (Round I)" |
      fed$PERIODB[i] == "Empowerment Zone (Round II); Enterprise Community (Round I)" )  {
        fed$desig00_01[i] <- "EMPZ"
        if (fed$PERIODB[i] == "Atlanta Empowerment Zone") {
              fed$desB00_01[i] <- "ATL"
        } else if (fed$PERIODB[i] == "DC Enterprise Zone beginning 1-1-1998") {
              fed$desB00_01[i] <- "DC"
        } else {
              fed$desB00_01[i] <- "None"
        }
        
        if (fed$PERIODB[i] == "Empowerment Zone (Round II); Enhanced Enterprise Community (Round I)" |
            fed$PERIODB[i] == "Empowerment Zone (Round II); Enterprise Community (Round I)") {
          if (fed$desig98_99[i] != "ENTC") {
            print(paste("line ", i, " has A-B ENTC mismatch"))
          }
          if (fed$PERIODB[i] == "Empowerment Zone (Round II); Enhanced Enterprise Community (Round I)" &
              fed$desB98_99[i] != "ENH") {
                print(paste("line", i, "has A-B ENHANCED subdes mismatch"))
          }
        }
  } else if (fed$PERIODB[i] == "Enterprise Community (Round I)"|
             fed$PERIODB[i] == "Enterprise Community (Round II)") {
      fed$desig00_01[i] <- "ENTC"
      fed$desB00_01[i] <- "None"   
  } else {
      fed$desig00_01[i] <- "None"
      fed$desB00_01[i] <- "None"
  }

  # Set 02-04
  if (fed$PERIODC[i] == "DC Enterprise Zone ending 12-31-09" |
      fed$PERIODC[i] == "Empowerment Zone (Round I)" |
      fed$PERIODC[i] == "Empowerment Zone (Round II)" |
      fed$PERIODC[i] == "Empowerment Zone (Round III)" |
      fed$PERIODC[i] == "Empowerment Zone (Round III); Enterprise Community (Round II)" )  {
        fed$desig02_04[i] <- "EMPZ"
        if (fed$PERIODC[i] == "DC Enterprise Zone ending 12-31-09") {
          fed$desB02_04[i] <- "DC"
        } else {
          fed$desB02_04[i] <- "None"
        }
        if (fed$PERIODC[i] == "Empowerment Zone (Round III); Enterprise Community (Round II)") {
          if (fed$desig00_01[i] != "ENTC") {
            print(paste("line ", i, " has B-C ENTC mismatch"))
          }
        }
  } else if (fed$PERIODC[i] == "Enhanced Enterprise Community (Round I) Ending 12-31-04; Empowerment Zone (Round II)" |
             fed$PERIODC[i] == "Enterprise Community (Round I) Ending 12-31-04; Empowerment Zone (Round I)" |
             fed$PERIODC[i] == "Enterprise Community (Round I) Ending 12-31-04; Empowerment Zone (Round II)" |
             fed$PERIODC[i] == "Enterprise Community (Round I) Ending 12-31-04; Empowerment Zone (Round III)" |
             fed$PERIODC[i] == "Enterprise Community (Round II)") {
        fed$desig02_04[i] <- "ENTC"
        if (fed$PERIODC[i] == "Enhanced Enterprise Community (Round I) Ending 12-31-04; Empowerment Zone (Round II)") {
          fed$desB02_04[i] <- "ENH"
        }
  } else if (fed$PERIODC[i]=="Renewal Community" | fed$PERIODC[i]=="Atlanta Renewal Community") {
      fed$desig02_04[i] <- "RC"
      if (fed$PERIODC[i]=="Atlanta Renewal Community") {
        fed$desB02_04[i] <- "ATL"
      } else {
        fed$desB02_04[i] <- "None"
      }
  } else {
      fed$desig02_04[i] <- "None"
      fed$desB02_04[i] <- "None"
  }
  
  # Set 10-11
  if (fed$PERIODD[i] == "DC Enterprise Zone" |
      fed$PERIODD[i] == "Empowerment Zone (Round I)" |
      fed$PERIODD[i] == "Empowerment Zone (Round II)" |
      fed$PERIODD[i] == "Empowerment Zone (Round III)" )  {
        fed$desig10_11[i] <- "EMPZ"
        if (fed$PERIODC[i] == "DC Enterprise Zone") {
          fed$desB10_11[i] <- "DC"
        } else {
          fed$desB10_11[i] <- "None"
        }
      
  } else {
    fed$desig10_11[i] <- "None"
    fed$desB10_11[i] <- "None"
    }
  
  # Set 12-17
  if (fed$PERIODD[i] == "Empowerment Zone (Round I)" |
      fed$PERIODD[i] == "Empowerment Zone (Round II)" |
      fed$PERIODD[i] == "Empowerment Zone (Round III)" )  {
    fed$desig12_17[i] <- "Retro EMPZ"
    fed$desB12_17[i] <- "None"
  } else {
    fed$desig12_17[i] <- "None"
    fed$desB12_17[i] <- "None"
  }
}

#zones <- aggregate(fed, by=list(fed$FULLNAME), FUN = return)
zoneranks <- c("EMPZ", "Retro EMPZ", "ENTC", "RC", "None")
levels(fed$desig94_97) <- zoneranks
levels(fed$desig98_99) <- zoneranks
levels(fed$desig00_01) <- zoneranks
levels(fed$desig02_04) <- zoneranks
levels(fed$desig05_09) <- zoneranks
levels(fed$desig10_11) <- zoneranks
levels(fed$desig12_17) <- zoneranks


# Generate alluvial diagram
library(ggplot2)
library(ggalluvial)
library(tidyverse)

fed %>%
  group_by(desig94_97, desig98_99, desig00_01, desig02_04, desig05_09, desig10_11, desig12_17) %>%
  count()  %>%
#  is_alluvia_form(axes = 1:4, silent = TRUE)
  ggplot(aes(y = n, axis1=desig94_97, axis2=desig98_99, axis3=desig00_01, axis4=factor(desig02_04, levels=c("EMPZ", "ENTC", "RC", "None")), axis5=desig05_09, axis6=desig10_11, axis7=desig12_17) )+ 
           geom_alluvium(aes(fill = desig94_97), width=1/12) + 
           geom_stratum(width = 1/12, fill=alpha(0.5), color="grey") + 
           geom_label(stat = "stratum", label.strata = TRUE) + 
           scale_x_discrete(limits = c("1994", "1998", "2000", "2002", "2005", "2010", "2012"), expand = c(.05, .05)) +
           scale_fill_brewer(type="qual", palette = "Set1") + 
           ggtitle("Federal zone type flow, 1994-2004")