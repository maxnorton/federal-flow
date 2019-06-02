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
  if (fed$PERIODA[i] == "Atlanta Empowerment Zone" | fed$PERIODA[i] == "Empowerment Zone (Round I)")  {
    fed$desig94_97[i] <- "EMPZ"
    if (fed$PERIODA[i] == "Atlanta Empowerment Zone") {
      fed$desB94_97[i] <- "ATL"
    } else {
      fed$desB94_97[i] <- "None"
    }
  } else if (fed$PERIODA[i] == "Enhanced Enterprise Community (Round I)" | fed$PERIODA[i] == "Enterprise Community (Round I)") {
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
  
  if (is.na(fed$desig94_97[i]) | is.na(fed$desB94_97[i]) | is.na(fed$desig98_99[i]) | is.na(fed$desB98_99[i])) {
    print(i)
  }
  
  # Set 00-01
  if (fed$PERIODB[i] == "Atlanta Empowerment Zone" | fed$PERIODB[i] == "DC Enterprise Zone beginning 1-1-1998" | fed$PERIODB[i] == "Empowerment Zone (Round I)" | fed$PERIODB[i] == "Empowerment Zone (Round I) Beginning Jan. 1, 2000" | fed$PERIODB[i] == "Empowerment Zone (Round I) Beginning Jan. 1, 2000; Enterprise Community (Round I)" | fed$PERIODB[i] == "Empowerment Zone (Round II)" | fed$PERIODB[i] == "Empowerment Zone (Round II); Enhanced Enterprise Community (Round I)" | fed$PERIODB[i] == "Empowerment Zone (Round II); Enterprise Community (Round I)" )  {
    fed$desig00_01[i] <- "EMPZ"
    if (fed$PERIODB[i] == "Atlanta Empowerment Zone") {
      fed$desB00_01[i] <- "ATL"
    } else if (fed$PERIODB[i] == "DC Enterprise Zone beginning 1-1-1998") {
      fed$desB00_01[i] <- "DC"
    } else {
      fed$desB00_01[i] <- "None"
    }
    
    if (fed$PERIODB[i] == "Empowerment Zone (Round II); Enhanced Enterprise Community (Round I)" | fed$PERIODB[i] == "Empowerment Zone (Round II); Enterprise Community (Round I)") {
      if (fed$desig98_99[i] != "ENTC") {
        print(paste("line ", i, " has A-B ENTC mismatch"))
      }
      if (fed$PERIODB[i] == "Empowerment Zone (Round II); Enhanced Enterprise Community (Round I)" & fed$desB98_99[i] != "ENH") {
        print(paste("line", i, "has A-B ENHANCED subdes mismatch"))
      }
    }
  } else {
    fed$desig00_01[i] <- "ENTC"
    fed$desB00_01[i] <- "None"
  }

}

