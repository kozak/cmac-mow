## utils for file IO and stats for linear models / prediction
require(utils);require(stats);

## column name for each column in villages.data file
columnNames <- c("DISTRICT", "SETTLE", "DISTP", "DISTD","CLASR", "STDB", "STDG", "TEACHER", "HOUSET",
"MIDSCH", "ASPHALT", "STAB", "WATERD", "WATERC", "FOUNT", "HEALTH", "CLINIC", "MANSION", "CANAL", "LAUND", 
"TOIL", "AGRICA", "TRACTOR", "COW", "BEEHIVE", "ELECTR", "MOSQUE", "POST", "GUARD", "SETTLEP", "CENTER", "POPUL", 
"SHEEP");

## create data frame from the file
dataFrame<- read.table("data/villages.data", col.names=columnNames);

## create a linear model, using parameters from the data
ourModel <- lm(SHEEP~DISTRICT+as.numeric(SETTLE)+as.numeric(DISTP)+
		   as.numeric(DISTD)+as.numeric(CLASR)+
		   as.numeric(STDB)+as.numeric(STDG)+
		   as.numeric(TEACHER)+HOUSET+MIDSCH+
               as.numeric(ASPHALT)+
               as.numeric(STAB)+as.numeric(FOUNT)+HEALTH+as.numeric(CLINIC)+
               as.numeric(MANSION)+CANAL+as.numeric(LAUND)+as.numeric(TOIL)+as.numeric(AGRICA)+as.numeric(TRACTOR)+
		   as.numeric(COW)+as.numeric(BEEHIVE)+as.numeric(ELECTR)+as.numeric(MOSQUE)+as.numeric(POST)+as.numeric(GUARD)+
		   SETTLEP+CENTER+as.numeric(POPUL), data=dataFrame);

## define a new entity
newEntity <- data.frame("DISTRICT" = 2, "SETTLE" = 1, "DISTP" = 5, "DISTD" = 5, "CLASR" = 1500, "STDB"  = 22000, 
"STDG" = 22000, "TEACHER" = 2000, "HOUSET" = 2000, "MIDSCH" = 50, "ASPHALT" = 500, "STAB" = 200, "WATERD" = 1, 
"WATERC" = 1, "FOUNT" = 20, "HEALTH" = 40, "CLINIC" = 50, "MANSION" = 5000, "CANAL" = 1, "LAUND" = 50, 
"TOIL" = 200, "AGRICA" = 90000, "TRACTOR" = 4000, "COW" = 50000, "BEEHIVE" = 0, "ELECTR" = 1, "MOSQUE" = 1, 
"POST" = 50, "GUARD" = 5000, "SETTLEP" = 1, "CENTER" = 1, "POPUL" = 700000);

predict(ourModel , newEntity );


## TODO. Analysis.  