### Matching TMT titles
library(tidyverse)

dictionary_titles <- read.csv("Data/TMT_raw/title_abreviations.csv")
df <- read.csv("Data/TMT_raw/test_data.csv")

df <- as.data.frame(df)
c.no <- 8
end.line <- length(colnames(df))
### Nursing
nursing <- grepl("nurs", tolower(df[,c.no]))

### executive medical functions
clinical <- grepl("clinical officer", tolower(df[,c.no]))
medical_affairs <- grepl("medical affairs", tolower(df[,c.no]))
medical_officer <- grepl("medical officer", tolower(df[,c.no]))

### Research and Training
scientific <- grepl("scientific officer", tolower(df[,c.no]))
academic <- grepl("academic", tolower(df[,c.no]))
program <- grepl("program", tolower(df[,c.no]))



### Code rank(s)
chief <- grepl("chief", tolower(df[,c.no]), perl = TRUE) | grepl("c[a-z]o\\b", tolower(df[,c.no]), perl = TRUE) # Chief
president <- grepl("(?<!vice )president", tolower(df[,c.no]), perl = TRUE) #President but not vice president
vp <- (grepl("(?<!senior )vice president", tolower(df[,c.no]), perl = TRUE)| grepl("vp\\b", tolower(df[,c.no]), perl = TRUE)) #(Vice President but not senior vice president)
director <- grepl("director", tolower(df[,c.no]), perl = TRUE)
chair <- grepl("chair", tolower(df[,c.no]), perl = TRUE)
senior <- grepl("senior", tolower(df[,c.no]), perl = TRUE)


#Strategic Planning
ceo <- grepl("chief executive officer", tolower(df[,c.no]), perl = TRUE) | grepl("ceo", tolower(df[,c.no]), perl = TRUE)# Chief Executive Officer
strategy <- grepl("strateg", tolower(df[,c.no]), perl = TRUE)# Chief Strategy Officer
development <- grepl("development", tolower(df[,c.no]), perl = TRUE)# Chief Development Officer


# Operations
operating <- (grepl("operati", tolower(df[,c.no]), perl = TRUE) | grepl("coo\\b", tolower(df[,c.no]), perl = TRUE)) # Chief Operating Officer
administrative <- (grepl("administrat", tolower(df[,c.no]), perl = TRUE) | grepl("cao\\b", tolower(df[,c.no]), perl = TRUE)) #  Chief Administrative Officer
information <- (grepl("information", tolower(df[,c.no]), perl = TRUE) | grepl("cio\\b", tolower(df[,c.no]), perl = TRUE)) # Chief Information Officer
affiliated <- (grepl("affiliated service", tolower(df[,c.no]), perl = TRUE) | grepl("ancillar", tolower(df[,c.no]), perl = TRUE)) # Affiliated Services
facility <- (grepl("facilit", tolower(df[,c.no]), perl = TRUE)  )# Facilities Management
controller <- (grepl("controll", tolower(df[,c.no]), perl = TRUE)  )# Controller
finance <- (grepl("financ", tolower(df[,c.no]), perl = TRUE) | grepl("cfo\\b", tolower(df[,c.no]), perl = TRUE)| grepl("treas", tolower(df[,c.no]), perl = TRUE)) # Chief Financial Officer
privacy <- (grepl("privacy", tolower(df[,c.no]), perl = TRUE) | grepl("compliance", tolower(df[,c.no]), perl = TRUE)) # Chief Privacy & Compliance Officer
legal <- (grepl("legal", tolower(df[,c.no]), perl = TRUE)  )# Chief Legal Officer
case <- (grepl("case\\b", tolower(df[,c.no]), perl = TRUE)  )# Chief of Case Management
secretary <- (grepl("secretary", tolower(df[,c.no]), perl = TRUE)  )# Secretary


#Purpose
philanthropy <- (grepl("philan", tolower(df[,c.no]), perl = TRUE)  )#Chief Philanthropy Officer
mission<- (grepl("\\bmission", tolower(df[,c.no]), perl = TRUE)  )#Chief Mission Officer

#HR
quality <- (grepl("quality", tolower(df[,c.no]), perl = TRUE) | grepl("risk", tolower(df[,c.no]), perl = TRUE))  # Chief Quality & Learning Officer
human <- (grepl("human res", tolower(df[,c.no]), perl = TRUE) )  # Chief Human Resources Officer
staff <- (grepl("staff", tolower(df[,c.no]), perl = TRUE) ) # Chief of Staff
people <- (grepl("people", tolower(df[,c.no]), perl = TRUE) )  #people

# Customer/Patients Facing
marketing <- (grepl("marketing", tolower(df[,c.no]), perl = TRUE) ) # Marketing
client <- (grepl("client", tolower(df[,c.no]), perl = TRUE) ) # Chief Client Officer
commercial <- (grepl("commercial", tolower(df[,c.no]), perl = TRUE) ) # Chief Commercial Officer

#pharmacy
pharma <- (grepl("pharmac", tolower(df[,c.no]), perl = TRUE) )  #Pharma

df <- as.data.frame(cbind(df, nursing,clinical,medical_affairs,medical_officer,scientific,academic,program,case,chief,president,vp,director,chair,senior,
                          ceo,strategy,development,operating,administrative,information,affiliated,facility,controller,finance,privacy,legal,case,secretary,
                          philanthropy,mission,quality,human,staff,people,marketing,client,commercial,pharma))
glimpse(df)

df[,(end.line+1):(length(colnames(df)))] <- apply(df[,(end.line+1):(length(colnames(df)))],2,function(x) ifelse(x==TRUE,1,0))


### Coding of groups

df$category.operations <- df$operating   +
  df$administrative   +
  df$information  +
  df$affiliated   +
  df$facility  +
  df$controller  +
  df$finance   +
  df$privacy   +
  df$legal +
  df$case  +
  df$secretary # Secretary

### medical
df$category.medical <- df$clinical + 
  df$medical_affairs +
  df$medical_officer 

#strategy & CEO
df$category.strat <- df$ceo + df$strategy + df$development

#HR
df$category.hr <- df$human + df$quality + df$privacy +df$people

#patients
df$category.patients <- df$marketing + df$client + df$commercial

#purpose
df$category.purpose <- df$philanthropy + df$mission

#research
df$category.research <- df$academic + df$scientific + df$program

relevant.codes <- as.data.frame(cbind("category.operations" = df$category.operations, "category.medical" = df$category.medical, "category.strategy" = df$category.strat,
                                      "category.hr" = df$category.hr,"category.patients" = df$category.patients,"category.purpose" = df$category.purpose,
                                      "category.research" = df$category.research, "category.pharma" = df$pharma, "category.nursing" = df$nursing, "rank.chief" = df$chief, 
                                      "rank.president" = df$president, "rank.vp" = df$vp, "rank.director" = df$director, "rank.senior" = df$senior, "rank.chair" = df$chair))

df <- as.data.frame(cbind(df, relevant.codes))
df$titles_available <- ifelse((df$job_title)=="",0,1)

