# Degree Matching
df.tmt <- read.csv("Data/TMT_raw/test_data.csv")
#chief <- grepl("chief", tolower(df[,c.no]), perl = TRUE) | grepl("c[a-z]o\\b", tolower(df[,c.no]), perl = TRUE) 
c.no <- 2
df.tmt$grad_degree <- sub('\\.',"", df.tmt$grad_degree)
#Medical Degrees
md <- ifelse((grepl("md\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("m\\.d\\.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
do_degree <- ifelse((grepl("do\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("d\\.o\\.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
dpt <- ifelse((grepl("dpt\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("d.p.t.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)


#Nursing degrees
rn <- ifelse((grepl("rn", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("r.n.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
bsn <- ifelse((grepl("bsn", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("b.s.n.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
msn <- ifelse((grepl("msn", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("m.s.n.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
ccrn <- ifelse((grepl("ccrn", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("c.c.r.n.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
crnp <- ifelse((grepl("crnp", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("c.r.n.p\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
dnp <- ifelse((grepl("dnp", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("d.n.p.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
nea_bc <- ifelse((grepl("nea-bc", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("n.e.a.-b.c.\\b", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
other.nurse <- ifelse((grepl("nurs", tolower(df.tmt[,c.no]), perl = TRUE) )==TRUE,1,0)


#Med-relevant degrees
mph <- ifelse((grepl("mph", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("public health", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
mpm <- ifelse((grepl("mpm", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("mpm,", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
mha <- ifelse((grepl("mha", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("mha,", tolower(df.tmt[,c.no]), perl = TRUE)| grepl("health administration", tolower(df.tmt[,c.no]), perl = TRUE)|grepl("health care administration", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
other.med <- ifelse((grepl("medical", tolower(df.tmt[,c.no]), perl = TRUE) )==TRUE,1,0)

#Business Degrees
mba <- ifelse((grepl("mba", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("m.b.a.\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("business administration", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
cpa <- ifelse((grepl("cpa", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("c.p.a.\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("accounting", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
cfa <- ifelse((grepl("cfa", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("c.f.a.\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("finance", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)

#Law Degree
df.tmt$law <- ifelse((grepl("jd", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("j.d.\\b", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("law", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
df.tmt$pharma <- ifelse((grepl("pharmd", tolower(df.tmt[,c.no]), perl = TRUE) | grepl("pharma", tolower(df.tmt[,c.no]), perl = TRUE))==TRUE,1,0)
df.tmt$psych <- ifelse((grepl("psychology", tolower(df.tmt[,c.no]), perl = TRUE) )==TRUE,1,0)

df.tmt$degrees.doc <- md+do_degree+dpt
df.tmt$degrees.nursing <- rn+bsn+msn+ccrn+crnp+dnp+nea_bc+other.nurse
df.tmt$degrees.med.rel <- mph+mpm+mha+other.med
df.tmt$degrees.business <- mba+cpa+cfa

df.tmt[,3:9] <- apply(df.tmt[,3:9], 2, function(x) ifelse(x>0,1,0))

glimpse(df.tmt)
colSums(df.tmt[,3:9])
rowSums(df.tmt[,3:9])
df.tmt$degrees_available <- ifelse((df.tmt$grad_degree)=="",0,1)

write_csv(df.tmt, "pa_test.csv")


