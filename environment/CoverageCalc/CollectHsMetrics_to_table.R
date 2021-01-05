args = commandArgs(trailingOnly=TRUE)
input <- args[1]
output <- args[2]


library(stringr)
library(dplyr)
library(tidyr)


CollectHsMetrics<-read.table(input, header = TRUE, as.is = TRUE, sep="\t",dec=",")


df_raw<-data.frame(name=CollectHsMetrics$name,min_coverage=CollectHsMetrics$min_coverage, pos=paste0(CollectHsMetrics$chrom,":",CollectHsMetrics$start,"-",CollectHsMetrics$end),length=CollectHsMetrics$length)
df<-df_raw %>%
  mutate(name = strsplit(name, "\\|")) %>%
  filter(name != "") %>%
  unnest() %>%
  select(name, min_coverage,pos,length)

df<-as.data.frame(df)

df<-within(df, lovcov_length <- ifelse(min_coverage<10, length,0 ))
df<-within(df, lovcov_pos<- ifelse(min_coverage<10, pos,"" ))


df_length<-data.frame(name=df$name,length=df$length)
df_length<-aggregate(.~name,df_length,sum)

df_lovcov_length<-data.frame(name=df$name,lovcov_length=df$lovcov_length)
df_lovcov_length<-aggregate(.~name,df_lovcov_length,sum)

df_lovcov_pos<-data.frame(name=df$name,lovcov_pos=df$lovcov_pos)
df_lovcov_pos<-aggregate(.~name,df_lovcov_pos,paste, collapse = " ")


df_final<-merge(df_length,df_lovcov_length,by="name")
df_final<-merge(df_final,df_lovcov_pos,by="name")

df_final$lovcov_pos<-str_squish(df_final$lovcov_pos)
df_final$perc<-(1-(df_final$lovcov_length/df_final$length))*100

table<-cbind(df_final$name,df_final$perc,df_final$lovcov_pos)
colnames(table)<-c("gene_id","perc_covered","lovcov_positions")
write.table(table,output,sep="\t",quote = FALSE,row.names=FALSE)


print("#################################")
print(paste0("RESULTS IN ",output))
print("#################################")