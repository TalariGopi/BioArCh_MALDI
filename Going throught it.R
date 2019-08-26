peo_trypsin <- ms_iso("SYSMEHFRWGKPVGKKR")

df<- list()
for (i in (standards$seq)){
  df[[i]] <- ms_iso(i)
#df[[i]] <- append(df,df)
}
df
