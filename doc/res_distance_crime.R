# Lng.crime=c(110.5,110.2,110.9)
# Lat.crime=c(55.6,56.1,55.8)
# Lng.res=c(110.4,110.6)
# Lat.res=c(55.7,55.8)

threshold=0.5

crime.count <- function(Lng.crime,Lat.crime,
                        Lng.res,Lat.res,threshold){
  if (length(Lng.crime)!=length(Lat.crime)) return("Different length")
  if (length(Lng.res)  !=  length(Lat.res)) return("Different length")
  
  n.crime=length(Lng.crime)
  n.res=length(Lng.res)
  
  Lng.crime.mx=matrix(Lng.crime,nrow=n.crime,ncol = n.res,byrow = F)
  Lat.crime.mx=matrix(Lat.crime,nrow=n.crime,ncol = n.res,byrow = F)
  
  Lng.res.mx=matrix(Lng.res,nrow=n.crime,ncol=n.res,byrow=T)
  Lat.res.mx=matrix(Lat.res,nrow=n.crime,ncol=n.res,byrow=T)
  
  dist.mx=sqrt(((Lng.crime.mx-Lng.res.mx)*52)^2+
                 ((Lat.crime.mx-Lat.res.mx)*69)^2)
  dist.mx[is.na(dist.mx)]=99999
  count.res=colSums(dist.mx<=threshold)
  count.crime=rowSums(dist.mx<=threshold)
  return(count.res)
}

yelp_data=read.csv("../data/yelp_data.csv", stringsAsFactors = F)
res=data.frame(yelp_data$latitude, yelp_data$longitude)
colnames(res) <- c("Lat.res","Lng.res")

crime = read.csv("../data/crime.csv", stringsAsFactors = F)
crime18 = crime %>%
  filter(CMPLNT_FR_DT > "2018-01-00") %>%
  mutate(CMPLNT_FR_DT = as.Date(CMPLNT_FR_DT)) %>%
  mutate(LAW_CAT_CD = as.factor(LAW_CAT_CD)) %>%
  mutate(CRM_ATPT_CPTD_CD = as.factor(CRM_ATPT_CPTD_CD))
crime18$month = gsub(x = crime18$CMPLNT_FR_DT, pattern ="[0-9]{4}-", replacement ="")
crime18$month = as.factor(gsub(x = crime18$month, pattern ="-[0-9]{2}", replacement =""))
crime18$hour = as.factor(gsub(x = crime18$CMPLNT_FR_TM, pattern =":[0-9]{2}:[0-9]{2}", replacement =""))
crime18$week = as.factor(weekdays(crime18$CMPLNT_FR_DT))
crime=data.frame(crime18$Latitude, crime18$Longitude)
colnames(crime)=c("Lat.crime","Lng.crime")

all.res.count=rep(0,nrow(res))

for (j in c(1:10)) {
  print("j=");
  print(j)
  temp.crime=crime[((j-1)*10000):(j*10000-1),]
  temp.res.count=crime.count(Lng.crime=temp.crime$Lng.crime, 
                             Lat.crime=temp.crime$Lat.crime,
                             Lng.res=res$Lng.res, 
                             Lat.res=res$Lat.res,
                             threshold)
  all.res.count=all.res.count+temp.res.count
  gc();gc();gc()
}
temp.crime=crime[100000:109498,]
temp.res.count=crime.count(Lng.crime=temp.crime$Lng.crime, 
                           Lat.crime=temp.crime$Lat.crime,
                           Lng.res=res$Lng.res, 
                           Lat.res=res$Lat.res,
                           threshold)
all.res.count=all.res.count+temp.res.count
gc();gc();gc()
write.csv(data.frame(lng=res$Lng.res,lat=res$Lat.res,counts=all.res.count),"rescount.csv")

