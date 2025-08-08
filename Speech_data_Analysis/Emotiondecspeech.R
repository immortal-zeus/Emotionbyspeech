library(tuneR)
library(readr)
library(wrassp)
library(warbleR)
library(signal)
library(oce)
library(reticulate)
conda_create("r-reticulate")
conda_install("r-reticulate", "librosa")
libro<-import("librosa")
path_file= "D:\\Speechemotion\\speech-emotion-recognition-ravdess-data\\"
list_ofiles = list.files (path = path_file)
list_ofaudiofiles<-c() 
for(i in list_ofiles){
  temp<-list.files(path = paste(path_file,i , sep=""))
  for(j in temp){
    full_path=paste(path_file,i,"\\",j,sep="")
    list_ofaudiofiles=c(list_ofaudiofiles,full_path)
  }

}
list_ofaudiofiles
play(list_ofaudiofiles[1])
audio = readWave(list_ofaudiofiles[1])
str(audio)
audio2 = readWave(list_ofaudiofiles[20])
str(audio2)
sr2 = audio2@samp.rate
mfcc.m = na.omit(melfcc(audio, sr = sr,
                wintime = 0.015,  
                hoptime = 0.005,        
                sumpower = TRUE,        
                nbands = 40,            
                bwidth = 1,             
                preemph = 0.95))
mfcc.t = na.omit(melfcc(audio2, sr = sr2,
                        wintime = 0.015,  
                        hoptime = 0.005,        
                        sumpower = TRUE,        
                        nbands = 40,            
                        bwidth = 1,             
                        preemph = 0.95,
                        fbtype="mel"))
mfcc.t2 = na.omit(melfcc(audio2, sr = sr2,
                        wintime = 0.015,  
                        hoptime = 0.005,        
                        sumpower = TRUE,        
                        nbands = 40,            
                        bwidth = 1,             
                        preemph = 0.95,
                        fbtype="bark"))
head(mfcc.m)
a= as.vector(t(mfcc.m))
head(mfcc.t)
head(mfcc.t2)
b= as.vector(t(mfcc.t))
length(a)
length(b)
oo=4
lol=matrix(nrow=1440)
o=1
for (i in a ) {
  lol[oo,o]=i
  o=o+1
  
}
lol[1,]=a
lol[2,]=b

