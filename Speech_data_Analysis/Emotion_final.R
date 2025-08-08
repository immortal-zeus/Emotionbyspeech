library(tuneR)
library(readr)
library(wrassp)
library(warbleR)
library(caret)
library(RSNNS)
emotions = c("01","02","03","04","05","06","07","08")
names(emotions) = c('neutral','calm','happy','sad','angry','fearful','disgust','surprised')
emotions
path_file= "D:\\Speechemotion\\speech-emotion-recognition-ravdess-data\\"
list_ofiles = list.files (path = path_file)
list_ofaudiofiles<-c()
all_audio_emotion<-c()
number_emotion<-c()
for(i in list_ofiles){
  temp<-list.files(path = paste(path_file,i , sep=""))
  for(j in temp){
    all_audio_emotion_temp= strsplit(j ,"-")
    qwer=as.numeric(all_audio_emotion_temp[[1]][3])
    wer=emotions[qwer]
    full_path=paste(path_file,i,"\\",j,sep="")
    number_emotion=c(number_emotion,qwer)
    list_ofaudiofiles=c(list_ofaudiofiles,full_path)
    if(is.na(wer)){
      bob=1
      
    }
    else{
      all_audio_emotion =c(all_audio_emotion,names(emotions[qwer]))
      #full_path=paste(path_file,i,"\\",j,sep="")
      #list_ofaudiofiles=c(list_ofaudiofiles,full_path)
      
    }
    
  }
  
}
p=1
vector_mat=matrix(ncol = 12*4,nrow = length(list_ofaudiofiles))
for(k in list_ofaudiofiles){
  audio = readWave(k)
  sr=audio@samp.rate
  mcff.this1 = na.omit(melfcc(audio, sr = sr,
                             wintime = 0.015,  
                             hoptime = 0.005,        
                             sumpower = TRUE,        
                             nbands = 40,            
                             bwidth = 1,             
                             preemph = 0.95,
                             fbtype="bark"))
  mcff.this2 = na.omit(melfcc(audio, sr = sr,
                             wintime = 0.015,  
                             hoptime = 0.005,        
                             sumpower = TRUE,        
                             nbands = 40,            
                             bwidth = 1,             
                             preemph = 0.95,
                             fbtype="mel"))
  mcff.this3 = na.omit(melfcc(audio, sr = sr,
                             wintime = 0.015,  
                             hoptime = 0.005,        
                             sumpower = TRUE,        
                             nbands = 40,            
                             bwidth = 1,             
                             preemph = 0.95,
                             fbtype="htkmel"))
  mcff.this4 = na.omit(melfcc(audio, sr = sr,
                             wintime = 0.015,  
                             hoptime = 0.005,        
                             sumpower = TRUE,        
                             nbands = 40,            
                             bwidth = 1,             
                             preemph = 0.95,
                             fbtype="fcmel"))

  all_mcff=c(colMeans(mcff.this1),colMeans(mcff.this2),colMeans(mcff.this3),colMeans(mcff.this4))
  l=1
  for(loi in all_mcff){
    vector_mat[p,l]=loi
    l=l+1
  }
  p=p+1
}
head(all_audio_emotion,20)
head(vector_mat)
vector_mat_t = t(vector_mat)


colnames(vector_mat_t)

write.csv(vector_mat_t,file="test.csv")
wop = splitForTrainingAndTest(vector_mat_t,all_audio_emotion,ratio = 0.65)
model123= mlp(wop$inputsTrain,wop$targetsTrain)







