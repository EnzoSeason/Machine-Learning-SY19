classifieur_astronomie <- function(dataset){
  library('randomForest')
  library("fastDummies")
  load("env.Rdata")
  dataset<-dataset[,-which(names(dataset)=='objid')]; dataset<-dataset[,-which(names(dataset)=='rerun')]
  dataset$camcol<-as.factor(dataset$camcol)
  pred<-predict(rf.astro,newdata=dataset,type='class')
  return(pred)
}

regresseur_mais <- function(dataset){
  library('kernlab')
  load("env.Rdata")
  # delete the coloum "X"
  dataset<-dataset[,-1]
  pred<-predict(svr,newdata=dataset) 
  return(pred)
}

classifieur_images <- function(dataset) {
  library(keras)
  library(EBImage)
  model<-load_model_hdf5("model.h5")
  images<-list()
  #read images
  for(i in 1:length(dataset)){
    images[[i]]<-readImage(dataset[i]);
  }
  #resize images
  for(i in 1:length(dataset)){
    images[[i]]<-resize(images[[i]], 32,32)
  }
  # transform images for the input of CNN
  if(length(dataset) == 1){
    images<-combine(images)
    images<-array(images, dim = c(1, 32, 32,3))
  } else {
    images<-combine(images)
    images<-aperm(images, c(4,1,2,3))
  }
  # predict
  pred<-model %>% predict_classes(images)
  output<-rep(1:length(pred),0)
  for (i in 1:length(pred)){
    if (pred[i] == 0){
      output[i] = "car"
    }
    if (pred[i] == 1){
      output[i] = "cat"
    }
    if (pred[i] == 2){
      output[i] = "flower"
    }
  }
  return(output)
}
