library(aplpack)
#**************************************
# name:face.roll
# Description:
# Random roll a face based on 4 traits: mouth, eye, hair, ear
# output a list with 1x15 matrix with the row name as "insurance" + trait code + trait desc
#**************************************
face.roll<-function(){
  baseFace<-  c(h_face=0,
                w_face=0,
                s_face=0,
                h_mouth=0,
                w_mouth=0,
                c_smile=0, # smile shape: 1: concave, -1:convex, 0:even
                h_eyes=0,  # eye height: -1:small eye, 1: big eye
                w_eyes=0,
                h_hair=0,
                w_hair=0,
                s_hair=0, # hair shape: 1: up, -1: down, 0:even
                h_nose=0,
                w_nose=0,
                w_ears=0,
                h_ears=0) # ear shape: -1:tip, 0:round
  
  smile.char<-c(1,-1)
  names(smile.char)<-c('happy','unhappy')
  
  eye.char<-c(1,-1)
  names(eye.char)<-c('big','small')
  
  hair.char<-c(1,-1)
  names(hair.char)<-c('up','down')
  
  ear.char<-c(-1,0)
  names(ear.char)<-c('tip','round')
  
  newFace<-baseFace
  trait<-c(rbinom(4,1,prob=0.5)+1)
  traitName<-paste(names(smile.char[trait[1]]),names(eye.char[trait[2]]),names(hair.char[trait[3]]),names(ear.char[trait[4]]))
  
  newFace['c_smile']<-smile.char[trait[1]]
  newFace['h_eyes']<-eye.char[trait[2]]
  newFace['s_hair']<-hair.char[trait[3]]
  newFace['h_ears']<-ear.char[trait[4]]
  faceMatrix<-matrix(newFace, nrow=1)

  company<-c('Insurance A','Insurance B')
  rownames(faceMatrix)<-company[rbinom(1,1,prob=0.5)+1]
  outputList<-list(faceMatrix, trait-1, traitName)
  return(outputList)
}
#**************************************
# Test case: roll 2 faces and print
#**************************************
fileHead<-paste('*****************Test @',Sys.time(),'******************************')
write(fileHead, "FaceCode.txt", sep="\n",append=T)
for(i in 1:10){
  face1List<-face.roll() # roll face 1
  face2List<-face.roll() # roll face 2
  if(sum(abs(face1List[[1]]-face2List[[1]]))>=1){ # faces are different
    testFaces<-rbind(face1List[[1]],face2List[[1]]) # row bind two faces in matrix
    Sys.sleep(0.1)

    #**************************************
    # Output to file
    #**************************************
    fileName<-paste(rownames(face1List[[1]]), paste(face1List[[2]], collapse=""),rownames(face2List[[1]]), paste(face2List[[2]],collapse=""), ".png", collapse="")
    png(filename=fileName,height=600,width=600)
    faces(testFaces,scale=FALSE,col.nose=terrain.colors(10),print.info=F)
    dev.off()
    
    #Output answer sheet
    detail<-paste(rownames(face1List[[1]]), paste(face1List[[2]], collapse=""),"v.s",rownames(face2List[[1]]), paste(face2List[[2]],collapse=""),collapse="")
    summary1<-paste('--- Disease 1:',face1List[[2]][1] | face1List[[2]][2],' Disease 2:',face1List[[2]][3] | face1List[[2]][4],'---')
    summary2<-paste(' Disease 1:',face2List[[2]][1] | face2List[[2]][2],' Disease 2:',face2List[[2]][3] | face2List[[2]][4],'---')
    write(paste(detail, summary1,summary2), "FaceCode.txt", sep="\n",append=T)
  }
}

