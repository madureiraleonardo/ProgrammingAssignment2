## The functions here allow for an optimized computation of the inverse of a matrix,
## as the computation occurs only when necessary--that is, when a new matrix is in place and
## its inverse has not been computed yet. The process takes two functions
## 
## The function 'makeCacheMatrix' creates a 'matrix' structure with some attached functions to 
## store and retrieve a matrix and its inverse. The matrix is stored in the variable 'x' and 
## its inverse is stored in the variable 'm'. The function 'cacheSolve' computes the inverse 
## of the matrix stored by 'makeCacheMatrix', but only when necessary.
##
## The way to communicate the need to compute a new inverse matrix is to check the contents of 
## the variable 'm': it it is NULL then the matrix needs to be computed. When storing a new matrix,
## the function 'setmatrix' checks whether the new matrix is the same as the already stored 
## matrix; if they are different, then the stored inverse is set to NULL, later indicating to 
## 'CacheSolve' that a new inverse will have to be computed
##
## Later on there are two functions to compare the time (in seconds) it takes to compute the 
## inverse of ## 'n' matrices, instead of the time to fetch the stored inverse 'n' times. Both
## function work on a 4 x 4 matrices. The difference is that the function 'loopNewMatrix' updates
## the stored matrix every iteration, which forces the computation of the inverse, while
## 'loopSameMatrix' always store the same matrix. 
## In my machine it takes roughly 4 times as much to run the 'loopNewMatrix' when compared 
## to running 'loopSameMatrix'.

####################################################################################
## Function: makeCacheMatrix
##
##   This function creates a matrix structure to store and retrieve the values of a 
##   a matrix and its inverse. When the function is initially called, both the matrix
##   and its inverse are set to NULL. 
##
####################################################################################
makeCacheMatrix <- function() {
  m<<-NULL
  x<<-NULL
  getmatrix<-function(){
    x
  } 
  setmatrix <-function(y){
    ## There are some conditions under which we set the stored inverse to NULL to
    ## flag the need of computation of the inverse. These conditions appear in the 
    ## three different 'if'S.
    ##
    ##  1. If the currently stored matrix is NULL, then the matrix 'y' is new
    if(is.null(x)){
      m<<-NULL
    }
    else 
      ##  2. If there is some currently stored matrix (because condition 1 was false) and
      ##     the current matrix and the new matrix 'y' have different dimensions, then
      ##     'y' is different from the currently stored matrix 
      if(nrow(x)!=nrow(y) | ncol(x)!=ncol(y)){
        m<<-NULL
      } 
      else
        ##  3. If the currently stored matrix and the new matrix 'y'  have the same dimensions
        ##     (because conditions 1 and 2 are false) and there is at least one different cell 
        ##      between these two matrices, then 'y' is different from the currently  stored 
        ##      matrix
        if( sum(x!=y)>0){
          m<<-NULL
        }
    x<<-y
  }
  getinverse<-function(){
    m
  }
  setinverse<-function(inverse){
    m<<-inverse
  }
  list(setmatrix=setmatrix,getmatrix=getmatrix,getinverse=getinverse,setinverse=setinverse)
}


####################################################################################
## Function: cacheSolve
##
##   This function computes the inverse of a matrix if that inverse is not already
##   cached.
##
####################################################################################

cacheSolve <- function(x) {

  imatrix<-x$getinverse()
  ## If stored inverse is null then the inverse needs to be computed
  ## and then stored in the matrix structure
  if(is.null(imatrix)){
    #print("Computing inverse")
    imatrix<-solve(x$getmatrix())
    x$setinverse(imatrix)
  }
  else{
    #print("Getting cached inverse")
  }
  imatrix
}

####################################################################################
## Function: loopNewMatrix
##
##   This function loops 'n' times in computing the inverse when the matrix
##   has been changed, then measures how long the procedure takes
##
####################################################################################
loopNewMatrix<-function(n=100){
  c<-makeCacheMatrix()
  z<-matrix(1:16,nrow=4,ncol=4)
  z[1,1]=100
  z[2,2]=42
  t1<-Sys.time()
  for(i in 1:n){
    upd=100+rnorm(1)
    nonupd=100
    z[1,1]=upd
    c$setmatrix(z)
    cacheSolve(c)
  }
  t2<-Sys.time()
  t2-t1
}
####################################################################################
## Function: loopSameMatrix
##
##   This function loops 'n' times in computing the inverse when the matrix
##   has NOT been changed, then measures how long the procedure takes
##
####################################################################################
loopSameMatrix<-function(n=100){
  c<-makeCacheMatrix()
  z<-matrix(1:16,nrow=4,ncol=4)
  z[1,1]=100
  z[2,2]=42
  t1<-Sys.time()
  for(i in 1:n){
    upd=100+rnorm(1)
    nonupd=100
    z[1,1]=nonupd
    c$setmatrix(z)
    cacheSolve(c)
  }
  t2<-Sys.time()
  t2-t1
}
