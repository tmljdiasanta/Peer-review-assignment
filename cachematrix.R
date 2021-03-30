## The overall functions carry out the instructions for the inversion
## of a given matrix. These functions make computation easier and more
## time-effective.


## The makeCacheMatrix function lists the value of the object and
## inverse (as defined in the set, get, setInverse and getInverse)

makeCacheMatrix <- function(x=matrix()){
  matinv <- NULL
  set <- function(y){
    x <<- y
    matinv<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse){matinv<<-inverse}
  getInverse<-function()matinv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The cacheSolve function is a response to the makeCacheMatrix function.
## If inverse (matinv) has already been calculated and still returns as
## null, the function adds a message beforehand and repeats the
## previous response.

cacheSolve<-function(x,...){
  matinv<-x$getInverse()
  if(!is.null(matinv)){
    message("null inverse: getting cached data")
    return(matinv)
  }
  mat<-x$get()
  matinv<-solve(mat,...)
  x$setInverse(matinv)
  matinv
}
        ## Return a matrix that is the inverse of 'x'
