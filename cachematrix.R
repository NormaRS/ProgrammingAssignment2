## Assignment 2 Week 3 Part 1 Course: R Programming
## Lexical Scoping
## Name: Norma Ruiz
## Date: March 20th, 2015
##
## Function: makeCacheMatrix 
## Creates a special matrix object that can cache its inverse
##  
makeCacheMatrix <- function(x=matrix()) {
  invx <- NULL                 ## inverse matrix
  print(environment())         ## print environment
  evn <- environment()
  print(parent.env(evn))       ## print parent environment
  set <- function(y) {
    x <<- y                    ## assign different environment
    invx <<- NULL              
  }
  get <- function() x
  setmatrix <- function(solve) invx <<- solve(x)  ## inverse matrix in cache
  getmatrix <- function() invx                    ## get inverse from cache
  getevn <- function() environment()
  list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix,getevn=getevn)
}
##
## Function: cacheSolve 
## Computes the inverse of the special matrix returned by
## makeCacheMatrix.R. If the inverse has already been calculated
## then the cacheSolve should retrieve the inverse form the cache.
##
cacheSolve <- function(mx, ...) {
  invm <- mx$getmatrix()        ## if inverse matrix exists retrieve it
  if (!is.null(invm)) {
    message("getting matrix cached data")
    return(invm)                 ## exit function
  }
  data <- mx$get()               ## if inverse does not exist in cache
  invm <- solve(data, ...)       ## compute the inverse matrix
  mx$setmatrix(invm)
  invm
}