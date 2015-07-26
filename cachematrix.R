## Programming assignment2, attempt 1, building on the example code
#Assignment: Caching the Inverse of a Matrix
#Write the following functions:
  
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#setwd("/Users/daniellerobinson/Documents/R Programming Coursera") #in case you haven't done it

#things that might be useful: solve() to get the inverse of a matrix such that solve(x) %*% x
#first I make a matrix
#a <- makeCacheMatrix() #a gets makeCacheMatrixa$ses
#a$set(matrix(1:4,2,2)) #makes 
      
makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setmatrix<- function(solve) m <<- solve #set the value of the matrix
  getmatrix <- function() m #get the value of the matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Hopefully return a matrix that is the inverse of 'x'
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
   m<-x$getmatrix()
   if(!is.null(m)){
    message("getting cached data")
        return(m) }
     matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m}


