## Matrix inversion is usually time-consuming computation
## these two functions calculate and cache the inverse of a matrix. 


## makeCacheMatrix creates a special "matrix", which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  invmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       invmatrix=invmatrix,
       getmatrix=getmatrix)
}


## cacheSolve function calculates the inverse of the special "matrix" created with the above function.
## it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$invmatrix(m)
  m
}