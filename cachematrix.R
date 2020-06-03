## This piece of r code contains two functions
## 1.makeCacheMatrix
      ##creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve
      ##computes the inverse of the special "matrix" returned by makeCacheMatrix


##this function returns a list which contains individual functions to  get,set the matrix
##and to get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function is to find the inverse of the matrix
##It checks if the inverse of the matrix has been already calculated if yes it returns the same 
##if no it calculates the inverse and updates it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
j<-x$getInverse()
if(!(is.null(j))){
  message("getting results from cache")
  return(j)
}
data<-x$get()
j<-solve(data,...)
x$setInverse(j)
j
}
