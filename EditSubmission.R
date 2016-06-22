## Put comments here that give an overall description of what your
## functions home/Arnab/Desktop/Coursera/Data Science Specialization/R Programming/ProgrammingAssignment2-master/cachematrix.R
## My function is going to return a inverse matrix and cache it.
## Shall the inverse already been calculated, it will retrieve the inverse from cache instead 
## of re-calculate.

## Write a short comment describing this function
## Cache a inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function()  x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setinverse(i)
  i
}

mymatrix <- makeCacheMatrix(matrix(1:4,2))
mymatrix$get()
mymatrix$getinverse()
cacheSolve(mymatrix)
mymatrix$getinverse()
# tes