## 1st function will calculate the inverse and
#catch it's value and 2nd function will return the inverse value if already exist

makeCacheMatrix <- function(x = matrix()) {
  z<- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setInv <- function(solve) z <<- solve
  getInv <- function() z
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}

cacheSolve <- function(x, ...) {
  z <- x$getInv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setInv(z)
  z
}


B = matrix(c(2, 4, 3, 1, 5, 7, 6, 9, 8), nrow=3, ncol=3) 
y = makeCacheMatrix(B)
z = cacheSolve(y)  


print(z)

