## 1st function will calculate the inverse and
#cache it's value and 2nd function will return the inverse value if already exist

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

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

# This function will get inverse from cache(if it has been calculated earlier).
# Else it will calculate the inverse again.
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

# The following is the test case.
B = matrix(c(2, 4, 3, 1, 5, 7, 6, 9, 8), nrow=3, ncol=3) 
y = makeCacheMatrix(B)
z = cacheSolve(y)  
print(z)





#Just Adding comment to commit through Branch

#Another commit