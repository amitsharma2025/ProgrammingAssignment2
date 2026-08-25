## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# this function return list of setting and getting matrix and it's inverse
#it store them in parent environment instead of storing them locally using "<<-", 
#so matrix and inverse are available in subsequent call, if they are stored earlier
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setinverse = setInverse,
       getinverse = getInverse)
}


## Write a short comment describing this function

#take modified matrix created using makeCacheMatrix as argument
#check if it is already has inverse, if yes then return inverse.
#otherwise calculate inverse and store it and return the inverse
cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

#above code can be tested using following commands

mat1 <- matrix(rnorm(25, 10, 2), 5, 5)
mat2 <- matrix(rnorm(25, 10, 2), 5, 5)
chMatrix <- makeCacheMatrix()
chMatrix$set(mat1)
inv1 <- cacheSolve(chMatrix)
inv2 <- cacheSolve(chMatrix)

chMatrix$set(mat2)
inv3 <- cacheSolve(chMatrix)
inv4 <- cacheSolve(chMatrix)  
  
chMatrix$set(mat1)
inv5 <- cacheSolve(chMatrix)
inv6 <- cacheSolve(chMatrix)  

