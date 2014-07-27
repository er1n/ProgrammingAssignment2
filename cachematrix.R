## Put comments here that give an overall description of what your
## functions do

#These functions take a matrix, convert it into a special kind of matrix that can store its inverse
#invert it and return the inverse. The inverse calculated is then saved and can be retrieved



## Write a short comment describing this function
#This function creates a list which stores
#functions to change a matrix's value, to retrieve a matrix's value, to save the inverse and to retrieve a saved inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set is to change the matrix
    x <<- y 
    m <<- NULL
  }
  get <- function() return(x) #get returns a cached inverse
  setinverse <- function(inverse) m <<- inverse #setinverse saves the inverse
  getinverse <- function() return(m) #getinverse returns the inverse (which has value null if it hasn't been previously calculated)
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function takes an object of type returned by makeCacheMatrix, and either retrieves the inverse
#or calculates, saves and returns the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse() #retrieves the mean (if it is stored)
  if(!is.null(m)) {
    message("getting cached data") 
    return(m)  #returns the mean it has retrieved (if it was stored)
  }
  data <- x$get() #if not stored, retrieve the matrix. 
  m <- solve(data) #actually calculates the inverse
  x$setinverse(m) 
  m 
}
