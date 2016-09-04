## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#creates a list with 4 attributes
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #sets the matrix to x 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #gets the matrix
  get <- function() x
  #sets the inverse
  setinv <- function(inverse) inv <<- inverse
  #gets the invers of the matrix
  getinv <- function() inv
  #creates list with 4 attributes
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
#returns inverse of matrix if already calculated, else calculates it and returns it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #if the inverse has already been calculated, then inv is not null, the message and the value of inv printed 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #else inverse is calculated and stored in inv
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv#is the inverse of the original matrix
}
