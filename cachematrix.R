# These functions solve and cache the inverse of a matrix instead of calculating it repeatedly 
# thus, saving precious computation time.

#makeCacheMatrix when called, it creates a list of functions with the variable inverse set to NULL as default. 
#It stores the matrix and the inverse of the matrix that will be solved by cacheSolve()


makeCacheMatrix <- function(x = matrix())#input x is a matrix 
{
  inverse <- NULL #set the default value of the inverse matrix to NULL
  set <- function(y)#Takes a matrix as an input  
  {
    x <<- y #saves the input matrix 
    inverse <<- NULL #inverse is set to NULL
  }
  get <- function() x #returns the value of the input matrix
  setinverse <- function(inv) inverse <<- inv 
  # cacheSolve calls for this function to store the inverse of the original matrix
  getinverse <- function() inverse #this function will return the cached matrix to cacheSolve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       #list of internal functions used by makeCacheMatrix
  )
}

#cacheSolve calculates the inverse of the matrix created by makeCacheMatrix(). It first checks to see if the inverse had been solved. If so, it 
#retrieves the cached inverse matrix and skips the calculation. If not, it calculates the inverse of the matrix and caches it.

cacheSolve <- function(x, ...)# input x is created by makeCacheMatrix 
{
  inverse<-x$getinverse()# checks the value of inverse matrix
  if(!is.null(inverse))# If it had been calculated before it gets the value
  {
    message("getting cached data")# prints this on the screen while it is retrieving the value.
    return(inverse)
  }
  data<-x$get()# If the inverse of the matrix is being calculated the first time,
  inverse<-solve(data)# it does the calculation
  x$setinverse(inverse) # stores the calculated value
  inverse# return the inverse of the matrix
}
