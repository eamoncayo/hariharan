## functions that helps in caching the inverse of a matrix
## matrix inverse is calculated and is cached
## cached inverse is displayed for unchanged data.. new inverse calculated for change in data

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## defines x as a matrix
  i  <- NULL                                 ## stores inverse value , initialised as NULL
  set  <- function(y){                       ## set function to set the value of matrix
    x <<- y                                  ##  assigns the matrix that is set to x
    i <<- NULL 
  }
  get  <- function() x                      ## get function displays the matrix
  setinverse  <- function(inverse) i  <<- inverse  ## sets the inverse of the matrix to i
  getinverse  <- function() i                     ## displays the value of inverse stored in i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}

## this function calculates the inverse of the matrix created in the above function
## checks if the inverse is already calculated or not
## if calculated, it gets the value from cache and skips computing the inverse
## if not, it computes and sets the new inverse value using setinverse function

cacheSolve <- function(x, ...) {
  
  i  <- x$getinverse()                      ## gets the inverse from the above function
  if (!is.null(i)){                        ## checks if the inverse value is present or not ( already computed means not NULL )
    message("getting cached data")         ## if not NULL, value already present , so cached data is obtained
    return(i)                               ## return inverse
  }
  data  <- x$get()           ## if not present , calculate the inverse for the data
  i  <- solve(data, ...)       ## solve function calculates the inverse of a matrix
  x$setinverse(i)           ## new inverse value is set using setinverse()
  i
  
       
}
