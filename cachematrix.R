##This set is comprised of the makeCacheMatrix() and cacheSolve() functions.
##They are used to solve the inverse of Martrix and a to cache the results. 
##This is helpful because it helps reduce processing time.  


##Work Flow
##1: The makeCacheMartrix() function creates the matrix.
##2: and then sets the list of subfunctions that will be passed to cacheSolve().
##3: cacheSolve() first checks to see if the inverse of the martrix has been solved.
##4: If it has, the results will be returned.
##5: If it has not, it will then solve the inverse and then return the findings.

##makeCacheMatrix()
##set the martrix
##place four subfunctions in a list 

makeCacheMatrix <- function(x = matrix()) {
  
  ## this NUll arguement cost me an hour of my life
  m <- NULL
  
  ##sets the four subfunctions
  get <- function() x
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  
  ##places the four subfunctions in a list to be used by cacheSolve() 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##cacheSolve()
##checks to see of the inverse for the martrix has been solved
##if so returns cached invesere
##if not, calculates and returns inverse of matrix

cacheSolve <- function(x, ...) {
  
  
  m <- x$getinverse()
  
  ##checks to see if the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##if not, calculates the inverese of matrix used solve()
  data <-x$get()
  m <- solve(data)
  x$setinverse(m)
  
  ##returns the results. 
  m
}


##Have a nice day.
