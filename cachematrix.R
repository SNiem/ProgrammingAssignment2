#The following two functions help you to save time in case of time consuming
#calculations of matrix inverses. If you have a matrix and want to calculate 
#it's inverse, it will be stored in the cache, so it doesn't has to be 
#calculated for the second time you need it.

#To use these functions, please set the input matrix (has to be a square 
#matrix due to the point, that the inverse here is calculated with solve-
#function). 

#For example:

#inMa<-makeCacheMatrix()
#inMa$set(matrix(),nrow="",ncol="")

#To have a look at this input matrix, use inMa$get().
#When you use the cacheSolve on inMa for the first time, it will calculate
#the inverse of inMa. For the second time, it will get the value of the 
#inverse from cache. You will see this by the command "Getting cached inverse"
#in you console.

###########################  Special Object  ###############################

#This function creates a Special Object that stores the input matrix and 
#caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {       
  inv <- NULL                                    
  set <- function(y) {                            
    x <<- y                                     #sets the value of the input matrix
    inv <<- NULL                            
  }
  get <- function() x                           #gets the value of the input matrix
  setinverse <- function(solve) inv <<- solve   #sets the value of the inverse
  getinverse<- function() inv                   #gets the value of the inverse
  list(set = set, get = get,                    #stores values in a list
       setinverse = setinverse,
       getinverse = getinverse)
}

########################  Calculation Function  ############################

#This function calculates the inverse of the input matrix regarding the spe-
#cial object above.

cacheSolve <- function(x=matrix(), ...) {       
  inv <- x$getinverse()                       #But first of all, it gets the value of the variable containing the inverse of input matrix,
  if(!is.null(inv)) {                         #to check, if the inverse of the 
    message("Getting cached inverse")         #input matrix is already existing 
    return(inv)                               #in the cache. In that case, it returns
  }                                           #the cached inverse.
  matrix <- x$get()                           #If not, it gets the input matrix
  inv <- solve(matrix, ...)                   #and calculates it's inverse.
  x$setinverse(inv)                           #Afterwards, it sets the inverse to the cache
  inv                                         #and prints it out
}
