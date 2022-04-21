##This function takes a matrix and then inverses it and obtains the inverse 
##either from a cache store or by solving for the inverse

#this function stores the matrix in a special function that can look for a 
#cached value for the inverse

makeCacheMatrix <- function (x = matrix()) {
       inv <- NULL
       set <- function (y) {                    #sets value of a matrix
              x <<- y
              inv <<- NULL
       }                                        
       get <- function() x                      #spits out value of matrix
       setinv <- function(inverse)inv <<- inverse #sets value of inverse of matrix
       getinv <- function() inv                 #spits out value of inverse of matrix
              list(set = set,
                   get = get,
              setinv = setinv,
              getinv = getinv)

}

#this function solves for the inverse of the matrix (x). If inverse has 
##already been calculated, it will be retrieved from a cache 


cacheSolve <- function(x,...){                  
       inv <- x$getinv() 
       #returns matrix that is the inverse of x
       if(!is.null(inv)){
              message ("getting cached data")  
              return (inv)
       }
       data <- x$get()
       inv <- solve(data,...)                   
       #inverse of matrix
       x$setinv(inv)
       inv
       }
