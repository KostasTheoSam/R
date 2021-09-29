## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #Initialize the x as a matrix
        X_inv <- NULL #Initialize the inverse matrix as NULL
        set <- function (y) {
                x <<- y
                X_inv <<- NULL
        }
        get <- function() x #Retrieve the x value (getter)
        set_inv <- function(Inverse) X_inv <<- Inverse #Set the Inverse value (setter)
        get_inv <- function() X_inv #Get the inverse value (setter)
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv) #So makeCacheMatrix is a list object with 4 fncs

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        X_inv <- x$get_inv() #Retrieve the inverse matrix of x
        if (!is.null(X_inv)) {
                message("getting cached data")
                return(X_inv)
        }
        #This section continues if there is no cached inverse
        data <-x$get() #Retrieve the x matrix
        X_inv <- solve(data) #Compute the inverse matrix
        x$set_inv(X_inv) #Cache the inverse
        X_inv ## Return a matrix that is the inverse of 'x'
}


#Example use of code
A <- matrix(c(2,3,4,1,2,3,2,3,1), nrow = 3, ncol = 3)
A_obj <- makeCacheMatrix(A) #Create a makeCacheMatrix object, list with 4 fncs
print(cacheSolve(A_obj)) #Cache the inverse matrix, 1st call
print(cacheSolve(A_obj)) #Retrieve the cached matrix, 2nd call