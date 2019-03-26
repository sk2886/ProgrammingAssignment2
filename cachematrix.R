## Function calculates the inverse of a matrix using solve function
## If the inverse is already in the cache it will return it instead of calculating

## This function creates setters and getters for matrix and the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) inv <<- solve
        getmatrix <- function() inv
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## This function takes the matrix and checks if the inverse is in cache 
#If not in cache, alculates inverse and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmatrix()
        if(!is.null(inv)){
                print("return cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setmatrix(inv)
        inv
}
