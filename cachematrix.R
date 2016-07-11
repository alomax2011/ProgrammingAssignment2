## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
+#makeCacheMatrix is a matrix that can cache its inverse
+#sets the value of the input matrix
+#gets the value of the input matrix
+#sets the value of the matrix inverse
+#gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
+  inv_x <- NULL
+  set <- function(y) {
+    x <<- y
+    inv_x <<- NULL
+  }
+  get <- function() x
+  setinverse<- function(inverse) inv_x <<-inverse
+  getinverse <- function() inv_x
+  list(set = set, get = get,
+       setinverse = setinverse,
+       getinverse = getinverse)
}


## Write a short comment describing this function
+## cacheSolve computes inverse of the special matrix
+#returned by 'makeCacheMatrix'
+#It checks to see if the inverse has already been calculated
+#if inverse is NULL, "special" matrix is calculated using 
+#'solve' and the inverse is set using the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
+  inv_x <- x$getinverse()
+  if (!is.null(inv_x)) {
+    message("getting cached inverse matrix")
+    return(inv_x)
+  } else {
+    inv_x <- solve(x$get())
+    x$setinverse(inv_x)
+    return(inv_x)
+  }
