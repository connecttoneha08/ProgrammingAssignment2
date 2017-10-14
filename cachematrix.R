makeCacheMatrix <- function( myMatrix = matrix() ) {
   inv <- NULL

   # first a function to set the myMatrix
   setMatrix <- function( newMatrix ) {
      myMatrix <<- newMatrix
      # inverse would change
      inv <<- NULL
   }

   # function to get the myMatrix
   getMatrix <- function() myMatrix
 
   # function to set the value of inverse
   setInverse <- function( inverse ) {
      inv <<- inverse
   }

   # finally, a function to get inverse
   getInverse <- function() inv

   # create a list of functions to get inverse of a myMatrix and set it
   list( setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse )

}

cacheSolve <- function( x, ... ) {

   # get the inverse for this matrix object
   inverse <- x$getInverse()

   # see if it's sane
   if( !is.null( inverse ) ) {
      # inverse found in cache, let's get it from there and be done
      return( inverse ) 
   }
   
   # can't find inverse in cache, calculate and save
   matrix <- x$get()
   inverse <- solve( matrix )
   x$setInverse( inverse )
   
   inverse
}
