*+  AIF_SUMX2D - Sum DOUBLE PRECISION 2-D array in 1st (X) dimension to give 1-D array
      SUBROUTINE AIF_SUMX2D( DIMS, ARR2D, ARR1D, STATUS )
*    Description :
*     This takes the 2-dimensional DOUBLE PRECISION array ARR2D and sums together its
*     contents in its first (X) dimension to give a 1-dimensional DOUBLE PRECISION array
*     ARR1D.
*     E.g.
*                 ARR2D          ARR1D
*                1 6 3 7          17
*                4 5 2 3   --->   14
*                8 9 5 6          28
*    Invocation :
*     CALL AIF_SUMX2D( DIMS, ARR2D, ARR1D, STATUS )
*    Parameters :
*     DIMS( 2 )                 = INTEGER( READ )
*           dimensions of 2-D array
*     ARR2D( DIMS(1), DIMS(2) ) = DOUBLE PRECISION( READ )
*           2-D array to be summed in the X direction
*     ARR1D( DIMS(2) )          = DOUBLE PRECISION( WRITE )
*           1-D array resulting from summation of ARR2D in X direction
*     STATUS                    = INTEGER( UPDATE )
*           Global status. If this has an error value on entry then
*           this routine will terminate without execution. If an error
*           occurs during execution of this routine, then STATUS will
*           be returned with an appropriate error value.
*    Method :
*     If no error on entry then
*        Loop through the second dimension of 2-D array
*           initialise the corresponding element of 1-D array
*           Loop through the first dimension of 2-D array
*              sum values into element of the 1-D array
*           Endloop
*        Endloop
*     Endif
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (ROE::SMB)
*    History :
*     21/06/1984:  original version (ROE::SMB)
*    Type Definitions :
      IMPLICIT NONE         ! must declare all variables
*    Global constants :
      INCLUDE 'SAE_PAR'     ! SSE global constants
*    Import :
      INTEGER
     :  DIMS( 2 )                  ! dimensions of arrays
      DOUBLE PRECISION
     :  ARR2D( DIMS(1), DIMS(2) )  ! 2-D array to be summed
*    Export :
      DOUBLE PRECISION
     :  ARR1D( DIMS(2) )           ! resultant 1-D array
*    Status :
      INTEGER
     :  STATUS   ! global status
*    Local variables :
      INTEGER
     :  I1,      ! index to first dimension of array
     :  I2       ! index to second dimension of array
*-

*   check for error on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*      scan the 2-D array in its second dimension
         DO I2 = 1,DIMS(2)

*         initialise the corresponding element of the 1-D array
            ARR1D( I2 ) = 0

*         sum all the values in the first dimension of the 2-D array
*         into this element of the 1-D array
            DO I1 = 1,DIMS(1)

               ARR1D( I2 ) = ARR1D( I2 ) + ARR2D( I1, I2 )

            ENDDO

         ENDDO

      ENDIF

      END
