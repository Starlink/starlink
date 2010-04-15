*+  AIF_SUMY2UW - Sum INTEGER*2 2-D array in 2nd (Y) dimension to give 1-D array
      SUBROUTINE AIF_SUMY2UW( DIMS, ARR2D, ARR1D, STATUS )
*    Description :
*     This takes the INTEGER*2 2-dimensional array ARR2D and sums together its
*     contents in its second (Y) dimension to give a INTEGER*2 1-dimensional
*     array ARR1D.
*     E.g.
*                1  6  3  7
*      ARR2D     4  5  2  3
*                8  9  5  6
*
*                    |
*                    V
*
*      ARR1D     13 20 10 16
*    Invocation :
*     CALL AIF_SUMY2UW( DIMS, ARR2D, ARR1D, STATUS )
*    Parameters :
*     DIMS( 2 )                 = INTEGER( READ )
*           dimensions of 2-D array
*     ARR2D( DIMS(1), DIMS(2) ) = INTEGER*2( READ )
*           2-D array to be summed in the Y direction
*     ARR1D( DIMS(1) )          = INTEGER*2( WRITE )
*           1-D array resulting from summation of ARR2D in Y direction
*     STATUS                    = INTEGER( UPDATE )
*           Global status. If this has an error value on entry then
*           this routine will terminate without execution. If an error
*           occurs during execution of this routine, then STATUS will
*           be returned with an appropriate error value.
*    Method :
*     If no error on entry then
*        initialise the 1-D array
*        Loop through the second dimension of 2-D array
*           Loop through the first dimension of 2-D array
*              sum the values into the appropriate element of the 1-D array
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
      INTEGER*2
     :  ARR2D( DIMS(1), DIMS(2) )  ! 2-D array to be summed
*    Export :
      INTEGER*2
     :  ARR1D( DIMS(1) )           ! resultant 1-D array
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

*      initialise the 1-D array
         DO I1 = 1,DIMS(1)
            ARR1D( I1 ) = 0
         ENDDO

*      scan the 2-D array in its second dimension
         DO I2 = 1,DIMS(2)

*         scan the 2-D array in its first dimension
            DO I1 = 1,DIMS(1)

*            sum the values into the appropriate element of the 1-D array
               ARR1D( I1 ) = ARR1D( I1 ) + ARR2D( I1, I2 )

            ENDDO

         ENDDO

      ENDIF

      END
