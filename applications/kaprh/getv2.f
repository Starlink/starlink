*+  GETV2 - Returns the value of a specified point in a 2-d array.

      SUBROUTINE GETV2( INARR, DIM1, DIM2, X, Y, VALUE, STATUS )
*
*    Description :
*
*     This routine accepts the co-ordinates of a point
*     on an image and determines the value of the image
*     at that point, which is returned to the calling routine.
*
*    Invocation :
*
*     CALL GETV2( INARR, DIM1, DIM2, X, Y, VALUE, STATUS )
*
*    Arguments :
*
*     INARR( DIMS1, DIMS2 ) = REAL( READ )
*         The 2-dimensional array.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     X = INTEGER( READ )
*         The X co-ordinate of the point whose value is to be
*           determined.
*     Y = INTEGER( READ )
*         The Y co-ordinate of the point whose value is to be
*           determined.
*     VALUE = REAL( WRITE )
*         The value of the specified pixel.
*     STATUS = INTEGER( READ, WRITE )
*         The status value on entry to this subroutine.
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     If input element numbers are outside bounds of the array then
*        Report the error and set bad status
*     Else
*        The value is found by indexing the image array using the
*          co-ordinates given.
*     Endif
*     End
*
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1988 Sep  8 : Original (RL.STAR::CUR).
*     1989 Jul 27 : Passed the array dimensions as two variables
*                   (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global Constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  DIM1, DIM2,
     :  X, Y

      REAL
     :  INARR( DIM1, DIM2 )

*    Export :

      REAL
     :  VALUE

*    Status :

      INTEGER STATUS

*-

*    If the status is bad, then return to the main program.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( X .LT. 1 .OR. X .GT. DIM1 .OR.
     :     Y .LT. 1 .OR. Y .GT. DIM2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_GETV2_POA',
     :     'GETV2: Element numbers outside array bounds.', STATUS )
      ELSE

*       Reference the image array using the point co-ordinates.

         VALUE = INARR( X, Y )
      END IF

      END
