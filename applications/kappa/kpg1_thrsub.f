      SUBROUTINE KPG1_THRSUB( BAD, EL, INARR, THRLO, THRHI, NEWLO,
     :                         NEWHI, OUTARR, NREPLO, NREPHI, STATUS )
*+
*  Name:
*     KPG1_THRSx
 
*  Purpose:
*     Sets pixels in array to defined new values outside limits.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_THRSx( BAD, EL, INARR, THRLO, THRHI, NEWLO, NEWHI,
*                      OUTARR, NREPLO, NREPHI, STATUS )
 
*  Description:
*     This routine takes an array and sets all values above a defined
*     upper threshold to a new defined value, and sets all those below a
*     defined lower threshold to another defined value.  In practice,
*     all values outside the two thresholds may be set to zero or
*     the bad value, for example.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*         The bad-pixel flag.  If it is .TRUE., tests are made for bad
*         array values.  When .FALSE., no tests are made for bad values,
*         and any encountered are treated literally.
*     EL = INTEGER (Given)
*         Dimension of the input and output arrays.
*     INARR( EL ) = ? (Given)
*         Input data to be thresholded.
*     THRLO = ? (Given)
*         Upper threshold level.
*     THRHI = ? (Given)
*         Lower threshold level.
*     NEWLO = ? (Given)
*         Value to which pixels below THRLO will be set.
*     NEWHI = ? (Given)
*         Value to which pixels above THRHI will be set.
*     NREPLO = ? (Returned)
*         The number of values less than the lower threshold and
*         substituted.
*     NREPHI = ? (Returned)
*         The number of values greater than the upper threshold and
*         substituted.
*     OUTARR( EL ) = ? (Returned)
*         Output thresholded data.
*     STATUS = INTEGER (Given)
*         Global status.
 
*  Algorithm:
*     - For all pixels of input array compare the value.  If pixel is
*     invalid or between the thresholds copy the input value straight
*     into the output value.  If the value is less than lower threshold
*     the output value equals NEWLO, and if it's greater than upper
*     threshold the output value is NEWHI.  Count the substitutions.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     arrays and values supplied to the routine must have the data type
*     specified.
 
*  Authors:
*     MJC: Malcolm Currie  (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 November 6 (MJC):
*        Original version.
*     1996 April 11 (MJC):
*        Added BAD argument, and its consequences.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_new_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT  NONE                 ! No default typing allowed
 
*  Global Constants:
      INCLUDE  'SAE_PAR'             ! SSE global definitions
      INCLUDE  'PRM_PAR'             ! Bad-value definitions
 
*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      BYTE INARR( EL )
      BYTE THRLO
      BYTE THRHI
      BYTE NEWLO
      BYTE NEWHI
 
*  Arguments Returned:
      BYTE OUTARR( EL )
      INTEGER NREPLO
      INTEGER NREPHI
 
*  Status:
      INTEGER STATUS
 
*  Local Variables:
      INTEGER J                      ! Loop counter
 
*.
 
*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise the counters.
      NREPLO = 0
      NREPHI = 0
 
*  Test for bad values.
*  ====================
      IF ( BAD ) THEN
 
*  Loop through all pixels in the input array.
         DO J = 1, EL
 
*  Check for a bad pixel.
            IF ( INARR( J ) .EQ. VAL__BADUB ) THEN
               OUTARR( J ) = VAL__BADUB
 
*  Check input array value and act accordingly.
            ELSE IF ( INARR( J ) .GT. THRHI ) THEN
 
*  The input pixel value is greater than upper threshold---set output
*  pixel to the given replacement value.  Increment the count of
*  substitutions.
               OUTARR( J ) = NEWHI
               NREPHI = NREPHI + 1
 
            ELSE IF ( INARR( J ) .LT. THRLO ) THEN
 
*  The input pixel value is less than lower threshold---set output
*  pixel to the given replacement value.  Increment the count of
*  substitutions.
               OUTARR( J ) = NEWLO
               NREPLO = NREPLO + 1
 
            ELSE
 
*  The input pixel value lies between thresholds---just copy it into
*  the output pixel.
               OUTARR( J ) = INARR( J )
 
*  End of the check to see whether the input pixel value lies in
*  between or beyond the range.
            END IF
 
*  End of loop round all pixels.
         END DO
 
*  Treated bad values literally.
*  =============================
      ELSE
 
*  Loop through all pixels in the input array.
         DO J = 1, EL
 
*  Check input array value and act accordingly.
            IF ( INARR( J ) .GT. THRHI ) THEN
 
*  The input pixel value is greater than upper threshold---set output
*  pixel to the given replacement value.  Increment the count of
*  substitutions.
               OUTARR( J ) = NEWHI
               NREPHI = NREPHI + 1
 
            ELSE IF ( INARR( J ) .LT. THRLO ) THEN
 
*  The input pixel value is less than lower threshold---set output
*  pixel to the given replacement value.  Increment the count of
*  substitutions.
               OUTARR( J ) = NEWLO
               NREPLO = NREPLO + 1
 
            ELSE
 
*  The input pixel value lies between thresholds---just copy it into
*  the output pixel.
               OUTARR( J ) = INARR( J )
 
*  End of the check to see whether the input pixel value lies in
*  between or beyond the range.
            END IF
 
*  End of loop round all pixels.
         END DO
 
      END IF
 
      END
