      SUBROUTINE KPS1_THGTI( PNTHLO, PNTHHI, PNRLO, PNRHI, THRLO,
     :                        THRHI, NEWLO, NEWHI, BAD, STATUS )
*+
*  Name:
*     KPS1_THGTx
 
*  Purpose:
*     Gets the threshold limits and replacement values for THRESH.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPS1_THGTx( PNTHLO, PNTHHI, PNRLO, PNRHI, THRLO,
*                      THRHI, NEWLO, NEWHI, BAD, STATUS )
 
*  Description:
*     This is just a server routine for THRESH that obtains the
*     threshold limits, and the values to replace those beyond the
*     limits via the ADAM parameter system.  Since the processing is
*     to performed generically, these values must be obtained in the
*     appropriate type.  There is also an option to substitute the bad
*     value.  Therefore, the limits are found in double precision, but
*     forced to lie within the range of the selected data type, and are
*     converted to that type for return to THRESH.  Given the number of
*     operations involved and it's generic nature it is more
*     cost-effective to code it in a subroutine.
 
*  Arguments:
*     PNTHLO = CHARACTER * ( * ) (Given)
*        The ADAM parameter name for the lower threshold.
*     PNTHHI = CHARACTER * ( * ) (Given)
*        The ADAM parameter name for the upper threshold.
*     PNRLO = CHARACTER * ( * ) (Given)
*        The ADAM parameter name for the value to replace those values
*        less than the lower threshold.
*     PNRHI = CHARACTER * ( * ) (Given)
*        The ADAM parameter name for the value to replace those values
*        greater than the upper threshold.
*     THRLO = ? (Returned)
*        The lower threshold.
*     THRHI = ? (Returned)
*        The upper threshold.
*     NEWLO = ? (Returned)
*        The value to replace those values less than the upper
*        threshold.
*     NEWHI = ? (Returned)
*        The value to replace those values greater than the upper
*        threshold.
*     BAD = LOGICAL (Returned)
*        If true either of the replacement values is the bad flag.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     values returned from the routine must have the data type
*     specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 November 6 (MJC):
*        Original version.
*     1994 September 26 (MJC):
*        Replace AIF calls with PAR, and used modern coding style.
*     1995 May 17 (MJC):
*        Fixed bug where numeric defaults were supplied to PAR_MIX0x
*        instead of strings.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primdat parameter definitions
 
*  Arguments Given:
      CHARACTER * ( * ) PNTHLO
      CHARACTER * ( * ) PNTHHI
      CHARACTER * ( * ) PNRLO
      CHARACTER * ( * ) PNRHI
 
      LOGICAL BAD
 
*  Arguments Returned:
      INTEGER THRLO
      INTEGER THRHI
      INTEGER NEWLO
      INTEGER NEWHI
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  External References:
      INTEGER VAL_DTOI          ! Double to INTEGER conversion
 
*  Local Variables:
      CHARACTER * ( 26 ) CDEF    ! Dynamic default
      CHARACTER * ( 26 ) CNEWHI  ! New value for pixels above THRHI
      CHARACTER * ( 26 ) CNEWLO  ! New value for pixels below THRLO
      DOUBLE PRECISION DEFREP    ! Default value for parameters
      DOUBLE PRECISION DTHRLO    ! Lower threshold value
      DOUBLE PRECISION DTHRHI    ! Upper threshold value
      DOUBLE PRECISION DNEWLO    ! New value for pixels below THRLO
      DOUBLE PRECISION DNEWHI    ! New value for pixels above THRHI
      DOUBLE PRECISION MAXREP    ! Maximum parameter value
      DOUBLE PRECISION MINREP    ! Minimum parameter value
      INTEGER NCHAR              ! Number of characters
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'      ! Definitions of conversion routines
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise the bad flag.
      BAD = .FALSE.
 
*  Define the acceptable range of values and the suggested default
*  (which is not actually used due to the ppath).
      MINREP = NUM_ITOD( VAL__MINI )
      MAXREP = NUM_ITOD( VAL__MAXI )
      DEFREP = 0.0
 
*  Get the low threshold value.
      CALL PAR_GDR0D( PNTHLO, DEFREP, MINREP, MAXREP, .FALSE., DTHRLO,
     :                STATUS )
 
*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
      THRLO = VAL_DTOI( .FALSE., DTHRLO, STATUS )
 
*  Obtain the default replacement value.  It must be character for
*  PAR_MIX0x.
      CALL CHR_DTOC( DTHRLO, CDEF, NCHAR )
 
*  Get the value to which numbers below THRLO are set.
      CALL PAR_MIX0D( PNRLO, CDEF, MINREP, MAXREP, 'Bad', .FALSE.,
     :                CNEWLO, STATUS )
 
*  It may be the bad-pixel value.
      IF ( CNEWLO .EQ. 'BAD' ) THEN
         NEWLO = VAL__BADI
         BAD = .TRUE.
      ELSE
 
*  Convert the output numeric string to its numeric value.  Note that
*  the character conversion must be done via double precision as it may
*  a string in E format and there aren't conversions to to one- and
*  two-byte integers.
         CALL CHR_CTOD( CNEWLO, DNEWLO, STATUS )
         NEWLO = VAL_DTOI( .FALSE., DNEWLO, STATUS )
      END IF
 
*  Get the high threshold value.
      CALL PAR_GDR0D( PNTHHI, DEFREP, MINREP, MAXREP, .FALSE., DTHRHI,
     :                STATUS )
 
*  Convert the threshold value to the desired type.  Use VAL_ to
*  protect against potentionally harmful values when there is a bad
*  status.
      THRHI = VAL_DTOI( .FALSE., DTHRHI, STATUS )
 
*  Obtain the default replacement value.  It must be character for
*  PAR_MIX0x.
      CALL CHR_DTOC( DTHRHI, CDEF, NCHAR )
 
*  Get the value to which numbers above THRHI are set.
      CALL PAR_MIX0D( PNRHI, CDEF, MINREP, MAXREP, 'Bad', .FALSE.,
     :                CNEWHI, STATUS )
 
*  It may be the bad-pixel value.
      IF ( CNEWHI .EQ. 'BAD' ) THEN
         NEWHI = VAL__BADI
         BAD = .TRUE.
      ELSE
 
*  Convert the output numeric string to its numeric value.  Note that
*  the character conversion must be done via double precision as it may
*  a string in E format and there aren't conversions to to one- and
*  two-byte integers.
         CALL CHR_CTOD( CNEWHI, DNEWHI, STATUS )
         NEWHI = VAL_DTOI( .FALSE., DNEWHI, STATUS )
      END IF
 
      END
