      SUBROUTINE KPG1_CHELR( EL, CHEL, NEWVAL, ARRAY, OLDVAL, STATUS )
*+
*  Name:
*     KPG1_CHELx
 
*  Purpose:
*     Replaces the value of an array element with a specified value.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_CHELx( EL, CHEL, NEWVAL, ARRAY, OLDVAL, STATUS )
 
*  Description:
*     This routine replaces a specified element of a vectorised array
*     with a supplied value and returns the former value of the
*     element.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The dimension of the array.
*     CHEL = INTEGER (Given)
*        The element number to be replaced.  It must lie between 1 and
*        EL, otherwise an error report is made.
*     NEWVAL = ? (Given)
*        The new value of the element.
*     ARRAY( EL ) = ? (Given and Replaced)
*        The array whose value is to be replaced.
*     OLDVAL = ? (Returned)
*        The former value of the array element.
*     STATUS  =  INTEGER (Given and Returned)
*        Global status value
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in the routine name by I, R, or D as
*     appropriate.  Arguments ARRAY, NEWVAL and OLDVAL must have the
*     data type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1995 April 25 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE  'SAE_PAR'         ! Standard SAE constants
 
*  Arguments Given:
      INTEGER EL
      INTEGER CHEL
      REAL NEWVAL
 
*  Arguments Given and Returned:
      REAL ARRAY( EL )
 
*  Arguments Returned:
      REAL OLDVAL
 
*  Status:
      INTEGER STATUS             ! Global status
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Check the element number.
      IF ( CHEL .LT. 1 .OR. CHEL .GT. EL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_CHELx_BOUND', 'The chosen element lies '/
     :     /'beyond the bounds of the array (programming error).',
     :     STATUS )
      ELSE
 
*  Set the old value.
         OLDVAL  =  ARRAY( CHEL )
 
*  Replace the old value with new value in the array.
         ARRAY( CHEL )  =  NEWVAL
      END IF
 
      END
