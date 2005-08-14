      INTEGER FUNCTION KPG1_FLOOR( VALUE )
*+
*  Name:
*     KPG1_FLOOR

*  Purpose:
*     Return the largest integer smaller than or equal to a supplied value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_FLOOR( VALUE )

*  Description:
*     This routine returns the largest integer smaller than or equal to
*     a supplied value.

*  Arguments:
*     VALUE = REAL (Given)
*        The value.

*  Function Value:
*     KPG1_FLOOR = INTEGER
*        The largest integer smaller than or equal to the supplied value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1998 (DSB):
*        Original version.
*     13-AUG-2005 (TIMJ):
*        On some compilers comparing a REAL value with an INTEGER
*        derived from a BOZ (HEX) definition, may result in the
*        bit pattern being treated as a REAL rather than the INTEGER
*        value being converted to REAL. To protect against PRM_PAR changing
*        its notation, use the NUM_ macros which force a type conversion.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      REAL VALUE

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! Declarations of conversion routines
      INCLUDE 'NUM_DEF_CVT'    ! Definitions of conversion routines

*  Do it.

      IF( VALUE .GT. NUM_ITOR( VAL__MAXI ) ) THEN
         KPG1_FLOOR = VAL__MAXI

      ELSE IF( VALUE .LT. NUM_ITOR( VAL__MINI ) ) THEN
         KPG1_FLOOR = VAL__MINI

      ELSE
         KPG1_FLOOR = NUM_RTOI( VALUE )
         IF( NUM_ITOR( KPG1_FLOOR ) .GT. VALUE ) THEN
            KPG1_FLOOR = KPG1_FLOOR - 1
         END IF
      END IF

      END
 
