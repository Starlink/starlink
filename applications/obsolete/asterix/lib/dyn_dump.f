      SUBROUTINE DYN_DUMP()
*+
*  Name:
*     DYN_DUMP

*  Purpose:
*     Dump common block contents for debugging

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_DUMP()

*  Description:

*  Arguments:

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYS_DIAG = LOGICAL (given)
*         Package diagnostics on?
*       DYS_PTR = INTEGER (given and returned)
*         DYN memory addresses
*       DYS_NBYTE = INTEGER (returned)
*         DYN memory sizes
*       DYS_NITEM = INTEGER (returned)
*         DYN memory number of elements
*       DYS_FID = INTEGER (returned)
*         DYN file identifiers

*  Arguments Given:
      INTEGER			PTR			! See above
      INTEGER			NITEM, NBYTE, FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I,N
*.

*  Report initialisation flag
      CALL MSG_SETL('IN',DYN_ISINIT)
      CALL ADP_OUT('Initialisation flag: ^IN')

*  Look for a filled slots
      N=0
      DO I=1,DYN__NMAX
        IF ( DYS_PTR(I) .NE. 0 ) THEN
          N=N+1
          CALL MSG_SETI('NI',DYS_NITEM(I))
          CALL MSG_SETI('NB',DYS_NBYTE(I))
          CALL MSG_SETI('PT',DYS_PTR(I))
          CALL MSG_SETI('FI',DYS_PTR(I))
          CALL ADP_OUT('^NI items / ^NB bytes at ^PT with FID ^FI')
        ENDIF
      ENDDO
      CALL MSG_SETI('N',N)
      CALL ADP_OUT('Total slots allocated: ^N')

      END
