      LOGICAL FUNCTION NDF1_ABSNT( ISTAT )
*+
* Name:
*    NDF1_ABSNT

*  Purpose:
*     Test status codes for absent NDF data structures or components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = NDF1_ABSNT( ISTAT )

*  Description:
*     The function tests an error status code and returns a .TRUE.
*     result if it is one of those indicating that an NDF data structure
*     or an HDS object (e.g. an NDF component) is absent. It exists so
*     that this test need only be defined in one place.

*  Arguments:
*     ISTAT = INTEGER (Given)
*        The error status code to be tested.

*  Returned Value:
*     NDF1_ABSNT = INTEGER
*        Whether an absent data structure or component is indicated.

*  Algorithm:
*     Currently, the routine tests for the status values: DAT__FILNF,
*     DAT__OBJNF, NDF__CNMIN and NDF__FILNF. It returns .TRUE. if any of
*     these is matched.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     15-NOV-1994 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_ERR'          ! DAT_ error codes
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      
*  Arguments Given:
      INTEGER ISTAT
      
*.

*  Test the status value against those which indicate an absent data
*  structure or component.
      NDF1_ABSNT = ( ( ISTAT .EQ. DAT__FILNF ) .OR.
     :               ( ISTAT .EQ. DAT__OBJNF ) .OR.
     :               ( ISTAT .EQ. NDF__CNMIN ) .OR.
     :               ( ISTAT .EQ. NDF__FILNF ) )

      END
