      SUBROUTINE MT_OPEN( STATUS )
*+
*  Name:
*     SUBROUTINE MT_OPEN

*  Purpose:
*     Assign tape descriptor.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MT_OPEN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     SLW: Sid Wright (UCL)
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     14-APR-80 (SLW):
*       AT4 version.
*     11-SEP-81 (JRG)
*     31-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*     01-JUN-89 (PCTR):
*       IUEDR Vn. 2.1
*       Conversion to SGP/16 style.
*     01-OCT-92 (DMILLS):
*       IUEDR Vn. 3.0
*       ADAMised version to run on multiple hardware platforms
*     01-AUG-94 (MJC)
*       IUEDR Vn. 3.1-2
*       Better parameter error handling
*     07-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*       Stripped out old MT_ library, now wrapper for MAG_ call.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MAG_ERR'

*  Global Variables:
      INCLUDE 'CMTAPE'

*  Local Constants:
      INTEGER MAXDEVN       ! Maximum length of device name.
      INTEGER MTUNDEF       ! Undefined file, position or block.
      PARAMETER ( MAXDEVN = 16, MTUNDEF = 0 )

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      INTEGER CODE          ! Local status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CODE = SAI__OK
      TCHAN = 0
      CALL MAG_ASSOC( 'DRIVE', 'READ', TCHAN, CODE )
      IF ( CODE .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( CODE )
         CALL ERROUT( '\\', STATUS )
         GO TO 999
      END IF

  999 CONTINUE

      END
