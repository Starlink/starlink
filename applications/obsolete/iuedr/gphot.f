      SUBROUTINE GPHOT( TP, TNMAX, APHOT, AQUAL, STATUS )
*+
*  Name:
*     SUBROUTINE GPHOT

*  Purpose:
*     Read GPHOT image from VICAR tape file into memory.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GPHOT( TP, TNMAX, APHOT, AQUAL, STATUS )

*  Arguments:
*     TP = INTEGER (Given)
*        Tape descriptor.
*     TNMAX = INTEGER (Given)
*        Maximum TN value in ITF table.
*     APHOT = INTEGER*2( MAXS, MAXL ) (Returned)
*        Photometric image array.
*     AQUAL = BYTE( MAXS, MAXL ) (Returned)
*        Data quality array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-AUG-82 (JRG):
*       AT4 version
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     16-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Local Constants:
      INTEGER MAXL                  ! Maximum number of IUE lines.
      INTEGER MAXS                  ! Maximum number of IUE scans.
      PARAMETER ( MAXL = 768, MAXS = 768 )

*  Arguments Given:
      INTEGER TP                    ! Tape descriptor.
      INTEGER TNMAX                 ! Maximum TN value in ITF table.

*  Arguments Returned:
      INTEGER*2 APHOT( MAXS, MAXL ) ! Photometric image array.

      BYTE AQUAL( MAXS, MAXL )      ! Data quality array.

*  Status:
      INTEGER STATUS                ! Global status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read VICAR image.
      CALL VIC_RDAT( TP, 2, MAXS, MAXL, APHOT, STATUS )

      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL GPHOTQ( TNMAX, MAXS, MAXL, APHOT, AQUAL, STATUS )
      END IF

      END
