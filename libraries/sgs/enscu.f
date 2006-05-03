      SUBROUTINE sgs_ENSCU
*+
*  Name:
*     ENSCU

*  Purpose:
*     Enable sampling of cursor on the current workstation.

*  Language:
*     Starlink Fortran 77

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GSAMPL   i     sample mode

*  Externals:
*     GSLCM, sgs_1ILCMO

*  Read From Common:
*     IZTW     i()   zone table - SGS workstation ID
*     IWTID    i()   workstation table - GKS workstation ID
*     ISZID    i     current zone ID

*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER MODE,IESW,JSTAT



*   Inquire echo switch setting
      CALL sgs_1ILCMO(MODE,IESW,JSTAT)                       

*   Enable for sample
      IF (JSTAT.EQ.0) CALL GSLCM(IWTID(ABS(IZTW(ISZID))),1,GSAMPL,IESW)

      END
