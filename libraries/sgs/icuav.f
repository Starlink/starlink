      SUBROUTINE sgs_ICUAV (AVAIL)
*+
*  Name:
*     ICUAV

*  Purpose:
*     Inquire availability of cursor on current SGS device.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     AVAIL = LOGICAL (Returned)
*         True if cursor input available

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Constants From GKS_PAR:
*     GINPUT      i      workstation category - input
*     GOUTIN      i           "         "     - input/output

*  Errors:
*     Error returned by GKS inquiry

*  Externals:
*     GQLI, sgs_1ERR

*  Read From Common:
*     IZTW        i()    zone table - SGS workstation ID
*     ISZID       i      current zone ID
*     IWTTY       i()    workstation table - workstation type
*     IWTCA       i()    workstation table - category

*-

      IMPLICIT NONE

      LOGICAL AVAIL

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER ISWKID, IERR, NLCD, NSKD, NVLD, NCHD, NPCD, NSTD, JSTAT
      CHARACTER RNAME*5
      PARAMETER (RNAME='ICUAV')



*   Assume no locator devices
      NLCD = 0

*   Current workstation ID
      ISWKID = ABS(IZTW(ISZID))

*   Inquire availability of input primitives
      IF (IWTCA(ISWKID).EQ.GINPUT .OR. IWTCA(ISWKID).EQ.GOUTIN) THEN
         CALL GQLI(IWTTY(ISWKID),IERR,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQLI',
     :                                                            JSTAT)
            NLCD = 0
         END IF
      END IF

*  Test number of locator devices
 9999 CONTINUE
      IF (NLCD.GT.0) THEN
         AVAIL = .TRUE.
      ELSE
         AVAIL = .FALSE.
      END IF

      END
