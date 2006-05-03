      SUBROUTINE sgs_1ICHMO (NCH, MODE, IESW, JSTAT)
*+
*  Name:
*     ICHMO

*  Purpose:
*     Enquire mode of choice device on the current SGS workstation.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     If an error occurs "request" and "echo on" are returned.

*  Arguments:
*     NCH = INTEGER (Given)
*         Choice device number
*     MODE = INTEGER (Returned)
*         Operating mode of choice device
*     IESW = INTEGER (Returned)
*         Echo switch setting of choice device
*     JSTAT = INTEGER (Returned)
*         Status (0=OK)

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
*     GREQU   i     operating mode - request
*     GECHO   i     echo on

*  Errors:
*     Error returned by GKS enquiry

*  Externals:
*     GQCHS, sgs_1ERR

*  Read From Commom:
*     IZTW    i()   zone table - SGS workstation ID
*     IWTID   i()   workstation table - GKS workstation ID
*     ISZID   i     current zone ID

*-

      IMPLICIT NONE

      INTEGER NCH,MODE,IESW,JSTAT

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'

      INCLUDE 'sgscom'


      INTEGER ICHNR,IPET,LDR,IERR,JISTAT
      REAL EAREA(4)
      CHARACTER*80 DATREC,RNAME*5
      PARAMETER (RNAME='ICHMO')



      JSTAT=0
      CALL GQCHS(IWTID(ABS(IZTW(ISZID))),NCH,1,IERR,MODE,IESW,
     :                               JISTAT,ICHNR,IPET,EAREA,LDR,DATREC)
      IF (IERR.NE.0)
     :         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQCHS',
     :                                                            JSTAT)

      END
