      SUBROUTINE sgs_1ILCMO (MODE, IESW, JSTAT)
*+
*  Name:
*     ILCMO

*  Purpose:
*     Enquire mode of locator device no 1 on current SGS device.

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     Internal routine

*  Description:
*     If an error occurs "request" and "echo on" are returned.

*  Arguments:
*     MODE = INTEGER (Returned)
*         Mode of locator device
*     IESW = INTEGER (Returned)
*         Echo switch setting
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
*     GREQU    i     operating mode - request
*     GECHO    i     echo on

*  Externals:
*     GQLCS, sgs_1ERR

*  Read From Common:
*     IZTW     i()   zone table - SGS workstation ID
*     ISZID    i     current zone idID
*     IWTID    i()   workstation table - GKS workstation ID

*-

      IMPLICIT NONE

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'

      INCLUDE 'sgscom'


      INTEGER MODE,IESW,JSTAT
      INTEGER IERR,ITNR,IPET,LDR
      REAL EAREA(4),RLPX,RLPY
      CHARACTER*80 DATREC,RNAME*5
      PARAMETER (RNAME='ILCMO')



      JSTAT = 0
      CALL GQLCS(IWTID(ABS(IZTW(ISZID))),1,GSET,1,IERR,MODE,IESW,ITNR,
     :                                  RLPX,RLPY,IPET,EAREA,LDR,DATREC)
      IF (IERR.NE.0) THEN
         CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQLCS',JSTAT)
         MODE = GREQU
         IESW = GECHO
      END IF

      END
