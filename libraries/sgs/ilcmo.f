      SUBROUTINE sgs_1ILCMO (MODE, IESW, JSTAT)
*+
*   - - - - - -
*    I L C M O    (Internal routine)
*   - - - - - -
*
*   Enquire mode of locator device no 1 on current SGS device.
*
*   If an error occurs "request" and "echo on" are returned.
*
*   Returned:
*      MODE     i     mode of locator device
*      IESW     i     echo switch setting
*      JSTAT    i     status (0=OK)
*
*   Read from COMMON:
*      IZTW     i()   zone table - SGS workstation ID
*      ISZID    i     current zone idID
*      IWTID    i()   workstation table - GKS workstation ID
*
*   Constants from GKS_PAR:
*      GREQU    i     operating mode - request
*      GECHO    i     echo on
*
*   Externals:
*      GQLCS, sgs_1ERR
*
*   P.T.Wallace,  D.L.Terrett   Starlink   14 September 1991
*+

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
