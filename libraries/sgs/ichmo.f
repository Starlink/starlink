      SUBROUTINE sgs_1ICHMO (NCH, MODE, IESW, JSTAT)
*+
*   - - - - - -
*    I C H M O    (Internal routine)
*   - - - - - -
*
*   Enquire mode of choice device on the current SGS workstation.
*
*   If an error occurs "request" and "echo on" are returned.
*
*   Given:
*      NCH     i     choice device number
*      
*   Returned:
*      MODE    i     operating mode of choice device
*      IESW    i     echo switch setting of choice device
*      JSTAT   i     status (0=OK)
*
*   Read from COMMOM:
*      IZTW    i()   zone table - SGS workstation ID
*      IWTID   i()   workstation table - GKS workstation ID
*      ISZID   i     current zone ID
*
*   Constants from GKS_PAR:
*      GREQU   i     operating mode - request
*      GECHO   i     echo on
*
*   Externals:
*      GQCHS, sgs_1ERR
*
*   Errors:
*      Error returned by GKS enquiry
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
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
