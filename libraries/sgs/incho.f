      SUBROUTINE sgs_INCHO (NCHOIC, N)
*+
*   - - - - - -
*    I N C H O
*   - - - - - -
*
*   Inquire number of choices on choice device on current SGS
*   workstation.
*
*   If the specified choice device does not exist N is set to zero.
*     
*   Given:
*      NCHOIC     i      SGS choice device
*
*   Returned:
*      N          i      number of choices
*
*   Read from COMMON:
*      IZTW       i()    zone table - SGS workstation ID
*      ISZID      i      current zone ID
*      IWTTY      i()    workstation table - workstation type
*      IWTCA      i()    workstation table - category
*
*   Constants from SGSCOM:
*      MAXCHO     i      maximum number of keyboard choices
*
*   Constants from GKS_PAR:
*      GINPUT      i      workstation category - input
*      GOUTIN      i           "         "     - input/output
*
*   Externals:
*      GQLI, GQCHD, sgs_1ERR
*
*   Errors:
*      Error returned by GKS inquiry
*
*   This routine assumes that if a workstation has a choice device 2
*   then this is a keyboard.
*      
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INTEGER NCHOIC,N

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'

      INCLUDE 'SGS_ERR'


      INTEGER IERR,ISWKID,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD
      INTEGER IOL,IPET,LDR,JSTAT,NCH
      CHARACTER*64 DATREC(1)*80,RNAME*5
      PARAMETER (RNAME='INCHO')
      REAL EAREA(4)



      IF (NCHOIC.EQ.0) THEN

*   Keyboard
         NCH = 2
      ELSEIF (NCHOIC.GT.0) THEN

*   GKS choice device
         NCH = NCHOIC
      ELSE

*   Not a valid SGS choice device number.
         N = 0
      END IF

*   Inquire availability of input primitives.
      ISWKID = ABS(IZTW(ISZID))
      IF (IWTCA(ISWKID).EQ.GINPUT .OR. IWTCA(ISWKID).EQ.GOUTIN) THEN
         CALL GQLI(IWTTY(ISWKID),IERR,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
         IF (IERR.NE.0) THEN
            CALL sgs_1ERR(SGS__INQER,RNAME,'Error returned by GQLI',
     :      JSTAT)
            NCHD = 0
            GO TO 9999
         END IF

         IF (NCHD.GE.NCH) THEN

*       Choice device exists so inquire number of choices
            CALL GQDCH(IWTTY(ISWKID),NCH,1,1,IERR,N,IOL,IPET,EAREA,LDR,
     :                                                           DATREC)
            IF (IERR.NE.0) THEN
               CALL sgs_1ERR(SGS__INQER,RNAME,
     :                                   'Error returned by GQLI',JSTAT)
               N = 0
               GO TO 9999
            END IF
         END IF
      END IF

 9999 CONTINUE

      END
