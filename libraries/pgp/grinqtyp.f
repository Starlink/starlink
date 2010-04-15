      SUBROUTINE GRQTYP(TYPE,INTER)
*+
*
*     - - - - - - - -
*       G R Q T Y P
*     - - - - - - - -
*
*   Returns the device type and whether it is interactive or not.
*
*   Returned
*      TYPE     c     Device type
*      INTER    i     Whether device is interactive
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRWKID   i()   Workstation id
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      LOGICAL INTER
      CHARACTER*(*) TYPE, TEMP*10

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      INTEGER ICON, IWKTYP, IERR
      INTEGER NLCD, NSKD, NVLD, NCHD, NPCD, NSTD

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRQTYP - No PGPLOT device open',
     :   GRNODO)
      ELSE

*  Inquire the device type (the real type not the target in the case
*  of a metafile
         CALL GQWKC(GRWKID(GRCIDE),IERR,ICON,IWKTYP)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRQTYP', 'GQWKC', IERR)
         ELSE
            WRITE(TEMP,'(I10)') IWKTYP
            TYPE = TEMP

*    see if the device has a cursor
            CALL GQLI(IWKTYP,IERR,NLCD,NSKD,NVLD,NCHD,NPCD,NSTD)
            INTER = (IERR.EQ.0 .AND. NLCD.GT.0)
         END IF
      END IF

      END
