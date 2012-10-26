*  History:
*     31 Jan 1994 (hme):
*        Remove second declaration of NMAPS, which is INTEGER*4 in PLOT2D.
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Test ISTAT .NE. 0
*        Unused in EXPORT_MAP4: IZ, IGETVM
*               in MAP_ASCIIWR: I1, I2, J1, J2
C-----------------------------------------------------------------------

      SUBROUTINE EXPORT_MAP4 (IFAIL)

*  Routine to take the 2D image stored in the file "mapplane.tmp"
*  and write the map it reads to an ASCII file for reading by other
*  programs.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL

*     Local variables:

      INTEGER   IMX, IMY
      INTEGER   IPTR
      INTEGER   ISTAT
      INTEGER   IX, IY
      INTEGER   LUN

      REAL      P(2), Q(2)

*     Global variables:

      INCLUDE 'FLAGCOMM'
      INCLUDE 'PLOT2D'
      INCLUDE 'CNF_PAR'

*     Functions:

      INTEGER   IFREEVM
      INTEGER   IGETLUN
      INTEGER   IFREELUN

*  Ok, go...

      IFAIL = 0

      CALL MAPIMAGE ('mapplane.tmp', IPTR, NMAPS, IMX, IMY, ISTAT)
      IF (ISTAT .NE. 0) THEN
        IFAIL = 67
        RETURN
      END IF

      PRINT *,'--- Export_map4 ---'
      PRINT *,'    Virtual address IPTR =       ', IPTR
      PRINT *,'    Number of maps in file =     ', NMAPS
      PRINT *,'    Map array size - IMX * IMY   ', IMX, IMY

*     Select the bit of the map we need

      IX   = LINK(1)
      IY   = LINK(2)

      P(1) = PBEG(IX)
      P(2) = PEND(IX)
      Q(1) = PBEG(IY)
      Q(2) = PEND(IY)

*     Open a file for output

      ISTAT = IGETLUN (LUN, 'EXPORT_MAP4', .TRUE.)
      OPEN (LUN,  FILE    = 'ascii_map.tmp',
     &            FORM    = 'FORMATTED',
     &            STATUS  = 'NEW',
     &            ACCESS  = 'SEQUENTIAL')

*     Now write the file

      CALL MAP_ASCIIWR (%VAL(CNF_PVAL(IPTR)), NAXX, NAXY,
     &                   NAX(LINK(1))-1, NAX(LINK(2))-1, P, Q, LUN)

*     Close the map

      CLOSE (LUN)
      ISTAT = IFREELUN (LUN)

*     Release virtual memory

  999 CONTINUE

      ISTAT = IFREEVM (IPTR)
      IF (ISTAT .NE. 0) THEN
        PRINT *, '--- Export_map4 ---'
        PRINT *, '    error freeing virtual memory: ', ISTAT
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE MAP_ASCIIWR (MAP, IX, IY, NXCELL, NYCELL, P, Q, LUN)

*  Routine to write data points in a map to a file, subject to the data
*  points having actually been measured in the first place.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IX, IY
      REAL      MAP(IX,IY)
      INTEGER   NXCELL, NYCELL
      REAL      P(2), Q(2)
      INTEGER   LUN

*     Include files

      INCLUDE   'PLOT2D'
      INCLUDE   'CNF_PAR'

*     Local variables

      INTEGER   I,J
      INTEGER   II, JJ, JG
      REAL      DX, DY
      REAL      XPOS, YPOS, DATA

*     Functions

      LOGICAL   GOODPT

*  Ok, go...

*     For all "true" points within the box, check that data were ACTUALLY
*     measured (the value of INDEX must be .gt. -1; that is, interpolated data
*     with INDEX=0 are OK). If so then work out whether max, min or neither.
*     (Note that map has been inverted in Y to allow it to be plotted with
*     MONGO -  so we have to index the "opposite" row to the one we are now
*     looking at when we want to check if the data are good or not)

      DX = (P(2) - P(1)) / NXCELL
      DY = (Q(1) - Q(2)) / NYCELL

      PRINT *,'--- MAP_ASCIIWR ---'
      PRINT *,'    Pixel sizes: ', DX, DY

      DO J = 1, (IY-1)/(LYPIX-1) + 1

        JG = (IY-1)/(LXPIX-1) + 2 - J
        JJ = 1 + (J-1)*(LYPIX-1)

        DO I = 1, (IX-1)/(LXPIX-1) + 1
          IF (GOODPT (%VAL(CNF_PVAL(INDEX_PTR)), I, JG, -1)) THEN
            II   = 1 + (I-1)*(LXPIX-1)
            XPOS = P(1) + (I-1)*DX
            YPOS = Q(2) + (J-1)*DY
            DATA = MAP(II,JJ)
            WRITE (LUN, *) XPOS, YPOS, DATA, 0.0
          END IF
        END DO
      END DO

      RETURN
      END

*-----------------------------------------------------------------------

