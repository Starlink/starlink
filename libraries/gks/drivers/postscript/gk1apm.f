*---------------------------------------------------------------------
      SUBROUTINE GK1APM(NRD, XD, YD)
*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send polymarker to the external PostScript file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*
*     INP NRD     - Integer number points
*     INP XD      - Array of real x-coordinates of points (DC)
*     INP YD      - Array of real y-coordinates of points (DC)
*
      INTEGER NRD
      REAL XD(*), YD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*  CHANGE  - Logical flag.
*  DUMMY   - Dummy character, required by the buffering routine.
*  FC      - Format of coordinates
*  I, J    - Temporary count variables.
*  IREM    - Dummy integer, required by the buffering routine.
*  LC      - Length of formatted coordinates
*  LFC     - Length of coordinate format code
*  RVS     - Real for temporary storage
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*  SMALL   - Small real for equality comparisons
*

*     small real
      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)

*     Offsets in KWKDAT & QWKDAT
      INTEGER    IMKTYP,   IMKSZ
      PARAMETER (IMKTYP=2, IMKSZ=2)
*
      LOGICAL CHANGE
      CHARACTER S*50, DUMMY, FC*7
      REAL    RVS
      INTEGER IREM, I, J, LC, LFC
*
*  ALGORITHM
*  ---------
*     Update the locally stored attributes (if necessary), output polymarker
*     coordinates, call the PS procedure.
*
*---------------------------------------------------------------------
*

*     Initialise the flag
      CHANGE = .FALSE.

*
*     See if locally stored copy of the attributes needs updating.
*
*     Marker type:
      IF(KWKDAT(IMKTYP,KWKIX).NE.KWMKTY(KWKIX)) THEN
         KWKDAT(IMKTYP,KWKIX) = KWMKTY(KWKIX)
         CHANGE = .TRUE.
      ENDIF

*     Marker size: validate, then check.
      RVS = AMAX1(QMNMKS(KWKIX),QWMKSZ(KWKIX))
      RVS = AMIN1(QMXMKS(KWKIX),RVS)
*
      IF(ABS(QWKDAT(IMKSZ,KWKIX)-RVS).GT.SMALL) THEN
         QWKDAT(IMKSZ,KWKIX) = RVS
         CHANGE = .TRUE.
      ENDIF

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     If change has occurred write the attributes out.
*
      IF (CHANGE) THEN
         WRITE(S, 50) KWKDAT(IMKTYP,KWKIX), QWKDAT(IMKSZ,KWKIX)
   50    FORMAT(I2, E11.3, ' pmstat')
         CALL GKFOCO(KIOPB, S(1:20), IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)
      END IF

*
*     Output the markers
*
*     Prepare and send the polymarker coordinates as arrays
*     selecting the format to save file space.
      CALL GKFOCO(KIOPB, '[', IREM)
      CALL GK1ASF(NRD,XD,LC,LFC,FC)
      DO 100 I=1, NRD
         WRITE(S,FC(1:LFC)) XD(I)
         CALL GKFOCO(KIOPB, S(1:LC), IREM)
  100 CONTINUE
      CALL GKFOCO(KIOPB, '][', IREM)
      CALL GK1ASF(NRD,YD,LC,LFC,FC)
      DO 200 I=1, NRD
         WRITE(S,FC(1:LFC)) YD(I)
         CALL GKFOCO(KIOPB, S(1:LC), IREM)
  200 CONTINUE
*     Now call the pm procedure
      CALL GKFOCO(KIOPB, ']pm', IREM)

      END
