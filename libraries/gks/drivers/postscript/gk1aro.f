*---------------------------------------------------------------
      SUBROUTINE GK1ARO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send pixel array to the external PostScript file
*     (This routine is no longer called.)
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP  X,Y     -  Coordinates of UL corner of array
*     INP  NXPIX   -  No of pixels per scan line
*     INP  NYPIX   -  Number of scan lines in raster
*     INP  NXDIM   -  First dimension of colour array
*     INP  ICOLAR  -  Colour array
*
      INTEGER NXPIX,NYPIX,NXDIM,ICOLAR(NXDIM,NYPIX)
      REAL X,Y
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*  DUMMY   - Dummy character, required by the buffering routine.
*  IC     - Current Column in cell array
*  ICCHST - Column of Cell Array at the Start of the Current Chunk
*  IR     - Current Row of Cell Array
*  JH     - Current position in colour code string
*  ICHUNK - Maximum number of array's bytes output at a time
*  ICOLR  - Offset in integer workspace of workstation's colour type
*  IREM   - Dummy integer, required by the buffering routine.
*  KMAX   - Temporary count variable
*  NBC    - Number of bytes in a colour code
*  RX, RY - Real arrays to hold pixel array's delimiting coordinates
*  S      - Character variable, via which chunks of PostScript are sent for
*           buffering.
*

*
      INTEGER    ICHUNK,    ICOLR
      PARAMETER (ICHUNK=50, ICOLR=17)
*
      REAL    RX(2),RY(2)
      INTEGER IC,ICCHST,IR,JH,KMAX,IREM,NBC
*
      CHARACTER S*255, DUMMY
*
*  ALGORITHM
*  ---------
*     Use im procedure as was done in Cell Array.
*
*------------------------------------------------------------------------
*
*     Derive number of bytes in colour code
      IF(KWKDAT(ICOLR,KWKIX) .EQ. GCOLOR)THEN
         NBC = 3
      ELSE
         NBC = 1
      ENDIF

*
*     We shall treat the raster output as a special case of a Cell Array.
*     So, get the Lower Left and Upper Right coordinates of the array first.
*
      RX(1)=X
      RX(2)=X+NXPIX-1.0
      RY(1)=Y-NYPIX+1.0
      RY(2)=Y

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'save',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Prepare PostScript string to hold one row's data
*
      WRITE(S,100) NXPIX
  100 FORMAT('/line',I5,' string def')
      CALL GKFOCO(KIOPB,S(1:21),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Now send out data in the format required by the ca procedure:
*     integerised Lower Left coordinates of the array (to where we translate),
*     area in X and Y onto which the array should be mapped (parameters for
*     scale), dimensions of source image (NXPIX, NYPIX), number of bits per
*     sample (8), and finally the image matrix which explains how the scanning
*     should be done (for left-to-right, top-to-bottom, i.e. GKS scanning
*     the matrix is [NXPIX 0 0 -NYPIX 0 NYPIX] ).
*
      WRITE(S,110) RX(1), RY(1), RX(2)-RX(1)+1.0,RY(2)-RY(1)+1.0
  110 FORMAT(2F11.3,' translate',2F11.3,' scale')
      CALL GKFOCO(KIOPB,S(1:60),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      WRITE(S,111)NXPIX, NYPIX, NXPIX, -NYPIX, NYPIX
  111 FORMAT(2I6,' 8 [',I6,' 0 0 ',I6,' 0', I6, ']{ca}im')
      CALL GKFOCO(KIOPB,S(1:48),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     This should be followed by the raster data. It is sent in hexadecimal.
*
      DO 130 IR= 1,NYPIX
         DO 120 ICCHST=1,NXPIX,ICHUNK/NBC
            KMAX=MIN(ICCHST+ICHUNK/NBC-1,NXPIX)
*           insert one space character in front of chunk of hex data
            S(1:1)=' '
            JH = 2
            DO 121 IC=ICCHST,KMAX
*              image operator HEX coded grey levels of RGB Values.
*              Convert them as required.
               CALL GK1ACX(ICOLAR(IC,IR),NBC,S(JH:JH+2*NBC-1))
               JH = JH + 2*NBC
121         CONTINUE
            CALL GKFOCO(KIOPB,S(1:JH-1),IREM)
120      CONTINUE
130   CONTINUE

*
*     Restore here, ca doesn't do it.
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'restore',IREM)

      END
