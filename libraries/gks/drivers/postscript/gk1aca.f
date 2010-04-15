


      SUBROUTINE GK1ACA(NXDIM,NYDIM,ICOLAR)
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
*     Send cell array to the external PostScript file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP  NXDIM   -  First dimension of colour array
*     INP  ICOLAR  -  Colour array
*
      INTEGER NXDIM,NYDIM,ICOLAR(NXDIM,NYDIM)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*     W/S Comms Area:
*     KWKIX     : Workstation Identifier
*     QWR1,QWR2 : WC X,Y of point P
*     QWR3,QWR4 : WC X,Y of point Q
*     QWR5,QWR6 : WC X,Y of point R
*
*     KWI1,KWI2 : dimensions of colour array
*     KWI3,KWI4 : start column, start row
*     KWI5,KWI6 : number of columns, number of rows
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
*  DCX     - Device Coordinates X-Coord array for transformation
*  DCY     - Device Coordinates Y-Coord array for transformation
*  DUMMY   - Dummy character, required by the buffering routine.
*  IC      - Current column in Cell Array
*  ICCHST  - Column in Cell Array at start of current chunk
*  IR      - Current row in Cell Array
*  JH      - Current position in Hex code string
*  ICHUNK  - Maximum number of array's bytes output at a time
*  IREM    - Dummy integer, required by the buffering routine.
*  ISTCOL  - Start Column index
*  ISTROW  - Start Row    index
*  NBC     - Number of bytes in colour code
*  NMAX    - Temporary count variable
*  NXDX    - Cell Array X-grid dimension
*  NYDY    - Cell Array Y-grid dimension
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*  TD      - Cell array transformation (Image Space to DC)
*  TI      - Cell array transformation (DC to Image Space)
*

*
      INTEGER    ICHUNK
      PARAMETER (ICHUNK=50)

*     small real
      REAL       SMALL
      PARAMETER (SMALL=1.0E-4)

*     Offset in Workspace
      INTEGER    ICOLR
      PARAMETER (ICOLR=17)
*
      INTEGER IC, ICCHST, IR, JH
      INTEGER NMAX, IREM, NXDX, NYDY, ISTCOL, ISTROW, NBC
*
      REAL DCX(4), DCY(4), TD(3,2), TI(3,2)
*
      CHARACTER S*255, DUMMY
*
*  ALGORITHM
*  ---------
*     Get cell array transfromation with GKCELT and cell image
*     operator using the inverse of this transformation.
*     Use the 'im' procedure to call the image or colorimage operator
*     according to colour type of workstation.
*
*--------------------------------------------------------------------------


*     Get Cell array transformation (image space to DC)
      CALL GKCELT(DCX,DCY,TD)
*     Invert the transformation (to convert from DC to image space)
      CALL GKMTIV(TD,TI)

*
*     Initialise cell array grid dimensions
*
      NXDX = KWI5
      NYDY = KWI6
*
*     Initialise start column and start row indices
*
      ISTCOL = KWI3
      ISTROW = KWI4

*
*     Initialise number of bytes in colour code
*
      IF(KWKDAT(ICOLR,KWKIX) .EQ. GCOLOR)THEN
         NBC = 3
      ELSE
         NBC = 1
      ENDIF

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'save',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Prepare PostScript string to hold one row's data
*
      WRITE(S,100) NXDX
  100 FORMAT('/line',I5,' string def')
      CALL GKFOCO(KIOPB,S(1:21),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Now send out data in the format required by the ca procedure:
*     dimensions of source image (NXDX, NYDX), number of bits per
*     sample (8), and in a new line the image matrix, which has been
*     obtained from GKCELT and inverted. In the case of colour image
*     the multiproc argument is added as 'false' followed by a '3' to
*     indicate 3 component colour.
*
      WRITE(S,105) NXDX,NYDY
  105 FORMAT( 2I6,' 8')
      CALL GKFOCO(KIOPB,S(1:14),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
*
      WRITE(S,111   ) TI(1,1),TI(1,2),TI(2,1),TI(2,2),TI(3,1),TI(3,2)
  111 FORMAT( '[', 4F11.6, 2F11.3, ']{ca}im')
      CALL GKFOCO(KIOPB,S(1:79),IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     This should be followed by the raster data.
*     It is sent in hexadecimal, encoded by GK1ACX.
*
      DO 130 IR= ISTROW, (ISTROW+NYDY)-1
          DO 120 ICCHST=ISTCOL,ISTCOL+NXDX,ICHUNK/NBC
             NMAX=MIN(ICCHST+ICHUNK/NBC-1,NXDX)
*            insert one space character in front of chunk of hex data
             S(1:1)=' '
             JH = 2
             DO 125 IC=ICCHST,NMAX
*              im operator expects Hex Codes for Grey or RGB Values.
               CALL GK1ACX(ICOLAR(IC,IR),NBC,S(JH:JH+2*NBC-1))
               JH = JH + 2*NBC
  125        CONTINUE
             CALL GKFOCO(KIOPB,S(1:JH-1),IREM)
  120     CONTINUE
  130 CONTINUE

*
*     Restore here, ca doesn't do it.
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      CALL GKFOCO(KIOPB,'restore',IREM)
      CALL GKFOCO(KIOSN,DUMMY,IREM)
      GOTO 999

  999 CONTINUE


      END
