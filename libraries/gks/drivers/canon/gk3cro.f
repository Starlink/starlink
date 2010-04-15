      SUBROUTINE GK3CRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Authors:          DLT, PTW  (Starlink)
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
*     Plot pixel array
*
*  ARGUMENTS
*  ---------
*     INP  X,Y     -  Coordinates of raster origin
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
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkhp.cmn'
*  Offsets into KWKDAT workspace
      INTEGER CT, ROTATE, TEX
      PARAMETER (CT=1, ROTATE=2, TEX=3)
*  End of offsets

*
*  ALGORITHM
*  ---------
*     Each point set or cleared depending on whether the intensity is
*     above or below the value in the mask or that x,y.
*

***
*  Problem with laserprinter 10/Sep/86 - odd number of bytes in
*  image draw causes image to vanish
      INTEGER NFUDGE
***

*
*  LOCALS
*  ------
      INTEGER NXB,NXC,NXR,I0,IX0,IX,IY0,IC,I,J,K,MASKY
      INTEGER IVAL,IBIT(0:7),NBYTE
      CHARACTER*10 CBYTE, WIDTH, CX0, CY0
      INTEGER STCBY, STWID, NCX0, NCY0
      INTEGER NLEFT
*  Laserprinter codes
      CHARACTER*1 IS1, IS2, VDM, CSI
*      PARAMETER (IS1=CHAR(31),
*     :           IS2=CHAR(30),
*     :           VDM=CHAR(125),
*     :           CSI=CHAR(155))
*  Array of bytes to be sent (reducing GKFOCO calls for speed)
      INTEGER ISTRIP,NSTRIP
      PARAMETER (NSTRIP=100)
      CHARACTER*(NSTRIP) STRIP
*  Dot size in 0.01mm units
      REAL FX, FY
      PARAMETER (FX=8.466667)
*  Mask width, mask, line from mask (extended to avoid MOD)
      INTEGER ISZ,ISZM1
      PARAMETER (ISZ=8, ISZM1=ISZ-1)
      INTEGER MASK(0:ISZM1,0:ISZM1)
      INTEGER MASKL(0:ISZM1+7)
      DATA MASK /
     :   0, 64,160,224,248,232,168, 72,
     :  80, 40, 96,184,216,192,112, 16,
     : 176,144, 48, 88,152,104, 24,128,
     : 240,208,136, 32, 56,  8,120,200,
     : 248,232,168, 72,  0, 64,160,224,
     : 216,192,112, 16, 80, 40, 96,184,
     : 152,104, 24,128,176,144, 48, 88,
     :  56,  8,120,200,240,208,136, 32
     :/
*  Lookup table for fast bit setting
      DATA IBIT / 128,64,32,16,8,4,2,1 /

      IS1=CHAR(31)
                IS2=CHAR(30)
                VDM=CHAR(125)
                CSI=CHAR(155)

*
*--------------------------------------------------------------------

*  Total bytes per line, complete bytes, bits in final partial byte
      NXB = (NXPIX+7)/8
      NXC = NXPIX/8
      NXR = MOD(NXPIX,8)

*  Encode start X,Y in plotter units
      FY = -FX
      CALL GK3CEI(NINT(AINT(X)*FX),CX0,NCX0)
      CALL GK3CEI(NINT(AINT(Y)*FY),CY0,NCY0)

***
*
*  (laserprinter problem)
*
      NFUDGE = MOD(NXB,2)
      NXB = NXB + NFUDGE
*
***

*
*  -----------
*  PRE-ERASURE
*  -----------
*

*  Select "white" line attribute and begin polyline
C    This doesn't work because the printer buffers text commands (pixel
C    plotting is text) and text before some graphics may get written to
C    the bit map after the graphics.
C      CALL GKFOCO(KIOPB,
C     :            VDM//
C     :            'G2'//
C     :            IS2//
C     :            '1',
C     :            NLEFT)
C
*  Encode delta X
C      CALL GK3CEI(NINT(REAL(NXPIX-1)*FX),CDX,NCDX)
C
*  Loop: line by line
C      DO 1100 J=1,NYPIX
C         CALL GK3CEI(NINT(AINT(Y+REAL(J-1))*FY),CY,NCY)
C         CALL GKFOCO(KIOPB,
C     :               CX0(:NCX0)//
C     :               CY(:NCY)//
C     :               CDX(:NCDX)//
C     :               '0',
C     :               NLEFT)
C         IF (J.LT.NYPIX) CALL GKFOCO(KIOPB,IS1,NLEFT)
C 1100 CONTINUE
C      CALL GKFOCO(KIOPB,IS2,NLEFT)

*
*  ----------------
*  PLOT PIXEL ARRAY
*  ----------------
*

*  Return to text mode
      CALL GKFOCO(KIOPB,VDM//'p'//CX0(:NCX0),NLEFT)
      CALL GKFOCO(KIOPB,CY0(:NCY0)//IS2,NLEFT)

*  Encode total number of bytes needed for image draw
      NBYTE = NXB * NYPIX
      WRITE( CBYTE, '(I10)') NBYTE
      DO 2010 I = 9,1,-1
        IF ( CBYTE(I:I).EQ.' ') GO TO 2020
 2010 CONTINUE
 2020 CONTINUE
      STCBY = I + 1

*  Encode width in bytes
      WRITE( WIDTH, '(I10)') NXB
      DO 2030 I = 9,1,-1
         IF ( WIDTH(I:I).EQ.' ') GO TO 2040
 2030 CONTINUE
 2040 CONTINUE
      STWID = I + 1

*  Image draw command
      CALL GKFOCO(KIOPB,
     :            CSI//
     :            CBYTE(STCBY:)//
     :            ';'//
     :            WIDTH(STWID:)//
     :            '.r',
     :            NLEFT)

*
*  Generate and encode the bit pattern
*  -----------------------------------

*  Initialise indices
      ISTRIP = 0
      I0 = KHPXI(KWKDAT(CT,KWKIX))
      IX0 = ABS(NINT(X))
      IY0 = ABS(NINT(Y))

*  Loop: line by line down array of bits
      DO 2500 J = 1,NYPIX
         MASKY = MOD(IY0+J,ISZ)

*     Copy line from mask
         DO 2100 K = 0,ISZM1
            MASKL(K) = MASK(K,MASKY)
 2100    CONTINUE

*     Replicate 7 elements to avoid MOD 8
         DO 2101 K = 0,6
            MASKL(ISZ+K) = MASKL(K)
 2101    CONTINUE

*     Loop: byte by byte along array (full bytes only)
         DO 2300 I = 0,NXC-1
            IC = I*8+1
            IX = MOD(IX0+I*8,ISZ)

*        Initialise output byte to white
            IVAL = 0

*        Loop: bit by bit along byte setting bits as appropriate
            DO 2200 K = 0,7
               IF (KHP(I0+ICOLAR(IC+K,J)).LE.MASKL(IX+K))
     :                              IVAL = IVAL + IBIT(K)
 2200       CONTINUE

*        Increment pointer and store byte locally
            ISTRIP = ISTRIP + 1
            STRIP(ISTRIP:ISTRIP) = CHAR(IVAL)

*        If array full, send to output routine
            IF (ISTRIP.GE.NSTRIP) THEN
               CALL GKFOCO(KIOPB,STRIP,NLEFT)
               ISTRIP = 0
            END IF

*        Next full byte
 2300    CONTINUE

*     Process final (partial) byte if present
         IF (NXR.NE.0) THEN
            IC = NXC*8+1
            IX = MOD(IX0+NXC*8,ISZ)
            IVAL = 0
            DO 2400 K = 0,NXR-1
               IF (KHP(I0+ICOLAR(IC+K,J)).LE.MASKL(IX+K))
     :                              IVAL = IVAL + IBIT(K)
 2400       CONTINUE
            ISTRIP = ISTRIP + 1
            STRIP(ISTRIP:ISTRIP) = CHAR(IVAL)
            IF (ISTRIP.GE.NSTRIP) THEN
               CALL GKFOCO(KIOPB,STRIP,NLEFT)
               ISTRIP = 0
            END IF
         END IF

***
*
*  (laserprinter problem)
*
         IF (NFUDGE.NE.0) THEN
            ISTRIP = ISTRIP + 1
            STRIP(ISTRIP:ISTRIP) = CHAR(0)
            IF (ISTRIP.GE.NSTRIP) THEN
               CALL GKFOCO(KIOPB,STRIP,NLEFT)
               ISTRIP = 0
            END IF
         END IF
*
***

*     Next line
 2500 CONTINUE

*  Flush local byte array
      IF (ISTRIP.GT.0) CALL GKFOCO(KIOPB,STRIP(:ISTRIP),NLEFT)

*  Return to vector mode
      CALL GKFOCO(KIOPB,CSI//'0&}',NLEFT)

      END
