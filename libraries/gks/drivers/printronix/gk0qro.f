


      SUBROUTINE GK0QRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT  Original Versatec routine
*                    AJC  Modifcations for Printronix P300
*                    PTW  Fix narrow picture bug
*                    PLP  Modifications for PRIME
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
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'
      INCLUDE '../../include/gkhp.cmn'

*
*     Intrinsic functions declaration
*
      INTRINSIC MOD,NINT,MIN0,MAX0,ABS
*
*     External functions declaration
*
      CHARACTER GKAN1
      INTEGER   GK0QLC
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NWSTYP,LPORT
      PARAMETER (NWSTYP=6,LPORT=0)
*
      INTEGER IX1,IX2,IX3,IX4,IY,I,K
      INTEGER IVAL,NX,NY,ILOOP,IX
      REAL X1,Y1
*
*  ALGORITHM
*  ---------
*     Each point set or cleared, depending on whether the value of colour
*     indices in colour array is above or below 1 . For LANDscape
*     orientation slight optimisation is employed: the line is split  into
*     three parts in the same ways as lines parallel to the X axis are
*     plotted in GK0QXL.
*
*
*--------------------------------------------------------------------

*     Determine Workstation orientation

      IF(KWKDAT(NWSTYP,KWKIX).EQ.LPORT)THEN
*
*        PORTrait
*        Initialise locals with changed orientation in mind:
*        on exit from transformation and clipping, X and Y have
*        right values , but (as well as NXPIX and NYPIX) must swap.
*
         NY=NXPIX
         NX=NYPIX
         X1=Y
         Y1=ABS(X)
*
*        No optimisation in this case. Raster is plotted bit by bit
*        due to colour indices in ICOLAR representing settings for
*        pixels along X-axis and in a rotated frame X-axis is to be
*        treated as Y-axis in a non-rotated.
*
         IY=NINT(Y1)
*
*        Now output the raster to the bitmap
*
         DO 99 ILOOP=1,NX
            IX=NINT(X1)
            DO 10 I=IY,IY-NY+1,-1
               IF(ICOLAR(IY+1-I,ILOOP).LT.1) THEN
                  CALL GK0QCB(IX,I)
               ELSE
                  CALL GK0QSB(IX,I)
               ENDIF
   10       CONTINUE
*
*           Next row
*
            X1=X1-1.0

   99    CONTINUE

      ELSE
*
*        LANDscape
*        Initialise  locals , no rotation
*
         NX=NXPIX
         X1=X
         Y1=Y
*
*        The line is plotted in 3 pieces, from the start to first byte
*        boundary, from the first to last byte boundary and from the last
*        byte boundary to the end. The boundary X coordinates:
*
         IX1 = NINT(X1)
         IX4 = IX1 + NX - 1
         IX2 = ((IX1+5)/6) * 6
         IX3 = (IX4/6) * 6 - 1
*
*        Now output the raster to the bitmap in a loop
*
         DO 999 ILOOP=1,NYPIX
            IY=NINT(Y1)
*
            DO 100 I = IX1,MIN0(IX2-1,IX4)
               IF (ICOLAR(I-IX1+1,ILOOP).LT.1) THEN
                  CALL GK0QCB(I,IY)
               ELSE
                  CALL GK0QSB(I,IY)
               END IF
  100       CONTINUE

            IF (IX4-IX1.GE.6) THEN
               DO 200 I = IX2,IX3,6
                  IVAL = 0
                  DO 300 K = 0,5
                     IF (ICOLAR(I+K-IX1+1,ILOOP).LT.1) THEN
                        IVAL = IVAL/2
                     ELSE
                        IVAL = IVAL/2 + 32
                     END IF
  300             CONTINUE
*                 Bit 6 and/or bit 7 must be set for PLOT mode
                  IVAL=IVAL+64
*                 Store the value of processed character
                  CHP(GK0QLC(I,IY))=GKAN1(IVAL)
  200          CONTINUE
            END IF

            DO 400 I = MAX0(IX3+1,IX1),IX4
               IF (ICOLAR(I-IX1+1,ILOOP).LT.1) THEN
                  CALL GK0QCB(I,IY)
               ELSE
                  CALL GK0QSB(I,IY)
               END IF
  400       CONTINUE
*
*        Next row
*
         Y1=Y1-1.0

  999    CONTINUE
      ENDIF

      END
