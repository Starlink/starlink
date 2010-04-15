C# IL>=a, OL>=2
      SUBROUTINE GK5VWD(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:
*
      INCLUDE (CHECKIBM)
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*
* Driver routine for colour Versatec 9242
*
*  SUBROUTINE CALLS
*  ----------------
*
*     GK5VIO  :  Open and Initialise Random 2.1
*
*     GK5VCS  :  Close down Plot file
*
*     GK5VCL  :  Clears Frame
*
*     GK5VCA  :  Outputs Cell Array
*
*     GK5VRO  :  Outputs Cell Array ?
*
*     GK5VFL  :  Output polyline using Fill Area
*
*     GK5VLN  :  Output polyline using PENCLR
*
*     GK5VXF  :  Supply Font details
*
*     GK5VCP  :  Convert pattern to Versatec form
*
*     GK5VPT  :  Split pattern into four colour passes
*
*     GK5VGC  :  Stores points from GKCRCS
*
*     GK5VPA  :  Fill area for user defined patterns
*
*     GK5VFI  :  Fill area
*
*  MAINTENANCE LOG
*  ---------------
*
*     01/11/87   RTP   Update for IS - version 7.4
*     13/11/87   RTP   Updated for New version of Random (2.1)
*                      includes direct colour
*     19/02/88   RTP   Correct Options for rotated plots
*                      Change linewidths to allow up to 31 pens
*                      Change default Text width to 2 pixels
*     13/05/88   DSG   Change GK0V.. to GK5V.. throughout.
*     19/07/88   RTP   Change GK5VCS to check for monochrome and
*                      output extra line to stop smudges
*     14/07/89   RTP   Change Clear screen to handle conditional clear
*                      Don't set colour 0 or 1 to (0,0,0)
*                      Correct cell array code to handle rotated cells
*     27/07/89   RTP   Reword Header to include Form code
*      4/10/89   RTP   Change Input stream for header info
*                      Spot if GSCR changes a primary colour
*
*    Last Modified on  25 Oct 1989  at  11:18:29
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code
*     INP NID   - Size of array IDAT
*     I/0 IDAT  - Integer data passed to or from workstation
*     INP NRD   - Size of arrays RX and RY
*     I/O RX    - Real X-coordinate data passed to or from workstation
*     I/O RY    - Real Y-coordinate data passed to or from workstation
*     INP NCD   - Size of character array
*     I/O STR   - Character array
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE (PRGKMC)
      INCLUDE (PRGKHP)
      INCLUDE (PRGKS)
      INCLUDE (PRGKDT)
      INCLUDE (PRGKWDT)
      INCLUDE (PRGKIO)
      INCLUDE (PRGKINP)
      INCLUDE (PRGKFLS)
      INCLUDE (CMGKWDT)
      INCLUDE (CMGKWCA)
      INCLUDE (CMGKWKD)
      INCLUDE (CMGKWSL)
      INCLUDE (CMGKFLS)
      INCLUDE (CMGKXFD)
      INCLUDE (CMGKSTK)
      INCLUDE (CMGKHP)
      INCLUDE (CMGKERR)
      INCLUDE (CMGKVHC)
*
*  EXTERNALS
*  ---------
*
      EXTERNAL GK5VLN,GK5VRO,GK5VFL, GK5VGC
      EXTERNAL GK5VXF
*
*  LOCALS
*  ------
*     GETPAT array to hold a pattern defined in array LISPAT
*     MXCOL  Maximum colour index
*     IBAUD  Offset in KWKDAT of baud rate for this terminal
*     INKWI1 Holds initial value of KWI1 in inquire fill area facils
*     ICHUNK Number of points in each chunk of output primitive
*     ICTLZ  ASCII code for ctrl/Z
*     INTXFP No. of font/precision pairs
*     IOFF   Stack offset for output primitive chunks
*  IXLN,IYLN Stack pointers for GK5VLN
*    XTX,YTX Real variables to hold transformed text positions
*  IXMK,IYMK Stack pointers for GK5VMK
*IOFFX,IOFFY Offsets for stack holding coordinates of initial stroke
*     IOCISW Function code for input from terminal
*     ITXF   Hardware fonts
*     ITXP   Hardware precisions
*     IVAL   Integer form of valuator if input that way
*     NOUT   Number of bytes returned on input
*     PR,PG,PB  Integer flags for red, green and blue colour components
*     I      Various
*     N      Various
*     NLEFT  Returned by output buffering routine (amount of buffer left
*     ICODE  Key hit returned with cursor position
*     NIPTS2 Number of points in initial stroke after checking
*     NSEE   Number of character we have room for both in echo area and
*            input buffer
*     ICHOIC Choice returned
*     INTA   Local integer array with multiple uses
*              - receiving input device state (size 10 max)
*              - receiving WDT info for Inq Text Facil (size 19)
*     IPREC  Stores available text precisions
*     REALA  Local real array with multiple uses
*              - receiving input device state (size 7 max)
*              - receiving WDT info for Inq Text Facil (size 12)
*   XDC,YDC  2-element arrays used to hold single locator value in DC
*            or part of stroke in DC (previous and current point)
*     FLAG   Used to indicate whether linetype simulation is required.
*     VALSTR String of characters representing valuator input
*     CHOSTR String of single character representing choice input
*     PROMPT The prompt preceding input implemented by keyboard
*     CDUMMY A dummy character variable
*     IXFI,IYFI Offsets in stack for fill area
*     DR,DG,DB - differences from set colours
*     DIFFS  Mean square difference from set colour
*     MNDIFF Minimum colour difference (squared)
*     LOGUNIT - Unit for diagnostic messages
*
      INTEGER    IBAUD, ICHUNK, ICTLZ, INTXFP
      PARAMETER (IBAUD = 1, ICHUNK=200, ICTLZ=26, INTXFP=2)
      INTEGER    MXCOL, LOGUNIT
      PARAMETER (MXCOL = 2048, LOGUNIT = 6)
      INTEGER INKWI1, IOFF, IOFFX,IOFFY, IOCISW,IVAL,NOUT, N,
     :    NLEFT, ICODE, NIPTS2, NSEE, ICHOIC
      INTEGER IXLN,IYLN,IXMK,IYMK, IWDTH, IXDP, IYDP
* GDP Parameters for Version 7.4
      INTEGER GARC, GCHORD, GPIE, GCIRCL
      PARAMETER (GARC = -1,  GCHORD = -2, GPIE = -3, GCIRCL = -4)
      REAL XTX,YTX, TRANSX, TRANSY, TOLDIF
      REAL RADIUS, R, PI, AXCX, ANGLE
      REAL AX, AY, BX, BY, CX, CY, DXAB, DYAB, DXBC, DYBC
      REAL C1, C2, D, D2, XCEN, YCEN
      REAL XVAL (5), YVAL (5)
      INTEGER I, J, ICOUNT
      INTEGER PR, PG, PB
      INTEGER GETPAT (16)
      INTEGER INTA(19),IPREC(KFNTMX),ITXP(INTXFP),ITXF(INTXFP)
      REAL REALA(12), XDC(2),YDC(2)
      LOGICAL FLAG,  RFLAG, GFLAG, BFLAG
      CHARACTER VALSTR*12,CHOSTR*1, PROMPT*50, CDUMMY*1
      INTEGER IXFI,IYFI
      INTEGER GDPCOL, PREMAP (0:15)
      REAL DR, DG, DB, DIFFS, MNDIFF
      INTEGER COL
      DATA PI /3.1459267/

*  Map RAL-GKS colours 0-15 to Versatec preset Colours
      DATA PREMAP /9, 1, 6, 7, 5, 4, 2, 3, 9, 15, 10, 11,
     :             12, 13, 14, 1/
      DATA ITXF /1, 1/,
     :     ITXP /GSTRP, GCHARP/
*
*  STACK USAGE
*  -----------
*     POLYLINE and POLYMARKER for transformations
*
*  ERRORS
*  ------
*      32   Specified workstation is not of category MO
*      34   Specified workstation is not of category MI
*      77   Fill area interior style not supported
*      83   Pattern not supported
*      86   Colour index is invalid
*     104   Cannot generate GDP
*     180   Specified function is not supported
*
*---------------------------------------------------------------------




* Conditional GOTO on entrypoint code

      GOTO (       10,  20,  30,  40,  50,  60,  70,  80,9999,
     :       100, 110, 120, 130, 140, 150, 160, 170, 180, 190,
     :       200, 210, 220, 230, 240, 250, 260, 270, 280,9999,
     :      9999, 310, 320, 330,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 410, 410, 410, 410, 410, 410, 410, 410,
     :       410, 410,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 610, 620, 630, 640, 650, 660, 670, 680, 690,
     :       700, 710, 720, 730, 740, 750, 760, 770, 780, 790,
     :       800, 810,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 910, 920, 930) IENT

      GOTO (1111,1111,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1111,1111,1330,1111,1111,1111,1370,1380,1111,
     :      1111,1410,1420,1111,1440,1111,1111,1111,1111,1111,
     :      1111,1510,1111,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      1111,1710,1111,1111,1111,1111,1111,1111,1111,1790,
     :      1111,1810,1111,1111,1840,1850,1111,1111,1111,1111,
     :      1111,1111,1111,1111,1111,1111,1960,1970,1111) IENT-119

      GOTO 9999


* Open workstation
   10 CONTINUE
* Set up workstation state list and workstation description table
      CALL GKIWSL(KWKIX,KWKTYP)
      IF (KERROR.EQ.0) THEN
        KCID(KWKIX) = KWI1
        KDFM(KWKIX) = GBNIG
        KWIO(KWKIX) = GNO
        KIMRGM(KWKIX) = GSUPPD

* Erase screen
        FSTPAS = .TRUE.
* open file to VERSATEC
        CALL GK5VIO
* Initialise variables for linetype simulation
        QWOLDX(KWKIX) = -99.0
        QWOLDY(KWKIX) = -99.0
      END IF
      KWI1 = GOUTPT
* Patterns 1 to 4 are predefined
      DO 11 I = 1,4
         PATDEF (I) = 0
  11  CONTINUE
* Patterns 5 - 40 are undefined
      DO 12 I = 5, 40
         PATDEF (I) = -1
  12  CONTINUE
      PATIDX = 1
* set up array to map colours for Set Colour Representation.
* Add extra Grey scale at 15
      CALL DEFRGB (15, 0.85, 0.85, 0.85)
      DO 15 I = 0, 15
         COLMAP (I) = PREMAP (I)
  15  CONTINUE
* set up remaining colours to Versatec defaults
      DO 16 I = 16, 256
         COLMAP(I) = I
  16  CONTINUE
* Clear screen to set viewport
      CALL GK5VCL
      GOTO 9999



* Close workstation
   20 CONTINUE
      IF( KWI1.EQ.1 ) THEN
        KWDONE = KRFUSE
      ELSE
        CALL GK5VCS
      END IF
      GOTO 9999



* Clear workstation
   30 CONTINUE
      IF( KWI1.EQ.2 ) THEN
        KWDONE=KRFUSE
        CALL GKSLDL(KSSGPT(KWKIX))
*       KWDONE=KACEPT
*       CALL GK5XCL
      ENDIF
      GOTO 9999



* Redraw all segments on workstation
   40 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Update workstation
   50 CONTINUE
      KWDONE = KRFUSE
      GOTO 9999



* Set deferral state
   60 CONTINUE
      KDFM(KWKIX) = KWI1
      KIMRGM(KWKIX) = KWI2
      IF (KWI1.EQ.GASAP) THEN
        KWIO(KWKIX) = GYES
        CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
      ELSE
        KWIO(KWKIX) = GNO
      END IF
      IF (KIMRGM(KWKIX).EQ.GALLOW .AND. KNFAUP(KWKIX).EQ.GYES .AND.
     :    KDSMT(KWKIX).EQ.GNEMPT) THEN
        KWRGN(KWKIX) = .TRUE.
        KRGN = .TRUE.
      END IF
      GOTO 9999



* Do deferred output actions
   70 CONTINUE
      CALL GKIOBO(KIOSN,1,KDAT,NLEFT)
      KWI1 = KNFAUP(KWKIX)
      GOTO 9999



* Clear display surface
   80 CONTINUE
      IF (KWI1.EQ.GALWAY.OR.KDSMT(KWKIX).EQ.GNEMPT) CALL GK5VCL
      CALL GKWCLD
      GOTO 9999



* Message
  100 CONTINUE
      GOTO 9999



* Escape
  110 CONTINUE
      KERROR = 180
      GOTO 9999



* Polyline
* The Versatec only allows 8 line colours when using PLOTS (see
* Versatec manual page 4-46 PENCLR) and 31 Linewdiths when using
* NEWPEN (see page 4-40 of manual). So, if a polyline is
* requested with a colour index > 8 or a linewidth > 31.0 then
* the line is drawn using Versaplot TONE (as suggested in the
* manual !) via subroutine GK5VFL ; otherwise the line is
* drawn using Versaplot PLOT, PENCLR & NEWPEN via GK5VLN.
* NOTE: 63 colours and 31 widths are now allowed
  120 CONTINUE
      ENTRYP = 12
      IWDTH = INT (QWLNWD (KWKIX) * 10000)
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 122 I = 1,NRD,ICHUNK-1
          IF (NRD-I.LT.ICHUNK-1) N = NRD-I + 1
          IF (N.EQ.1) GOTO 122
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          IF (IWDTH .GT. 310000 .OR. COLMAP (KWPLCI (KWKIX)) .GT. 9)
     :    THEN
             CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.TRUE.,
     :                250.0,QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK5VFL)
          ELSE
             CALL GKLCLP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),.TRUE.,
     :                250.0,QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                   QWCLYT(KWKIX),GK5VLN)
          END IF
  122   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
        END IF

      GOTO 8888



* Polymarker
* Because the utility GKMYTP uses the workstations Line drawing
* routines (ie in this case GK5VFL & GK5VLN) then the same
* colour principles apply to PolyMarker as do to PolyLine.
  130 CONTINUE
      ENTRYP = 13
      CALL GKSTAL(KREALS,ICHUNK*2,IOFF)
      IF (KERROR.EQ.0) THEN
        N = ICHUNK
        DO 132 I = 1,NRD,ICHUNK
          IF (NRD-I.LT.ICHUNK) N = NRD-I + 1
          CALL GKTWD(N,RX(I),RY(I),QSTACK(IOFF),QSTACK(IOFF+ICHUNK))
          IF ( COLMAP(KWPMCI(KWKIX)) .GT. 9 ) THEN
             CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                      KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                      QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                       QWCLYT(KWKIX),GK5VFL)
          ELSE
             CALL GKMTYP(N,QSTACK(IOFF),QSTACK(IOFF+ICHUNK),
     :                      KWMKTY(KWKIX),QNMMKS(KWKIX),QWMKSZ(KWKIX),
     :                      QWCLXL(KWKIX),QWCLYB(KWKIX),QWCLXR(KWKIX),
     :                       QWCLYT(KWKIX),GK5VLN)
          END IF
  132   CONTINUE
        CALL GKSTDA(KREALS,IOFF)
      END IF
      GOTO 8888



* Text
* At the momemt only STROKE precision available ... the routine
* GK0CXF is written as it is used in the user end driver but
* GK5VXC is not written so can't do char & string yet.
* If precision needs CHAR or STRING? then GKXDWC will be required
* along with two routines GK5VXF and GK5VXC. See the EXTERNAL statement

* NOTE : need to call GKXDWO with GK5VFL if colour index > 9.
*        this could be a bit dodgey as it will produce gushings
*        of data points and in some case dotty text eg if the
*        dotty colour 247 is used.
  140 CONTINUE
      ENTRYP = 14
      IF (COLMAP(KWTXCI(KWKIX)).GT. 9) THEN
         CALL GKXDWO (NID, IDAT, GK5VFL)
      ELSE
         CALL GKXDWO (NID, IDAT, GK5VLN)
      END IF
      GOTO 8888



* Fill area uses Versaplot TONE to shade in styles Solid, Hatch,
* and pattern as this makes best use of the device whereas if
* GKFILS is used it uses gushings of CPU time and is a nightmare.
*
  150 CONTINUE
      ENTRYP = 15
*TRANSFORM TO DEVICE COORDS
*
         CALL GKSTAL( KREALS,NRD,IXFI )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKSTAL( KREALS,NRD,IYFI )
         IF( KERROR.NE.0 ) GO TO 8888
         CALL GKTWD( NRD,RX,RY,QSTACK(IXFI),QSTACK(IYFI) )
*
* GK5VPA is a routine used by Fill area and GDP to basically
* produce the correct interior style for a 'san area (see routine).
      CALL GK5VPA (NRD, QSTACK (IXFI), QSTACK (IYFI), 0.0, 0)
      CALL GKSTDA( KREALS,IYFI )
      CALL GKSTDA( KREALS,IXFI )
      GOTO 8888



* Call GK5VCA to do cell array. It only outputs VERSAPLOT TONE patterns
* which are not transformed.
  160 CONTINUE
*
* Let Versatec do the clipping
*
         CALL GK5VCA( KWI1,KWI2,KWI3,KWI4,KWI5,KWI6,IDAT )
      GOTO 8888



* GDP
  170 CONTINUE
      ENTRYP = 17
      IF (KCVIS.EQ.GINVIS)  GOTO 8888

      IF (KWI1 .EQ. GARC) THEN
* ARC - Make entry point for Polyline
         ENTRYP = 12
         IF ( QWLNWD(KWKIX) .GT. 7.0
     :    .OR. COLMAP (KWPLCI(KWKIX)) .GT. 9) THEN
            CALL GKCRCS (GARC, NRD, RX, RY, 1, KWLNTY(KWKIX).NE.1,
     :                250.0, GK5VFL, GK5VRO)
         ELSE
            CALL GKCRCS (GARC, NRD, RX, RY, 1, KWLNTY(KWKIX).NE.1,
     :                250.0, GK5VLN, GK5VRO)
         END IF
      ELSE IF (KWI1 .EQ. GCHORD) THEN

* Since GDP CHORD use the current fill area attributes to draw
* the Chord then it is better to use the fill area mechanism
* via GK5VPA as it is far more efficient and doesn't use the
* dreaded GKFILS unlike GKCRCS which would in due course cause
* loads of hassle.
* The co-ordinates for the specified have been calculated as
* follows (this seems a more efficient way ) :-
*        GKCRCS is called with the GDP identifier = GARC
*        the co-ordinates for the arc are stored in ARCPTCX etc.;
*        TONE is then used to fill the arc via GK5VPA
* NOTE : Versaplot TONE will automatically joint the last
*         point in the array to the first so as to close a
*         toneable area ie in  this case it will create
*         the required chord .
*
         IDX = 1
         CALL GKCRCS(GARC, NRD, RX, RY, 1, .FALSE.,
     :                20.0, GK5VGC, GK5VRO)

         CALL GK5VPA (IDX, ARCPTX, ARCPTY, RADIUS, GCHORD)

      ELSE IF (KWI1 .EQ. GPIE) THEN
* Pie - need to call GKCIRC to calculate points for arc then
* add the middle point and end point to the array of points
* so the pie sector can be drawn using the fill area mechanism.
* as this is more efficient than using GKCIRC because GKCIRC
* in turn calls GKFILS which creates nightmares on the Versatec.


* Calculate the centre point of circle for PIE.
         CALL GKSTAL (KREALS, 3, IXDP)
         IF (KERROR .NE. 0) GOTO 8888
         CALL GKSTAL (KREALS, 3, IYDP)
         IF (KERROR .NE. 0) GOTO 8888
         CALL GKTWD (3, RX, RY, QSTACK(IXDP), QSTACK(IYDP))
* calculation from GKCRCS
         AX = QSTACK (IXDP)
         BX = QSTACK (IXDP + 1)
         CX = QSTACK (IXDP + 2)
         AY = QSTACK (IYDP)
         BY = QSTACK (IYDP + 1)
         CY = QSTACK (IYDP + 2)
* deallocate stack before proceeding.
         CALL GKSTDA (KREALS, IYDP)
         CALL GKSTDA (KREALS, IXDP)
         DXAB = AX - BX
         DYAB = AY - BY
         DXBC = BX - CX
         DYBC = BY - CY
         C1 = DXAB * (AX + BX) + DYAB * (AY + BY)
         C2 = DXBC * (BX + CX) + DYBC * (BY + CY)
         D2 = 2.0 * (DXAB * DYBC - DXBC * DYAB)
* centre point of circle is : -
         XCEN = (C1 * DYBC - C2 * DYAB) / D2
         YCEN = (C2 * DXAB - C1 * DXBC) / D2
         IDX = 1
         CALL GKCRCS (GARC, NRD, RX, RY, 1, .FALSE.,
     :              20.0, GK5VGC, GK5VRO)
* The centre point is added on to the end of the data for the
* co-ordintaes of the arc part of the sector
         ARCPTX (IDX) = XCEN
         ARCPTY (IDX) = YCEN
         IDX = IDX + 1
         ARCPTX (IDX) = ARCPTX (1)
         ARCPTY (IDX) = ARCPTY (1)
* Test if sector is  Convex (ie angle is < =  180 degrees or PI/180)
         RADIUS = SQRT ((AX - XCEN)*(AX - XCEN)
     :                + (AY - YCEN)*(AY - YCEN))

         AXCX  = SQRT ((CX - AX)*(CX - AX) + (CY - AY)*(CY - AY)) / 2
         ANGLE = ASIN (AXCX / RADIUS)
         ANGLE = ANGLE * 2
         IF (ANGLE .GT. PI .OR. KWFAIS(KWKIX) .EQ. GHOLLO . OR.
     :       RADIUS .LT. 500 ) THEN
            CALL GK5VPA (IDX, ARCPTX, ARCPTY, 0.0, 0)
         ELSE
            CALL GK5VPA (IDX, ARCPTX, ARCPTY, 0.0, GPIE)
         END IF

      ELSE IF (KWI1 .EQ. GCIRCL) THEN
* Special case for circle  - ie if fill area interior style
* is SOLID or HOLLOW then call CIRCLE (Versaplot) directly
* otherwise call GK5VPA to define the pattern or hatch style
* which in turn calls CIRCLE. Because i dont't make use
* of the utility GKCIRC here (which transforms points from
* world to device) i have to transform to device co-ords.

* NOTE : only the first eight colours are available
*        using Versaplot CIRCLE so if a colour index > 8
*        is specified then the utility GKCIRC will
*        have to be called with GK5VFL.

         GDPCOL = COLMAP (KWFACI (KWKIX))

         CALL GKSTAL (KREALS, NRD, IXDP)
         IF (KERROR .NE. 0) GOTO 8888
         CALL GKSTAL (KREALS, NRD, IYDP)
         IF (KERROR .NE. 0) GOTO 8888
         CALL GKTWD (NRD, RX, RY, QSTACK(IXDP), QSTACK(IYDP))

         RADIUS = SQRT ( (QSTACK(IXDP + 1) - QSTACK(IXDP)) * * 2 +
     :         (QSTACK(IYDP + 1) - QSTACK(IYDP)) * * 2)
         RAD = RADIUS
*
*  Check if centre is outside clipping rectangle
*  if so use GK5VPA as CIRCLE does not clip
*
         IF ( QSTACK(IXDP+1) .LT. QWCLXL(KWKIX)+RAD .OR.
     :        QSTACK(IXDP+1) .GT. QWCLXR(KWKIX)-RAD .OR.
     :        QSTACK(IYDP+1) .LT. QWCLYB(KWKIX)+RAD .OR.
     :        QSTACK(IYDP+1) .GT. QWCLYT(KWKIX)-RAD ) THEN
*
*           Work out opposing points on circumference
*
            XVAL (1) = RX (1)
            XVAL (2) = RX (2) + RX (2) - RX (1)
            XVAL (3) = XVAL (1)
            YVAL (1) = RY (1)
            YVAL (2) = RY (2) + RY (2) - RY (1)
            YVAL (3) = YVAL (1)
            IDX = 1
            CALL GKCRCS (GARC, 3, XVAL, YVAL, 1, .FALSE., 20.0,
     :                   GK5VGC, GK5VRO)
            CALL GK5VPA (IDX, ARCPTX, ARCPTY, 0.0 , 0)
         ELSE IF (KWFAIS(KWKIX) .EQ. GSOLID) THEN
            CALL TONFLG (-1)
            CALL TONCLR (GDPCOL)
            CALL CIRCLE (QSTACK(IXDP + 1), QSTACK(IYDP + 1),
     :                    RADIUS, -1)
         ELSE IF (KWFAIS(KWKIX) .EQ. GHOLLO) THEN
            IF (GDPCOL .LT. 9) THEN
               CALL PENCLR (1, GDPCOL)
               CALL NEWPEN (1)
               CALL CIRCLE (QSTACK(IXDP + 1), QSTACK(IYDP + 1),
     :                     -RADIUS, 1)
            ELSE
               CALL GKCRCS (GCIRCL, NRD, RX, RY, 1, .FALSE., 20.0,
     :                  GK5VFL, GK5VRO)
            END IF
         ELSE IF (GDPCOL .GT. 8 .AND. KWFAIS(KWKIX) .EQ. GHATCH) THEN
            CALL GKCRCS (GCIRCL, NRD, RX, RY, 1, .FALSE.,
     :                 250.0, GK5VFL, GK5VRO)
         ELSE
            CALL GK5VPA (NRD, QSTACK(IXDP), QSTACK(IYDP),
     :                   RADIUS, GCIRCL)
         END IF
         CALL GKSTDA (KREALS, IYDP)
         CALL GKSTDA (KREALS, IXDP)

      ELSE
*  GDP not valid for Versatec
         KERROR = 104
         GOTO 9999
      ENDIF
      GO TO 8888

* Set polyline attributes
  180 CONTINUE
      CALL GKDPLB
* Need to check because individual settings won't have been checked.
* Changed for IS defaults to Linetype SOLID
      IF (KWLNTY(KWKIX) .LT. 1 .OR. KWLNTY(KWKIX) .GT. 5)
     :        KWLNTY(KWKIX) = GLSOLI
      IF (KWPLCI(KWKIX) .GE. KPCI(KWKIX)) KWPLCI(KWKIX) = 1
      GOTO 9999



* Set polymarker attributes
  190 CONTINUE
      CALL GKDPMB
* Need to check because individual settings won't have been checked.
* Changed for IS defaults to Marker type Asterisk
      IF (KWMKTY(KWKIX).LT.0 .OR. KWMKTY(KWKIX).GT.5)
     :           KWMKTY(KWKIX) = GAST
      IF (KWPMCI(KWKIX) .GE. KPCI(KWKIX)) KWPMCI(KWKIX) = 1
      GOTO 9999



* Set text attributes
  200 CONTINUE
      CALL GKDTXB
* Need to check because individual settings won't have been checked.
      IF (KWTXCI(KWKIX) .GE. KPCI(KWKIX)) KWTXCI(KWKIX) = 1

*     done if stroke precision
      IF(KWTXPR(KWKIX).EQ.GSTRKP) GOTO 9999
      KWTXFN(KWKIX) = 1
*     character rotation
      QWCHRX(KWKIX) = QWCHWX(KWKIX)
      QWCHRY(KWKIX) = QWCHWY(KWKIX)
      GOTO 9999

* Set fill area attributes
  210 CONTINUE
      CALL GKDFAB
* Need to check because individual settings won't have been checked.
* Changed for IS defaults to style 1
      IF (KWFAIS(KWKIX) .EQ. GHATCH .AND.
     :   (KWFASI(KWKIX) .GT. -1 .OR. KWFASI(KWKIX) .LT. -10))
     :         KWFASI(KWKIX) = 1
* Default pattern = 1
      IF (KWFAIS(KWKIX) .EQ. GPATTR .AND.
     :   (KWFASI(KWKIX) .LT. 1 .OR. KWFASI(KWKIX) .GT. 40))
     :         KWFASI(KWKIX) = 1

*
* VERSATEC has up to 256 colour TONES available. Currently (May 86),
* the maximum number of Fill Area Bundles is 20. (eg KMXFAB). We
* therefore assume that the user sets the Fill area bundles to
* individual and then sets the colour index.
* IS default colour is Workstation dependent ( set to 1)
*
      IF (KWFACI(KWKIX) .GE. KPCI(KWKIX)) KWFACI(KWKIX) = 1
      GOTO 9999



* Set pick identifier
  220 CONTINUE
      GOTO 9999



* Set polyline representation
  230 CONTINUE
      INTA(1) = 5
      CALL GKSRPL(1,INTA,.TRUE.)
      GOTO 9999



* Set polymarker representation
  240 CONTINUE
      CALL GKSRPM(0,INTA,.TRUE.)
      GOTO 9999



* Set text representation
  250 CONTINUE
      IF( KWI3.EQ.GSTRKP ) THEN

*       Stroke Precision
*       Make sure that fonts are available
        IF( KDBFLS.EQ.KFLNA ) THEN
          KERROR = -1009
          GOTO 9999
        END IF
        IF( KDBFLS.EQ.KFLCL ) CALL GKXON
        IF( KERROR.NE.0 ) GOTO 9999
        DO 255 I = 1,KFNTMX
          IPREC(I) = GSTRKP
  255   CONTINUE
        CALL GKSRTX(KFNTMX,KHFONT,IPREC,.FALSE.)
      ELSE

*       String or Char precision
*      This part of the enrty point uses STROKE precision no
*     matter what precision is specified ........ need to be changed?
        IPREC(1) = KWI3
        INTA(1) = 1
        CALL GKSRTX(1,INTA,IPREC,.FALSE.)
      END IF
      GOTO 9999



* Set fill area representation
  260 CONTINUE
      CALL GKSRFA( .TRUE. )
      GOTO 9999



* Set pattern representation
* Pattern  supported
* Pattern is defined using two independent routines
* GK5VPT & GK5VCP. This only allows a pattern size of
* 16 by 16 ( ie limits on versatec see p. 4-70 in
* Versatec manual. The utilities take up too much
* processor time.
*
  270 CONTINUE

* Check if pattern too big

      IF (KWI6 .GT. 16 .OR. KWI7 .GT. 16) THEN
         KERROR = 91
         GOTO 9999
      END IF

* Check if colour indices valid

      DO 272 J = KWI5, KWI5+KWI7-1
        DO 271 I = KWI4, KWI4+KWI6-1
          IF (IDAT((J-1)*KWI2+I).LT.0 .OR.
     :        IDAT((J-1)*KWI2+I).GE.KPCI(KWKIX)) THEN
            KERROR = 93
            GOTO 9999
          END IF
  271   CONTINUE
  272 CONTINUE

* NOTE : need to rotate pattern as the versatec doesn't rotate
* the pattern, so simply read in the pattern differently
* ie if the rotation flag is set then pattern is read
* starting from row KWI4 column KWI5
* else from row KWI4 column KWI5+KWI7-1.

      PATIDX = KWI1
      IDIMX = KWI6
      IDIMY = KWI7
      IF ( ROT ) THEN
         DO 276 J = KWI4+KWI6-1, KWI4, -1
            DO 275 I = KWI5, KWI5+KWI7-1
               PAT (I, J) = IDAT((J-1)*KWI2+I)
  275       CONTINUE
  276    CONTINUE
      ELSE
         DO 278 J = KWI5, KWI5+KWI7-1
            DO 277 I = KWI4, KWI4+KWI6-1
               PAT (I, J) = IDAT((J-1)*KWI2+I)
  277       CONTINUE
  278    CONTINUE
      END IF

* convert and store pattern in LISPAT (global) for
* future and present reference
      CALL GK5VPT
      PATDEF (PATIDX) = 1
      CALL GKSRPA (NID, IDAT)
      GOTO 9999



* Set colour representation
* At RANDOM 2.0  RGB values are set to the nearest current value
* in the colour table of the WDT file  -
* so in effect the user is limited to the 256 colours in the versatec
* colour table.
*
* At Random 2.1 Colours are settable as RGB values and 2048 colours
* available. As Metafile only has 256 colours user settable colour
* are in the range 257 - 2048, COLIDX is index to current value.
  280 CONTINUE
* See if colour index valid and not background
      IF (KWI1.GT.0 .AND. KWI1.LT.KPCI(KWKIX)) THEN
* Work out colour components as 8 bit values
         PR = INT(255.0 * QWR1 + 0.001)
         PG = INT(255.0 * QWR2 + 0.001)
         PB = INT(255.0 * QWR3 + 0.001)

* Check that colour is not foreground and White
         IF ( KWI1.EQ.1 .AND. PR+PG+PB .EQ. 765 ) THEN

* Check for primary colours
         ELSE IF ( PR.EQ.0   .AND. PG.EQ.0   .AND. PB.EQ.0 ) THEN
            COLMAP(KWI1) = PREMAP(1)
         ELSE IF ( PR.EQ.255 .AND. PG.EQ.0   .AND. PB.EQ.0 )THEN
            COLMAP(KWI1) = PREMAP(2)
         ELSE IF ( PR.EQ.0   .AND. PG.EQ.255 .AND. PB.EQ.0 ) THEN
            COLMAP(KWI1) = PREMAP(3)
         ELSE IF ( PR.EQ.0   .AND. PG.EQ.0   .AND. PB.EQ.255 ) THEN
            COLMAP(KWI1) = PREMAP(4)
         ELSE IF ( PR.EQ.255 .AND. PG.EQ.255 .AND. PB.EQ.0 )THEN
            COLMAP(KWI1) = PREMAP(5)
         ELSE IF ( PR.EQ.0   .AND. PG.EQ.255 .AND. PB.EQ.255 ) THEN
            COLMAP(KWI1) = PREMAP(6)
         ELSE IF ( PR.EQ.255 .AND. PG.EQ.0   .AND. PB.EQ.255 ) THEN
            COLMAP(KWI1) = PREMAP(7)
         ELSE IF ( PR.EQ.255 .AND. PG.EQ.255 .AND. PB.EQ.255 ) THEN
            COLMAP(KWI1) = PREMAP(8)

         ELSE IF ( COLIDX .LT. MXCOL ) THEN
* Increment colour index pointer and set colour
            COLIDX = COLIDX + 1
            CALL DEFRGB ( COLIDX, QWR1, QWR2, QWR3 )
            COLMAP (KWI1) = COLIDX
         ELSE
* Colour table exhausted find nearest colour in first 256
            MNDIFF = 3.0
            N = 1
            DO 285 I = 1, 256
               DR = ABS (QWR1 - QHP(KHPXR(KCTBPT(1,KWKIX))+I) )
               DG = ABS (QWR2 - QHP(KHPXR(KCTBPT(2,KWKIX))+I) )
               DB = ABS (QWR3 - QHP(KHPXR(KCTBPT(3,KWKIX))+I) )
               DIFFS = DR*DR + DG*DG + DB*DB
               IF(DIFFS .LT. MNDIFF) THEN
                   MNDIFF = DIFFS
                   N = COLMAP (I)
               ENDIF
  285       CONTINUE
            COLMAP(KWI1) = N
         ENDIF
*        WRITE(LOGUNIT,'(A8,I4,A5,I4,I4,I4,A5,I4)')
*    :         ' Colour:',KWI1,' RGB:',PR,PG,PB,' Map:',COLMAP(KWI1)

      ENDIF
* Ignore setting of Background
      GOTO 9999



* Normalisation transformation
  310 CONTINUE
      CALL GKWKC4
      GOTO 9999



* Set workstation window
  320 CONTINUE
      CALL GKSWKW
      GOTO 9999



* Set workstation viewport
  330 CONTINUE
      CALL GKSWKV
      GOTO 9999



* Segment entrypoints *
  410 CONTINUE
      CALL GKSGWK(IENT,.FALSE.)
      GOTO 9999



* Initialise locator
  610 CONTINUE
      GOTO 9999



* Initialise stroke
  620 CONTINUE
      GOTO 9999



* Initialise valuator
  630 CONTINUE
      GOTO 9999



* Initialise choice
  640 CONTINUE
      GOTO 9999



* Initialise pick
  650 CONTINUE
      GOTO 9999



* Initialise string
  660 CONTINUE
      GOTO 9999



* Set input operating modes
  670 CONTINUE
      GOTO 9999



* Set input mode
  680 CONTINUE
      CALL GKSIPM
      GOTO 9999



* Request locator
  690 CONTINUE
      GOTO 9999



* Request stroke
  700 CONTINUE
      GOTO 9999



* Request valuator
  710 CONTINUE
      GOTO 9999



* Request choice
  720 CONTINUE
      GOTO 9999



* Request pick
  730 CONTINUE
      GOTO 9999



* Request string
  740 CONTINUE
      GOTO 9999



* Sample locator
  750 CONTINUE
      GOTO 9999



* Sample stroke
  760 CONTINUE
      GOTO 9999



* Sample valuator
  770 CONTINUE
      GOTO 9999



* Sample choice
  780 CONTINUE
      GOTO 9999



* Sample pick
  790 CONTINUE
      GOTO 9999



* Sample string
  800 CONTINUE
      GOTO 9999



* Flush device events
  810 CONTINUE
      GOTO 9999



* Write item to GKSM
  910 CONTINUE
      KERROR = 32
      GOTO 9999



* Get item type from GKSM
  920 CONTINUE
      KERROR = 34
      GOTO 9999



* Read item from GKSM
  930 CONTINUE
      KERROR = 34
      GOTO 9999



* Inquire everything *
 1111 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire polyline representation
 1330 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
        QWR1 = 1.0
      END IF
      GOTO 9999



* Inquire text representation
C THIS WILL CHANGE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 1370 CONTINUE
      IF (KWI2.EQ.GSET) THEN
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      ELSE
        CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      END IF
      GOTO 9999

* --------------------------------------------------------------
* Inquire text extent
* --------------------------------------------------------------
 1380 CONTINUE
*     Input data :
*     NID    : length of string
*     IDAT   : integer array character codes for string
*     QWR1   : x-text position
*     QWR2   : y-text position
*     QWR3   : x-width vector
*     QWR4   : y-width vector
*     QWR5   : x-height vector
*     QWR6   : y-height vector
*     QWCHRX(KWKIX),QWCHRY(KWKIX) : baseline vector
*
*     Data returned:
*     KERROR : error indicator
*     RX(1-4): x-text extent
*     RY(1-4): y-text extent
*     QWR7   : x-concatenation point
*     QWR8   : y-concatenation point

*     stroke precision
      IF (KWTXPR(KWKIX) .EQ. GSTRKP) THEN
        CALL GKXQXO(NID,IDAT,RX,RY)
*     string and char precision
      ELSE
*       baseline vector from ws Set text attributes entry
        CALL GKXQXC (NID,IDAT,QWCHRX(KWKIX),QWCHRY(KWKIX),
     :                  RX,RY,GK5VXF)

      END IF
      GOTO 9999




* Inquire list of pattern indices
 1410 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      GOTO 9999



* Inquire pattern representation
 1420 CONTINUE
      CALL GKQWK (IENT, NID, IDAT, NRD, RX, RY, NCD, STR)
      GOTO 9999



* Inquire colour representation
 1440 CONTINUE
      CALL GKQWK(IENT, NID, IDAT, NRD, RX, RY, NCD, STR)
      GOTO 9999



* Inquire pick device state
 1510 CONTINUE
      GOTO 9999



* Inquire workstation classification
 1710 CONTINUE
      KWI1 = GVECTR
      GOTO 9999



* Inquire text facilities ... on entry KWI1 specifies list element reque
* Allow for string and char precision font (number 1) explicitly
 1790 CONTINUE
      IF( KWI1.GT.KFNTMX+2 ) THEN
        KERROR = 2002
        KWI1 = KFNTMX+2
        GOTO 9999
      END IF
      IF( KWI1.GT.KFNTMX ) THEN

*       String or Char precision font
          IF( KWI1.EQ.KFNTMX+1 ) KWI3 = GSTRP
          IF( KWI1.EQ.KFNTMX+2 ) KWI3 = GCHARP
          KWI2 = GSTRKP
      ELSE

*       Stroke precision font
*       Make sure that fonts are available
          IF( KDBFLS.EQ.KFLNA ) THEN
            KERROR = -1009
            GOTO 9999
          END IF
          IF( KDBFLS.EQ.KFLCL ) CALL GKXON
          IF( KERROR.NE.0 ) GOTO 9999
          KWI2 = KHFONT(KWI1)
          KWI3 = 2
      END IF
      KWI1 = KFNTMX+2
      IF (KWKIX.NE.KNIL) THEN
        KWI4 = KCHH(KWKIX)
        KWI5 = KCHXPF(KWKIX)
        KWI6 = KPTXI(KWKIX)
        QWR1 = QMNCHH(KWKIX)
        QWR2 = QMXCHH(KWKIX)
        QWR3 = QMNCHX(KWKIX)
        QWR4 = QMXCHX(KWKIX)
      ELSE
        CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
        IF (KERROR.EQ.0) THEN
          KWI4 = INTA(5)
          KWI5 = INTA(6)
          KWI6 = INTA(10)
          QWR1 = REALA(9)
          QWR2 = REALA(10)
          QWR3 = REALA(11)
          QWR4 = REALA(12)
        END IF
      END IF
      GOTO 9999



* ---------------------------------------------------------------
* Inquire Fill Area facilities
* ---------------------------------------------------------------
 1810 CONTINUE
*     Data sent:
*     KWI1   : list element of interior styles requested
*     KWI2   : list element of hatch styles requested
*     Data returned:
*     KERROR : error indicator
*     KWI1   : number of available fill area interior styles
*     KWI2   : Nth element of list of available fill area interior styles
*     KWI3   : number of available hatch styles
*     KWI4   : Nth element of list of available hatch styles
*     KWI5   : number of predefined fill area indices

      IF ((KWI1.LE.0 .OR. KWI1.GT.4) .OR.
     : (KWI1 .EQ. 4 .AND. (KWI2 .LT. -10 .OR. KWI2 .GT. -1)))
     :THEN
        KERROR = 2002
      ELSE
        KWI3 = 10
        KWI2 = KWI1 - 1
        KWI4 = KWI2
        IF (KWKIX.NE.KNIL) THEN
          KWI5 = KPFAI(KWKIX)
        ELSE
          CALL GKQWDT(KWKTYP,KWDT,KNONE,19,12,INTA,REALA)
          IF (KERROR.EQ.0) THEN
            KWI5 = INTA(11)
          END IF
        END IF
        KWI1 = 4
      END IF
      GOTO 9999




* Inquire predefined pattern representation
 1840 CONTINUE
      CALL GKQWK (IENT, NID, IDAT, NRD, RX, RY, NCD, STR)
      GOTO 9999



* Inquire colour facilities
 1850 CONTINUE
      KWI2 = GCOLOR
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI1 = 257
      GOTO 9999



* Inquire default choice device data
 1960 CONTINUE
      CALL GKQWK(IENT,NID,IDAT,NRD,RX,RY,NCD,STR)
      KWI4 = 9
      GOTO 9999



* Inquire default pick device data
 1970 CONTINUE
      GOTO 9999



*   Here after all output primitives to sort out buffering
 8888 CONTINUE
      KDSMT(KWKIX) = GNEMPT
      IF (KWIO(KWKIX).EQ.GYES) CALL GKIOBO(KIOSN,1,KDAT,NLEFT)

 9999 CONTINUE

      END
         SUBROUTINE GK5VIO
         INCLUDE (CHECKIBM)

*
* Type of Routine: W/S
* Author:          GMC
*

*
* PURPOSE OF ROUTINE
*
*
* To open and initialise a file to the versatec 9242 colour printer
*

*
* Alogrithm
*
* Call VPOPT to initialise colour output
* Call VPOPT to set up measure of units/inch eg millimetres
* Call ROTATE to turn the plotted output through 90 degrees.
* Call PLOTS to initialise plotting
* Set up the values for multiple plots on the output area eg increments
* Read in the viewport dimensions from disk. Previously set up by
*    EXECIO in CMS from information in the GKS metafile tag.
* Set up the number of frames along X and along Y
* Call WINDOW to set up window from NDC 'world' coordinates which have
*    already been transformed to device coords.
* Call VPORT to set up viewport on the output medium (eg paper or foil)
*

* XINC     - The X increment between output frames
* YINC     - The Y increment between output frames
* IFRAME   - The current frame
* IFROW    - The current row along X
* IFCOL    - The current column along Y
* XWIDTH   - The width along X of the output frame
* YDEPTH   - The depth along Y of the output frame
* MFWID    - The maximum number of frames along the X width
* MFDEP    - The maximum number of frames along the Y depth.
* XMAX     - The maximum X width (1015 mms)
* YMAX     - The maximum Y depth (4060 mms ie 4x as much)
* RANGE    - This acts as the transformed NDC range (ie Device Coords)
*            for input to the window routine in Versaplot.
* XBORDR   -  X border for Header info
* YBORDR   -  Y border for Header info
* MNCOL    -  Minimum colour index for user
*

         INCLUDE (CMGKVHC)
         INCLUDE (PRGKDT)
         INCLUDE (PRGKWDT)
         INCLUDE (CMGKWDT)
         INCLUDE (CMGKWSL)

*
* LOCALS
*
* IWIDTH    - Frame width from disk file
* IDEPTH    - Frame depth from disk file
* IN9242    - The input stream to the disk file
* LOGUNIT   - Unit for Diagnostic messages
* MONUNIT   - Unit for Information messages
* IERROR    - The error value
* FORMS     - The forms code
* FORID     - The user identifier
* FORDST    - The user distribution
* FILNAM    - The filename
* FILTYP    - The filetype
* BLNK3     - Blank variable
* BLNK8     - Blank variable
* BLNK4     - Blank variable
* ORIENT    - Orientation of output, Portrait or Landscape.
* PORT      - Portrait (default)
* LAND      - Landscape
* SAVE      - Save variable
* USER      - The output user string
* USER60    - The output string as character*1
* IUSE15    - The output string as an integer array
* REALA     - stores parameters for VPOPT
* VA0       - The A0 forms code
* VA1       - The A1 forms code
* VA2       - The A2 forms code
* VA3       - The A3 forms code
* VA4       - The A4 forms code
* V         - The user definable generic forms code
* MAXV      - Maximum V forms code size
* DATE      - Date plot was created
* TIME      - Time plot was created
* ANGLE     - Angle of writing out Header info
*
      INTEGER    MNCOL
      PARAMETER (MNCOL = 257)
      INTEGER IERROR, IN9242, IWIDTH, IDEPTH, LOGUNIT, MONUNIT
      PARAMETER (IN9242 = 9, LOGUNIT = 6, MONUNIT = 6)
      INTEGER MAXV, MAJOR, MINOR, NINTA, NREALA
      INTEGER INTA(19)
      REAL REALA (12)
      CHARACTER*3 FORMS,BLNK3,VA0,VA1,VA2,VA3,VA4,V
      CHARACTER*3 VA5, VB0, VB1, VSQ, VLP, VLS
      CHARACTER*8 FORM8, BLNK8, FORID, FORDST, FILNAM, FILTYP
      CHARACTER*8 DATE, TIME
      CHARACTER*10 SCREAT
      CHARACTER*7 SDIST,SFILE,SFORM
      CHARACTER*6 SUSER
      CHARACTER*60 USER
      CHARACTER*1 USER60(60)
      CHARACTER*1 ORIENT,PORT,LAND
      REAL SAVE, ANGLE, A0L, A0W, B0L, B0W
      INTEGER IUSE15(15)
      REAL XSYM,YSYM
      CHARACTER*30 DOTS
      CHARACTER*1 BLNK1
      EQUIVALENCE ( USER60(1),USER )
      EQUIVALENCE ( USER60(1),IUSE15(1) )
      DATA SUSER/'USER: '/
      DATA SDIST/' DIST: '/
      DATA SFILE/' FILE: '/
      DATA SFORM/' FORM: '/
      DATA SCREAT/' CREATED: '/
      DATA DOTS/'..............................'/
      DATA BLNK1/' '/
      DATA BLNK3/'   '/,BLNK8/'        '/
      DATA PORT/'P'/, LAND/'L'/
      DATA VA0/'VA0'/, VA1/'VA1'/, VA2/'VA2'/, VA3/'VA3'/,
     :     VA4/'VA4'/, VA5/'VA5'/, VB0/'VB0'/, VB1/'VB1'/,
     :     VSQ/'VSQ'/, VLP/'VLP'/, VLS/'VLS'/, V/'V  '/
      DATA A0L, A0W / 1189.20, 840.89 /
      DATA B0L, B0W / 1414.21, 1000.00 /

         IWIDTH = 0
         IDEPTH = 0
         FORM8 = BLNK8
         ORIENT = PORT
         FORID = BLNK8
         FORDST = BLNK8
         FILNAM = BLNK8
         FILTYP = BLNK8
         DATE = BLNK8
         TIME = BLNK8

      IF (FSTPAS) THEN
         INQUIRE( UNIT = IN9242,IOSTAT=IERROR )
         IF( IERROR.EQ.0 ) THEN
             READ( IN9242,*,END = 1500,ERR=1500 ) IWIDTH, IDEPTH,
     :              FORM8, FORID, FORDST, FILNAM, FILTYP, DATE, TIME

* Ignore swathe
             READ( IN9242,*,END = 1500,ERR=1500 ) IERROR

* Write Diagnostic Output to Listing file

             WRITE(MONUNIT,*)
             WRITE(MONUNIT,*) ' File: ', FILNAM, FILTYP,
     :                   ' at ', DATE,' ', TIME
             WRITE(MONUNIT,*) ' User: ', FORID, ' Dist: ', FORDST,
     :                   ' Form:', FORM8
             WRITE(MONUNIT,*)
             PRVFRM = FORM8(1:4)
             BOXED = ( FORM8(5:5) .NE. 'N')
         END IF
      ELSE
         IWIDTH = PREWTH
         IDEPTH = PREDEP
         FORM8 = PRVFRM
      END IF
 1500 CONTINUE

*
* Set up frame width and depth, if problems set to default of 297
*
         FORMS = FORM8(1:3)
         MAXV = 1015
         IF( IWIDTH.LE.0 .OR. IWIDTH.GT.MAXV ) IWIDTH = 297
         IF( IDEPTH.LE.0 .OR. IDEPTH.GT.MAXV ) IDEPTH = 297
*
* Split off the last character of FORM8. If LANDSCAPE, then
* change XWIDTH and YDEPTH.
*
         ORIENT = FORM8(4:4)
         ROT = .FALSE.

         XINC = 15.0
         YINC = 15.0

         IF (FORMS .EQ. V) THEN
            XWIDTH = FLOAT (IWIDTH)
            YDEPTH = FLOAT (IDEPTH)
         ELSE IF( FORMS.EQ.VA0 ) THEN
            XWIDTH = A0W
            YDEPTH = A0L
            ROT = ( ORIENT.NE.LAND )
         ELSE IF( FORMS.EQ.VA1 ) THEN
            XWIDTH = A0L / 2.0
            YDEPTH = A0W
            ROT = ( ORIENT.EQ.LAND )
         ELSE IF( FORMS.EQ.VA2 ) THEN
            XWIDTH = A0W / 2.0
            YDEPTH = A0L / 2.0
            ROT = ( ORIENT.NE.LAND )
         ELSE IF( FORMS.EQ.VA3 ) THEN
            XWIDTH = A0L / 4.0
            YDEPTH = A0W / 2.0
         ELSE IF( FORMS.EQ.VA4 ) THEN
            XWIDTH = A0W / 4.0
            YDEPTH = A0L / 4.0
         ELSE IF (FORMS .EQ. VA5) THEN
            XWIDTH = A0L / 8.0
            YDEPTH = A0W / 4.0
         ELSE IF (FORMS .EQ. VB0) THEN
            XWIDTH = B0W
            YDEPTH = B0L
            ROT = (ORIENT .NE. LAND)
         ELSE IF (FORMS .EQ. VB1) THEN
            XWIDTH = B0L / 2.0
            YDEPTH = B0W
            ROT = (ORIENT .EQ. LAND)
         ELSE IF (FORMS .EQ. VLS) THEN
            XWIDTH = 1015.0
            YDEPTH = 1015.0
            YINC = 0.0
         ELSE IF (FORMS .EQ. VSQ) THEN
            XWIDTH = 259.0
            YDEPTH = 259.0
         ELSE IF (FORMS .EQ. VLP) THEN
            XWIDTH = 1015.0
            YDEPTH = 4060.0
            ROT = (ORIENT .NE. LAND)
         ELSE
* Shouldn't get here ... just in case set to default 297*297mms.
            XWIDTH = 297.0
            YDEPTH = 297.0
         END IF

         IF( ORIENT.EQ.LAND ) THEN
             SAVE = XWIDTH
             XWIDTH = YDEPTH
             YDEPTH = SAVE
         END IF

*
* Check ROT, and change X/YMAX
*
         IF( ROT ) THEN
             XMAX = 1015.87
             YMAX = 4063.48
             XBORDR = 0.0
             YBORDR = 75.0
             ANGLE = 0.0
             IFROW = 0
             IFCOL = 1
         ELSE
             XMAX = 4063.48
             YMAX = 1015.87
             XBORDR = 75.0
             YBORDR = 0.0
             ANGLE = 90.0
             IFROW = 1
             IFCOL = 0
         END IF

*  If Largest plot then set borders to zero, so Veiwport is OK.

         IF ( FORMS .EQ. 'VLP' ) THEN
             XBORDR = 0.0
             YBORDR = 0.0
             XINC = 0.0
             YINC = 0.0
         ENDIF

*
* The frame counter is set to 1. The first plot is on the first
* row and in column 1
*
         IFRAME = 0
*
* Set the RANGE to 10000 units
* Use 10,000 so that CELL ARRAY on an Nx1 basis matches up at boundaries
*

         RANGE = 10000.0

*
* Set up number of frames along X and Y
*
         MFWID = (XMAX-XBORDR)/( XWIDTH+XINC )
         MFDEP = (YMAX-YBORDR)/( YDEPTH+YINC )

* Write Diagnostic Output to Listing file

         WRITE(MONUNIT,*) ' Paper size: ', XMAX, YMAX, ' Rotate: ', ROT
         WRITE(MONUNIT,*) ' Picture size: ', XWIDTH, YDEPTH
         WRITE(MONUNIT,*) ' No of Pictures: ', MFWID,' by ', MFDEP
         WRITE(MONUNIT,*)

* Set space after monochrome plots using VPOPT as sometimes
* the begining of a colour plot will overwrite a previous monochrome
* plot at present (03/87).

* Set RPM controller Memory size
         CALL VPOPT ( 34, 0, 11000000.0, IERROR)

* Basic plotting units = mm
         CALL VPOPT ( 8, 0, 25.4, IERROR )

* Set space = 300mm after each copy (18/7/88)
         CALL VPOPT ( 23, 300, 0.0, IERROR)

* Set space = 300mm after each plot (18/7/88)
         CALL VPOPT ( 24, 300, 0.0, IERROR)

* Initialize Colour mode with default passes
         CALL VPOPT ( 101, 0, 0.0, IERROR )

* Invoke VRF Output flag
         CALL VPOPT ( 35, 1, 0.0, IERROR)

* Set rotation
         IF ( ROT ) CALL VPOPT ( 5, -1, 0.0, IERROR)

* Set Default Paper size
         REALA(1) = 0.0
         REALA(2) = XMAX
         REALA(3) = 0.0
         REALA(4) = YMAX
         CALL VPOPT ( 2, 0, REALA, IERROR)

* Initialise plotting
         CALL PLOTS ( 0, 0, 0 )

      IF (FSTPAS) THEN
*
* Set up the Header information, read in from JOB9242 DATA file
*
         IF( ROT ) THEN
             XSYM = 400.0
             YSYM = YMAX-30.0
         ELSE
             XSYM = 30.0
             YSYM = 0.0
         ENDIF
         USER = SUSER//FORID//SDIST//FORDST
         USER = USER(1:29)
         CALL SYMBOL( XSYM,YSYM,20.0,IUSE15,ANGLE,29 )
         USER = SFILE//FILNAM//BLNK1//FILTYP//SFORM//FORM8
         USER = USER(1:44)
         IF( ROT ) THEN
             CALL SYMBOL( XSYM,(YSYM-15.0),10.0,IUSE15,ANGLE,44 )
         ELSE
             CALL SYMBOL( (XSYM+15.0),YSYM,10.0,IUSE15,ANGLE,44 )
         ENDIF
         USER = SCREAT//TIME//BLNK1//DATE
         USER = USER(1:27)
         IF( ROT ) THEN
             CALL SYMBOL( XSYM,(YSYM-30.0),10.0,IUSE15,ANGLE,27 )
             CALL SYMBOL( 0.0,YSYM,8.0,'PLOTS START HERE ORDERED',
     :                    ANGLE,24 )
             CALL SYMBOL( 0.0,(YSYM-12.0),8.0,'ACROSS WIDTH IN ROWS',
     :                    ANGLE,20 )
         ELSE
             CALL SYMBOL( (XSYM+30.0),YSYM,10.0,IUSE15,ANGLE,27 )
             CALL SYMBOL( XSYM,790.0,8.0,'PLOTS START HERE ORDERED',
     :                    ANGLE,24 )
             CALL SYMBOL( (XSYM+11.0),790.0,8.0,
     :                    'ACROSS WIDTH IN COLUMNS',ANGLE,23 )
         END IF
         FSTPAS = .FALSE.

      END IF
*
* Set up WINDOW depending on forms code orientation
*
         IF( ORIENT.EQ.LAND ) THEN
             CALL WINDOW( 0.0,RANGE,0.0,(RANGE*YDEPTH/XWIDTH) )

*
* Set up correct WDT device and raster values
*
             QDSDX( 1 ) = RANGE
             QDSDY( 1 ) = RANGE*YDEPTH/XWIDTH
         ELSE
             CALL WINDOW( 0.0,(RANGE*XWIDTH/YDEPTH),0.0,RANGE )
*
* Set up correct WDT device and raster values
*
             QDSDX( 1 ) = RANGE*XWIDTH/YDEPTH
             QDSDY( 1 ) = RANGE
         END IF
         KDSRX( 1 ) = INT( QDSDX( 1 ) )
         KDSRY( 1 ) = INT( QDSDY( 1 ) )

* Set Gamma correction Factor = Removed
*        ANGLE = 3.5
*        CALL SGAMMA (ANGLE)
*
*  Set User colour index to fixed table + 1
      COLIDX = MNCOL
         QRWVXL(1) = 0.0
         QRWVXR(1) = QDSDX(1)-1.0
         QRWVYB(1) = 0.0
         QRWVYT(1) = QDSDY(1)-1.0
         QCWVXL(1) = 0.0
         QCWVXR(1) = QDSDX(1)-1.0
         QCWVYB(1) = 0.0
         QCWVYT(1) = QDSDY(1)-1.0
*
         RETURN
         END
        SUBROUTINE GK5VCS

*
* Type of Routine: W/S
* Author:          GMC
*


*
* PURPOSE OF ROUTINE
*
* To close down the plot file for Versatec
* If monochrome output an extra plot to stop smudges
*
*

*   VRFCOM needed to Access colour passes VRFKNT(4)
C
C...  COMMON /VRFCOM/ - VRF COMMON VARIABLES
C
*
         INCLUDE (VRFCOM)
         INCLUDE (CMGKVHC)
*
      INTEGER MONUNIT
      PARAMETER (MONUNIT = 6)
*
* Algorithm
*
* Call PLOT with last argument 999
*
*
*
* LOCALS
*
*   NCPASS  -  No of colour passes
*   I       -  Loop counter
*   REALA   -  Array of 4 reals for VPOPT
*

      INTEGER IERROR, NCPASS, I
      REAL REALA(4)

*  Work out number of colour passes

      NCPASS = 0
      DO 100 I = 1, 4
         IF ( VRFKNT(I) .GT. 0.0 ) NCPASS = NCPASS + 1
 100  CONTINUE

*  If Monocrome then print extra space

      IF (NCPASS .EQ. 1) THEN
         WRITE(MONUNIT,*) ' Monochrome plot'

*   Close plot and generate a new one with default values

         CALL PLOT (0.0, 0.0, 999)
         REALA(1) = 0.0
         REALA(2) = 100.0
         REALA(3) = 0.0
         REALA(4) = YMAX
*   Set no rotation
         CALL VPOPT (5, 0, 0.0, IERROR)
*   Set paper size
         CALL VPOPT (2, 0, REALA, IERROR)
         CALL PLOTS (0, 0, 0)

*   Draw a line across plot

         CALL PLOT ( 0.0, 0.0, 3)
         CALL PLOT ( 0.0, YMAX, 2)

      ENDIF

      CALL PLOT (0.0, 0.0, 999)

      RETURN
      END
         SUBROUTINE GK5VCL
         INCLUDE (CHECKIBM)

*
* Type of Routine: W/S
* Author:          GMC
*

*
* PURPOSE OF ROUTINE
*
* To clear the frame, ie do a frame advance
*

*
* Algorithm
*
* Check to see if frame can be plotted, if not close down
*

         INCLUDE (CMGKVHC)

*
* LOCALS
*
* XSTART,XEND; YSTART,YEND - Define Viewport in terms of plotting
*                            rows and columns
*
         INTEGER MONUNIT
         PARAMETER (MONUNIT = 6)

         REAL XSTART,XEND,YSTART,YEND
         REAL XRANGE,YRANGE

         IFRAME = IFRAME+1
* Check for Maximum number of Frames
         IF( IFRAME.GE.(MFWID*MFDEP) ) THEN
            IFRAME = 1
            PREWTH = XWIDTH
            PREDEP = YDEPTH
            CALL PLOT (0.0, 0.0, 999)
            CALL GK5VIO
         END IF

         IF( ROT ) THEN
             IFROW = IFROW+1
             IF( IFROW.GT.MFWID ) THEN
                 IFCOL = IFCOL+1
                 IFROW = 1
             END IF
         ELSE
             IFCOL = IFCOL + 1
             IF( IFCOL.GT.MFDEP ) THEN
                 IFROW = IFROW + 1
                 IFCOL = 1
             END IF
         END IF

         XSTART = (XWIDTH+XINC)*(IFROW-1) + XBORDR
         XEND = XSTART+XWIDTH
         YSTART = YMAX-(YDEPTH+YINC)*IFCOL - YBORDR
         YEND = YSTART+YDEPTH
         CALL VPORT( XSTART,XEND,YSTART,YEND )

* Set up box around plot if needed
*
*
* DO IT ON RANGE
*
         IF (BOXED) THEN
            IF( XWIDTH.GT.YDEPTH ) THEN
                XRANGE = RANGE
                YRANGE = RANGE*YDEPTH/XWIDTH
            ELSE IF( XWIDTH.LT.YDEPTH ) THEN
                XRANGE = RANGE*XWIDTH/YDEPTH
                YRANGE = RANGE
            ELSE
                XRANGE = RANGE
                YRANGE = RANGE
            END IF

            CALL PENCLR (1)
            CALL NEWPEN (1, 1)
C
C  Draw Rectangular Box round plot
C
            CALL PLOT( 0.0, 0.0, 3 )
            CALL PLOT( XRANGE, 0.0, 2 )
            CALL PLOT( XRANGE, YRANGE, 2 )
            CALL PLOT( 0.0, YRANGE, 2 )
            CALL PLOT( 0.0, 0.0, 2 )
         END IF
         RETURN
         END
      SUBROUTINE GK5VCA(NXDIM,NYDIM,INDX,INDY,NXPIX,NYPIX,ICOLAR)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             GMC
*
      INCLUDE (CHECKIBM)
      INCLUDE (PRGKDT)
      INCLUDE (CMGKWCA)
      INCLUDE (CMGKVHC)
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Outputs a NXPIX x NYPIX array.
*   Each cell is considered to be a block bounded by vectors.
*   This block is output as a VERSAPLOT TONE pattern.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP NXPIX  - No of pixels per scanline
*     INP NYPIX  - No of scanlines in raster
*     INP NXDIM  - First dimension of colour array
*     INP NYDIM  - Second dimension of colour array
*     INP INDX   - First element of first dimension
*     INP INDY   - First element of Second dimension
*     INP ICOLAR - Integer colour array to output
*
      INTEGER NXPIX, NYPIX, NXDIM, NYDIM, INDX, INDY,
     :        ICOLAR(NXDIM,NYDIM)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*
*  LOCALS
*  ------
*
* IPATT - The patterm number for VERSAPLOT TONE
* XSIZ, YSIZ - The size of each block
* NCOLS - The number of Colour Indices supported
* XX, YY - The vectors defining the block to VERSAPLOT TONE
* XP,YP - The NDC values of cell array boundary
*  DXP, DYP - The DC values of XP,YP
*  IXPIX, IYPIX - The number of pixels per CA element
*  XS, YS   - Start point of line
*  XSJ, YSJ   - Start point of for each box
*  DX, DY   - Increment in X and Y if rotated

         INTEGER IXPIX,IYPIX,NCOLS,IPATT
         REAL XSIZ, YSIZ
         INTEGER IROW,ICLM
         REAL XX(4),YY(4)
         REAL XP(5),YP(5),DXP(5),DYP(5)
         REAL XS,YS,DX,DY, XSJ, YSJ
         PARAMETER (NCOLS = 256)
*
*---------------------------------------------------------------------

* Transform X/YP to DX/YP
*
* Top left is point P = (QWR1,QWR2)
* Bottom right is point Q   = (QWR3,QWR4)
* Top right is point R      = (QWR5,QWR6)
*
* Thus, define each individual box with R as top right point,
* and the Y coordinates of the bottom of the box as R(Y) - ANYBOX
*

      XP(1) = QWR3 - (QWR5 - QWR1)
      YP(1) = QWR4 - (QWR6 - QWR2)
      XP(2) = QWR1
      YP(2) = QWR2
      XP(3) = QWR5
      YP(3) = QWR6
      XP(4) = QWR3
      YP(4) = QWR4
      XP(5) = XP(1)
      YP(5) = XP(1)

*
* Transform points, but leave clipping to VERSAPLOT TONE
*
      CALL GKTWD(5,XP,YP,DXP,DYP)

* Set up ANXBOX and ANYBOX, the block X and Y sizes

      XSIZ = ( DXP(3)-DXP(2) )/NXPIX
      YSIZ = ( DYP(2)-DYP(1) )/NYPIX
      DX = ( DXP(4)-DXP(3) ) / NXPIX
      DY = ( DYP(3)-DYP(2) ) / NYPIX
* Output each row of raster

      XS = DXP(2)
      YS = DYP(2)
      DO 200 IROW = 1,NYPIX

            XSJ = XS + IROW*DX
            YSJ = YS - IROW*YSIZ
* Output each pixel in the row

             DO 100 ICLM = 1,NXPIX
                   XX(4) = XSJ + ICLM*XSIZ
                   YY(4) = YSJ + ICLM*DY
                   XX(1) = XX(4) - XSIZ
                   YY(1) = YY(4) - DY
                   XX(2) = XX(1) - DX
                   YY(2) = YY(1) + YSIZ
                   XX(3) = XX(2) + XSIZ
                   YY(3) = YY(2) + DY

                   IPATT = COLMAP(MOD(ICOLAR(INDX+ICLM-1,INDY+IROW-1),
     :                                 NCOLS ))

                   CALL TONFLG( -1 )
                   CALL TONCLR( IPATT )
                   CALL CONVEX( XX,YY,4 )
  100   CONTINUE

  200 CONTINUE

      END
      SUBROUTINE GK5VRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             GMC
*
      INCLUDE (CHECKIBM)
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Outputs a NXPIX x NYPIX array.
* Each scanline consists of a set of 'grey' level blocks.
* Each block consists of an NXBOX by NYBOX set of pixels.
* This block is output as a VERSAPLOT TONE pattern.
* A pixel in the scanline is selected as a source of a block if
* its modulus wrt NXBOX and NYBOX is 0. Non zero pixel positions are
* skipped. Therefore each block is really an average of NXBOX by NYBOX
* pixels.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP X,Y    - coordinates of raster origin
*     INP NXPIX  - No of pixels per scanline
*     INP NYPIX  - No of scanlines in raster
*     INP NXDIM  - First dimension of colour array
*     INP ICOLAR - Integer colour array to output
*
      REAL    X, Y
      INTEGER NXPIX, NYPIX, NXDIM, ICOLAR(NXDIM,NYPIX)
*
*  COMMON BLOCK USAGE
*  ------------------
*
         INCLUDE(CMGKVHC)
*
*  LOCALS
*  ------
*
* IX,IY  - Integer values of X and Y
* IXOK, IYOK - Integers to hold the X and Y moduli
* IPATT - The patterm number for VERSAPLOT TONE
* NXBOX, NYBOX - The size of each block
* NCOLS - The number of 'grey' levels
* XX, YY - The vectors defining the block to VERSAPLOT TONE
*
*

         INTEGER IX,IY,IXOK,IYOK,NXBOX,NYBOX,NCOLS,IPATT
         INTEGER IROW,ICLM
         REAL XX(4),YY(4)
         PARAMETER (NCOLS = 256,NXBOX=10,NYBOX=10)
*
*---------------------------------------------------------------------

* Set up IX,IY, XX, YY



* Output each row of raster

      DO 200 IROW = 1,NYPIX

* Find if this row is to be output

         IYOK = MOD( IY,NYBOX )
         IF( IYOK.EQ.0 ) THEN

* Output each pixel in the row

             DO 100 ICLM = 1,NXPIX
                 IX = INT(X)
                 IY = INT(Y)
                 XX(1) = X+FLOAT(NXBOX)
                 YY(1) = Y
                 XX(2) = XX(1)
                 YY(2) = Y+FLOAT(NYBOX)
                 XX(3) = X
                 YY(3) = YY(2)
                 XX(4) = X
                 YY(4) = Y

* Find out if this column is to be source of block

              IXOK = MOD( IX,NXBOX )
              IF( IXOK.EQ.0 ) THEN

                   IPATT = COLMAP(MOD( ICOLAR(ICLM,IROW),NCOLS ))

* Set up 'white' level, VERSAPLOT TONE pattern 9

                   CALL TONFLG( -1 )
                   CALL TONCLR( IPATT )
                   CALL CONVEX( XX,YY,4 )
              END IF
              X = X + 10
  100         CONTINUE

         END IF

         Y = Y - 10
  200 CONTINUE

      END
      SUBROUTINE GK5VFL (N, X, Y)



* Purpose of routine
* ------------------
* To produce solid lines of different colours using the
* Versatec TONE routine as this allows 256 colours to be
* used , whilst PLOT only allows eight colours.
*

      INCLUDE (CHECKIBM)
      INCLUDE (PRGKS)
      INCLUDE (PRGKDT)
      INCLUDE (CMGKWCA)
      INCLUDE (CMGKWKD)
      INCLUDE (CMGKVHC)

*
* LOCALS
* ------
*
      INTEGER LCOLR, N
      INTEGER LCOL(9), I, TESTX1, TESTX2, TESTY1, TESTY2
      REAL PX(4), PY(4), X(N), Y(N), WIDTH
      REAL ANGLE, DISTX, DISTY, HWIDTH

*  Old Version of Random only
*        need to map linewidth if it is > 2.0 as linewidths
*        in the range 3.0 -> 7.0 don't correspond to linewidths
*        3.0 -> 7.0 in the routine GK5VLN (which uses the standard
*        Versatec linewidth mechanism - so the linewidth is adjusted
*        accordingly ( ie by adding 4).
      IF (ENTRYP .EQ. 12)  THEN
         LCOLR = COLMAP(KWPLCI (KWKIX))
         WIDTH = QWLNWD (KWKIX)
*        Deleted with Random 2.1
*        IF (INT (WIDTH * 10) .GT. 20 ) WIDTH = WIDTH + 4.0
* Note : min linewidth = 1.0 and max = 200.0 ( can be easily altered).
         IF (INT (WIDTH) .LT. 1) WIDTH = 1.0
         IF (INT (WIDTH) .GT. 200) WIDTH = 200.0
      ELSE IF (ENTRYP .EQ. 13) THEN
         LCOLR = COLMAP(KWPMCI (KWKIX))

         WIDTH = 1.0
      ELSE IF (ENTRYP .EQ. 14) THEN
         LCOLR = COLMAP(KWTXCI (KWKIX))
*  Change Default width to 2.0 for Font 1  & 1.0 otherwise
         WIDTH = 1.0
         IF (KWTXFN(KWKIX) .EQ. 1) WIDTH = 2.0
      ELSE IF (ENTRYP .EQ. 15 .OR. ENTRYP .EQ. 17) THEN
         LCOLR = COLMAP(KWFACI (KWKIX))
         WIDTH = 1.0
      ELSE
         LCOLR = 1
         WIDTH = 1.0
      END IF

*  If White then Don't draw
      IF (LCOLR .EQ. 9) WIDTH = 0.0

      HWIDTH = WIDTH / 2
      DO 10 I = 1, N - 1
         TESTX1 = INT ( X (I + 1) * 10000)
         TESTX2 = INT ( X (I) * 10000)
         TESTY1 = INT ( Y (I + 1) * 10000)
         TESTY2 = INT ( Y (I) * 10000)
* Test for vertical line as it is a special case ..
* because of limitations of Versatec the minimum width
* for a vertical line ( ie a line drawn using TONE)
* is 3.0 , so reset width accordingly.
* NOTE : need to check Y if picture is rotated (this
* is indicated by ROT flag set in GK5VIO).
* Note : a vertical line (ie fill area in this case) in respect
* to the physical plotter is a line perpendicular to the
* registration marks produced on a colour plot.
      IF ( ROT )THEN
         IF (TESTX1 .EQ. TESTX2) THEN
            ANGLE = ATAN ((Y(I + 1) - Y(I)) / 1.0)
            DISTX = SIN (ANGLE) * HWIDTH
            DISTY = COS (ANGLE) * HWIDTH
         ELSE IF (TESTY1 .EQ. TESTY2) THEN
            IF (INT (HWIDTH * 10) .LT. 15) HWIDTH = 1.5
            DISTX = 0.0
            DISTY = HWIDTH
         ELSE
            ANGLE = ATAN ((Y(I + 1) - Y(I)) / (X(I + 1)-X(I)))
            DISTX = SIN (ANGLE) * HWIDTH
            DISTY = COS (ANGLE) * HWIDTH
         END IF
      ELSE
         IF (TESTX1 .EQ. TESTX2) THEN
             IF (INT (HWIDTH * 10) .LT. 15) HWIDTH = 1.5
             DISTX = HWIDTH
             DISTY = 0.0
         ELSE
            ANGLE = ATAN ((Y(I + 1) - Y(I)) / (X(I + 1) - X(I)))
            DISTX = SIN (ANGLE) * HWIDTH
            DISTY = COS (ANGLE) * HWIDTH
         END IF
      END IF
         PX(1) = X(I) + DISTX
         PY(1) = Y(I)  - DISTY
         PX(2) = X(I + 1) + DISTX
         PY(2) = Y(I + 1) - DISTY
         PX(3) = X(I + 1) - DISTX
         PY(3) = Y(I + 1) + DISTY
         PX(4) = X(I) - DISTX
         PY(4) = Y(I) + DISTY
         CALL TONFLG (-1)
         CALL TONCLR (LCOLR)
         CALL CONVEX (PX, PY, 4)
  10  CONTINUE
      RETURN
      END


         SUBROUTINE GK5VLN( N,X,Y )
          INCLUDE (CHECKIBM)
          INCLUDE (PRGKS)
          INCLUDE (PRGKDT)
          INCLUDE (CMGKWCA)
          INCLUDE (CMGKWKD)
          INCLUDE (CMGKVHC)
*
* Purpose of Routine
* ------------------
*
*
* To output a polyline using PENCLR and PLOT
* this routine is called if the colour index specified is
* < =  8 or the linewidth is <= 31.0
*

*
* Arguments
*
* INP N - number of points
* INP X,Y - coordinates
*
         INTEGER N
         REAL X(N),Y(N)

*
* LOCALS
*
* WIDTH  - The line width scale factor
* MINWID - The minimum width dcale factor
* MAXWID - The maximum width scale factor
* LCOLR  - The line colour index
* I      - loop counter
*
         INTEGER LCOLR
         INTEGER I
         REAL WIDTH,MINWID,MAXWID
         DATA MINWID/1.0/,MAXWID/31.0/
*
*
*
      IF (ENTRYP .EQ. 12 ) THEN
         LCOLR = COLMAP(KWPLCI (KWKIX))
         WIDTH = QWLNWD (KWKIX)
*
* Set min or max line width
*
         IF (WIDTH .LT. MINWID) WIDTH = MINWID
         IF (WIDTH .GT. MAXWID) WIDTH = MAXWID
      ELSE IF (ENTRYP .EQ. 13) THEN
         LCOLR = COLMAP(KWPMCI (KWKIX))
         WIDTH = 1.0
      ELSE IF (ENTRYP .EQ. 14) THEN
         LCOLR = COLMAP(KWTXCI (KWKIX))
*  Change Default width to 2.0 for Font 1  & 1.0 otherwise
         WIDTH = 1.0
         IF (KWTXFN(KWKIX) .EQ. 1) WIDTH = 2.0
      ELSE IF (ENTRYP .EQ. 15 .OR. ENTRYP .EQ. 17) THEN
         LCOLR = COLMAP(KWFACI (KWKIX))
         WIDTH = 1.0
      ELSE
         LCOLR = 1
         WIDTH = 1.0
      END IF
*
* If WHITE set width to zero
*
         IF (LCOLR .EQ. 9) WIDTH = 0.0

         CALL PENCLR (INT(WIDTH), LCOLR)
         CALL NEWPEN( INT(WIDTH) )
*
* Set up polyline
*
         CALL PLOT( X(1),Y(1),3 )
         DO 1000 I = 2,N
            CALL PLOT( X(I),Y(I),2 )
 1000    CONTINUE
         RETURN
         END
*
        SUBROUTINE GK5VXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)
*
*  (C) COPYRIGHT ICL & SERC  1986
*

*
*   RUTHERFORD / ICL GKS SYSTEM
*
*   Type of routine:    Part of workstation driver
*   Author:             MJD
*
        INCLUDE (CHECKIBM)
*
*   PURPOSE OF THE ROUTINE
*   ----------------------
*      VERSATEC  Supply Font Details
*
*   MAINTENANCE LOG
*   ---------------
*
*   ARGUMENTS
*   ---------
*      INP IFID     Font identifier (ignored)
*      OUT RHT      Height from base to cap line
*      OUT RMAXWD   Width of widest character
*      OUT RBOT     Distance from base to bottom line
*      OUT RTOP     Distance from cap to top line
*      OUT RWD      Character widths array
*
        INTEGER IFID
        REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*   COMMON BLOCK USAGE
*   ------------------
*
        INCLUDE (PRGKDT)
        INCLUDE (CMGKWCA)
        INCLUDE (CMGKWKD)
*
*   LOCALS
*   ------
*
        INTEGER II
*
        RHT = FLOAT(KWCHHT(KWKIX))
        RMAXWD = FLOAT(KWCHWD(KWKIX))
        RBOT = FLOAT(KWCHHT(KWKIX))/3
        RTOP = 0.0
*      widths for chars
        DO 10 II = 1,95
          RWD(II) = RMAXWD
   10   CONTINUE
        RETURN
        END


        SUBROUTINE GK5VCP (NEWPAT, COLPAS)

*
*  (C) COPYRIGHT ICL & SERC  1986
*

*
*   RUTHERFORD / ICL GKS SYSTEM
*
*   Type of routine:    Part of workstation driver
*   Author:             MJD
*
        INCLUDE (CHECKIBM)
*
*   PURPOSE OF THE ROUTINE
*   ----------------------
*   To convert pattern array for each toner (CMY+B) defined
*   in GK5VPT, to integer data so that pattern can br defined
*   on Versatec . (see TONE p 4-57)
*
*   MAINTENANCE LOG
*   ---------------
*
*   ARGUMENTS
*   ---------
*
        INTEGER COLPAS
        INTEGER NEWPAT (16, 16)
*
*   COMMON BLOCK USAGE
*   ------------------
*
        INCLUDE (CMGKVHC)
*
*   LOCALS
*   ------
*
        INTEGER ITOT, IVAL, I, J, IN
        INTEGER PATDAT (16), PATSIZ
*
* The 2D array holding the pattern (NEWPAT) for the specified
* colour pass (COLPAS) is converted into integer form
* so it can be interpreted by the Versatec. The four colour
* pass patterns (for each pattern the user specifies) are
* stored in the global 3D array LISPAT as follows : _
*   LISPAT (pattern index, 1 (black pattern), data array 1,
*                          2 (cyan pattern ), data array 2 ,
*                          3 (magenta patern), data array 3,
*                          4 (yellow patern), data array 4,
*           pattern index, ......, etc.
      ITOT = 0
      IVAL = 0
      DO 10 I = 1, 16
         LISPAT (PATIDX, COLPAS, I) = 0
  10  CONTINUE
*
      DO 20 I = 1,IDIMX
         IN = 1
         DO 25 J = IDIMY, 1, -1
            IVAL = NEWPAT (I, IN) * (2 * * (J - 1) )
            ITOT = ITOT + IVAL
            IN = IN + 1
  25     CONTINUE
         PATDAT (I) = ITOT
         ITOT = 0
         IVAL = 0
  20  CONTINUE
*               Note: Current pattern size is always 16x16
*                     Although pattern definition may be less
      PATSIZ = 16
      DO 40 I = 1, IDIMX
         LISPAT (PATIDX, COLPAS, I) = PATDAT (I)
  40  CONTINUE
      PATLEN (PATIDX) = PATSIZ
      RETURN
      END


        SUBROUTINE GK5VPT
*
      INCLUDE (CHECKIBM)
*  (C) COPYRIGHT ICL & SERC  1986
*

*
*   RUTHERFORD / ICL GKS SYSTEM
*
*   Type of routine:    Part of workstation driver
*   Author:             MJD
*
*
*   PURPOSE OF THE ROUTINE
*   ----------------------
*     To split the given pattern into 4 patterns
*     for CMYB etc.
* The versatec allows 'patterns' to be drawn using one colour at
* a time only and using only the first 8 colours (see TONE p4-57
* in programming manual). In order to acheive multi-colour patterns
* the given pattern has to be split  into 4 patterns ie
* all the first eight colours are produced using CMY+B ie : -
*
*    colour       black    cyan   magenta   yellow
*
* white 0           0       0        0         0
* black 1           1       0        0         0
* red   2           0       1        1         0
* green 3           0       1        0         1
* blue  4           0       1        1         0
* yellow5           0       0        0         1
* cyan  6           0       1        0         0
* magen 7           0       0        1         0
* white 8           0       0        0         0
*
*  A pattern is produced for the corresponding occurances of
* CMYB in the original pattern and these patterns are stored
* in NEWPAT. When these four patterns are superimposed they
* produce a multicolour pattern .... voila !
*
*   MAINTENANCE LOG
*   ---------------
*
*   ARGUMENTS
*   ---------
*
*   COMMON BLOCK USAGE
*   ------------------
*
         INCLUDE (CMGKVHC)

*
*   LOCALS
*   ------
*
        INTEGER IK, I, J, ELEMT, MINCOL, MAXCOL
        INTEGER NEWPAT (16, 16)
        INTEGER COLS (0:8, 4)
        PARAMETER (MINCOL = 0, MAXCOL = 8)
      DATA COLS /0, 1, 0, 0, 0, 0, 0, 0, 0,
     :           0, 0, 0, 1, 1, 0, 1, 0, 0,
     :           0, 0, 1, 0, 1, 0, 0, 1, 0,
     :           0, 0, 1, 1, 0, 1, 0, 0, 0 /
* Data for COLS is read in column major order ie
* the four lines of data represent the values
* for black, cyan, magenta yellow respectively.
*
*
      DO 6 I = 1, 16
         DO 5 J = 1, 16
            NEWPAT (I, J) = 0
  5      CONTINUE
  6   CONTINUE
      DO 30 IK = 1, 4
         DO 20 I = 1, IDIMX
            DO 10 J = 1, IDIMY
               ELEMT = PAT (I, J)
               IF (ELEMT .LT. MINCOL) ELEMT = MINCOL
               IF (ELEMT .GT. MAXCOL) ELEMT = MAXCOL
               NEWPAT (I, J) = COLS (ELEMT, IK)
  10        CONTINUE
  20     CONTINUE
*    Convert pattern to Versatec readable form ie integer data
         CALL GK5VCP (NEWPAT, IK)
  30  CONTINUE
      RETURN
      END


      SUBROUTINE GK5VGC (N, X, Y)


      INCLUDE (CHECKIBM)

* This is a diddy subroutine which is called by GKCRCS and
* simply stores the data points of the requested polygon in
* the global arrays ARCPTX & ARCPTY (indexed by IDX), instead
* of actually physically producing the polygon.
      INTEGER N, I
      REAL X(N), Y(N)
      INCLUDE (PRGKMC)
      INCLUDE (CMGKVHC)

*
*   If not first line and a corner then add the corner point(s)
*   Note this works for a single corner only
*
      IF (IDX .GT. 1 .AND.
     :     ABS(X(1) - ARCPTX(IDX-1)) .GT. QTOL .AND.
     :     ABS(Y(1) - ARCPTY(IDX-1)) .GT. QTOL ) THEN
*   Top Left or Bottom Right
         IF (ARCPTX(IDX-1).GT.X(1).AND.ARCPTY(IDX-1).GT.Y(1) .OR.
     :       ARCPTX(IDX-1).LT.X(1).AND.ARCPTY(IDX-1).LT.Y(1)) THEN
            ARCPTX (IDX) = X(1)
            ARCPTX (IDX) = ARCPTY (IDX-1)
            IDX = IDX + 1
*   Top Right or Bottom Left
         ELSE IF (ARCPTX(IDX-1).GT.X(1).AND.ARCPTY(IDX-1).LT.Y(1) .OR.
     :       ARCPTX(IDX-1).LT.X(1).AND.ARCPTY(IDX-1).GT.Y(1)) THEN
            ARCPTX (IDX) = ARCPTX (IDX-1)
            ARCPTX (IDX) = Y(1)
            IDX = IDX + 1
         ENDIF
*
      ENDIF

      DO 10 I = 1, N
         ARCPTX (IDX) = X (I)
         ARCPTY (IDX) = Y (I)
         IDX = IDX + 1
  10  CONTINUE
*
*  Close area
*
      ARCPTX (IDX) = ARCPTX (1)
      ARCPTY (IDX) = ARCPTY (1)
      RETURN
      END

         SUBROUTINE GK5VPA (N, X, Y, RADIUS, IGDP)

         INCLUDE (CHECKIBM)
         INCLUDE (PRGKS)
         INCLUDE (PRGKDT)
         INCLUDE (CMGKWCA)
         INCLUDE (CMGKWKD)
         INCLUDE (CMGKVHC)

* GDP Parameters
      INTEGER GARC, GCHORD, GPIE, GCIRCL
      PARAMETER (GARC = -1,  GCHORD = -2, GPIE = -3, GCIRCL = -4)

*
* Purpose
*
* This routine is called from entrypoint 15 and 17. It's main
* pupose is to decifer which is the best fill area method to
* use for the interior style specified ( as u probably alredy
* know .. the versatec is far more efficient  than using
* the GKFILS routine.... all this sidetracking is worth it
* in the end Honest !!).
* Algorithm
* ---------
*
* if interior style = pattern then
*     call gk0vfi if predefined patterns 1 - 4 are specified
*                    and the haven't been redefined.
*     otherwise retreive pattern from LISPAT and draw
* else
*    call gk0vfi
*
         INTEGER N, IGDP
         REAL X(N), Y(N), RADIUS

         INTEGER I, J, GETPAT (16)
         INTEGER STYLE, FACI, IHATCH
         INTEGER  IPAT, PATSIZ

      FACI = KWFACI (KWKIX)
      STYLE = KWFASI (KWKIX)
      IF ( FACI .EQ. 0) FACI = 1
      IF (KWFAIS(KWKIX) .EQ. GPATTR) THEN
         IF (PATDEF (STYLE) .EQ. 0) THEN
*  Pattern predefined
            CALL GK5VFI( N, X, Y, KWFAIS(KWKIX), STYLE,
     :                FACI, IGDP)
         ELSE IF (PATDEF(STYLE) .EQ. 1) THEN
*  User defined pattern - Do 4 colour passes
            PATSIZ = PATLEN (STYLE)
            DO 155 I = 1, 4
               DO 154 J = 1, PATSIZ
                  GETPAT (J) = LISPAT ( STYLE, I, J)
  154          CONTINUE
               CALL PENCLR (1, I)
               CALL NEWPEN (1)
               CALL TONFLG (0)
               CALL TONE (0.0, 0.0, GETPAT, -PATSIZ)
               IF (ENTRYP .EQ. 17 .AND. IGDP .EQ. GCIRCL) THEN
                      CALL CIRCLE (X(2), Y(2), RADIUS, 0)
****** Removed until Utilities are fixed *********
*              ELSE IF (ENTRYP .EQ. 17 .AND.
*    :              IGDP .EQ. GPIE .OR. IGDP .EQ. GCHORD) THEN
*                     CALL CONVEX (X, Y, N)
               ELSE
                  CALL TONE (X, Y, N, 1)
               END IF
               CALL TONFLG (-1)
  155       CONTINUE
         ELSE
*  Pattern undefined - use 1
            STYLE = 1
            CALL GK5VFI( N, X, Y, KWFAIS(KWKIX), STYLE,
     :                FACI, IGDP)
         END IF
      ELSE

          CALL GK5VFI( N,X,Y, KWFAIS(KWKIX), STYLE,
     :                        FACI, IGDP)
      END IF
      RETURN
      END
      SUBROUTINE GK5VFI( N, X,Y, FAIS, FASI, FACI, IGDP)
      INCLUDE (CHECKIBM)
      INCLUDE (PRGKS)
      INCLUDE (PRGKDT)
      INCLUDE (CMGKWCA)
      INCLUDE (CMGKWKD)
      INCLUDE (CMGKVHC)
*
* Purpose
*
*
*
* To fill an area bounded by the N points with coords in X,Y
* it is called for GDP and Fill area.
* NOTE : this routine is used to fill areas in the style solid,
* hollow, hatch and Pattern styles 1 to 4 only ie the 4 predefined
* patterns. Other patterns are drwawn using GK5VPA.
*
      INTEGER N,FAIS,FASI,FACI,IGDP
      REAL X(N),Y(N)
*
* LOCALS
*
* IPAT   - The current pattern number
* IFASI  - Index to current Hatch or pattern styles
* MINHAT - The minimum hatch number
* MAXHAT - The maximum hatch number
* HATLIS - List of current Hatch styles
* HATROT - List of rotated Hatch styles
* HATCH  - An integer array holding 10 hatch patterns in hex constants
* MINPAT - The minimum pattern number
* MAXPAT - The maximum pattern number
* PATTN  - An integer array holding 4 patterns in hex constants
* CURHAT - The current hatch pattern selected from HATCH
* CURPAT - The current pattern selectd from PATTN
* STYLE  - The style of hatching
* IHATCH - Counter
* ISTYLE - Counter
* MAXHAT - Maximum number of hatch patterns
* DEFINE - A flag to define the hatch patterns only once
*
      INTEGER MNPAT, MXPAT, PATSIZ
      PARAMETER (MNPAT = 1, MXPAT = 40, PATSIZ = 16)
      INTEGER MINHAT, MAXHAT
      PARAMETER (MINHAT = -10, MAXHAT = -1)

      INTEGER IFASI, I, IFACI, IPAT, IPATTR, MINPAT, PAXPAT
      INTEGER HATCH(160), PATTN(64), IHATCH
      INTEGER HATLIS(10), HATROT(10)
      INTEGER CURPAT(16), CURHAT(16)
      REAL X2(2), Y2(2)
* GDP Parameters
      INTEGER GARC, GCHORD, GPIE, GCIRCL
      PARAMETER (GARC = -1,  GCHORD = -2, GPIE = -3, GCIRCL = -4)
      DATA HATCH /Z0000,Z0000,Z0000,Z0000,Z0000,Z0000,Z0000,Z0000,
     :            Z0000,Z0000,Z0000,Z0000,Z0000,Z0000,Z0000,ZFFFF,

     :            Z8000,Z8000,Z8000,Z8000,Z8000,Z8000,Z8000,Z8000,
     :            Z8000,Z8000,Z8000,Z8000,Z8000,Z8000,Z8000,Z8000,

     :            Z0001,Z0002,Z0004,Z0008,Z0010,Z0020,Z0040,Z0080,
     :            Z0100,Z0200,Z0400,Z0800,Z1000,Z2000,Z4000,Z8000,

     :            Z8000,Z4000,Z2000,Z1000,Z0800,Z0400,Z0200,Z0100,
     :            Z0080,Z0040,Z0020,Z0010,Z0008,Z0004,Z0002,Z0001,

     :            ZFFFF,Z0001,Z0001,Z0001,Z0001,Z0001,Z0001,Z0001,
     :            Z0001,Z0001,Z0001,Z0001,Z0001,Z0001,Z0001,Z0001,

     :            Z8001,Z4002,Z2004,Z1008,Z0810,Z0420,Z0240,Z0180,
     :            Z0180,Z0240,Z0420,Z0810,Z1008,Z2004,Z4002,Z8001,

     :            Z8001,Z4002,Z2004,Z1008,Z0810,Z0420,Z0240,ZFFFF,
     :            ZFFFF,Z0240,Z0420,Z0810,Z1008,Z2004,Z4002,Z8001,

     :            ZFFFF,Z0001,ZFFFC,Z0003,Z0003,Z0003,Z0003,Z0003,
     :            Z0003,Z0003,Z0003,Z0003,Z0003,Z0003,Z0003,Z0003,

     :            Z4002,ZB005,Z500B,Z2814,Z1428,Z0B50,Z05B0,Z02C0,
     :            Z0340,Z05B0,Z0B50,Z1428,Z2814,Z500B,ZB005,Z4002,

     :            ZFFFF,Z0810,Z0420,Z0240,Z0240,Z0240,Z0240,Z0180,
     :            Z0180,Z0240,Z0240,Z0240,Z0240,Z0420,Z0810,Z1008 /

      DATA PATTN /Z1C00,Z2200,Z4100,Z4100,Z4100,Z2200,Z1C00,Z0000,
     :            Z0041,Z0022,Z0014,Z0008,Z0014,Z0022,Z0041,Z0000,

     :            Z1C00,Z2200,Z4100,Z4100,Z4100,Z2200,Z1C00,Z0000,
     :            Z0038,Z0044,Z0082,Z0082,Z0082,Z0044,Z0038,Z0000,

     :            Z7F00,Z4100,Z5D00,Z5500,Z5D00,Z4100,Z7F00,Z0000,
     :            Z007F,Z0041,Z005D,Z0055,Z005D,Z0041,Z0075,Z0000,

     :            Z001C,Z002B,Z004A,Z007F,Z004A,Z002B,Z001C,Z0000,
     :            Z1C00,Z2B00,Z4A00,Z7F00,Z4A00,Z2B00,Z1C00,Z0000 /

      DATA HATLIS /-1, -2, -3, -4, -5, -6, -7, -8, -9, -10/
      DATA HATROT / 2,  1,  4,  3,  5,  6,  7,  8,  9,  10/

* NOTE : the Versatec manual is a bit unclear when describing
* pattern fill. The characterristics of Pattern fill are :-
* a.) if a pattern fill is defined using SETPAT & DEFPAT
*     then you are limited to the no of patterns;
*     also patterns can only be defined once per plot !
* b.) if TONE is used ie the pattern array is slapped in
*     the call to tone then you can redefine patterns
*     as the patterns are not defined  as such.
*     and store the pattern in a seperate array (which
*     is what i've done !).
* The hatch styles in effect are psuedo defined each
* time this routine is called as are the 4 patterns (1 - 4).
*

* Check for INTROR being SOLID, and set the pattern.
** Note: The Aspects of fill area probably need to be set individual.

      IFACI = COLMAP (FACI)
      IFASI = FASI
      IF (FAIS .EQ. GSOLID) THEN
         CALL TONFLG (-1)
         CALL TONCLR (IFACI)
         CALL TONE (X, Y, N, 1)
      ELSE IF (FAIS .EQ. GHATCH) THEN
*  Work out index to Hatch table
         IFASI = 1
         DO 5 I = 1, 10
           IF ( HATLIS (I) .EQ. FASI ) IFASI = I
   5     CONTINUE

* NOTE : as with pattern HATCH styles are not rotated by the
* Versatec so if the hatch index is in the range 1 - 4 then
* a different hatch style may have to be read in which will
* appear as the hatch style the user specified when the
* plot is rotated or not as the case may be !. ie : _
*       hatch style no 1 (horizontal lines) gives the
*       appearance of a vertical hatch on the actual plot
*       but also gives the appearance of a horizontal hatch
*       on a rotated plot ( honest ). ie. the lines are still
*       facing the same direction but the plot arond them have
*       changed 90 degrees. (hatch -5 to -10 are symmetrical)

         IF ( ROT ) IFASI = HATROT (IFASI)

         DO 10 IHATCH = 1, PATSIZ
            CURHAT(IHATCH) = HATCH ( (IFASI - 1) * PATSIZ + IHATCH )
  10     CONTINUE

         CALL PENCLR (1, IFACI)
         CALL NEWPEN (1)
         CALL TONFLG (0)
         CALL TONE (0.0, 0.0, CURHAT, -PATSIZ)
         IF (ENTRYP .EQ. 17 .AND. IGDP .EQ. GCIRCL) THEN
               CALL CIRCLE (X(2), Y(2), RAD, 0)
****** Removed until Utilities are fixed *********
*        ELSE IF (ENTRYP .EQ. 17 .AND.
*    :            IGDP .EQ. GPIE .OR. IGDP .EQ. GCHORD) THEN
*              CALL CONVEX (X, Y, N)
         ELSE
            CALL TONE (X, Y, N, 1)
         END IF
         CALL TONFLG (-1)
      ELSE IF (FAIS .EQ. GPATTR) THEN
*  Work out index to Pattern table
         IF (IFASI .LT. MNPAT .OR. IFASI .GT. MXPAT ) IFASI = 1
         DO 20 IPATTR = 1, PATSIZ
            CURPAT (IPATTR) = PATTN ( (IFASI - 1) * PATSIZ + IPATTR)
  20     CONTINUE
         CALL PENCLR (1, IFACI)
         CALL NEWPEN (1)
         CALL TONFLG (0)
         CALL TONE (0.0, 0.0, CURPAT, -PATSIZ)
         IF (ENTRYP .EQ. 17 .AND. IGDP .EQ. GCIRCL) THEN
            CALL CIRCLE (X(2), Y(2), RAD, 0)
****** Removed until Utilities are fixed *********
*        ELSE IF (ENTRYP .EQ. 17 .AND. IGDP .EQ. GPIE
*    :           .OR. IGDP .EQ. GCHORD) THEN
*           CALL CONVEX (X, Y, N)
         ELSE
            CALL TONE (X, Y, N, 1)
         END IF
         CALL TONFLG (-1)
      ELSE
*     draw using hollow style
         X2 (1) = X (N)
         Y2 (1) = Y (N)
         X2 (2) = X (1)
         Y2 (2) = Y (1)
         IF (IFACI .GT. 8) THEN
            CALL GK5VFL (N, X, Y)
            CALL GK5VFL (2, X2, Y2)
         ELSE
            CALL GK5VLN (N, X, Y)
            CALL GK5VLN (2, X2, Y2)
         END IF
      END IF
      RETURN
      END
