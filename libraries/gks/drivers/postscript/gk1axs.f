


*---------------------------------------------------------------------
      SUBROUTINE GK1AXS(IFID,ITXPR,NCHARS,ICHSTR,X,Y)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Send hardware text string to the external PostScript file
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     INP ITXPR    Text precision
*     INP NCHARS   Number of characters in text string
*     INP ICHSTR   Text string (ASCII value)
*     INP X,Y      Centre positions for characters in text string
*
      INTEGER IFID,ITXPR,NCHARS,ICHSTR(*)
      REAL X(*),Y(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkmc.par'
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkhp.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkxfd.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*  CHANGE  - Logical flag.
*  DUMMY   - Dummy character, required by the buffering routine.
*  I,J,N   - Temporary count variables
*  ICHUNK  - Maximum number of characters output at a time
*  IEND    - Pointer to the fontname's last character in a heap allocation
*  ILEN    - Length of current fontname
*  INDTXT  - Local pointer into the RCHW array
*  IREM    - Dummy integer, required by the buffering routine.
*  ISTRT   - Pointer to the fontname's first character in a heap allocation
*  RANG    - Hardware character's rotation angle
*  RCENX   - Centre point of first character's bounding box
*  RCENY   - Centre point of first character's bounding box
*  RCHCOS  - Character rotation angle's cosinus
*  RCHH    - Hardware text's base to top line distance (validated)
*  RCHRL   - Character rotation vector's modulus
*  RCHRX   - Character rotation vector's projection
*  RCHRY   - Character rotation vector's projection
*  RCHEXP  - Character expansion factor
*  RCHSIN  - Character rotation angle's sinus
*  RUHH    - Hardware text's base to top line distance (unvalidated)
*  RWDSP   - Absolute width of the space character in the current font and
*            current height.
*  S       - Character variable, via which chunks of PostScript are sent for
*            buffering.
*  STX     - Each character's X origin temporary variable
*  STY     - Each character's Y origin temporary variable
*

*     Parameters
      INTEGER ICHUNK
      PARAMETER (ICHUNK=64)

*     pi radians
      REAL       PI
      PARAMETER (PI=3.14159)

*     Offsets in KWKDAT and QWKDAT
      INTEGER    ICCHHT,   ICCHAN,   ICHWFT,    IFTNAM,   IFTPSE
      PARAMETER (ICCHHT=3, ICCHAN=4, ICHWFT=6, IFTNAM=9, IFTPSE=10)
      INTEGER    ICCHXF
      PARAMETER (ICCHXF=11)
      INTEGER    IFTMAP,    IFTUSD
      PARAMETER (IFTMAP=11, IFTUSD=12)
*
      INTEGER  I, J, N, IREM
      INTEGER  INDTXT, ILEN, ISTRT, IEND
*
      REAL RCHH,RCHSIN,RCHCOS,RANGL,STX,STY,RWDSP
      REAL RCENX,RCENY,RCHRX,RCHRY,RCHRL,RUHH,RCHEXP
*
      CHARACTER S*(2*ICHUNK), AUXS1*50, AUXS2*32, DUMMY
*
      LOGICAL CHANGE
*
*  ALGORITHM
*  ---------
*     All hardware text is output at CHAR precision using the GKXDCS
*     utility. Individual coordinates of characters that make up a
*     string are sent down to PostScript. Note how the information on
*     character height is passed down.
*
*---------------------------------------------------------------------
*
*     Get current values against which to compare the stored attributes
*

*     Base to top line distance - get and valuate:
      RUHH   = SQRT( QWCHHX(KWKIX)*QWCHHX(KWKIX) +
     :               QWCHHY(KWKIX)*QWCHHY(KWKIX) )
      RCHH   = AMAX1(QMNCHH(KWKIX),RUHH)
      RCHH   = AMIN1(QMXCHH(KWKIX),RCHH)

*     Character rotation:
      RCHRX  = QWCHWX(KWKIX)
      RCHRY  = QWCHWY(KWKIX)
      RCHRL  = SQRT(RCHRX*RCHRX+RCHRY*RCHRY)
      RCHSIN = RCHRY/RCHRL
      RCHCOS = RCHRX/RCHRL
      RANGL  = AMOD(ATAN2(RCHRY,RCHRX)+2*PI,2*PI)*180.0/PI

*     Character Expansion factor (taking account of transformation)
      IF(QWCHXP(KWKIX)*RCHRL .GE. QMXCHX(KWKIX)*RUHH)THEN
         RCHEXP = QMXCHX(KWKIX)
      ELSE
         RCHEXP = QWCHXP(KWKIX)*RCHRL/RUHH
      ENDIF
      RCHEXP = AMAX1(QMNCHX(KWKIX),RCHEXP)

*     Realised PostScript Height:
*     PostScript's font height is  defined  to include vertical spacing and
*     base to bottom line distance. Therefore, GKS Character Height must be
*     amended. This was already done in GK1AXF when RWD array was being set,
*     but the information about the amended height was local and can now be
*     retrieved only from the widths. So, we take the  normalised width  of
*     space character from the font details common (QFWIDS(1) in GKXFD.CMN)
*     and multiply it by the unvalidated GKS character height (unvalidated
*     height was used to normalise the widths) and pass this information on
*     to PostScript where true font height will be calculated.

      RWDSP  = QFWIDS(1) * RUHH

*
*     Initialise the flag
*
      CHANGE = .FALSE.

*
*     See if locally stored copy of the attributes needs updating.
*

*     Hardware Text Font Number. First map the WDT font index to that of
*     RCHW entry.

      INDTXT = KHP(KHPXI(KWKDAT(IFTMAP,KWKIX))-KWTXFN(KWKIX)-1)

*     Now compare
      IF(KWKDAT(ICHWFT,KWKIX).NE.INDTXT) THEN
         KWKDAT(ICHWFT,KWKIX) = INDTXT
*        record that the font has been used
         KHP(KHPXI(KWKDAT(IFTUSD,KWKIX))+INDTXT-1) = 1
         CHANGE = .TRUE.
      ENDIF

*     PostScript Font height  (actually width of space character in device
*     coordinates on the basis of which the correct font height would be
*     calculated  - see above) :
      IF(QWKDAT(ICCHHT,KWKIX).NE.RWDSP) THEN
         QWKDAT(ICCHHT,KWKIX) = RWDSP
         CHANGE = .TRUE.
      ENDIF

*     Character rotation angle
      IF(QWKDAT(ICCHAN,KWKIX).NE.RANGL) THEN
         QWKDAT(ICCHAN,KWKIX) = RANGL
         CHANGE = .TRUE.
      ENDIF

*     Character expansion factor
      IF(QWKDAT(ICCHXF,KWKIX).NE.RCHEXP) THEN
         QWKDAT(ICCHXF,KWKIX) = RCHEXP
         CHANGE = .TRUE.
      ENDIF

*
*     Start from a new line in the external file
*
      CALL GKFOCO(KIOSN,DUMMY,IREM)

*
*     Set up the text attributes (hardware font number, font height and
*     rotation angle) if change has occurred.
*
      IF (CHANGE) THEN
*        Get the fontname information from the heap
         ISTRT = KHP(KHPXI(KWKDAT(IFTPSE,KWKIX))+INDTXT-1)
         IEND  = KHP(KHPXI(KWKDAT(IFTPSE,KWKIX))+INDTXT)-1
         ILEN  = IEND-ISTRT+1
*        Now send the name, height, angle  and expansion factor
*        information down
         AUXS1(1:1)='/'
         DO 90 I=2,ILEN+1
         AUXS1(I:I)=CHP(KHPXC(KWKDAT(IFTNAM,KWKIX))+I+ISTRT-3)
   90    CONTINUE
         WRITE(AUXS2,100) QWKDAT(ICCHHT,KWKIX),
     :                    QWKDAT(ICCHAN,KWKIX),
     :                    QWKDAT(ICCHXF,KWKIX)
  100    FORMAT(F11.3, 2F7.2, ' txstat')
         S(1:33+ILEN)=AUXS1(1:ILEN+1)//AUXS2(1:32)
         CALL GKFOCO(KIOPB, S(1:33+ILEN), IREM)
         CALL GKFOCO(KIOSN,DUMMY,IREM)
      ENDIF

*
*     Output text (STRING or CHAR precision)
*

*     Send the string in hexadecimal down to PostScript file
      N = ICHUNK
      CALL GKFOCO(KIOPB, '<', IREM)
      DO 130 I = 1, NCHARS, ICHUNK
         IF (NCHARS-I .LT. ICHUNK) N = NCHARS - I + 1
         DO 125 J = 1, N
             CALL GKDHEX(ICHSTR(I+J-1),S(J*2-1:J*2))
  125    CONTINUE
         CALL GKFOCO(KIOPB, S(1:2*N), IREM)
  130 CONTINUE
      CALL GKFOCO(KIOPB, '>', IREM)

*
*     CHAR precision
*
      IF (KWTXPR(KWKIX).EQ.GCHARP) THEN

*     Prepare and send the origins, first X, then Y.
         CALL GKFOCO(KIOPB, '[', IREM)
         DO 150 I=1,NCHARS
*        Calculate X origin of each character - subtract half the width (not
*        forgetting to take  into account the angle). Again, the  width  is a
*        product of normalised width and unvalidated current character height
*        (see above).
            RCENX  = QFWIDS(ICHSTR(I)-KBEGIN) * RUHH/2.0
            RCENY  = RCHH/2.0
            STX    = X(I)+(RCENY*RCHSIN-RCENX*RCHCOS)
            WRITE(S, 210) STX
            CALL GKFOCO(KIOPB, S(1:11), IREM)
  150    CONTINUE

*
*     Finish X, start Y.
*
         CALL GKFOCO(KIOPB, '][', IREM)

         DO 300 I=1, NCHARS
*        Calculate Y origin of each character - substract half the height (not
*        forgetting  to take  into account the angle). Again, the  width  is a
*        product of normalised  width and unvalidated current character height
*        (see above).
            RCENX  = QFWIDS(ICHSTR(I)-KBEGIN) * RUHH/2.0
            RCENY  = RCHH/2.0
            STY    = Y(I)-(RCENY*RCHCOS+RCENX*RCHSIN)
            WRITE(S, 210) STY
            CALL GKFOCO(KIOPB, S(1:11), IREM)
  300    CONTINUE
  210    FORMAT(F11.3)

*
*     Now call the txchar procedure
*
         CALL GKFOCO(KIOPB, '] txchar', IREM)

*
*     STRING precision
*
      ELSE
*        Calculate origin of first character - substract half the height (not
*        forgetting  to take  into account the angle). Again, the  width  is a
*        product of normalised  width and unvalidated current character height
*        (see above).
         RCENX  = QFWIDS(ICHSTR(1)-KBEGIN) * RUHH/2.0
         RCENY  = RCHH/2.0
         STX    = X(1)+(RCENY*RCHSIN-RCENX*RCHCOS)
         STY    = Y(1)-(RCENY*RCHCOS+RCENX*RCHSIN)

*
*     Now call the txstr procedure
*
         WRITE(S, 220) STX, STY
  220    FORMAT(2F11.3, ' txstr')
         CALL GKFOCO(KIOPB, S(1:28), IREM)

      ENDIF
*
      END
