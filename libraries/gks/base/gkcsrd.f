C# IL>=a, OL>=1
      SUBROUTINE GKCSRD(IENT,NR,NI,NC,NRP,NIP,NCP,MORE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM

*  Type of routine:    W/S
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The "read" interface to CSS - the Central Segment Store.
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     22/02/84  CJW   Bug in Text, Cell array and GDP fixed (S19)
*     16/03/84  CJW   Bug in CELL ARRAY fixed
*     27/03/84  CJW   Corrections to GDP and CELL ARRAY
*     19/04/84  CJW   Reorder parameters in set text repn, see gkcswd
*                     (I184)
*     01/05/84  CJW   Cell array wkstn interface corrected (I194)
*                     Use correct PQR.
*     01/05/84  CJW   Grab less of the stack for CA (I191)
*     01/05/84  CJW   Change due to mod to GKPCP (I200)
*     07/05/84  MGC   Increment IFIRST for cell array (I199)
*     10/05/84  JGW   GDP passes refernce points as X,Y vectors
*     04/02/88  DCS   Return marker size in correct variable at Set
*                     polymarker attributes (S107).
*     06/08/90  KEVP  Removed unused local variable (S342).
*     11/12/91  KEVP  Corrected misleading cell array comments (C92).
*
*  ARGUMENTS
*  ---------
*     OUT IENT  - Entrypoint code
*     OUT NI    - Size of integer data
*     OUT NIP   - Stack pointer to Integer data
*     OUT NR    - Size of real data
*     OUT NRP   - Stack pointer to Real data
*     OUT NC    - Size of character data
*     OUT NCP   - Stack pointer to Character data
*     OUT MORE  - MORE in this segment item
*
      INTEGER IENT, NI, NIP, NR, NC, NRP, NCP, MORE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
      INCLUDE '../include/gkcca.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
*     IRXP   Stack pointer to X coordinates
*     IRYP   Stack pointer to Y coordinates
*     ISIZE  Actual size of stack allocation used
*     IHALF  Maximum fraction of stack to use [other than CA] (P)
*     ITHIRD Maximum fraction of stack to use in CA (P)
*     IFIRST Beginning of a chunk
*     ILAST  End of a chunk
*
      INTEGER IRXP, IRYP, ISIZE, IFIRST, ILAST
      INTEGER    IHALF,     ITHIRD
      PARAMETER (IHALF = 2, ITHIRD = 3 )
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*     It is assumed that CSRD can be called while a segment is open,
*     but that CSWD cannot be called while reading a segment.
*
*---------------------------------------------------------------------


      NI = 0
      NR = 0
      NC = 0
      NIP = KNIL
      NRP = KNIL
      NCP = KNIL
      MORE = GNMORE
      CALL GKCSBI
      IENT = KCSENT
      IF (KERROR .NE. 0) GO TO 9999

*     CASE IENT of

      GOTO (     9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999, 120, 130, 140, 120, 160, 170, 180, 190,
     :       200, 210, 220,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 310,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999,9999,9999,9999,9999,9999, 460) IENT


*     Otherwise
      GOTO 9999


* ---------------------------------------------------------------------
* Polyline / Fill area
* ---------------------------------------------------------------------
  120 CONTINUE
*     Data returned:
*     NR     : Number of points
*     NRP    : Coordinates


*     put limit on size grabbed. Also dont want to grab all the stack
      CALL GKLUMP(KREALS, 2, (KCSR-KCSRA), 2, IHALF, NR, NRP)
      IF (KERROR .EQ. 0) THEN
         IRXP = NRP
         IRYP = NRP + NR
         IF (KCSRA .EQ. 0) THEN
            CALL GKCSGP(NR, QSTACK(IRXP), QSTACK(IRYP))
         ELSE
*           First point = previous last
            QSTACK(IRXP) = QCSWRK(1)
            QSTACK(IRYP) = QCSWRK(2)
            CALL GKCSGP(NR-1, QSTACK(IRXP+1), QSTACK(IRYP+1))
         END IF
         KCSRA = KCSRA + NR
         IF (KCSRA .LT. KCSR) THEN
            MORE = GMORE
*           Save last point
            QCSWRK(1) = QSTACK(IRXP+NR-1)
            QCSWRK(2) = QSTACK(IRYP+NR-1)
         END IF
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* Polymarker
* ---------------------------------------------------------------------
  130 CONTINUE
*     Data returned:
*     NR     : Number of points
*     NRP    : Coordinates


*     put limit on size grabbed. Also dont want to grab all the stack
      CALL GKLUMP(KREALS, 2, (KCSR-KCSRA), 1, IHALF, NR, NRP)
      IF (KERROR .EQ. 0) THEN
         IRXP = NRP
         IRYP = NRP + NR
         CALL GKCSGP(NR, QSTACK(IRXP), QSTACK(IRYP))
         KCSRA = KCSRA + NR
         IF (KCSRA .LT. KCSR) MORE = GMORE
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* Text
* ---------------------------------------------------------------------
  140 CONTINUE
*     Data returned:
*     NI    : Number of characters
*     NIP   : Text in ASCII
*     QSS1   : X coordinate of text
*     QSS2   : Y coordinate of text

      QSS1 = QCSWRK(1)
      QSS2 = QCSWRK(2)
      ISIZE = KCSI - KCSIA
*     put limit on size grabbed. Dont want to grab all the stack
      CALL GKLUMP(KINTGS, 1, (KCSI-KCSIA), 1, IHALF, NI, NIP)
      IF (KERROR .EQ. 0) THEN
         CALL GKCSGI(NI, KSTACK(NIP))
         KCSIA = KCSIA + NI
         IF (KCSIA .LT. KCSI) MORE = GMORE
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* Cell array
* ---------------------------------------------------------------------
  160 CONTINUE
*     Data returned:
*     NI     : Number of cells
*     NIP    : Array of colour indexes
*     KSS1   : size of colour index array (DX)
*     KSS2   : size of colour index array (DY)
*     KSS3   : dimension of colour index array (IDIMX)
*     QSS1   : X coordinate of point P
*     QSS2   : Y coordinate of point P
*     QSS3   : X coordinate of point Q
*     QSS4   : Y coordinate of point Q
*     QSS5   : X coordinate of point R
*     QSS6   : Y coordinate of point R

      KSS1 = KCSWRK(1)
      KSS2 = KCSWRK(2)
      KSS3 = KSS1
      ISIZE = KCSI - KCSIA
*     put limit on size grabbed. Dont want to grab all the stack - I do
*     my calulations in CELL ROWS rather than individual cells
*     'cos I must have a whole number of rows in the chunk, that I grab.
      CALL GKLUMP(KINTGS,KSS1, (KCSI-KCSIA)/KSS1, 1, ITHIRD, NI, NIP)
      NI = NI * KSS1
      IF (KERROR .EQ. 0) THEN
         IFIRST = KCSIA/KSS1 + 1
         CALL GKCSGI(NI, KSTACK(NIP))
         KCSIA = KCSIA + NI
         IF (KCSIA .LT. KCSI) MORE = GMORE
         ILAST = KCSIA/KSS1
*        Calculate corners P, Q, R of chunk.
         CALL GKPCP(KSS2, IFIRST, ILAST, QCSWRK(1), QCSWRK(4),
     :                                      QCSWRK(7), QCSWRK(10))

         QSS1 = QCSWRK(7)
         QSS2 = QCSWRK(10)

         QSS3 = QCSWRK(8)
         QSS4 = QCSWRK(11)

         QSS5 = QCSWRK(9)
         QSS6 = QCSWRK(12)

         KSS2 = ILAST - IFIRST + 1
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* GDP
* ---------------------------------------------------------------------
  170 CONTINUE
*     Data returned:
*     NR     : Number of points
*     NRP    : Coordinates
*     NC     : Number of characters
*     NCP    : CHaracter data record
*     KSS1   : GDP identifier
*     QSS1   : X coordinate of point P
*     QSS2   : X coordinate of point Q
*     QSS3   : X coordinate of point R
*     QSS4   : Y coordinate of point P
*     QSS5   : Y coordinate of point Q
*     QSS6   : Y coordinate of point R

*     put limit on size grabbed. Also dont want to grab all the stack
      CALL GKLUMP(KREALS, 2, (KCSR-KCSRA), 2, IHALF, NR, NRP)
      IF (KERROR .EQ. 0) THEN
         KSS1 = KCSWRK(1)

         QSS1 = QCSWRK(1)
         QSS2 = QCSWRK(2)

         QSS3 = QCSWRK(3)
         QSS4 = QCSWRK(4)

         QSS5 = QCSWRK(5)
         QSS6 = QCSWRK(6)

         IRXP = NRP
         IRYP = NRP + NR
         IF (KCSRA .EQ. 0) THEN
            CALL GKCSGP(NR, QSTACK(IRXP), QSTACK(IRYP))
         ELSE
*           First point = previous last
            QSTACK(IRXP) = QCSWRK(7)
            QSTACK(IRYP) = QCSWRK(8)
            CALL GKCSGP(NR-1, QSTACK(IRXP+1), QSTACK(IRYP+1))
         END IF
         KCSRA = KCSRA + NR
         IF (KCSRA .LT. KCSR) THEN
            MORE = GMORE
*           Save last point
            QCSWRK(7) = QSTACK(IRXP+NR-1)
            QCSWRK(8) = QSTACK(IRYP+NR-1)
         END IF
*         CALL GKCSGC()
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* Set polyline attributes
* ---------------------------------------------------------------------
  180 CONTINUE
*     Data returned:
*     QSLNWD : Line width
*     KSPLI  : Polyline index
*     KSLNTY : Line type
*     KSPLCI : Polyline colour index
*     KSPLAF : Polyline ASF

      KSPLI = KCSWRK(1)
      KSLNTY = KCSWRK(2)
      KSPLCI = KCSWRK(3)
      KSPLAF(1) = KCSWRK(4)
      KSPLAF(2) = KCSWRK(5)
      KSPLAF(3) = KCSWRK(6)
      QSLNWD = QCSWRK(1)
      GOTO 9999


* ---------------------------------------------------------------------
* Set polymarker attributes
* ---------------------------------------------------------------------
  190 CONTINUE
*     Data returned:
*     QSMKSZ : Marker size
*     KSPMI  : Polymarker index
*     KSMKTY : Marker type
*     KSPMCI : Polymarker colour index
*     KSPMAF : Polymarker ASF

      KSPMI  = KCSWRK(1)
      KSMKTY = KCSWRK(2)
      KSPMCI = KCSWRK(3)
      KSPMAF(1) = KCSWRK(4)
      KSPMAF(2) = KCSWRK(5)
      KSPMAF(3) = KCSWRK(6)
      QSMKSZ = QCSWRK(1)
      GOTO 9999



* ---------------------------------------------------------------------
* Set text attributes
* ---------------------------------------------------------------------
  200 CONTINUE
*     Data returned:
*     QSCHXP : Character expaction factor
*     QSCHSP : Character spacing factor
*     QSCHHX : Character X height vector
*     QSCHHY : Character Y height vector
*     QSCHWX : Character X width vector
*     QSCHWY : Character Y width vector
*     KSTXI  : Text index
*     KSTXFN : Text font
*     KSTXPR : Text precision
*     KSTXCI : Text colour index
*     KSTXAF : Text ASF
*     KSTXP  : Text path
*     KSHTXA : Text horizontal alignment
*     KSVTXA : Text vertical alignment

      KSTXI = KCSWRK(1)
      KSTXFN = KCSWRK(2)
      KSTXPR = KCSWRK(3)
      KSTXCI = KCSWRK(4)
      KSTXAF(1) = KCSWRK(5)
      KSTXAF(2) = KCSWRK(6)
      KSTXAF(3) = KCSWRK(7)
      KSTXAF(4) = KCSWRK(8)
      KSTXP = KCSWRK(9)
      KSHTXA = KCSWRK(10)
      KSVTXA = KCSWRK(11)
      QSCHXP = QCSWRK(1)
      QSCHSP = QCSWRK(2)
      QSCHHX = QCSWRK(3)
      QSCHWX = QCSWRK(4)
      QSCHHY = QCSWRK(5)
      QSCHWY = QCSWRK(6)
      GOTO 9999



* ---------------------------------------------------------------------
* Set fill area attributes
* ---------------------------------------------------------------------
  210 CONTINUE
*     Data returned:
*     QSPAHX : Pattern X height vector
*     QSPAHY : Pattern Y height vector
*     QSPAWX : Pattern X width vector
*     QSPAWY : Pattern Y width vector
*     QSPAX  : Pattern size X
*     QSPAY  : Pattern size Y
*     KSFAI  : Fill area index
*     KSFAIS : Fill area interior style
*     KSFASI : Fill area style index
*     KSFACI : Fill area colour index
*     KSFAAF : Fill area ASF

      KSFAI = KCSWRK(1)
      KSFAIS = KCSWRK(2)
      KSFASI = KCSWRK(3)
      KSFACI = KCSWRK(4)
      KSFAAF(1) = KCSWRK(5)
      KSFAAF(2) = KCSWRK(6)
      KSFAAF(3) = KCSWRK(7)
      QSPAHX = QCSWRK(1)
      QSPAHY = QCSWRK(2)
      QSPAWX = QCSWRK(3)
      QSPAWY = QCSWRK(4)
      QSPAX = QCSWRK(5)
      QSPAY = QCSWRK(6)
      GOTO 9999



* ---------------------------------------------------------------------
* Set pick identifier
* ---------------------------------------------------------------------
  220 CONTINUE
*     Data returned:
*     KSS1   : Pick identifier

      KSS1 = KCSWRK(1)
      GOTO 9999



* ---------------------------------------------------------------------
* Normalisation transformation
* ---------------------------------------------------------------------
  310 CONTINUE
*     Data returned:
*     QSS7 - QSS10 : Clipping rectangle

      QSS7 = QCSWRK(1)
      QSS8 = QCSWRK(2)
      QSS9 = QCSWRK(3)
      QSS10 = QCSWRK(4)
      GOTO 9999
* ---------------------------------------------------------------------
* End Segment
* ---------------------------------------------------------------------
  460 CONTINUE
*     Data returned:
*      'close' the reading of the segment
      GOTO 9999


 9999 CONTINUE
*     End Case

      IF (MORE .EQ. GNMORE) THEN
         IF (KCSENT .EQ. KENSG) THEN
            CALL GKCSUL(KCSRDX)
            KCSRDX = KNIL
            KCSRDN = KNIL
         ELSE
         ENDIF
         KCSENT = KNIL
      ENDIF

      END
