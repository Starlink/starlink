C# IL>=a, OL>=1
      SUBROUTINE GKCSWD(IENT, NID, IDAT, NRD, XW, YW, NCD, STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     The "write" interface to CSS - the Central Segment Store.
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     30/01/84  JGW   GDP to transform QWR1-6, and provide WCA
*     30/01/84  JGW   Set NT error. Use matrix C2 not C3
*     22/02/84  CJW   Coordinate of text lost (S20)
*     27/03/84  CJW   Corrections to GDP
*     19/04/84  CJW   Transform Character Height and width vectors
*                     (I184)
*     19/04/84  CJW   Transform pattern ref points
*                     (I186)
*     30/04/84  CJW   Cell array wkstn interface corrected (I194)
*     03/05/84  PGLS  Wrong trans routine used for pat ref pt (I186 again)
*     08/05/84  PGLS  Allow one-point polymarkers (I209)
*     10/05/84  JGW   GDP gives reference points as X,Y vectors.
*     10/10/84  MGC   VME: Remove ,0,2) args on call (Text) to GKTWD (I235)
*     07/11/84  RSK   Actually use QIMKSZ not QILNWD (I229)
*     11/01/85  MGC   Allow create on segments already in CSS
*     12/03/85  MGC   Avoid KCSWRK,QCSWRK: instead use KCSWWK,QCSWWK
*     19/01/87  PKY   IS conversion. Error number change. Also add
*                     entry to error list.
*     23/04/87  KWB   IS conversion. Changes to cell array to reflect
*                     new workstation interface.
*     05/06/87  RMK   Merging ICL and RAL versions of this routine.
*                     The above fix for I235 was same as S92.
*     10/12/91  KEVP  Removed unused variable j (C91).
*
*  ARGUMENTS
*  ---------
*     INP IENT  - Entrypoint code
*     INP NID   - Size of array IDAT
*     I/O IDAT  - Integer data passed to or from workstation
*     INP NRD   - Size of arrays XW and YW
*     I/O XW    - Real X-coordinate data passed to or from workstation
*     I/O YW    - Real Y-coordinate data passed to or from workstation
*     INP NCD   - Size of character array STR
*     I/O STR   - Character array
*
      INTEGER IENT, NID, IDAT(NID), NRD, NCD
      REAL XW(NRD), YW(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*     I      Loop index
*     IPTR   Stack pointer
*     ICHUNK Number of points processed at a time
*     N      Number of points to transform
*     IMIN   Minimum number of stack locations acceptable
*     IWHOLE Fraction of stack that may be used
*
      INTEGER I, IPTR, ICHUNK, N, IMIN
      INTEGER    IWHOLE
      PARAMETER (IWHOLE = 1)
*
*  ERRORS
*  ------
*    -1017 Entry not found in list
*
*  COMMENTS
*  --------
*
*
*
*                  +------------+      +----------------+
*                  | Directory  |---<--|  Directory Ptr |
*                  +------------+      +----------------+
*                  | Ref. Count |
*        +------<--| Rec. number|
*        |         +------------+
*        |
*    +--------+      +--------+
*    |  DISK  |      |  CORE  |
*    +--------+      +--------+        +----------------+
*    | Record |---<--|  Page  |-----<--|  Open Segment  |
*    |--------|      +--------+        +----------------+
*    | Record |---<--|  Page  |
*    |--------|      +--------+
*    | Record |---<--|  Page  |
*    |--------|      +--------+        +----------------+
*    | Record |---<--|  Page  |-----<--|  Read Segment  |
*    |--------|      +--------+        +----------------+
*    | Record |---<--|  Page  |
*    +--------+      +--------+
*
*
*
*                                 DATA
*                                 ====
*
*
*                                       -------+         +--------+
*     +--------+                Rec. number    |         |  CORE  |
*     |  DISK  |                    Type       |         +--------+
*     +--------+            +------            |---------|  Page  |
*     | Record |------------|       Next       |         +--------+
*     +--------+            |       Data       |
*                           +------     -------+
*
*---------------------------------------------------------------------

*     Case ENTRYPOINT CODE of

      GOTO (       10,  20,  30,9999,9999,9999,9999,9999,9999,
     :      9999,9999, 120, 120, 140, 120, 160, 170, 180, 190,
     :       200, 210, 220,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 310,9999,9999,9999,9999,9999,9999,9999,9999,
     :      9999, 410, 420, 430, 440) IENT


*     Otherwise - ignore call
      GOTO 9999


* ---------------------------------------------------------------------
* Open workstation
* ---------------------------------------------------------------------
   10 CONTINUE
*     Data expected:
*     KWI1   : connection id (ignored!)
*     KWKIX  : workstation index
*     Data returned:
*     KWI1   : workstation category (input/output)
*     KERROR : error response or zero

      CALL GKIOOP(KFCSS, KNIL, KCSFLU)

*     Initialize

      CALL GKDRCR(KCSSZ, KCSDRE, 0, KCSDIR)
      KCSFCH = KNIL
      KCSNFR = 1
      KCSSTA = KNIL
      KCSRDN = KNIL
      DO 11 I = 1, KCSPAG
         KCSTYP(I) = KCSFRE
   11 CONTINUE

      KWI1 = GOUTIN

      GOTO 9999



* ---------------------------------------------------------------------
* Close workstation
* ---------------------------------------------------------------------
   20 CONTINUE
*     Data expected:
*     KWKIX  : workstation index

      CALL GKDRDL(KCSDIR)
      CALL GKIOCL(KFCSS, KNIL, KCSFLU)

      GOTO 9999



* ---------------------------------------------------------------------
* Clear workstation
* ---------------------------------------------------------------------
   30 CONTINUE
*     Data expected:
*     KWI1   : First or second entry (ignored)
*     KWI2   : Clear control flag (ignored)

*     Remove all segments
      CALL GKDRDL(KCSDIR)
      CALL GKDRCR(KCSSZ, KCSDRE, 0, KCSDIR)
      KCSFCH = KNIL
      KCSNFR = 1
      KCSSTA = KNIL
      DO 31 I = 1, KCSPAG
         KCSTYP(I) = KCSFRE
   31 CONTINUE
      GOTO 9999



* ---------------------------------------------------------------------
* Polyline / Polymarker / Fill area
* ---------------------------------------------------------------------
  120 CONTINUE
*     Data expected:
*     NRD    : Number of points
*     XW     : X coordinates
*     YW     : Y coordinates

*     Minimum acceptable size = 1 for GPM, 2 for GPL and GFA
      IF (IENT.EQ.13) THEN
         IMIN = 1
      ELSE
         IMIN = 2
      ENDIF
      CALL GKLUMP(KREALS, 2, NRD, IMIN, IWHOLE, ICHUNK, IPTR)
      IF (KERROR .EQ. 0) THEN
         CALL GKCSIT(IENT, 0, NRD, 0, 0, 0)
         DO 121 I = 1, NRD, ICHUNK
            N = MIN(ICHUNK, NRD-I+1)
            CALL  GKTWD(N, XW(I),        YW(I),
     :                        QSTACK(IPTR), QSTACK(IPTR+ICHUNK))
            CALL GKCSAP(N, QSTACK(IPTR), QSTACK(IPTR+ICHUNK))
  121    CONTINUE
         CALL GKSTDA(KREALS, IPTR)
         KCSSTA = GNEMPT
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* Text
* ---------------------------------------------------------------------
  140 CONTINUE
*     Data expected:
*     NID    : Number of characters
*     IDAT   : Text in ASCII
*     QWR1   : X coordinate of text
*     QWR2   : Y coordinate of text

      QCSWWK(3) = QWR1
      QCSWWK(4) = QWR2
      CALL GKTWD(1, QCSWWK(3), QCSWWK(4), QCSWWK(1), QCSWWK(2))
      CALL GKCSIT(IENT, NID, 0, 0, 0, 2)
      CALL GKCSAI(NID, IDAT)
      KCSSTA = GNEMPT
      GOTO 9999



* ---------------------------------------------------------------------
* Cell array
* ---------------------------------------------------------------------
  160 CONTINUE
*     Data expected:
*     NID    : Number of cells
*     IDAT   : Array of colour indexes
*     KWI1   : first dimension of colour index array
*     KWI2   : second dimension of colour index array
*     KWI3   : index of start column
*     KWI4   : index of start row
*     KWI5   : number of cell columns
*     KWI6   : number of cell rows
*     QWR1   : X coordinate of point P
*     QWR2   : Y coordinate of point P
*     QWR3   : X coordinate of point Q
*     QWR4   : Y coordinate of point Q
*     QWR5   : X coordinate of point R
*     QWR6   : Y coordinate of point R

      KCSWWK(1) = KWI5
      KCSWWK(2) = KWI6
      QCSWWK(7) = QWR1
      QCSWWK(8) = QWR3
      QCSWWK(9) = QWR5
      QCSWWK(10) = QWR2
      QCSWWK(11) = QWR4
      QCSWWK(12) = QWR6
      CALL GKTWD(3, QCSWWK(7), QCSWWK(10), QCSWWK(1), QCSWWK(4))
      CALL GKCSIT(IENT, KWI5*KWI6, 0, 0, 2, 6)
      DO 161 I = KWI4, KWI4+KWI6-1
        CALL GKCSAI(KWI5, IDAT((I-1)*KWI1+KWI3))
  161 CONTINUE
      KCSSTA = GNEMPT
      GOTO 9999



* ---------------------------------------------------------------------
* GDP
* ---------------------------------------------------------------------
  170 CONTINUE
*     Data expected:
*     NRD    : Number of points
*     XW     : X coordinates
*     YW     : Y coordinates
*     NCD    : Number of characters
*     STR    : Data record
*     KWI1   : GDP identifier
*     QWR1   : X coordinate of point P
*     QWR2   : X coordinate of point Q
*     QWR3   : X coordinate of point R
*     QWR4   : Y coordinate of point P
*     QWR5   : Y coordinate of point Q
*     QWR6   : Y coordinate of point R
*
      KCSWWK(1) = KWI1
      CALL GKTWD(3, QWRA(1), QWRA(4), QCSWWK(1), QCSWWK(4))
      CALL GKLUMP(KREALS, 2, NRD, 2, IWHOLE, ICHUNK, IPTR)
      IF (KERROR .EQ. 0) THEN
         CALL GKCSIT(IENT, 0, NRD, NCD, 1, 6)
         DO 171 I = 1, NRD, ICHUNK
            N = MIN(ICHUNK, NRD-I+1)
            CALL  GKTWD(N, XW(I),        YW(I),
     :                        QSTACK(IPTR), QSTACK(IPTR+ICHUNK))
            CALL GKCSAP(N, QSTACK(IPTR), QSTACK(IPTR+ICHUNK))
  171    CONTINUE
         CALL GKCSAC(NCD, STR)
         CALL GKSTDA(KREALS, IPTR)
         KCSSTA = GNEMPT
      END IF
      GOTO 9999



* ---------------------------------------------------------------------
* Set polyline attributes
* ---------------------------------------------------------------------
  180 CONTINUE
*     Data expected:
*     QILNWD : Line width
*     KIPLI  : Polyline index
*     KILNTY : Line type
*     KIPLCI : Polyline colour index
*     KIPLAF : Polyline ASF

      KCSWWK(1) = KIPLI
      KCSWWK(2) = KILNTY
      KCSWWK(3) = KIPLCI
      KCSWWK(4) = KIPLAF(1)
      KCSWWK(5) = KIPLAF(2)
      KCSWWK(6) = KIPLAF(3)
      QCSWWK(1) = QILNWD
      CALL GKCSIT(IENT, 0, 0, 0, 6, 1)
      GOTO 9999


* ---------------------------------------------------------------------
* Set polymarker attributes
* ---------------------------------------------------------------------
  190 CONTINUE
*     Data expected:
*     QIMKSZ : Marker size
*     KIPMI  : Polymarker index
*     KIMKTY : Marker type
*     KIPMCI : Polymarker colour index
*     KIPMAF : Polymarker ASF

      KCSWWK(1) = KIPMI
      KCSWWK(2) = KIMKTY
      KCSWWK(3) = KIPMCI
      KCSWWK(4) = KIPMAF(1)
      KCSWWK(5) = KIPMAF(2)
      KCSWWK(6) = KIPMAF(3)
      QCSWWK(1) = QIMKSZ

      CALL GKCSIT(IENT, 0, 0, 0, 6, 1)
      GOTO 9999



* ---------------------------------------------------------------------
* Set text attributes
* ---------------------------------------------------------------------
  200 CONTINUE
*     Data expected:
*     QICHXP : Character expaction factor
*     QICHSP : Character spacing factor
*     QICHHX : Character X height vector
*     QICHHY : Character Y height vector
*     QICHWX : Character X width vector
*     QICHWY : Character Y width vector
*     KITXI  : Text index
*     KITXFN : Text font
*     KITXPR : Text precision
*     KITXCI : Text colour index
*     KITXAF : Text ASF
*     KITXP  : Text path
*     KIHTXA : Text horizontal alignment
*     KIVTXA : Text vertical alignment

      KCSWWK(1) = KITXI
      KCSWWK(2) = KITXFN
      KCSWWK(3) = KITXPR
      KCSWWK(4) = KITXCI
      KCSWWK(5) = KITXAF(1)
      KCSWWK(6) = KITXAF(2)
      KCSWWK(7) = KITXAF(3)
      KCSWWK(8) = KITXAF(4)
      KCSWWK(9) = KITXP
      KCSWWK(10) = KIHTXA
      KCSWWK(11) = KIVTXA
      QCSWWK(1) = QICHXP
      QCSWWK(2) = QICHSP

      CALL GKTWDV(QICHHX,QICHHY,QCSWWK(3),QCSWWK(5))
      CALL GKTWDV(QICHWX,QICHWY,QCSWWK(4),QCSWWK(6))
      CALL GKCSIT(IENT, 0, 0, 0, 11, 6)
      GOTO 9999



* ---------------------------------------------------------------------
* Set fill area attributes
* ---------------------------------------------------------------------
  210 CONTINUE
*     Data expected:
*     QIPAHX : Pattern X height vector
*     QIPAHY : Pattern Y height vector
*     QIPAWX : Pattern X width vector
*     QIPAWY : Pattern Y width vector
*     QIPAX  : Pattern ref point X
*     QIPAY  : Pattern ref point Y
*     KIFAI  : Fill area index
*     KIFAIS : Fill area interior style
*     KIFASI : Fill area style index
*     KIFACI : Fill area colour index
*     KIFAAF : Fill area ASF

      KCSWWK(1) = KIFAI
      KCSWWK(2) = KIFAIS
      KCSWWK(3) = KIFASI
      KCSWWK(4) = KIFACI
      KCSWWK(5) = KIFAAF(1)
      KCSWWK(6) = KIFAAF(2)
      KCSWWK(7) = KIFAAF(3)
      QCSWWK(1) = QIPAHX
      QCSWWK(2) = QIPAHY
      QCSWWK(3) = QIPAWX
      QCSWWK(4) = QIPAWY
*     Transform pattern ref point
      QCSWWK(7) = QIPAX
      QCSWWK(8) = QIPAY
      CALL GKTWD(1,QCSWWK(7),QCSWWK(8),QCSWWK(5),QCSWWK(6))
      CALL GKCSIT(IENT, 0, 0, 0, 7, 6)
      GOTO 9999



* ---------------------------------------------------------------------
* Set pick identifier
* ---------------------------------------------------------------------
  220 CONTINUE
*     Data expected:
*     KWI1   : Pick identifier

      KCSWWK(1) = KWI1
      CALL GKCSIT(IENT, 0, 0, 0, 1, 0)
      GOTO 9999



* ---------------------------------------------------------------------
* Normalisation transformation
* ---------------------------------------------------------------------
  310 CONTINUE
*     Data expected:
*     QWR1 - QWR6 : Transformation C2
*     QWR7 - QWR10 : Clipping rectangle
*     QWR11 - QWR16 : Transformation C3 (ignored)

*     Store C2 as total transformation
      QWTOTT(1, KWKIX) = QWR1
      QWTOTT(2, KWKIX) = QWR2
      QWTOTT(3, KWKIX) = QWR3
      QWTOTT(4, KWKIX) = QWR4
      QWTOTT(5, KWKIX) = QWR5
      QWTOTT(6, KWKIX) = QWR6
*     Put clipping rectangle into segment
      QCSWWK(1) = QWR7
      QCSWWK(2) = QWR8
      QCSWWK(3) = QWR9
      QCSWWK(4) = QWR10
      CALL GKCSIT(IENT, 0, 0, 0, 0, 4)
      GOTO 9999



* ---------------------------------------------------------------------
* Create segment
* ---------------------------------------------------------------------
  410 CONTINUE
*     Data expected:
*     KWI1   : segment name
*     QWR1   : segment priority (ignored)
*     KSGRQ  : CSS request count
*     Data returned:
*     KDAT(1) : segment client count

      CALL GKDRGE(KCSDIR, KWI1, KCSIWK, KCSRWK, KCSWWK, QCSWWK)
      IF(KERROR.EQ.-1017) THEN
        KERROR=0
        KCSOPX = KNIL
        CALL GKCSNX(KCSOPX)
        KCSOPO = 1
        KCSWWK(KCSRF) = KSGRQ
        KCSWWK(KCSRC) = KCSREC(KCSOPX)
        KCSSTA = GEMPTY
      ELSE IF(KERROR.EQ.0) THEN
        KCSWWK(KCSRF) = KCSWWK(KCSRF) + 1
        KCSSTA = GNEMPT
      ENDIF
      CALL GKDRPU(KCSDIR, KWI1, KCSIWK, KCSRWK, KCSWWK, QCSWWK)
      IDAT(1) = KCSWWK(KCSRF)
      GOTO 9999



* ---------------------------------------------------------------------
* Close segment
* ---------------------------------------------------------------------
  420 CONTINUE

      CALL GKCSIT(KENSG, 0, 0, 0, 0, 0)
      CALL GKCSUL(KCSOPX)
      KCSOPX = KNIL
      KCSOPO = 1
      KCSSTA = KNIL
      GOTO 9999



* ---------------------------------------------------------------------
* Rename segment
* ---------------------------------------------------------------------
  430 CONTINUE
*     Data expected:
*     KWI1   : old segment name
*     KWI2   : new segment name

      CALL GKDRRN(KCSDIR, KWI1, KWI2)
      IF (KCSRDN .EQ. KWI1) KCSRDN = KWI2
      GOTO 9999



* ---------------------------------------------------------------------
* Delete segment
* ---------------------------------------------------------------------
  440 CONTINUE
*     Data expected:
*     KWI1   : segment name
*     KSGRQ  : CSS request count

      CALL GKDRGE(KCSDIR, KWI1, KCSIWK, KCSRWK, KCSWWK, QCSWWK)
      KCSWWK(KCSRF) = KCSWWK(KCSRF) - KSGRQ
      IF (KCSWWK(KCSRF) .EQ. 0) THEN
*        Delete the segment
         CALL GKDRDE(KCSDIR, KWI1)
*        Return segment to free chain
         CALL GKCSFR(KCSWWK(KCSRC))
      ELSE
         CALL GKDRPU(KCSDIR, KWI1, KCSIWK, KCSRWK, KCSWWK, QCSWWK)
      END IF
      GOTO 9999


 9999 CONTINUE
*     End Case ENTRYPOINT CODE


      END
