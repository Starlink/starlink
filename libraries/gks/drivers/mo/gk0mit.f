*
*
*
*
*
      SUBROUTINE GK0MIT(ITEM,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  Author:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Constructs GKSM data items
*
*  MAINTENANCE LOG
*  ---------------
*     01/03/83  DSG  Original version stabilized
*     19/06/86  RMK  Maintenance log incorporated into main driver routine
*
*  ARGUMENTS
*  ---------
*     INP   ITEM   Item identification number
*     INP   NID    Size of array IDAT
*     INP   IDAT   Integer data passed to or from workstation
*     INP   NRD    Size of arrays RX and RY
*     INP   RX     Real X-coordinate data passed to workstation
*     INP   RY     Real Y-coordinate data passed to workstation
*     INP   NCD    Dimension of character array
*     INP   STR    Character array
*
      INTEGER ITEM,NID,IDAT(NID),NRD,NCD
      REAL RX(NRD),RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/  Interface variables
*     Modify /GKYWKD/  Real workspace used for NDC values
*     Modify /GKYSTK/  Stack
*     Modify /GKYERR/  Report stack overflow
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkhp.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkstk.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
* Size of integer or real stack space requested
      INTEGER ICHUNK
      PARAMETER (ICHUNK=200)
*
* Metafile field lengths
      INTEGER IHFLD,ITFLD,LFIELD,IFIELD,IRFLD
      PARAMETER (IHFLD=1,ITFLD=3,LFIELD=7,IFIELD=4,IRFLD=9)
*
* Arrays to hold world x- and y- coordinates before transformation
      REAL WCX(3), WCY(3)
*
      CHARACTER SGKSM*4
*
      CHARACTER*17 SFMT(21)
*
      INTEGER LENDAT,IFUNC,NPTS,NTOK,
     :        I,J,K,M,N,ISETR,IL,IRL
*
* STACK USAGE
* -----------
*
*  ERRORS
*  ------
*
*      160   Item type is not allowed for user items
*      161   Item length is invalid
*
*  COMMENTS
*  --------

* This part of the MO driver deals with the actual encoding of
* the metafile.
*
* The ESCAPE and GDP data records contain no data in this implementation
* and the contents are currently ignored if supplied by an application.
*
*---------------------------------------------------------------------


* Check user item data record length supplied
      IF(ITEM.EQ.101.AND.NCD.LT.1) THEN
        KERROR=161
        GOTO 999
      ENDIF

* Check user item type supplied
      IF(ITEM.EQ.101.AND.(KWI1.LE.100.OR.KWI1.GT.999)) THEN
        KERROR=160
        GOTO 999
      ELSE IF(ITEM.EQ.101) THEN
        ITEM=KWI1
      ENDIF

* End of error checking
* ---------------------

* Output the GKSM field and the Item Type field of the item
* header. The GKSM field can contain from 0 to 4 of the
* characters 'GKSM'.

      SGKSM = 'GKSM'

      IF(IHFLD.EQ.0) THEN
        WRITE(CSTR(1),'(I3)') ITEM
      ELSE
        WRITE(CSTR(1),'(A,I3)') SGKSM(:IHFLD),ITEM
      ENDIF
      IFUNC=2
      CALL GK0MBU(IFUNC,CSTR(1)(:IHFLD+ITFLD))

* Construct every type of format specification which will be
*  used in the internal WRITE statements.
      SFMT(1)='(I7)'
      SFMT(2)='(I7,I4)'
      SFMT(3)='(I7,2I4)'
      SFMT(4)='(I7,3I4)'
      SFMT(5)='(I7,4I4)'
      SFMT(6)='(I7,13I4)'
      SFMT(7)='(I7,F9.6)'
      SFMT(8)='(I7,2F9.6)'
      SFMT(9)='(I7,4F9.6)'
      SFMT(10)='(I7,I4,F9.6)'
      SFMT(11)='(I7,I4,3F9.6)'
      SFMT(12)='(I7,I4,6F9.6)'
      SFMT(13)='(I7,2F9.6,I4)'
      SFMT(14)='(I7,6F9.6,2I4)'
      SFMT(15)='(I7,2I4,F9.6,I4)'
      SFMT(16)='(I7,3I4,2F9.6,I4)'
      SFMT(19)='(56F9.6)'
      SFMT(18)='(2I4)'
      SFMT(20)='(16(16I4))'
      SFMT(21)='(113F9.6)'

* Conditional goto on item code

      IF(ITEM.EQ.0) GOTO 5
      GOTO (10,20,30,40,50,60,998,998,998,998,110,110,130,
     :      110,150,160,998,998,998,998,210,220,230,240,250,
     :      260,270,280,290,300,310,320,330,340,350,360,370,
     :      380,390,400,410,420,430,440,998,998,998,998,998,
     :      998,510,520,530,540,550,560,998,998,998,998,610,
     :      998,998,998,998,998,998,998,998,998,710,720,998,
     :      998,998,998,998,998,998,998,810,820,830,840,998,
     :      998,998,998,998,998,910,920,930,940,950) ITEM
      IF (ITEM.GT.100) GOTO 1010

* END ITEM
* ---------
* Data expected: NONE

    5 LENDAT=0
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD))
      GOTO 999

* CLEAR WORKSTATION
* -----------------
* Data expected:
* KWI2 - CLEARING CONTROL FLAG

   10 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI2
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* REDRAW ALL SEGMENTS ON WORKSTATION
* ----------------------------------
* (LEVEL 1a)
* Data expected: NONE

   20 LENDAT=0
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD))
      GOTO 999

* UPDATE WORKSTATION
* ------------------
* Data expected:
* KWI1 - REGENERATION FLAG

   30 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* DEFERRAL STATE
* --------------
* (LEVEL1a)
* Data expected:
* KWI1 - DEFFERAL MODE
* KWI2 - IMPLICIT REGENERATION FLAG

   40 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI2
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* MESSAGE
* -------
* (LEVEL 1a)
* Data expected:
* IDAT(NID) - MESSAGE

   50 LENDAT=IFIELD+NID
      WRITE(CSTR(1),SFMT(2)) LENDAT,NID
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))

* Convert from Ascii integers to characters and send the string
      N=KWCST
      DO 55 I=1,NID,KWCST
        IF(NID-I.LT.KWCST) N=NID-I+1
        CALL GKATON (N, IDAT(I), CSTR(1))
        CALL GK0MBU(IFUNC,CSTR(1)(:N))
   55 CONTINUE

      GOTO 999

* ESCAPE
* ------
* Data expected:
* KWI1 - ESCAPE FUNCTION IDENTIFIER
* STR(NCD) - ESCAPE DATA RECORD

   60 CONTINUE

* Code to unpack data record would be required here.

      IL=0
      IRL=0
      LENDAT=(IL+3)*IFIELD+IRL*IRFLD
      WRITE(CSTR(1),SFMT(4)) LENDAT,KWI1,IL,IRL
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+3*IFIELD))

* Send the integer array
*     NTOK=KWCST/IFIELD
*     DO 62 I=1,IL,NTOK
*       IF(IL-I+1.LT.NTOK) N=IL-I+1
*       WRITE(CSTR(1),SFMT(20)) (KSTACK(ISETI+J-1),J=1,N)
*       CALL GK0MBU(IFUNC,CSTR(1)(:N*IFIELD))
*  62 CONTINUE

* Send the real array
*     NTOK=KWCST/IRFLD
*     N=NTOK
*     DO 64 I=1,IRL,NTOK
*       IF(IRL-I.LT.NTOK) N=IRL-I+1
*       WRITE(CSTR(1),SFMT(21)) (QSTACK(ISETR+J-1),J=1,N)
*       CALL GK0MBU(IFUNC,CSTR(1)(:N*IRFLD))
*  64 CONTINUE

*     CALL GKSTDA(KREALS,ISETR)
*     CALL GKSTDA(KINTGS,ISETI)


      GOTO 999

* POLYLINE, POLYMARKER, FILL AREA
* -------------------------------
* Data expected:
* RX(NRD), RY(NRD) - POINTS IN WORLD COORDINATES

  110 LENDAT=IFIELD + 2*NRD*IRFLD
      WRITE(CSTR(1),SFMT(2)) LENDAT,NRD
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
*
* Transform points to NDC, then format and send
      CALL GKSTAL(KREALS,ICHUNK*2,ISETR)
      IF(KERROR.NE.0) GOTO 999
      N=ICHUNK
      DO 114 I=1,NRD,ICHUNK
        IF(NRD-I.LT.ICHUNK) N=NRD-I+1
        CALL GKTWD(N,RX(I),RY(I),QSTACK(ISETR),
     :    QSTACK(ISETR+ICHUNK))
        NPTS=KWCST/(2*IRFLD)
        M=NPTS
        DO 117 J=1,N,NPTS
          IF(N-J.LT.NPTS) M=N-J+1
          WRITE(CSTR(1),SFMT(19)) (QSTACK(ISETR+J+K-2),
     :      QSTACK(ISETR+ICHUNK+J+K-2),K=1,M)
          CALL GK0MBU(IFUNC,CSTR(1)(:2*M*IRFLD))
  117   CONTINUE
  114 CONTINUE
      CALL GKSTDA(KREALS,ISETR)
*
      GOTO 999

* TEXT
* ----
* Data expected:
* IDAT(NID) - STRING OF CHARACTERS IN ASCII
* QWR1,QWR2 - STARTING POINT OF CHARACTER STRING

* Transform starting point from WC to NDC

  130 CONTINUE

      CALL GKTWD(1,QWRA(1),QWRA(2),QWKDAT(1,KWKIX),QWKDAT(2,KWKIX))

      LENDAT=2*IRFLD+IFIELD+NID
      WRITE(CSTR(1),SFMT(13)) LENDAT,QWKDAT(1,KWKIX),
     :  QWKDAT(2,KWKIX),NID
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+2*IRFLD+IFIELD))
*
* Convert from Ascii integers to characters and send the string
      N=KWCST
      DO 135 I=1,NID,KWCST
        IF(NID-I.LT.KWCST) N=NID-I+1
         CALL GKATON( N,IDAT(I),CSTR(1) )
        CALL GK0MBU(IFUNC,CSTR(1)(:N))
  135 CONTINUE
      GOTO 999

* CELL ARRAY
* ----------
* Data expected:
* QWR1,..,6 Coordinates of corner points of pixel array
* KWI1      Dimension (in X)
* KWI2      Dimension (in Y)
* KWI3      Start column
* KWI4      Start row
* KWI5      Number of columns
* KWI6      Number of rows
* IDAT(NID) Array of colour indices stored row by row

* Transform points from WC to NDC

  150 CONTINUE

* [PX,PY]
      WCX(1)=QWR1
      WCY(1)=QWR2
* [QX,QY]
      WCX(2)=QWR3
      WCY(2)=QWR4
* [RX,RY]
      WCX(3)=QWR5
      WCY(3)=QWR6

      CALL GKTWD(3,WCX,WCY,QWKDAT(1,KWKIX),QWKDAT(4,KWKIX))

      LENDAT=6*IRFLD+(2+KWI5*KWI6)*IFIELD
      WRITE(CSTR(1),SFMT(14)) LENDAT,(QWKDAT(I,KWKIX),
     :  QWKDAT(I+3,KWKIX),I=1,3),KWI5,KWI6
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+6*IRFLD+2*IFIELD))
*
* Send the array in I-Format
      NTOK=KWCST/IFIELD
      DO 152 K=KWI4,KWI4+KWI6-1
        N=NTOK
        DO 155 I=KWI3,KWI3+KWI5-1,NTOK
          IF(KWI3+KWI5-I.LT.NTOK) N=KWI3+KWI5-I
          WRITE(CSTR(1),SFMT(20)) (IDAT(KWI1*(K-1)+I+J-1),J=1,N)
          CALL GK0MBU(IFUNC,CSTR(1)(:N*IFIELD))
  155   CONTINUE
  152 CONTINUE
*
      GOTO 999

* GENERALISED DRAWING PRIMITIVE
* -----------------------------
* Data expected:
* STR(NCD) - GDP DATA RECORD
* RX(NRD), RY(NRD) - LIST OF POINTS
* KWI1 - GDP IDENTIFIER
* QWR1,...QWR6  Three corners of unit square in WC

  160 CONTINUE

* Code to unpack data record would be required here.

      IL=0
      IRL=0
      LENDAT=(4+IL)*IFIELD + ((NRD*2)+IRL+6)*IRFLD
      WRITE(CSTR(1),SFMT(5)) LENDAT,KWI1,NRD+3,IL,IRL
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+4*IFIELD))

* Transform first three points from WC to NDC, reformat, and send

      CALL GKTWD(3,QWRA(1),QWRA(4),QWKDAT(1,KWKIX),QWKDAT(4,KWKIX))

      WRITE(CSTR(1),SFMT(19)) (QWKDAT(J,KWKIX),J=1,6)
      CALL GK0MBU(IFUNC,CSTR(1) (:6*IRFLD))

* Transform points to NDC, then format and send
      CALL GKSTAL(KREALS,ICHUNK*2,ISETR)
      IF(KERROR.NE.0) GOTO 999
      N=ICHUNK
      DO 164 I=1,NRD,ICHUNK
        IF(NRD-I.LT.ICHUNK) N=NRD-I+1
        CALL GKTWD(N,RX(I),RY(I),QSTACK(ISETR),
     :    QSTACK(ISETR+ICHUNK))
        NPTS=KWCST/(2*IRFLD)
        M=NPTS
        DO 167 J=1,N,NPTS
          IF(N-J.LT.NPTS) M=N-J+1
          WRITE(CSTR(1),SFMT(19)) (QSTACK(ISETR+J+K-2),
     :      QSTACK(ISETR+ICHUNK+J+K-2),K=1,M)
          CALL GK0MBU(IFUNC,CSTR(1)(:2*M*IRFLD))
  167   CONTINUE
  164 CONTINUE
      CALL GKSTDA(KREALS,ISETR)

* Try and get enough space in the stack for the integer and
* real data to be unpacked. Then recall UNPACK, this
* time supplying the actual lengths required.

*     CALL GKSTAL(KINTGS,IL,ISETI)
*     IF(KERROR.NE.0) GOTO 999
*     CALL GKSTAL(KREALS,IRL,ISETR)
*     IF(KERROR.NE.0) GOTO 999
*     IIL=IL
*     IRIL=IRL
*     CALL GUREC(NCD,STR,IIL,IRIL,1,IL,KSTACK(ISETI),
*    :  IRL,QSTACK(ISETR),ICC,SDAT)

* Send the data record array lengths
*     WRITE(CSTR(1),SFMT(18)) IL,IRL
*     CALL GK0MBU(IFUNC,CSTR(1)(:2*IFIELD))

* Send the integer array
*     NTOK=KWCST/IFIELD
*     DO 166 I=1,IL,NTOK
*       IF(IL-I+1.LT.NTOK) N=IL-I+1
*       WRITE(CSTR(1),SFMT(20)) (KSTACK(ISETI+J-1),J=1,N)
*       CALL GK0MBU(IFUNC,CSTR(1)(:N*IFIELD))
* 166 CONTINUE

* Send the real array
*     NTOK=KWCST/IRFLD
*     N=NTOK
*     DO 168 I=1,IRL,NTOK
*       IF(IL-I.LT.NTOK) N=IL-I+1
*       WRITE(CSTR(1),SFMT(21)) (QSTACK(ISETR+J-1),J=1,N)
*       CALL GK0MBU(IFUNC,CSTR(1)(:N*IRFLD))
* 168 CONTINUE

*     CALL GKSTDA(KREALS,ISETR)
*     CALL GKSTDA(KINTGS,ISETI)

      GOTO 999

* POLYLINE INDEX
* --------------
* Data expected:
* KIPLI - POLYLINE INDEX

  210 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPLI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* LINETYPE
* --------
* Data expected:
* KILNTY - LINETYPE

  220 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KILNTY
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* LINEWIDTH SCALE FACTOR
* ----------------------
* Data expected
* QILNWD - LINEWIDTH SCALE FACTOR

  230 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QILNWD
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* POLYLINE COLOUR INDEX
* ----------------------
* Data expected:
* KIPLCI - POLYLINE COLOUR INDEX

  240 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPLCI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* POLYMARKER INDEX
* ----------------
* Data expected:
* KIPMI - POLYMARKER INDEX

  250 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPMI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* MARKER TYPE
* -----------
* Data expected:
* KIMKTY - MARKER TYPE

  260 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIMKTY
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* MARKER SIZE SCALE FACTOR
* ------------------------
* Data expected:
* QIMKSZ - MARKER SIZE SCALE FACTOR

  270 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QIMKSZ
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* POLYMARKER COLOUR INDEX
* -----------------------
* Data expected:
* KIPMCI - POLYMARKER COLOUR INDEX

  280 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIPMCI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* TEXT INDEX
* ----------
* Data expected:
* KITXI -TEXT INDEX

  290 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KITXI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* TEXT FONT AND PRECISION
* -----------------------
*Data expected:
* KITXFN - TEXT FONT
* KITXPR - TEXT PRECISION

  300 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KITXFN,KITXPR
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* CHARACTER EXPANSION FACTOR
* --------------------------
* (LEVEL 1a)
* Data expected:
* QICHXP - CHARACTER EXPANSION FACTOR

  310 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QICHXP
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* CHARACTER SPACING
* -----------------
* (LEVEL 1a)
* Data expected:
* QICHSP - CHARACTER SPACING

  320 LENDAT=IRFLD
      WRITE(CSTR(1),SFMT(7)) LENDAT,QICHSP
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IRFLD))
      GOTO 999

* TEXT COLOUR INDEX
* -----------------
* Data expected:
* KITXCI - TEXT COLOUR INDEX

  330 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KITXCI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* CHARACTER VECTORS
* -----------------
* Data expected:
* QICHHX,QICHHY - CHARACTER HEIGHT VECTOR
* QICHWX,QICHWY - CHARACTER WIDTH VECTOR

  340 CONTINUE

* Transform vectors from WC to NDC
      CALL GKTWDV(QICHHX,QICHHY,QWCHHX(KWKIX),QWCHHY(KWKIX))
      CALL GKTWDV(QICHWX,QICHWY,QWCHWX(KWKIX),QWCHWY(KWKIX))

      LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWCHHX(KWKIX),QWCHHY(KWKIX),
     :  QWCHWX(KWKIX),QWCHWY(KWKIX)
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* TEXT PATH
* ---------
*Data expected:
* KITXP- TEXT PATH

  350 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KITXP
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* TEXT ALIGNMENT
* --------------
* Data expected:
* KIHTXA,KIVTXA - TEXT ALIGNMENT

  360 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KIHTXA,KIVTXA
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* FILL AREA INDEX
* --------------
*Data expected:
* KIFAI- FILL AREA INDEX

  370 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFAI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* FILL AREA INTERIOR STYLE
* ------------------------
* Data expected:
* KIFAIS - FILL AREA INTERIOR STYLE

  380 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFAIS
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* FILL AREA STYLE INDEX
* ---------------------
* Data expected:
* KIFASI - FILL AREA STYLE INDEX

  390 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFASI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

*FILL AREA COLOUR INDEX
* ---------------------
* Data expected:
* KIFACI - FILL AREA COLOUR INDEX

  400 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KIFACI
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* PATTERN SIZE
* ------------
* (LEVEL 1a)
* Data expected:
* QIPAHX,QIPAHY,QIPAWX,QIPAWY - PATTERN HEIGHT AND WIDTH VECTORS

  410 CONTINUE

* Transform vectors from WC to NDC
      CALL GKTWDV(QIPAWX,QIPAWY,QWPAWX(KWKIX),QWPAWY(KWKIX))
      CALL GKTWDV(QIPAHX,QIPAHY,QWPAHX(KWKIX),QWPAHY(KWKIX))


      LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWPAWX(KWKIX),QWPAWY(KWKIX),
     :  QWPAHX(KWKIX),QWPAHY(KWKIX)
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* PATTERN REFERENCE POINT
* -----------------------
* (LEVEL 1a)
* Data expected:
* QIPAX,QIPAY - REFERENCE POINT

  420 CONTINUE

* Transform point from WC to NDC
      QWRA(1) = QIPAX
      QWRA(2) = QIPAY
      CALL GKTWD(1,QWRA(1),QWRA(2),QWPAX(KWKIX),QWPAY(KWKIX))

      LENDAT=2*IRFLD
      WRITE(CSTR(1),SFMT(8)) LENDAT,QWPAX(KWKIX),QWPAY(KWKIX)
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* ASPECT SOURCE FLAGS
* -------------------
* Data expected:
* KWKDAT(1)->(13)  -  ASF's

  430 CONTINUE
      LENDAT=13*IFIELD
      WRITE(CSTR(1),SFMT(6)) LENDAT,(KWKDAT(I,KWKIX),I=1,13)
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* PICK IDENTIFIER
* ---------------
* (LEVEL 1a)
* Data expected:
* KWI1 - PICK IDENTIFIER

  440 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* POLYLINE REPRESENTATION
* -----------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - POLYLINE INDEX
* KWI2 - LINETYPE
* QWR1 - LINEWIDTH SCALE FACTOR
* KWI3 - POLYLINE COLOUR INDEX

  510 LENDAT=3*IFIELD+IRFLD
      WRITE(CSTR(1),SFMT(15)) LENDAT,KWI1,KWI2,QWR1,
     : KWI3
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* POLYMARKER REPRESENTATION
* -------------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - POLYMARKER INDEX
* KWI2 - MARKER TYPE
* QWR1 - MARKER SIZE SCALE FACTOR
* KWI3 - POLYMARKER COLOUR INDEX

  520 LENDAT=3*IFIELD+IRFLD
      WRITE(CSTR(1),SFMT(15)) LENDAT,KWI1,KWI2,
     :  QWR1,KWI3
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* TEXT REPRESENTATION
* -------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - TEXT INDEX
* KWI2 - TEXT FONT
* KWI3 - TEXT PRECISION
* QWR1 - CHARACTER EXPANSION FACTOR
* QWR2 - CHARACTER SPACING
* KWI4 - TEXT COLOUR INDEX

  530 LENDAT=4*IFIELD+2*IRFLD
      WRITE(CSTR(1),SFMT(16)) LENDAT,KWI1,KWI2,KWI3,
     : QWR1,QWR2,KWI4
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* FILL AREA REPRESENTATION
* ------------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - FILL AREA INDEX
* KWI2 - FILL AREA INTERIOR STYLE
* KWI3 - FILL AREA STYLE INDEX
* KWI4 - FILL AREA COLOUR INDEX

  540 LENDAT=4*IFIELD
      WRITE(CSTR(1),SFMT(5)) LENDAT,KWI1,KWI2,KWI3,KWI4
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* PATTERN REPRESENTATION
* ----------------------
* (LEVEL 1a)
* Data expected:
* KWI1 - PATTERN INDEX
* KWI2,KWI3 - DIMENSIONS OF PATTERN ARRAY
* KWI4,KWI5 - START COLUMN AND START ROW OF PATTERN ARRAY
* KWI6,KWI7 - NUMBER OF COLUMNS AND ROWS TO BE OUTPUT FROM PATTERN ARRAY
* PATTERN ARRAY

  550 LENDAT=(3+KWI6*KWI7)*IFIELD
      WRITE(CSTR(1),SFMT(4)) LENDAT,KWI1,KWI6,KWI7
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+3*IFIELD))
*
* Send the array in I-Format
      NTOK=KWCST/IFIELD
      DO 552 K=KWI5,KWI5+KWI7-1
        N=NTOK
        DO 555 I=KWI4,KWI4+KWI6-1,NTOK
          IF(KWI6+KWI4-I.LT.NTOK) N=KWI6+KWI4-I
          WRITE(CSTR(1),SFMT(20)) (IDAT(KWI2*(K-1)+I+J-1),J=1,N)
          CALL GK0MBU(IFUNC,CSTR(1)(:N*IFIELD))
  555   CONTINUE
  552 CONTINUE
*
      GOTO 999

* COLOUR REPRESENTATION
* ---------------------
* Data expected:
* KWI1 - COLOUR INDEX
* QWR1,QWR2,QWR3 - COLOUR (RED,GREEN,BLUE INTENSITIES)

  560 LENDAT=IFIELD+3*IRFLD
      WRITE(CSTR(1),SFMT(11)) LENDAT,KWI1,QWR1,QWR2,
     : QWR3
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* CLIPPING RECTANGLE
* ------------------
* Data expected:
* QWR7,QWR8,QWR9,QWR10 - LIMITS OF CLIPPING RECTANGLE (XMIN,
*  XMAX,YMIN,YMAX)

* These values were copied from the GKS state list by the
* front-end utitity GKCCTG if clipping was ON, and set to 0.0,
* 1.0,0.0,1.0 by GKCCTG if clipping was OFF.

  610 LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWR7,QWR8,QWR9,QWR10
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* WORKSTATION WINDOW
* ------------------
* Data expected:
* QWR1,QWR2,QWR3,QWR4 - WORKSTATION WINDOW LIMITS (XMIN,XMAX,
*  YMIN,YMAX)

  710 LENDAT=4*IRFLD
      WRITE(CSTR(1),SFMT(9)) LENDAT,QWR1,QWR2,QWR3,QWR4
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* WORKSTATION VIEWPORT
* --------------------
* Data expected:
* QWR1,QWR2,QWR3,QWR4 - WORKSTATION VIEWPORT LIMITS (XMIN,XMAX,
*  YMIN,YMAX)

  720 LENDAT=4*IRFLD
      IF(QWR1.GE.100.0.OR.QWR2.GE.100.0.OR.QWR3.GE.100.0.OR.
     : QWR4.GE.100.0) THEN
         WRITE(CSTR(1),'(I7,4F9.4)') LENDAT,QWR1,QWR2,QWR3,QWR4
      ELSE
         WRITE(CSTR(1),SFMT(9)) LENDAT,QWR1,QWR2,QWR3,QWR4
      ENDIF
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* CREATE SEGMENT
* --------------
* Data expected:
* KWI1 - SEGMENT NAME

  810 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* CLOSE SEGMENT
* --------------
* Data expected: NONE

  820 LENDAT=0
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD))
      GOTO 999

* RENAME SEGMENT
* --------------
* Data expected:
* KWI1 - OLD SEGMENT NAME
* KWI2 - NEW SEGMENT NAME

  830 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI2
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* DELETE SEGMENT
* --------------
* Data expected:
* KWI1 - SEGMENT NAME

  840 LENDAT=IFIELD
      WRITE(CSTR(1),SFMT(2)) LENDAT,KWI1
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+IFIELD))
      GOTO 999

* SET SEGMENT TRANSFORMATION
* --------------------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI5 - 1 OR 2 INDICATING 1ST OR 2ND ENTRY
* QWR1,..,.QWR6 - TRANSFORMATION MATRIX

  910 LENDAT=IFIELD+6*IRFLD
      WRITE(CSTR(1),SFMT(12)) LENDAT,KWI1,QWR1,QWR2,QWR3,QWR4,
     :  QWR5,QWR6
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET VISIBILITY
* --------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI4 - VISIBILITY

  920 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI4
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET HIGHLIGHTING
* ----------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI5 - HIGHLIGHTING

  930 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI5
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET SEGMENT PRIORITY
* --------------------
* Data expected:
* KWI1 - SEGMENT NAME
* QWR1 - SEGMENT PRIORITY

  940 LENDAT=IFIELD+IRFLD
      WRITE(CSTR(1),SFMT(10)) LENDAT,KWI1,QWR1
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* SET DETECTABILITY
* -----------------
* Data expected:
* KWI1 - SEGMENT NAME
* KWI2 - DETECTABILITY

  950 LENDAT=2*IFIELD
      WRITE(CSTR(1),SFMT(3)) LENDAT,KWI1,KWI2
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD+LENDAT))
      GOTO 999

* USER ITEM
* ----------
* Data expected:
* STR(NCD) - ITEM DATA RECORD
* KWI2 - LENGTH OF ITEM DATA RECORD (CHARACTERS)

 1010 LENDAT=KWI2
      IF(LENDAT.GT.NCD*80 .OR. LENDAT.LT.0) THEN
        KERROR=161
        GOTO 999
      ENDIF
      WRITE(CSTR(1),SFMT(1)) LENDAT
      CALL GK0MBU(IFUNC,CSTR(1)(:LFIELD))

      DO 1015 I=1,(LENDAT-1)/80+1
        J=MIN(80,LENDAT-80*(I-1))
        CALL GK0MBU(IFUNC,STR(I)(1:J))
 1015 CONTINUE

      GOTO 999

* Could have a call to GKBUG here to trap invalid metafile
* item type, but all that is needed is for GK0MWD to be
* consistent with this routine.

  998 CONTINUE

  999 RETURN
      END
