      SUBROUTINE GK2NIL (ITYPE,LENGTH)
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    (Part of) Workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*    Called at GTITM entrypoint to calculate the
*    internal packed length of the RDITM data record.
*
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver GK2NWD
*
*  ARGUMENTS
*  ---------
*     in    ITYPE  Item type (GKSM)
*     out   LENGTH RDITM data record length (as packed in
*                  user's CHARACTER*80 array)
*
      INTEGER ITYPE,LENGTH
*
*  COMMON BLOCK USAGE
*  ------------------
* Read   /GKYWCA/   KWKIX - Workstation index
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkpid.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'

*   Include the CGM data Common Block
      INCLUDE '../../include/gkcgn.cmn'
*
*  ALGORITHM
*  ---------
*
*  The item data will not be packed into the user's character array
*  by using the Fortran binding's PACK utility, but by using our own
*  internal packing utilities:
*  GKPKSI packs each 'short' integer (range 1 - 16383) into two
*  bytes, GKPKR packs each real into five bytes, and GKPKC
*  packs each integer into one byte of the character array.
*
*  The packed length is then calculated as a function  of the
*  real, integer, and character field lengths.
*
*
*  COMMENTS
*  --------
*
*  Binding changes not yet (10/11/83) made:
*    1) Metafile index and attribute lists are retained for the
*       moment. When these are abandoned, all references to the
*       metafile index in this routine must be removed.
*
*
*---------------------------------------------------------------------
      INTEGER POLYSET, CIRCLE
      PARAMETER (POLYSET = 39, CIRCLE = 13344 )

      LENGTH=0

      IF(ITYPE.LE.0) GOTO 9999

      GOTO (  10,  20,  10,  40,  50,  50,9999,9999,9999,9999,
     :       110, 110, 130, 110, 150, 160,9999,9999,9999,9999,
     :        10,  30,  10,  10,  10,  30,  10,  10,  10, 300,
     :        10,  10,  10, 340,  10,  40,  10,  10,  30,  10,
     :       340,  40, 430,  10,9999,9999,9999,9999,9999,9999,
     :       340, 340, 530, 340, 550, 340,9999,9999,9999,9999,
     :       340,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :       340, 340,9999,9999,9999,9999,9999,9999,9999,9999,
     :        10,  20,  40,  10,9999,9999,9999,9999,9999,9999,
     :      9999,  40,  40,  40,  40) ITYPE

*  User Item
      LENGTH = KPDCSZ*KXCHA
      GOTO 9999

*  One Positive Integer
   10 LENGTH=KPDSSZ
      GOTO 9999

*  Zero Length
   20 LENGTH=0
      GOTO 9999

*  Any Integer
   30 LENGTH=KPDISZ
      GOTO 9999

*  Two Positive Integers
   40 LENGTH=2*KPDSSZ
      GOTO 9999

* Message and Escape - One integer + Data record
   50 LENGTH = KPDSSZ + KPDCSZ*KXCHA
      GOTO 9999

*---------------------------------------------------------------------
* Polyline, Polymarker, Fill Area
  110 CONTINUE

*   Suss out number of points
      KNOPTS = KXNUM/2
      IF ( KWKDAT(1,KWKIX) .EQ. POLYSET ) THEN
         KNOPTS = KXNUM/3
      ELSE
         KNOPTS = KXNUM/2
      ENDIF
      LENGTH = KPDSSZ+KPDRSZ*KXNUM

      GOTO 9999
*---------------------------------------------------------------------

* Text
  130 LENGTH = 2*KPDRSZ+KPDSSZ+KPDCSZ*KXCHA
      GOTO 9999

* Cell Array
  150 CONTINUE

*   length = total no of cells + 6 reals with exponents + 2 integers
      KNOPTS = KXNUM
      LENGTH = 6*KPDRSZ + KPDSSZ*(KNOPTS+2)
      GOTO 9999

*   Set Font & Precision
  300 LENGTH=KPDSSZ+KPDISZ
      GOTO 9999

*---------------------------------------------------------------------

* Generalised Drawing Primitive
* For this item, the item length returned is correct for RAL GKS ,
*  September 87
  160 CONTINUE

*   Suss out number of points
      KNOPTS = (KXNUM-1)/2
*  Add one for CGM Circle to make two points
      IF(KWKDAT(1,KWKIX).EQ.CIRCLE) KNOPTS=KNOPTS+1
      LENGTH = KPDISZ + 3*KPDSSZ + 2*KPDRSZ*KNOPTS + KPDCSZ*KXCHA
      GOTO 9999
*---------------------------------------------------------------------

  340 LENGTH = 4*KPDRSZ
      GOTO 9999

* Aspect source flags
  430 LENGTH = 13*KPDSSZ
      GOTO 9999

* Text representation
  530 LENGTH = 4*KPDSSZ+2*KPDRSZ
      GOTO 9999

* Pattern Representation
  550 LENGTH = KPDSSZ*KXNUM
      GOTO 9999

 9999 RETURN

      END
