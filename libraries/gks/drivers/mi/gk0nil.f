C# IL>=a, OL>=0
*
*
*
*
*
      SUBROUTINE GK0NIL (ITYPE,ILEN,LENGTH)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION
*  AUTHOR:             DSG
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Called at GTITM entrypoint to calculate the internal
*     packed length of the RDITM data record.
*
*  MAINTENANCE LOG
*  ---------------
*     01/04/83  DSG   Original version stabilized
*     19/06/86  RMK   Maintenance log incorporated into main driver routine
*     19/06/89  RTP   Check for Sun GKSM and set length correctly
*
*  ARGUMENTS
*  ---------
*     INP   ITYPE  Item type
*     INP   ILEN   External item data record length in bytes
*     OUT   LENGTH - RDITM data record length in characters
*
      INTEGER ITYPE,ILEN,LENGTH
*
*  COMMON BLOCK USAGE
*  ------------------
* Read   /GKYWCA/   KWKIX - Workstation index
* Modify /GKYWKD/   KWKDAT - Usage is described in GK0NWD
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkpid.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
*     IFLD    Length of integer fields in the external metafile
*     IRFLD   Ditto, real fields
*     I       DO loop variable
*     STRING
*     NCH
*
      INTEGER IFLD,IRFLD,NCH,I

* Different types of metafile to handle
      INTEGER IRAL, ISUN, IUNI, IGRAL
      PARAMETER (IRAL = 1, ISUN = 2, IUNI = 3, IGRAL = 4)
*
      CHARACTER STRING*(80)
      CHARACTER SFMTI
      CHARACTER*7 SFMT(2)
*
*  ALGORITHM
*  ---------
*
*  The item data will not be packed into the user's character array
*  by using the Fortran binding's PACK utility, but by using our own
*  internal packing utilities:
*  GKPKSI packs each 'short' integer (range 1 - 16383) into two
*  bytes, GKPKI packs each integer into five bytes,
*  GKPKR packs each real into five bytes, and GKPKC
*  packs each integer into one byte of the character array.
*
*  The packed length is then calculated as a function  of the
*  external item data length ILEN, and the external and internal
*  real, integer, and character field lengths.
*
*  There are two exceptions:
*
*  In the case of Escape and GDP it is necessary to input enough
*  of the item data record to reveal the lengths of the real and
*  the integer data in the record.
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


      IFLD=KWKDAT(4,KWKIX)
      IRFLD=KWKDAT(5,KWKIX)

      WRITE(SFMTI,'(I1)') IFLD
      SFMT(1) = '(3I'//SFMTI//')'
      SFMT(2) = '(4I'//SFMTI//')'

      LENGTH=0

      IF(ITYPE.EQ.0) GOTO 20

      GOTO (  10,  20,  10,  40,  50,  60,9999,9999,9999,9999,
     :       110, 110, 130, 110, 150, 160,9999,9999,9999,9999,
     :        10,  30, 230,  10,  10,  30, 230,  10,  10, 300,
     :       230, 230,  10, 340,  10,  40,  10,  10, 390,  10,
     :       340, 420, 430,  10,9999,9999,9999,9999,9999,9999,
     :       510, 510, 530, 540, 550, 560,9999,9999,9999,9999,
     :       340,9999,9999,9999,9999,9999,9999,9999,9999,9999,
     :       340, 340,9999,9999,9999,9999,9999,9999,9999,9999,
     :        10,  20,  40,  10,9999,9999,9999,9999,9999,9999,
     :       910,  40,  40, 940,  40,1000) ITYPE

   10 LENGTH=KPDSSZ
      GOTO 9999

   20 LENGTH=0
      GOTO 9999

   30 LENGTH=KPDISZ
      GOTO 9999

   40 LENGTH=2*KPDSSZ
      GOTO 9999

* Message
C
C  Don't count Number of characters for Sun, They've got it wrong
C
   50 IF ( KWKDAT(16,KWKIX) .EQ. ISUN ) THEN
         LENGTH = KPDSSZ+KPDCSZ*ILEN
      ELSE
         LENGTH = KPDSSZ+KPDCSZ*(ILEN-IFLD)
      ENDIF
      GOTO 9999

* Escape
* The function identifier and
* the lengths of the integer and real arrays are read in and
* kept in COMMON for the RDITM entrypoint.
   60 CONTINUE
      NCH=3*IFLD
      CALL GK0NBU(2,NCH,STRING)
      IF (KERROR.NE.0) GOTO 9999
      READ (STRING(:NCH),SFMT(1)) (KWKDAT(5+I,KWKIX),I=1,3)
      LENGTH = KPDISZ +  KPDSSZ*(2+KWKDAT(7,KWKIX)) +
     :         KPDRSZ*KWKDAT(8,KWKIX)
      GOTO 9999

* Polyline, Polymarker, Fill Area
  110 LENGTH = KPDSSZ+KPDRSZ*(ILEN-IFLD)/IRFLD
      GOTO 9999

* Text
  130 LENGTH = 2*KPDRSZ+KPDSSZ+KPDCSZ*(ILEN-2*IRFLD-IFLD)
      GOTO 9999

* Cell Array
  150 LENGTH = 6*KPDRSZ+KPDSSZ*(2+(ILEN-6*IRFLD-2*IFLD)/IFLD)
      GOTO 9999

* Generalised Drawing Primitive
* The identifier and lengths are read into COMMON for RDITM
* GDP id KWKDAT(6,KWKIX)
* GDP number of points KWKDAT(7,KWKIX)
* GDP number of integers in data record KWKDAT(8,KWKIX)
* GDP number of reals in data record KWKDAT(9,KWKIX)

  160 NCH = 4*IFLD
      CALL GK0NBU(2,NCH,STRING)
      IF(KERROR.NE.0) GOTO 9999
      READ(STRING(:NCH),SFMT(2))(KWKDAT(5+I,KWKIX),I=1,4)
      LENGTH = KPDSSZ*(3+KWKDAT(8,KWKIX)) + KPDRSZ*(KWKDAT(7,KWKIX)*2
     : +KWKDAT(9,KWKIX))+KPDISZ
      GOTO 9999

  230 LENGTH = KPDRSZ
      GOTO 9999

* Set text font and precision
  300 LENGTH = KPDISZ+KPDSSZ
      GOTO 9999

  340 LENGTH = 4*KPDRSZ
      GOTO 9999

* Set fill area style index
  390 LENGTH = KPDISZ
      GOTO 9999

* Set pattern reference point
  420 LENGTH = 2*KPDRSZ
      GOTO 9999

* Aspect source flags
  430 LENGTH = 13*KPDSSZ
      GOTO 9999

  510 LENGTH = 3*KPDSSZ+KPDRSZ
      GOTO 9999

* Text representation
  530 LENGTH = 3*KPDSSZ+2*KPDRSZ+KPDISZ
      GOTO 9999

* Fill area representation
  540 LENGTH = 3*KPDSSZ+KPDISZ
      GOTO 9999

* Pattern Representation
  550 LENGTH = KPDSSZ*(ILEN/IFLD)
      GOTO 9999

* Colour representation
  560 LENGTH = KPDSSZ+3*KPDRSZ
      GOTO 9999

* Set Segment Transformation
  910 LENGTH = KPDSSZ+6*KPDRSZ
      GOTO 9999

* Set segment priority
  940 LENGTH = KPDSSZ+KPDRSZ
      GOTO 9999

* User Item
 1000 LENGTH = KPDCSZ*ILEN-KPDSSZ

 9999 RETURN
      END
