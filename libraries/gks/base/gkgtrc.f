C# IL>=a, OL>=0
      SUBROUTINE GKGTRC ( IREC,ITEM,NINTA,NREALA,NQIR,NQRR,
     :                                 INTA, REALA)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Get an item from the WDT, which is the ITEMth item of the WDT entry,
*  whose record number is IREC.
*  If the item is not completely within record IREC, further records
*  are read until the whole item is read.
*  Data are returned in INTA and REALA
*
*  MAINTENANCE LOG
*  ---------------
*     10/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     16/04/84  RSK   Replace call to GKSBUG with error 2005, with
*                     KERROR = 1017. Update error list in routine
*                     comments.
*     22/01/87  JCS   IS conversion. Error number changes.
*     01/02/90  KEVP  Made it work with a multi-record chain with the
*                     item or a later item crossing a record boundary
*                     (S322).  Improved commenting.
*
*  ARGUMENTS
*  ---------
*     INP   IREC   Record number (first record in chain)
*     INP   ITEM   Required item number (place in record chain)
*     INP   NINTA  Dimension of array INTA
*     INP   NREALA Dimension of array REALA
*     OUT   NQIR   Number of integer values found
*     OUT   NQRR   Number of real values found
*     OUT   INTA   Returned integer data
*     OUT   REALA  Returned real data
*
      INTEGER IREC, ITEM, NINTA, INTA(*), NREALA, NQIR, NQRR
      REAL REALA(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKFLS/    KWDFLU
*     Modify /GKYERR/   Set error status
*
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
*
*  LOCALS
*  ------
*     JREC    Current record number
*     J       Loop index
*     I       Implied DO loop index
*     JITEM   Relative Item Position
*     NITEM   Number of items in current record
*     INEXT   Record number of the next record of a multi record chain.
*     MORE    Indicates overflow into next record (=1). Last item cut.
*     IISZ    Number of integers read
*     IRSZ    Number of reals read
*     IIEND   Number of integers that can still be read into NINTA
*     IREND   Number of reals that can still be read into NREALA
*     IGASH   Dummy to skip over unwanted integers in the input record
*     RGASH   Dummy to skip over unwanted reals in the input record
*     LDONE   TRUE when we have the correct record OR have an error
*
      INTEGER JREC, I, J, JITEM, NITEM, INEXT, MORE, IISZ,
     :        IRSZ, IIEND, IREND, IGASH
      REAL RGASH
      LOGICAL LDONE
*
*  STREAMS USED
*  ------------
*     Stream   Connection   Comment
*     KWDFLU   disk         read WDT file
*
*  ERRORS
*  ------
*   307  I/O error has occured when reading WDT
*  2001  Output parameter size insufficient
* -1017  Entry not found in list
*
*  COMMENTS
*  --------
*  Format of a record -
*
*  KITEM  Number of items and cut items in this record.
*  INEXT  Next record number
*
*  items and cut items  "KITEM" lists each consisting of -
*
*     KINT   Number of integer data per item. (may be zero)
*     KREAL  Number of real data per item. (may be zero)
*     KDATA  Integer data. (only if KINT > 0)
*     QDATA  Real data. (only if KREAL > 0)
*
*---------------------------------------------------------------------



*     Prepare for first record.
      JREC = IREC
      JITEM = ITEM
      NQIR = 0
      NQRR = 0
      LDONE = .FALSE.

* --- repeat { until LDONE } per record in chain
 1    CONTINUE

         IF (JREC .GT. 0) THEN

            IIEND = NINTA - NQIR
            IREND = NREALA - NQRR
*     J  Index of (cut) item being read
*        I  Index of Number in (cut) item being read
            READ(KWDFLU,REC=JREC,IOSTAT=KERROR) NITEM, INEXT, MORE,
     :            (    IISZ, IRSZ,
     :                (INTA(I+NQIR),  I =       1, MIN(IISZ,IIEND)),
     :                (IGASH,         I = IIEND+1, IISZ),
     :                (REALA(I+NQRR), I =       1, MIN(IRSZ,IREND)),
     :                (RGASH,         I = IREND+1, IRSZ),
     :                                     J=1,MIN(JITEM,NITEM))
*        Reading terminates if either the required (cut) item is read
*        or the last (cut) item in record is read.
            IF (KERROR .NE. 0) THEN

*              Quit loop if error
               LDONE = .TRUE.
               KERROR = 307

            ELSE IF (JITEM .GT. NITEM) THEN

*              Item not found in current record - prepare for next one.
               JITEM = JITEM - NITEM + MORE
               JREC = INEXT

            ELSE
*              Item found and read - may be cut item

               LDONE = (JITEM .LT. NITEM) .OR. (MORE .EQ. 0)
*                       not last item           last item but not cut

*              Prepare for next record - whether to be read or not
               JITEM = 1
               JREC = INEXT
               NQIR = NQIR + IISZ
               NQRR = NQRR + IRSZ

               IF ((IRSZ.GT.IREND) .OR. (IISZ.GT.IIEND)) THEN

*                 We have not read whole item due to lack of space.
                  LDONE = .TRUE.
                  KERROR = 2001

               END IF

            ENDIF

         ELSE
*           Non-positive record number
            LDONE = .TRUE.
           KERROR = -1017

         ENDIF

* --- until LDONE move to next record

      IF (.NOT. LDONE) GOTO 1

      END
