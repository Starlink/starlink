C# IL>=a,OL>=0
      SUBROUTINE GKQWDT(IWKTY, MAJOR, MINOR, NINTA, NREALA, INTA, REALA)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
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
*     Input item from WDT file. The item is identified by a MAJOR
*     and MINOR key
*
*  MAINTENANCE LOG
*  ---------------
*     10/05/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     11/08/83  AS    GKOPIO to GKIOOP
*      7/11/83  CJW   Correct Input data record
*     16/11/83  AS    Declare NDEV
*      3/09/85  DLT   Cache directory records
*      9/04/86  DLT   Moved reading of master and workstation directories
*                     to seperate routine
*      2/06/86  DCS   Tidied and removed unused label.
*                     Removed unused local IWDT.
*     22/01/87  JCS   IS conversion. Error changes.
*     28/07/87  PJWR  Modified code dealing with input device data to
*                     use data records because PIDs have gone.
*     01/02/90  KEVP  Allowed the largest itemsize ITEMSZ  to be big
*                     enough to accomodate KMXWKT workstations (S322).
*                     Improved commenting.
*
*  ARGUMENTS
*  ---------
*     INP   IWKTY  Workstation type index
*     INP   MAJOR  Major key to identify item (WDT entry)
*     INP   MINOR  Minor key to identify item (place in entry)
*     INP   NINTA  Dimension of array INTA
*     INP   NREALA Dimension of array REALA
*     OUT   INTA   Returned integer data
*     OUT   REALA  Returned real data
*
      INTEGER IWKTY, MAJOR, MINOR, NINTA, INTA(*), NREALA
      REAL REALA(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKOPS/    KOPS
*     Read   /GKWDT/    CWDTFN
*     Read   /GKFLS/    KWDFLU, KWDTRL
*     Modify /GKYERR/   KERROR
*     Modify /GKFLS/    KWDFLS, KEMFLS
*     Modify /GKSTK/    KSTACK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkops.cmn'
*
*  LOCALS
*  ------
*     IDROFF  Offset of Data record pointer (P)
*     ITEMSZ  Largest item size
*     NMAJOR  Major name (100 * NDEV + NMAJOR = MAJOR)
*     NDEV    Device number
*     IMAJOR  MAJOR key used for second probe (a record number)
*     IMINOR  Local copy of MINOR (may be changed) (an item number)
*     ISET    Offset returned by stack allocation
*     J       Loop index
*     LSTR    Total length of string data in input device data record
*     NSTR    Number of strings in input device data record
*     ISTR    String length index for input device data records
*     IHPOFF  Heap offset
*     NQIR    Number of integer values found
*     NQRR    Number of real values found
*
      INTEGER   IDROFF
      PARAMETER(IDROFF = 5)
      INTEGER   ITEMSZ
      INTEGER IMINOR, NMAJOR, IMAJOR, NDEV, ISET, J, LSTR, NQIR, NQRR,
     :        ISTR, NSTR, IHPOFF
*
*  ERRORS
*  ------
*       2001  Output parameter size insufficient.
*      -1007  Unable to access WDT file.
*      -2004  Documented condition to be satisfied by parameter(s) of
*             internal routine is not satisfied.
*      -2006  Value of internal enumerated type is invalid.
*
*  COMMENTS
*  --------
*     The WDT consists of of records containing  variable sized items
*     (accessed by GKGTRC) which may span over record boundaries.
*
*     A cut item is part of an item cut from the rest by a record
*     boundary. It has the same structure as a whole item not cut
*     by a record boundary.
*
*     A record contains -
*
*       KITEM       Number of items and cut items in this record
*       INEXT       Next record number
*       MORE        Record overflow flag (last item whole=0, cut=1)
*       (cut) item  KITEM lists each consisting of -
*
*         KINT   Number of integer data in list. (may be zero)
*         KREAL  Number of real data in list. (may be zero)
*         KDATA  Integer data. (only if KINT > 0)
*         QDATA  Real data. (only if KREAL > 0)
*
*     Master directory
*       Record 1 is the directory, it contains only one item (item 1).
*       The directory only has integer data (NQIR=0) consisting of
*       pairs:
*             Workstation type
*             WDT directory record number
*
*     WDT directory
*       One for each workstation type. It contains only one item,
*       containing pointers (record numbers) to the bundle tables for
*       that workstation type.
*
*     Bundle Entries
*       One for each of WDT, PL, PM, TX, FA, PA(Size), LC, SK, VL, CH,
*       PC, ST, PA,  - for each workstation type. WDT has only one item
*       and contains general information. CT is indexed from 0 and so
*       the minor key must be incremented by one.
*
*     Data Records
*       Each of LC, SK, VL, CH, PC, ST refer to a Data Record by storing
*       a record number as there IDROFF integer.
*
*-----------------------------------------------------------------------
*
*     Set item size - large enough for master directory item
*                     and any item upto one record in length
      ITEMSZ = MAX(2*KMXWKT, KWDTRL/KNIBYT - 4)
*
*     Derive NMAJOR, NDEV, IMINOR from MAJOR and MINOR
      IF (MAJOR.EQ.KWDT.OR.MAJOR.EQ.KWKLST) THEN
*       WDT and index only have one item
        IMINOR = 1
      ELSE IF (MAJOR.EQ.KPCTB) THEN
*       Colour table indexed from 0 not 1
        IMINOR = MINOR + 1
      ELSE
        IMINOR = MINOR
      END IF

      IF (MAJOR.GE.100) THEN
*       Input device data record - derive device number
        NDEV = MAJOR / 100
        NMAJOR = MOD(MAJOR,100)
*       Derive item number in device data record from data type
*       requested.
        IF (MINOR.EQ.KINTGS) THEN
          IMINOR = 1
        ELSE IF (MINOR.EQ.KREALS) THEN
          IMINOR = 2
        ELSE IF (MINOR.EQ.KCHARS) THEN
          IMINOR = 3
        ELSE
*         Force error -2006 in error checking section below.
          IMINOR = KNIL
        END IF
      ELSE
        NMAJOR = MAJOR
      END IF

*     Main argument checking section - if we get to the ELSE then its OK
      IF ((NINTA.LT.0).OR.(NREALA.LT.0)) THEN
*       Insufficient space for data.
        KERROR = 2001
      ELSE IF ((NMAJOR.LT.KWKLST).OR.(NMAJOR.GT.KSTDR)) THEN
*       Major key is invalid.
        CALL GKBUG(-2006,'GKQWDT')
      ELSE IF ((NMAJOR.NE.KWKLST).AND.(IMINOR.LT.1)) THEN
*       Minor key is invalid.
        CALL GKBUG(-2004,'GKQWDT')
      ELSE

*       Obtain information from WDT file if possible.

*       Access WDT to find record number of required item
        CALL GKLWDT(IWKTY, NMAJOR, NREALA, REALA, IMAJOR)
        IF (KERROR.EQ.0) THEN
*         Allocate stack.
          CALL GKSTAL(KINTGS, ITEMSZ, ISET)
          IF (KERROR.EQ.0) THEN
*           Check that the required information is available
            IF (IMAJOR .LT. 1) THEN
              CALL GKBUG(-2005,'GKQWDT')
            ELSE IF (NMAJOR.EQ.KWKLST) THEN
*             List of workstation types is a special case
              CALL GKGTRC(IMAJOR, IMINOR, ITEMSZ, NREALA,
     :                    NQIR, NQRR, KSTACK(ISET), REALA )
              IF (KERROR.EQ.0) THEN
                DO 100 J = 1, NQIR/2
                  INTA(J) = KSTACK(ISET+2*(J-1))
  100           CONTINUE
                DO 200 J = (NQIR/2) + 1,NINTA
                  INTA(J) = KNIL
  200           CONTINUE
            END IF
          ELSE IF (NMAJOR.LT.KLCDR) THEN
*             WDT, bundle or input device entry.
            CALL GKGTRC(IMAJOR, IMINOR, NINTA, NREALA,
     :                    NQIR, NQRR, INTA, REALA)
          ELSE
*             Input device data record.  First get device details.
              CALL GKGTRC(IMAJOR, NDEV, ITEMSZ, NREALA,
     :                    NQIR, NQRR, KSTACK(ISET), REALA)
*             Obtain data record's WDT record number.
              IMAJOR = KSTACK(ISET+IDROFF-1)
              IF (IMAJOR .LT. 1) THEN
                CALL GKBUG(-2005,'GKQWDT')
              ELSE IF (MINOR.EQ.KINTGS.OR.MINOR.EQ.KREALS) THEN
*               No extra work for integer and real data.
                CALL GKGTRC(IMAJOR, IMINOR, NINTA, NREALA,
     :                      NQIR, NQRR, INTA, REALA)
              ELSE
*               String data returned as [length, heap offset] pairs,
*               and total length of string data is held in the device
*               record at IDROFF + 1.  First get the total length and
*               number of strings,  the latter held at IDROFF - 1.
            NSTR = KSTACK(ISET + IDROFF - 2)
            LSTR = KSTACK(ISET + IDROFF)
*               Get more stack space if necessary.  It shouldn't be.
            IF (LSTR.GT.ITEMSZ) THEN
              CALL GKSTDA(KINTGS, ISET)
              IF (KERROR.EQ.0) CALL GKSTAL(KINTGS, LSTR, ISET)
            ENDIF
*               Check for errors in case we reallocated stack.
            IF (KERROR.EQ.0) THEN
*                 Get string data.
                  CALL GKGTRC(IMAJOR, IMINOR, LSTR, NREALA,
     :                        NQIR, NQRR, KSTACK(ISET), REALA)
*                 Copy each string onto the heap and place the length
*                 and heap offset in INTA. INTA is assumed to be large
*                 enough ...
              ISTR = ISET
              DO 300 J = 1, NSTR * 2, 2
                CALL GKHPAL(KSTACK(ISTR), KINTGS, IHPOFF)
                IF (KERROR.NE.0) GO TO 400
                CALL GKHPPI(IHPOFF, 0, KSTACK(ISTR), KSTACK(ISTR+1))
                IF (KERROR.NE.0) GO TO 400
                INTA(J) = KSTACK(ISTR)
                INTA(J + 1) = IHPOFF
                ISTR = ISTR + KSTACK(ISTR) + 1
  300             CONTINUE
  400             CONTINUE
            ENDIF
              END IF
            END IF
*           Stack no longer required
          CALL GKSTDA(KINTGS, ISET)
        END IF
      END IF
      END IF

      RETURN

      END
