C# IL>=a, OL>=0
      SUBROUTINE GKIWSI(IWKIX,IWKTY)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise input from workstation description table.
*
*  MAINTENANCE LOG
*  ---------------
*     25/07/83  AS    Original version stabilized
*     09/11/83  AS    NIPDEV goes into common
*     22/11/83  CJW   Sort out length of char pids
*     22/12/83  AS    Clear bug I10
*     10/12/83  AS    Use delete data record utility GKWDDL
*     03/09/85  RSK   Fix bug I259
*     14/01/86  DRJF  Removed unused local variables.
*                     Corrected variable in call to GKQWDT (S93).
*     30/01/86  MGC   Allocate two extra words for pick device
*     01/06/97  RMK   Merged ICL and RAL versions of this routine.
*     29/07/87  PJWR  Modified to use FORTRAN binding style data
*                     records instead of PIDs.  Extended documentation
*                     and made variable names more meaningful.
*     30/07/87  PJWR  Made the code less dependant on PARAMETER values
*                     by substituting MIN(..), MAX(..) expressions for
*                     loop limits and replacing literal constants in
*                     comparisons involving the input class.
*     31/07/87  PJWR  Corrected order of stack deallocation.
*     14/08/87  PJWR  Altered stack allocation so that GKQWDT had room
*                     to obtain device details when getting data record.
*     09/11/87  RMK   Changed declaration of RDUMMY from INTEGER to REAL.
*     07/07/88  RMK   Removed equivalencing of NRDWDT and NRDWSL (S324).
*     25/05/89  NMH   Changed initial choice number (KCHINN) to 1 from 0
*                     (S355).
*
*  ARGUMENTS
*  ---------
*     INP     IWKIX   Workstation index
*     INP     IWKTY   Workstation type
*
      INTEGER IWKIX, IWKTY
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify  /GKYWDT/  KIPDEV
*             /GKYWSL/  KIPDPT
*             /GKYSTK/  KSTACK, QSTACK
*             /GKYERR/  KERROR
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ICLASS  Input class,  used as loop index.
*     IDEV    Device number,  used as loop index.
*     IOFFI   Integer stack offset.
*     IOFFR   Real stack offset.
*     IERROR  Local copy of KERROR,  used to preserve original error
*             found in case any occur during cleanup.
*     ITYPE   The data type to be read from the WDT when obtaining the
*             input device data record.  Used as a loop index.
*     IIDC    Number of integers to be input when reading device data
*             record.
*     IRDC    Number of reals to be input when reading device data
*             record.
*     IDCOFF  Table of offsets of data type count in device data,
*             indexed by data type.
*     NIDWDT  Table holding number of integers expected from WDT per
*             input class.
*     NIDWSL  Table holding number of integers stored in the input
*             device directory entry per input class.
*     NRDWDT  Table holding number of reals expected from WDT per input
*             class.
*     NRDWSL  Table holding number of reals stored in the input device
*             directory entry per input class.
*     IDATA   Array to hold integer data from WDT.
*     IDVDAT  Array to hold integer data for input device directory,
*             contents derived from those of IDATA.
*     RDVDAT  Array to hold real data for input device directory,
*             contents read direct from WDT.
*     IDRENT  Array to hold entry for device data record subdirectory.
*     RDUMMY  Dummy real array for use in call to GKDRPU.
*
      INTEGER ICLASS, IDEV, IOFFI, IOFFR, IERROR, ITYPE, IIDC, IRDC,
     :        IDCOFF(0:2), NIDWDT(6), NRDWDT(6), NIDWSL(6), NRDWSL(6),
     :        IDATA(8), IDVDAT(10), IDRENT(2)

      REAL RDVDAT(7), RDUMMY(1)

      DATA IDCOFF / 3, 2, 4/,
     :     NIDWDT / 6, 8, 6, 6, 6, 8/,
     :     NIDWSL / 5,10, 4, 6, 9, 9/,
     :     NRDWDT / 6, 4, 7, 4, 4, 4/,
     :     NRDWSL / 6, 4, 7, 4, 4, 4/
*
*  COMMENTS
*  --------
*     Details of the format of data returned from the WDT file by GKQWDT
*     are in Chapter 14 of the Workstation Driver Manual for RAL GKS.
*     The data formats changed when PIDs were removed from the WDT in
*     July 1987,  so any manual from before that date will not contain
*     the correct information.
*
*     Each input device data record is held in a subdirectory with three
*     entries,  one for integer data, one for real data and one for
*     string data.  The keys for these entries are the type PARAMETERs
*     from 'GKHP.PAR'.  Each entry consists of a data count and a heap
*     offset.  If there are no data of a given type,  the count will be
*     0 and the offset will be KNIL.
*
*     The offset in the integer entry is the index of the first integer
*     datum on KHP.
*
*     The offset in the real entry is the index of the first real datum
*     on QHP.
*
*     The offset in the string entry is the index of the first item in
*     a string description table stored on KHP.  This table consists of
*     pairs of [String Length, Offset],  stored seqentially.  The offset
*     is the index of the first character in the string which is stored
*     as a series of ASCII codes on KHP.
*
*     WARNING: The bounds and contents of the tables NIDWSL, NIDWDT,
*              NRDWSL and NRDWDT are dependant on the values of the
*              input class parameters GLOCAT, etc. in 'GKS.PAR'.
*
*     WARNING: The bounds and contents of the table IDCOFF are dependant
*              on the values of the data type parameters KREALS, etc. in
*              'GKHP.PAR'.
*
*-----------------------------------------------------------------------

*     Initialise stack pointers.
      IOFFI = KNIL
      IOFFR = KNIL

*     Initialise input class directory pointers.
      DO 10 ICLASS=MIN(GLOCAT,GSTROK,GVALUA,GCHOIC,GPICK,GSTRIN),
     :           MAX(GLOCAT,GSTROK,GVALUA,GCHOIC,GPICK,GSTRIN)
        KIPDPT(ICLASS,IWKIX) = KNIL
   10 CONTINUE

*     For each input class,  initialise the input class directory.
      DO 40 ICLASS=MIN(GLOCAT,GSTROK,GVALUA,GCHOIC,GPICK,GSTRIN),
     :           MAX(GLOCAT,GSTROK,GVALUA,GCHOIC,GPICK,GSTRIN)
*       If the workstation has devices in this class,  create an input
*       class directory.
        IF (KIPDEV(ICLASS,IWKIX).GT.0) THEN
          CALL GKDRCR(KIPDEV(ICLASS,IWKIX),NIDWSL(ICLASS),
     :                NRDWSL(ICLASS),KIPDPT(ICLASS,IWKIX))
          IF (KERROR.NE.0) GO TO 999
*         Get information from WDT for each input device
          DO 30, IDEV = 1, KIPDEV(ICLASS,IWKIX)
            CALL GKQWDT(IWKTY,KLC+ICLASS-1,IDEV,NIDWDT(ICLASS),
     :                  NRDWDT(ICLASS),IDATA,RDVDAT)
*           IF (KERROR.NE.0) GO TO 999
*           Set things for device directory which do not come from WDT.
            IDVDAT(KIPOPM) = GREQU
            IDVDAT(KIPE)   = GECHO
            IDVDAT(KIPPRT) = 1
*           Check data counts from WDT to see if there is a data record,
*           and create a subdirectory then process the data record if
*           there is.
            IF (IDATA(2).GT.0.OR.IDATA(3).GT.0.OR.IDATA(4).GT.0) THEN
              CALL GKDRCR(3,2,0,IDVDAT(KIPD))
              IF (KERROR.NE.0) GO TO 999
*             Allocate stack space as required.  As GKQWDT requests the
*             device details before accessing the data record,  enough
*             space for these must always be allocated.
              CALL GKSTAL(KINTGS,
     :                    MAX(IDATA(2),IDATA(4)*2,NIDWDT(ICLASS)),IOFFI)
            IF (KERROR.NE.0) GO TO 999
              CALL GKSTAL(KREALS,
     :                    MAX(IDATA(3),NRDWDT(ICLASS)),IOFFR)
              IF (KERROR.NE.0) GO TO 999
*             Set up the entry for each data type.  If there is data of
*             the given type,  get it from the WDT and copy it to the
*             heap then put the count and heap offset in the entry, if
*             there is no data,  make the entry [0, KNIL].
            DO 20, ITYPE = MIN(KREALS,KINTGS,KCHARS),
     :                   MAX(KREALS,KINTGS,KCHARS)
            IDRENT(1) = IDATA(IDCOFF(ITYPE))
            IDRENT(2) = KNIL
            IF (IDRENT(1).GT.0) THEN
              IF (ITYPE.EQ.KINTGS) THEN
                IIDC = MAX(IDRENT(1),NIDWDT(ICLASS))
                IRDC = NRDWDT(ICLASS)
              ELSE IF (ITYPE.EQ.KREALS) THEN
                IIDC = NIDWDT(ICLASS)
                IRDC = MAX(IDRENT(1),NRDWDT(ICLASS))
              ELSE
                IIDC = IDRENT(1)*2
                IRDC = NRDWDT(ICLASS)
              END IF
              CALL GKQWDT(IWKTY,IDEV*100+KLCDR+ICLASS-1,ITYPE,
     :                        IIDC,IRDC,KSTACK(IOFFI),QSTACK(IOFFR))
              IF (KERROR.NE.0) GO TO 999
              IF (ITYPE.EQ.KINTGS) THEN
                CALL GKHPAL(IDRENT(1),KINTGS,IDRENT(2))
                IF (KERROR.NE.0) GO TO 999
                CALL GKHPPI(IDRENT(2),0,IDRENT(1),KSTACK(IOFFI))
                IF (KERROR.NE.0) GO TO 999
              ELSE IF (ITYPE.EQ.KREALS) THEN
                CALL GKHPAL(IDRENT(1),KREALS,IDRENT(2))
                IF (KERROR.NE.0) GO TO 999
                CALL GKHPPR(IDRENT(2),0,IDRENT(1),QSTACK(IOFFR))
                IF (KERROR.NE.0) GO TO 999
              ELSE
                CALL GKHPAL(IDRENT(1)*2,KINTGS,IDRENT(2))
                IF (KERROR.NE.0) GO TO 999
                  CALL GKHPPI(IDRENT(2),0,IDRENT(1)*2,KSTACK(IOFFI))
                IF (KERROR.NE.0) GO TO 999
              END IF
            END IF
            CALL GKDRPU(IDVDAT(KIPD),ITYPE,2,0,IDRENT,RDUMMY)
   20         CONTINUE
*             Deallocate stack space.
              IF (IOFFR.NE.KNIL) CALL GKSTDA(KREALS,IOFFR)
              IF (KERROR.NE.0) GO TO 999
              IF (IOFFI.NE.KNIL) CALL GKSTDA(KINTGS,IOFFI)
              IF (KERROR.NE.0) GO TO 999
            ELSE
              IDVDAT(KIPD) = KNIL
            END IF
*           Fill other entries specific to input class
            IF (ICLASS.EQ.GLOCAT) THEN
              IDVDAT(KLCINN) = 0
            ELSE IF (ICLASS.EQ.GSTROK) THEN
              IDVDAT(KSKINN) = 0
              IDVDAT(KSKINP) = 0
              IDVDAT(KSKINX) = KNIL
              IDVDAT(KSKINY) = KNIL
              IDVDAT(KSKINB) = IDATA(8)
              IDVDAT(KSKMXB) = IDATA(7)
            ELSE IF (ICLASS.EQ.GCHOIC) THEN
              IDVDAT(KCHINN) = 1
            IDVDAT(KCHINS) = GNCHOI
            ELSE IF (ICLASS.EQ.GPICK) THEN
              IDVDAT(KPCINS) = GNPICK
              IDVDAT(KPCISG) = 1
              IDVDAT(KPCINI) = 1
              IDVDAT(KPCIWI) = KNIL
              IDVDAT(KPCIWR) = KNIL
            ELSE IF (ICLASS.EQ.GSTRIN) THEN
              IDVDAT(KSTINL) = 0
              IDVDAT(KSTINS) = KNIL
              IDVDAT(KSTINB) = IDATA(8)
              IDVDAT(KSTICP) = 1
              IDVDAT(KSTMXB) = IDATA(7)
            END IF
*           Fill directory entry for this input device
            CALL GKDRPU(KIPDPT(ICLASS,IWKIX),IDEV,NIDWSL(ICLASS),
     :                  NRDWSL(ICLASS),IDVDAT,RDVDAT)
   30     CONTINUE
        ENDIF

   40 CONTINUE

      RETURN

*     All errors trap to here for cleanup.
  999 CONTINUE
*     Save error number in case we hit more errors during cleanup.
      IERROR = KERROR
*     Deallocate stack if necessary.
      IF (IOFFR.NE.KNIL) CALL GKSTDA(KREALS,IOFFR)
      IF (IOFFI.NE.KNIL) CALL GKSTDA(KINTGS,IOFFI)
*     For each input class,  recursively delete the input class
*     directory.
      DO 1002, ICLASS = MIN(GLOCAT,GSTROK,GVALUA,GCHOIC,GPICK,GSTRIN),
     :                MAX(GLOCAT,GSTROK,GVALUA,GCHOIC,GPICK,GSTRIN)
        IF (KIPDPT(ICLASS,IWKIX).NE.KNIL) THEN
          DO 1001, IDEV = 1, KIPDEV(ICLASS,IWKIX)
            KERROR = 0
            CALL GKDRGE(KIPDPT(ICLASS,IWKIX),IDEV,10,7,IDVDAT,RDVDAT)
            IF (KERROR.EQ.0) CALL GKWDDL(IDVDAT(KIPD))
 1001     CONTINUE
          CALL GKDRDL(KIPDPT(ICLASS,IWKIX))
        END IF
 1002 CONTINUE
      KERROR = IERROR

      RETURN

      END
