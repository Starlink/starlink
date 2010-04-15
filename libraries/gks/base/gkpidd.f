C# IL>=a, OL>=0
      SUBROUTINE GKPIDD(ICLASS,NCD,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    WORKSTATION INQUIRY UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Gets default device data record. Assumes device in the given class
*     exists.
*
*  MAINTENANCE LOG
*  ---------------
*     10/01/84  AS    Original version stabilized
*     15/02/84  JL    Make it work (I122)
*     31/07/87  PJWR  Rewritten to use FORTRAN binding style data
*                     records.
*     17/08/87  PJWR  Fixed stack allocations and GKQWDT parameters so
*                     GKQWDT has enough space for enquiring device
*                     details when reading data record.
*
*  ARGUMENTS
*  ---------
*     INP     ICLASS  Device class
*     INP     NCD     Dimension of character array
*     OUT     LDR     Actual length of data record
*     OUT     DATREC  Data record
*
      INTEGER ICLASS, NCD, LDR
      CHARACTER*80 DATREC(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /GKYWCA/  KWKTYP
*     Modify  /GKYERR/  KERROR
*             /GKYSTK/  KSTACK, QSTACK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IOFFI   Integer stack offset for integer data.
*     IOFFR   Real stack offset for real data.
*     IOFFS   Integer stack offset for string table.
*     NID     Table giving number of integer data expected from WDT when
*             obtaining input device information.
*     NRD     Table giving number of real data expected from WDT when
*             obtaining input device information.
*     IDATA   Array for return of integer data from WDT when obtaining
*             input device information.
*     IERROR  Copy of KERROR for use during cleanup.
*     IDUMMY  Dummy integer array for use with GKQWDT.
*     RDUMMY  Dummy reals array for use with GKQWDT.
*
      INTEGER IOFFI, IOFFR, IOFFS, NID(6), NRD(6), IDATA(8), IERROR,
     :        IDUMMY(8)

      REAL RDUMMY(7)

      DATA NID / 6, 8, 6, 6, 6, 8/,
     :     NRD / 6, 4, 7, 4, 4, 4/
*
*  COMMENTS
*  --------
*     This routine can only be called from a workstation driver Inquire
*     Default Device Data entrypoint,  as it assumes the device number
*     is in KWI1.
*
*-----------------------------------------------------------------------

*     Initialise stack pointers for deallocation on error.
      IOFFI = KNIL
      IOFFR = KNIL
      IOFFS = KNIL

*     Get information from WDT for this input device.
      CALL GKQWDT(KWKTYP,KLC+ICLASS-1,KWI1,NID(ICLASS),NRD(ICLASS),
     :            IDATA,RDUMMY)
      IF (KERROR.NE.0) GO TO 999

*     For each type of data,  check that there is data of that type and
*     if there is then allocate stack space for the data and read it
*     from the WDT.  If there is no data of that type then allocate a
*     single stack element of the appropriate type to use as a dummy
*     when creating the data record.  First integers ...
      IF (IDATA(2).GT.0) THEN
      CALL GKSTAL(KINTGS,MAX(IDATA(2),NID(ICLASS)),IOFFI)
      IF (KERROR.NE.0) GO TO 999
        CALL GKQWDT(KWKTYP,KWI1*100+KLCDR+ICLASS-1,KINTGS,IDATA(2),7,
     :              KSTACK(IOFFI),RDUMMY)
      IF (KERROR.NE.0) GO TO 999
      ELSE
      CALL GKSTAL(KINTGS,1,IOFFI)
      IF (KERROR.NE.0) GO TO 999
      END IF
*     ... then reals ...
      IF (IDATA(3).GT.0) THEN
      CALL GKSTAL(KREALS,MAX(IDATA(3),NRD(ICLASS)),IOFFR)
      IF (KERROR.NE.0) GO TO 999
      CALL GKQWDT(KWKTYP,KWI1*100+KLCDR+ICLASS-1,KREALS,8,IDATA(3),
     :              IDUMMY,QSTACK(IOFFR))
      IF (KERROR.NE.0) GO TO 999
      ELSE
      CALL GKSTAL(KREALS,1,IOFFR)
      IF (KERROR.NE.0) GO TO 999
      END IF
*     ... and finally strings.  The total length of string data is held
*     in IDATA(6).
      IF (IDATA(4).GT.0) THEN
      CALL GKSTAL(KINTGS,MAX(IDATA(4)*2,NID(ICLASS)),IOFFS)
      IF (KERROR.NE.0) GO TO 999
        CALL GKQWDT(KWKTYP,KWI1*100+KLCDR+ICLASS-1,KCHARS,IDATA(4)*2,7,
     :              KSTACK(IOFFS),RDUMMY)
      IF (KERROR.NE.0) GO TO 999
      ELSE
      CALL GKSTAL(KINTGS,1,IOFFS)
      IF (KERROR.NE.0) GO TO 999
      END IF

*     Create the data record.
      CALL GKPREC(IDATA(2),KSTACK(IOFFI),IDATA(3),QSTACK(IOFFR),
     :            IDATA(4),KSTACK(IOFFS),NCD,LDR,DATREC)
      IF (KERROR.NE.0) GO TO 999

*     Deallocate stack.  Errors trap to here as well and ordinary stack
*     deallocations take a two assignment hit to maintain a copy of the
*     original error code.
  999 CONTINUE
*     Copy any error code in case we hit errors during cleanup.
      IERROR = KERROR

*     Deallocate stack space as required.
      IF (IOFFS.NE.KNIL) CALL GKSTDA(KINTGS,IOFFS)
      IF (IOFFR.NE.KNIL) CALL GKSTDA(KREALS,IOFFR)
      IF (IOFFI.NE.KNIL) CALL GKSTDA(KINTGS,IOFFI)

*     Reinstate the error code.
      KERROR = IERROR

      RETURN

      END
