      SUBROUTINE GKINIP(IPCLS,NID,IDAT,NRD,RX,RY,NCD,STR)
*
* COPYRIGHT (C) SERC 1987
*
*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:    WORKSTATION UTILITY
*  Author:             ACA
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Stores input device data in workstation state list.
*
*  MAINTENANCE LOG
*  ---------------
*     06/02/87  ACA   Original version created. Based on gksipm.f.
*     08/03/87  ACA   Removed storage of data record pending PID
*                     decision.
*     12/03/87  ACA   Added code to store input buffer size.
*     14/07/87  PJWR  INCLUDEd GKHP.PAR'.
*     28/07/87  ACA   Fixed storing of data, should use device number
*                     and not PET.
*     12/08/87  PJWR  Updated to GKS 7.4, added code to store data
*                     records now PIDs no longer used as data records.
*     18/08/87  PJWR  Implemented better cleanup to reduce "follow on"
*                     errors.
*     30/09/87  PJWR  Added error checks that had been omitted.
*
* ARGUMENTS
* ---------
*     INP     IPCLS   Class of input being initialised
*     INP     NID     Number of integers in IDAT
*     INP     IDAT    Integer data array
*     INP     NRD     Number of reals in arrays RX and RY
*     INP     RX,RY   Real arrays of data
*     INP     NCD     Number of elements in STR
*     INP     STR     Data record
*
      INTEGER IPCLS, NID, IDAT(NID), NRD, NCD
      REAL RX(NRD), RY(NRD)
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /GKYWCA/ KWI1..5, QWR1..7, KWKIX
*             /GKYWSL/ KIPDPT
*     Modify  /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NIWSL   Table giving number of integer data to/from WSL by input
*             class.
*     NRWSL   Table giving number of real data to/from WSL by input
*             class.
*     IDVDAT  Device directory entry integers.
*     RDVDAT  Device directory entry reals.
*     IDRENT  Array to hold device data record subdirectory entry.
*     NIDR    Number of integers in data record.
*     IID     Heap offset of integer data from data record.
*     NRDR    Number of reals in data record.
*     IRD     Heap offset of real data from data record.
*     NSDR    Number of strings in data record.
*     IST     Heap offset of string table derived from data record.
*     RDUMMY  Dummy real array for use with GKDRPU.
*     IERROR  Holds copy of error code during cleanup.
*
      INTEGER NIWSL(6), NRWSL(6), IDVDAT(10), IDRENT(2), NIDR, IID,
     :        NRDR, IRD, NSDR, IST, IERROR
      REAL RDVDAT(7), RDUMMY(1)
      DATA NIWSL/5,10,4,6,9,9/
      DATA NRWSL/6,4,7,4,4,4/
*
*  ERRORS
*  ------
*     140   Specified input device doesn't exist
*
*  COMMENTS
*  --------
*     Written by: A C Arnold, University of Manchester Computer Graphics
*     Unit, Oxford Road, Manchester M13 9PL Tel: 061-273 7121 x 5405
*
*     This routine must be called from the Initialise Device entrypoints
*     of a workstation driver,  as it assumes the device number is in
*     KWI1.
*
*-----------------------------------------------------------------------

*     Get the input device directory.
      CALL GKDRGE(KIPDPT(IPCLS,KWKIX),KWI1,NIWSL(IPCLS),NRWSL(IPCLS),
     :            IDVDAT,RDVDAT)
      IF (KERROR.EQ.0) THEN

*       INPUT CLASS INDEPENDANT DATA.
*       -----------------------------
*       Store echo area.
        RDVDAT(KIPEXL) = QWR1
        RDVDAT(KIPEXR) = QWR2
        RDVDAT(KIPEYB) = QWR3
        RDVDAT(KIPEYT) = QWR4
*       Store prompt and echo type.
        IDVDAT(KIPPRT) = KWI2
*       Store the device data record.  This is done by deleting the old
*       device data record subdirectory,  then creating a new one with
*       the supplied data record,  unless the data record is empty.
        CALL GKWDDL(IDVDAT(KIPD))
      IF (KERROR.NE.0) GO TO 9999
*       Unpack the data record onto the heap.
        CALL GKUREC(NCD,STR,NIDR,IID,NRDR,IRD,NSDR,IST)
      IF (KERROR.NE.0) GO TO 9999
*       If there is any data,  set up the directory,  otherwise set
*       the directory pointer to KNIL.
        IF (NIDR.GT.0.OR.NRDR.GT.0.OR.NSDR.GT.0) THEN
        CALL GKDRCR(3,2,0,IDVDAT(KIPD))
        IF (KERROR.NE.0) GO TO 9999
*         Set up integer entry.
        IDRENT(1) = NIDR
        IDRENT(2) = IID
        CALL GKDRPU(IDVDAT(KIPD),KINTGS,2,0,IDRENT,RDUMMY)
        IF (KERROR.NE.0) GO TO 9999
*         Set up real entry.
        IDRENT(1) = NRDR
        IDRENT(2) = IRD
        CALL GKDRPU(IDVDAT(KIPD),KREALS,2,0,IDRENT,RDUMMY)
        IF (KERROR.NE.0) GO TO 9999
*         Set up string entry.
        IDRENT(1) = NSDR
        IDRENT(2) = IST
        CALL GKDRPU(IDVDAT(KIPD),KCHARS,2,0,IDRENT,RDUMMY)
        IF (KERROR.NE.0) GO TO 9999
      ELSE
        IDVDAT(KIPD) = KNIL
      END IF

*       INPUT CLASS DEPENDENT DATA.
*       ---------------------------
        IF (IPCLS.EQ.GLOCAT) THEN
*         Initial normalisation transformation number.
          IDVDAT(KLCINN) = KWI3
*         Initial location.
          RDVDAT(KLCINX) = RX(1)
          RDVDAT(KLCINY) = RY(1)
        ELSE IF (IPCLS.EQ.GSTROK) THEN
*         Initial normalisation transformation number.
          IDVDAT(KSKINN) = KWI3
*         Input buffer size (minimise with maximum buffer size).
        IDVDAT(KSKINB) = MIN(KWI4,IDVDAT(KSKMXB))
*         Number of points in initial stroke.
          IDVDAT(KSKINP) = NRD
*         Points in initial stroke.  Old points are deallocated first.
        IF (IDVDAT(KSKINX).NE.KNIL) THEN
          CALL GKHPDA(IDVDAT(KSKINX),KREALS)
          IF (KERROR.NE.0) GO TO 9999
          IDVDAT(KSKINX) = KNIL
        END IF
        IF (IDVDAT(KSKINY).NE.KNIL) THEN
            CALL GKHPDA(IDVDAT(KSKINY),KREALS)
          IF (KERROR.NE.0) GO TO 9999
          IDVDAT(KSKINY) = KNIL
        END IF
        IF (NRD.GT.0) THEN
          CALL GKHPAL(NRD,KREALS,IDVDAT(KSKINX))
          IF (KERROR.NE.0) GO TO 9999
          CALL GKHPPR(IDVDAT(KSKINX),0,NRD,RX)
          IF (KERROR.NE.0) GO TO 9999
          CALL GKHPAL(NRD,KREALS,IDVDAT(KSKINY))
          IF (KERROR.NE.0) GO TO 9999
          CALL GKHPPR(IDVDAT(KSKINY),0,NRD,RY)
          IF (KERROR.NE.0) GO TO 9999
        END IF
        ELSE IF (IPCLS.EQ.GVALUA) THEN
*         Initial value.
          RDVDAT(KVLINV) = QWR5
*         Minimum and maximum values.
          RDVDAT(KVLMNV) = QWR6
          RDVDAT(KVLMXV) = QWR7
        ELSE IF (IPCLS.EQ.GCHOIC) THEN
*         Initial choice number.
          IDVDAT(KCHINN) = KWI3
*         Initial choice status.
          IDVDAT(KCHINS) = KWI4
        ELSE IF (IPCLS.EQ.GPICK) THEN
*         Initial status.
          IDVDAT(KPCINS) = KWI3
*         Initial segment name.
          IDVDAT(KPCISG) = KWI4
*         Initial pick ID.
          IDVDAT(KPCINI) = KWI5
        ELSE IF (IPCLS.EQ.GSTRIN) THEN
*         Input buffer length.
          IDVDAT(KSTINB) = KWI3
*         Initial cursor position
          IDVDAT(KSTICP) = KWI4
*         Length of initial string.
          IDVDAT(KSTINL) = NID
*         Initial string.
          IF (IDVDAT(KSTINS).NE.KNIL) THEN
          CALL GKHPDA(IDVDAT(KSTINS),KINTGS)
          IF (KERROR.NE.0) GO TO 9999
          IDVDAT(KSTINS) = KNIL
        END IF
          IF (NID.GT.0) THEN
          CALL GKHPAL(NID,KINTGS,IDVDAT(KSTINS))
          IF (KERROR.NE.0) GO TO 9999
          CALL GKHPPI(IDVDAT(KSTINS),0,NID,IDAT)
          IF (KERROR.NE.0) GO TO 9999
        END IF
        END IF
*       Put the data back into the workstation state list.
        CALL GKDRPU(KIPDPT(IPCLS,KWKIX),KWI1,NIWSL(IPCLS),NRWSL(IPCLS),
     :              IDVDAT,RDVDAT)
      IF (KERROR.NE.0) GO TO 9999
      ELSE
        KERROR = 140
      END IF

*     Return from here for a normal exit.
      RETURN

*     All errors trap to here.
 9999 CONTINUE
*     Copy the error code in case we run into errors during cleanup.
      IERROR = KERROR
*     The device data record directory may be corrupt,  so delete it.
      IF(IDVDAT(KIPD).NE.KNIL) THEN
      CALL GKWDDL(IDVDAT(KIPD))
      IDVDAT(KIPD) = KNIL
      END IF
*     The heap objects specific to each class may be corrupt,  so
*     delete them.
      IF (IPCLS.EQ.GSTROK) THEN
      IDVDAT(KSKINP) = 0
        IF (IDVDAT(KSKINX).NE.KNIL) THEN
        CALL GKHPDA(IDVDAT(KSKINX),KREALS)
        IDVDAT(KSKINX) = KNIL
      END IF
        IF (IDVDAT(KSKINY).NE.KNIL) THEN
        CALL GKHPDA(IDVDAT(KSKINY),KREALS)
        IDVDAT(KSKINY) = KNIL
      END IF
      ELSE IF (IPCLS.EQ.GSTRIN) THEN
      IDVDAT(KSTINL) = 0
      IF (IDVDAT(KSTINS).NE.KNIL) THEN
        CALL GKHPDA(IDVDAT(KSTINS),KINTGS)
        IDVDAT(KSTINS) = KNIL
      END IF
      END IF
*     Reinstate the error code and return.
      KERROR = IERROR
      RETURN

      END
