C# IL>=a, OL>=0
      SUBROUTINE GKPIPD(IPROOT, MXD, ILEN, DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:   WORKSTATION INQUIRY UTILITY
*  Author:            RS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Get input device data record directory from the workstation
*     statelist, and pack it into a data record.
*
*  MAINTENANCE LOG
*  ---------------
*      6/12/83  RS    Original version stabilised
*      8/12/83  RS    Extraneous argument ISTAT removed
*     12/12/83  RS    Stack space used for temporary Int. & Real storage
*      8/01/84  NB    failure paths to deallocate stack before return
*     20/01/84  JL    Change character-PID handling.
*                     Allow for KERROR = 1017
*     02/02/84  JL    Change character-PID handling back to old form
*                     as this works and the new doesn't
*     21/02/84  CJW   Merge changes in SERC and ICL versions
*     22/03/84  JRG   Make it work even if there are no PID's
*     16/04/84  JL    Reinstate 'lost' fix on character PIDs
*     22/01/87  JCS   IS conversion. Error changes
*     12/08/87  PJWR  Rewritten to return a FORTRAN binding data record
*                     and access the new style of device data record
*                     directory.
*
*   ARGUMENTS
*   ---------
*     INP    IPROOT   Pid directory root
*     INP    MXD      Max length of data record
*     OUT    ILEN     length of data record
*     OUT    DATREC   Data record
*
      INTEGER         IPROOT, MXD, ILEN
      CHARACTER*80    DATREC(MXD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify  /GKYSTK/ KSTACK, QSTACK
*             /GKYERR/ KERROR
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'

*  LOCALS
*  ------
*     IOFFD   Stack offset for device data record directory contents.
*     IOFFI   Stack offset for device data record integer data.
*     IOFFR   Stack offset for device data record real data.
*     IOFFS   Stack offset for device data record string data.
*     IERROR  Copy of error code KERROR for use during cleanup.
*     IDUMMY  Dummy integer array for creating empty data records.
*     RDUMMY  Dummy real array for creating empty data records and
*             reading the data record directory.
*
      INTEGER IOFFD, IOFFI, IOFFR, IOFFS, IERROR, IDUMMY(1)
      REAL RDUMMY(1)

*-----------------------------------------------------------------------

*     Initialise stack pointers for deallocation on error.
      IOFFD = KNIL
      IOFFI = KNIL
      IOFFR = KNIL
      IOFFS = KNIL

*     If there is a data record directory then extract the data and
*     create a data record,  otherwise create an empty data record.
      IF (IPROOT.NE.KNIL) THEN
*       Allocate stack for subdirectory entries.  Three entries of two
*       integers => 6 integers needed.
        CALL GKSTAL(KINTGS,6,IOFFD)
	IF (KERROR.NE.0) GO TO 999
*       Get the the subdirectory entries and put them on the stack.
        CALL GKDRGE(IPROOT,KINTGS,2,1,KSTACK(IOFFD),RDUMMY)
	IF (KERROR.NE.0) GO TO 999
        CALL GKDRGE(IPROOT,KREALS,2,1,KSTACK(IOFFD+2),RDUMMY)
	IF (KERROR.NE.0) GO TO 999
        CALL GKDRGE(IPROOT,KCHARS,2,1,KSTACK(IOFFD+4),RDUMMY)
	IF (KERROR.NE.0) GO TO 999
*       Allocate stack for each data type as necessary.  A minimum of
*       one stack element is required for each data type to use as a
*       dummy when creating the data record.
	CALL GKSTAL(KINTGS,MAX(KSTACK(IOFFD),1),IOFFI)
	IF (KERROR.NE.0) GO TO 999
	CALL GKSTAL(KREALS,MAX(KSTACK(IOFFD+2),1),IOFFR)
	IF (KERROR.NE.0) GO TO 999
	CALL GKSTAL(KINTGS,MAX(KSTACK(IOFFD+4)*2,1),IOFFS)
	IF (KERROR.NE.0) GO TO 999
*       Copy any data of a given type onto the stack.  First integers:
	IF (KSTACK(IOFFD).GT.0) THEN
	  CALL GKHPGI(KSTACK(IOFFD+1),0,KSTACK(IOFFD),KSTACK(IOFFI))
	  IF (KERROR.NE.0) GO TO 999
	END IF
*       Then reals:
	IF (KSTACK(IOFFD).GT.0) THEN
	  CALL GKHPGR(KSTACK(IOFFD+3),0,KSTACK(IOFFD+2),QSTACK(IOFFR))
	  IF (KERROR.NE.0) GO TO 999
	END IF
*       Finally strings:
	IF (KSTACK(IOFFD).GT.0) THEN
          CALL GKHPGI(KSTACK(IOFFD+5),0,KSTACK(IOFFD+4)*2,KSTACK(IOFFS))
	  IF (KERROR.NE.0) GO TO 999
	END IF
*       Create the data record.
	CALL GKPREC(KSTACK(IOFFD),KSTACK(IOFFI),KSTACK(IOFFD+2),
     :              QSTACK(IOFFR),KSTACK(IOFFD+4),KSTACK(IOFFS),MXD,
     :              ILEN,DATREC)
      ELSE
	CALL GKPREC(0,IDUMMY,0,RDUMMY,0,IDUMMY,MXD,ILEN,DATREC)
      END IF

*     Deallocate stack.  All errors trap to here as well,  so ordinary
*     stack deallocations suffer a two assignment hit to copy / restore
*     the error code.
  999 CONTINUE
*     Copy the error code.
      IERROR = KERROR
*     Deallocate the stack.
      IF (IOFFS.NE.KNIL) CALL GKSTDA(KINTGS,IOFFS)
      IF (IOFFR.NE.KNIL) CALL GKSTDA(KREALS,IOFFR)
      IF (IOFFI.NE.KNIL) CALL GKSTDA(KINTGS,IOFFI)
      IF (IOFFD.NE.KNIL) CALL GKSTDA(KINTGS,IOFFD)
*     Restore the error code.
      KERROR = IERROR

      RETURN

      END
