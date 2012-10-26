*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused I
*-----------------------------------------------------------------------

      SUBROUTINE SPLICEQ (NQUAD, NQ, NREP, BUF, MAXDAT,
     &                    NPTS, DATA, IFAIL)

*  Routine to splice expanded or shortened sector into multi-sector
*  data array.

      IMPLICIT   NONE

      INCLUDE 'CNF_PAR'

*     Formal parameters

      INTEGER    NQUAD          ! # of sectors in data
      INTEGER    NQ             ! Sector to be replaced
      INTEGER    NREP           ! # of channels in replacement sector
      REAL       BUF(NREP)      ! Data for replacement sector
      INTEGER    MAXDAT         ! Maximum length of data array
      INTEGER    NPTS(NQUAD)    ! # of channels in each sector (input/output)
      REAL       DATA(*)        ! Concatenated data array (input/output)
      INTEGER    IFAIL          ! Error return

*     Functions

      INTEGER    IGETVM
      INTEGER    IFREEVM
      INTEGER    NTOT

*     Local variables

      INTEGER    IPTR
      INTEGER    ISTAT
      INTEGER    N
      INTEGER    NBYTES
      INTEGER    NCHAN
      INTEGER    NOFF
      INTEGER    NST

*  Ok, go...

*     Find out how much storage required

      NCHAN = NTOT(NQUAD) - NPTS(NQ) + NREP
      IF (NCHAN .gt. MAXDAT) THEN
        PRINT *, ' -- SpliceQ --  No room on stack for lengthened array'
        IFAIL = 18
        RETURN
      END IF

*     Get some work space

      NBYTES = 4*NCHAN
      ISTAT = IGETVM (NBYTES, .TRUE., 'SPLICEQ', IPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *, ' -- SpliceQ -- error in UGETVM'
        IFAIL = 18
      END IF

*     Copy data...
*     -- First the initial unchanged block

      N = NTOT(NQ-1)
      IF (N .ne. 0) CALL XCOPY (4*N, DATA(1), %VAL(CNF_PVAL(IPTR)))
      NOFF = 4*N

*     -- then splice in the new array

      N = NREP
      IF (N .ne. 0) CALL XCOPY (4*N, BUF(1),  %VAL(CNF_PVAL(IPTR)+NOFF))
      NOFF = NOFF + 4*N

*     -- and finally the tail (if any)

      IF (NQ .ne. NQUAD) THEN
        N = NTOT(NQUAD) - NTOT(NQ)
        NST = NTOT(NQ) + 1
        CALL XCOPY (4*N, DATA(NST), %VAL(CNF_PVAL(IPTR)+NOFF))
      END IF

*     Update NPTS and copy workspace back to data array

      NPTS(NQ) = NREP
      CALL XCOPY (4*NCHAN, %VAL(CNF_PVAL(IPTR)), DATA)

*     Free work space

      ISTAT = IFREEVM (IPTR)
      IF (ISTAT .ne. 0) THEN
        PRINT *, ' -- SpliceQ -- error in UFREEVM'
        IFAIL = 18
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
