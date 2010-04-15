C+
      SUBROUTINE FIT_DINIT (LU,SWAP,STATUS)
C
C     F I T _ D I N I T
C
C     Initialises the FIT_ routines for output to a disk file.
C     This routine is an alternative to FIT_INIT, and only one of
C     them should be used.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) LU       (Integer) The logical unit number of an already
C                  open disk file to which 2880 byte 'FITS-type'
C                  records can be written.  LU should NOT be zero.
C
C     (>) SWAP     (Logical) True if byte swapping is to be performed
C                  - ie if data is to be written in the normal FITS
C                  (that is, IBM) byte order; false if byte swapping
C                  is to be suppressed - ie if data is to be written
C                  to disk in DEC order.
C
C     (<) STATUS   (Integer) Return status code.  Will be zero unless
C                  LU is zero, in which case it will be set to a non-
C                  zero value that can be decoded by FIT_ERROR.
C
C     Common variables used -
C
C     (<) MTUNIT   (Integer) The ID code used to access the tape.
C     (<) FPTR     (Integer) The pointer to the main output buffer.
C     (<) MTERR    (Character) Descriptor of last tape I/O error.
C     (<) LU       (Integer) Logical unit for disk file, if used.
C     (<) NOSWAP   (Logical) Used to suppress byte swapping.
C
C     MTERR is defined in the file COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -  None
C
C                                          KS / CIT 14th June 1984
C     Modified:
C
C     17th June 1986.  KS / AAO.  SWAP parameter added.
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL SWAP
      INTEGER LU,STATUS
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Set file logical unit, and clear tape id code and set the
C     noswap flag.
C
      MTUNIT=0
      LUFILE=LU
      NOSWAP=.NOT.SWAP
C
C     Initialise pointer
C
      FPTR=1
C
C     Set status return
C
      IF (LU.EQ.0) THEN
         STATUS=-1
         MTERR='Disk logical unit number was passed as zero'
      ELSE
         STATUS=0
      END IF
C
      END
