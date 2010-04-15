C+
      SUBROUTINE IREV
C
C     I R E V
C
C     Figaro routine to reverse an image (or a spectrum, which
C     is treated as an image with one dimension 1).  The reversal
C     is in the X-direction or the Y-direction.
C
C     Command parameters -
C
C     IMAGE    (Character) The name of the file containing the image.
C              The data and error components are reversed; the quality
C              component is reversed implicitly by internally using
C              flagged values; the x-axis or y-axis data
C              component is reversed if it contains anything other than
C              just the bin numbers.
C
C     OUTPUT   (Character) The name of the result of the operation.
C              This can be the same as IMAGE, in which case the
C              operation is performed in situ.
C
C     Command keywords - None
C
C     User variables - None
C
C                                         KS / CIT 20th July 1983
C     Modified:
C
C     2nd  Jul 1984  PCP / CIT.  Renamed from IREVX to IREV, now
C                    supports both IREVX and IREVY.
C     27th May 1986  KS / AAO.  Now closes down the output file
C                    properly.
C     27th Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Modified dynamic emmeory handling - now
C                    uses DYN_ routines.
C     18th Sep 1988  KS / AAO. Use of SAME corrected.  Was only working
C                    properly for in situ reversals.
C     18th Dec 1990  KS / AAO. Now handles non-float data properly.
C     4th  Jul 1991  HME / UoE, Starlink.  Reverse also the error array.
C                    Use flagged values in order to implicitly reverse
C                    any quality array (or to keep flagged values in
C                    the data array).
C                    Actually DO reverse the axis array (in OUTPUT
C                    instead of INPUT, and if STATUS ok instead of not
C                    ok).
C     7th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     2005 June 8  MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions
C
      LOGICAL GEN_CHKNSF
C
C     Local variables
C
      INTEGER      APTR          ! Dynamic-memory pointer to xy-axis
                                 ! data
      INTEGER      ASLOT         ! Map slot number of xy-axis data
      INTEGER      AXIS          ! The axis number of rotation
      CHARACTER    COMMAND*64    ! The actual FIGARO command used
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      LOGICAL      EXIST         ! TRUE if an axis data structure exists
      INTEGER      IY            !
      INTEGER      NA1           ! Size of 1st dimension of xy-axis data
      INTEGER      NA2           ! Size of 2st dimension of xy-axis data
      INTEGER      NAELM         ! Number of elements in axis array
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of 1st dimension of main data
                                 ! array
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output
                                 ! data array
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      EPTR          ! Dynamic-memory pointer to errors
                                 ! array
      INTEGER      ESLOT         ! Map slot number for errors array
      LOGICAL      REV           ! Axis-data is to be reversed?
      LOGICAL      SAME          ! Input and output data areas are same?
      INTEGER      STATUS        ! Running status for DSA_ routines
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of image
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get image dimensions
C
      CALL DSA_DATA_SIZE('IMAGE',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
C
C     Get name of output file.
C     Using flagged values serves either of two purposes:
C     (i)  Existing flagged values are retained.
C     (ii) An existing quality array is converted into flagged values by
C          DSA_MAP_DATA, these are reversed with the data array and on
C          unmapping result in a reversed quality array.
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      CALL DSA_USE_FLAGGED_VALUES( 'OUTPUT', STATUS )
      IF (STATUS.NE.0) GO TO 500
      CALL PAR_COMMAND(COMMAND)
      IF (COMMAND.EQ.'IREVX') THEN
         AXIS=1
      ELSE
         IF (COMMAND.EQ.'IREVY') THEN
            AXIS=2
         ELSE
            GOTO 500
         END IF
      END IF
C
C     Now see if there is an X/Y data array. (We'll accept either
C     1D or 2D arrays, but 1D should be the usual case.) Note that
C     errors are ignored for the X/Y reversal; if one occurs, we
C     assume its because the X/Y data shouldn't be being reversed.
C
      CALL DSA_SEEK_AXIS('IMAGE',AXIS,EXIST,STATUS)
C
C     If axis exists then map it.
C
      IF (STATUS.NE.0) GOTO 500
      IF (EXIST) THEN
         CALL DSA_AXIS_SIZE('IMAGE',AXIS,2,NDIM,DIMS,NAELM,STATUS)
         NA1=DIMS(1)
         NA2=NAELM/NA1
         CALL DSA_MAP_AXIS_DATA('OUTPUT',AXIS,'UPDATE','FLOAT',APTR,
     :                          ASLOT,STATUS)
         IF (STATUS.NE.0) GOTO 500
C
C        Make sure array isn't just bin numbers
C
         REV=.FALSE.
         DO IY=1,NA2
            REV = .NOT.GEN_CHKNSF(%VAL(CNF_PVAL(APTR)),NA1)
         END DO
         SAME=.TRUE.
         IF (REV) THEN
            CALL GEN_REV2D(%VAL(CNF_PVAL(APTR)),NA1,NA2,SAME,
     :                          %VAL(CNF_PVAL(APTR)))
         END IF
      END IF
C
C     Now map and reverse the main data array
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
      SAME=.TRUE.
      IF (AXIS.EQ.1) THEN
         CALL GEN_REV2D(%VAL(CNF_PVAL(OPTR)),NX,NY,SAME,
     :                  %VAL(CNF_PVAL(OPTR)))
      ELSE
         CALL GEN_REF2D(%VAL(CNF_PVAL(OPTR)),NX,NY,SAME,
     :                  %VAL(CNF_PVAL(OPTR)))
      END IF
C
C     Now see if there is an errors array. Map and reverse it.
C
      CALL DSA_SEEK_ERRORS( 'IMAGE', EXIST, STATUS )
      IF ( EXIST ) THEN
         CALL DSA_MAP_ERRORS( 'OUTPUT', 'UPDATE', 'FLOAT', EPTR,
     :                        ESLOT, STATUS )
         IF ( STATUS .NE. 0 ) GO TO 500
         SAME = .TRUE.
         IF ( AXIS .EQ. 1 ) THEN
            CALL GEN_REV2D( %VAL(CNF_PVAL(EPTR)), NX, NY, SAME,
     :                      %VAL(CNF_PVAL(EPTR)) )
         ELSE
            CALL GEN_REF2D( %VAL(CNF_PVAL(EPTR)), NX, NY, SAME,
     :                      %VAL(CNF_PVAL(EPTR)) )
         END IF
      END IF
C
C     Tidy up.
C
  500 CONTINUE

      CALL DSA_CLOSE(STATUS)

      END
