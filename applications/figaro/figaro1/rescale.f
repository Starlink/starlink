C+
      SUBROUTINE RESCALE
C
C     R E S C A L E
C
C     This routine rescales spectra/images using user-defined upper and
C     lower limits. The scaling is such that the interval
C     [LOWFACT,HIGHFACT] is becomes the interval [0,1]. The "limits" are
C     not thresholds, i.e. data values beyond the limits are also
C     scaled.
C
C     Command parameters -
C
C     IMAGE  (Character) The name of the structure containing the image.
C            Uses main data array, or the x-axis data for the XCxxx
C            routines.
C
C     LOWFAC (Numeric) The lower limit of the rescaling.
C
C     HIGHFAC (Numeric) The upper limit of the rescaling.
C
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     History:
C
C     02-MAY-1992  MAS (UoM): Original version.
C     30-JUL-1991  HME (UoE, Starlink): Remove MAS_ prefix. Say
C                  precisely what the routine does. LOWFACT default 0.
C     06-NOV-1991  HME (UoE, Starlink): Map the data for update access.
C     30-SEP-1992  HME (UoE, Starlink): INCLUDE changed.
C     2005 June 10 MJC (Starlink)  Use CNF_PVAL for pointers to
C                  mapped data.  Remove unused variables.
C
C     This routine is a modification of ICONST.FOR+GEN_ADDCAF
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER      DIMS(10)      ! The sizes of the data's dimensions
      REAL         HIGHFAC       ! UPPER factor used to operate on the
                                 ! data
      REAL         LOWFAC        ! LOWER factor used to operate on the
                                 ! data
      INTEGER      NDIM          ! The number of dimensions in the data
      INTEGER      NELM          ! Total number of elements in the data
      INTEGER      OSLOT         ! Output data slot number
      INTEGER      OUTELM        ! Dynamic-memory pointer to output data
      INTEGER      STATUS        ! Running status for DSA_ routines
C
C     Numeric parameter limits - close to VAX real limits
C
      REAL FMAX, FMIN
      PARAMETER (FMAX=1.7E38, FMIN=-1.7E38)
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Get value of constant factor
C
      CALL PAR_RDVAL('LOWFACT', FMIN,FMAX,0.,' ',LOWFAC)
      CALL PAR_RDVAL('HIGHFACT',FMIN,FMAX,1.,' ',HIGHFAC)
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
C
C     Map data arrays
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OUTELM,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Operate on the data using FACTOR, depending on
C     the actual command.
C
      CALL ADDCAF(%VAL(CNF_PVAL(OUTELM)),NELM,LOWFAC,HIGHFAC,
     :            %VAL(CNF_PVAL(OUTELM)))
C
C     Closedown everything
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE ADDCAF(IN,N,LOWCONST,HIGHCONST,OUT)
C
C     A D D C A F
C
C     Rescales a spectrum between user-specified limits.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Real array IN(N)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) N      (Integer) The number of elements of IN.
C     (>) LOWCONST  (Real) The LOWER LIMIT constant to be imposed to all the
C                   elements of IN.
C     (>) HIGHCONST (Real) The UPPER LIMIT constant to be imposed to all the
C                   elements of IN.
C     (<) OUT    (Real array OUT(N)) The result of the
C                addition. Note that IN and OUT may
C                be the same array.
C
C     Subroutines / functions used - None
C
C                                         MAS / U of M 2 May 1991.
C
C     30-JUL-1991  HME (UoE, Starlink): Remove MAS_ prefix.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      REAL IN(N),LOWCONST,HIGHCONST,OUT(N)
C
C     Local variable
C
      INTEGER I
C
C     Perform operation
C
      DO I=1,N
         OUT(I) = ( IN(I) - LOWCONST )/( HIGHCONST - LOWCONST )
      END DO
C
      END
