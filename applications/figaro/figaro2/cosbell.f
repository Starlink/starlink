C+
      SUBROUTINE COSBELL
C
C     C O S B E L L
C
C     Given a template data file, COSBELL creates a data file that is
C     the same as the template but in which the data is a cosine bell
C     filter.  This can then be applied to the original data (or to
C     other data with the same dimensions) using IMULT.
C
C     Command parameters -
C
C     SPECTRUM (Character) The name of the structure containing the
C              template data.
C
C     BELLPC   (Numeric) The percentage of the data that is to be
C              covered by the rising (or falling) part of the cosine
C              bell.
C
C     OUTPUT   (Character) The name of the result of the operation.
C              This can be the same as for SPECTRUM. If not, a new
C              structure is created, with everything but the data a
C              direct copy of the input.
C
C                                                KS / AAO 23rd Sept 1986
C
C     Modified:
C
C     25th Jul 1987  DJA / AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic
C                    memory handling
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed, TAB removed.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NX            ! Size of first image dimension
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number output data array
      REAL         PERCENT       ! See BELLPC above
      INTEGER      STATUS        ! Running status for DSA_ routines
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT ('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('SPECT',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      IF (STATUS.NE.0) GO TO 500
C
C     Get percentage of data to be apodised by the cosine bell.
C
      CALL PAR_RDVAL('BELLPC',0.0,50.0,10.0,' ',PERCENT)
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map output data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Now generate the cosine bell.
C
      IF (NDIM.EQ.1) THEN
         CALL FIG_1DCOSB(NX,PERCENT,%VAL(CNF_PVAL(OPTR)))
      ELSE
         CALL FIG_2DCOSB(NX,DIMS(2),PERCENT,%VAL(CNF_PVAL(OPTR)))
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
C
      END
C+
      SUBROUTINE FIG_1DCOSB (NX,PERCENT,DATA)
C
C     F I G _ 1 D C O S B
C
C     Generates a 1-dimensional data array that is zero at each end,
C     1 for most of the central section, with a certain percentage
C     at each end apodised using a cosine bell.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (>) NX      (Integer) Number of elements in the data
C     (>) PERCENT (Real) Percentage of data at each end that is to be
C                 apodised by the cosine bell.
C     (<) DATA    (Real array, DATA(NX)) The resulting data array.
C
C     Common variables used - None
C
C                                                KS / AAO 23rd Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX
      REAL PERCENT, DATA(NX)
C
C     Local variables
C
      INTEGER I, NSQ
      REAL ARG, FACTOR
C
      REAL PI
      PARAMETER (PI = 3.14159265)
C
      IF (NX.GT.1) THEN
         NSQ = NX * .01 * PERCENT
         IF (NSQ.GT.NX/2) NSQ=NX/2
         IF (NSQ.GT.1) THEN
            DO I = 1,NSQ
               ARG = PI * FLOAT(I-1)/(NSQ-1)
               FACTOR = .5 * (1 - COS(ARG))
               DATA(I) = FACTOR
               DATA(NX-I+1) = FACTOR
            END DO
         END IF
         DO I = NSQ+1,NX-NSQ
            DATA(I) = 1.0
         END DO
      ELSE
         DATA(1)=1.0
      END IF
C
      END
C+
      SUBROUTINE FIG_2DCOSB (NX,NY,PERCENT,DATA)
C
C     F I G _ 2 D C O S B
C
C     Generates a 2-dimensional data array that is zero at each end,
C     1 for most of the central section, with a certain percentage
C     at each end apodised using a cosine bell.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (>) NX      (Integer) First dimension of data.
C     (>) NY      (Integer) Second dimension of data.
C     (>) PERCENT (Real) Percentage of data at each end that is to be
C                 apodised by the cosine bell.
C     (<) DATA    (Real array, DATA(NX,NY)) The resulting data array.
C
C     Common variables used - None
C
C                                                KS / AAO 23rd Sept 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY
      REAL PERCENT, DATA(NX,NY)
C
C     Local variables
C
      INTEGER IX, IY, NYSQ
      REAL ARG, YFACT
C
      REAL PI
      PARAMETER (PI = 3.14159265)
C
      IF (NY.GT.1) THEN
         NYSQ = NY*.01*PERCENT
         IF (NYSQ.GT.NY/2) NYSQ=NY/2
         IF (NYSQ.GT.1) THEN
            DO IY = 1, MIN(NY,NYSQ)
               ARG = PI * FLOAT (IY-1)/(NYSQ-1)
               YFACT = 0.5*(1-COS(ARG))
               CALL FIG_1DCOSB(NX,PERCENT,DATA(1,IY))
               CALL FIG_1DCOSB(NX,PERCENT,DATA(1,NY-IY+1))
               DO IX=1,NX
                  DATA(IX,IY)=DATA(IX,IY)*YFACT
                  DATA(IX,NY-IY+1)=DATA(IX,NY-IY+1)*YFACT
               END DO
            END DO
         END IF
         DO IY = NYSQ+1, NY-NYSQ
            CALL FIG_1DCOSB(NX,PERCENT,DATA(1,IY))
         END DO
      ELSE
         CALL FIG_1DCOSB(NX,PERCENT,DATA)
      END IF
C
      END
