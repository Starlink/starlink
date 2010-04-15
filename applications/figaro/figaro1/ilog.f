C+
      SUBROUTINE ILOG
C
C     I L O G    /    I A L O G
C
C     Takes the base 10 log or antilog of an image.
C     For ILOG, any pixels that are negative will
C     give a zero pixel in the resulting image.
C
C     Command parameters -
C
C     IMAGE  The name of the structure containing the image.
C
C     OUTPUT The name of the result of the operation.  This can
C            be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the data a direct
C            copy of the input.
C
C     Command keywords  - None
C
C     User variables used - None
C                                      KS / CIT 22nd April 1984
C
C     Modified:
C
C     24th Jul 1987  DJA/AAO . Revised DSA_ routines - some specs
C                    changed. Also dynamic memory handling modified -
C                    now uses the DYN_ set of routines.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected in
C                    mapping calls.
C     6th  Oct 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     26th Jul 1997  MJCL / Starlink, UCL.  Initialise VEXIST to .FALSE.
C     2005 June 8    MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Local variables
C
      INTEGER   DIMS(10)         ! Sizes of the dimensions of the data
      INTEGER   NDIM             ! Dimensionality of input structure
      INTEGER   NELM             ! Total number of elements in data
                                 ! array
      INTEGER   OPTR             ! Dynamic memory pointer to output
                                 ! array
      INTEGER   OSLOT            ! Map slot number for output data
      INTEGER   STATUS           ! Status return from DSA_xxx routines
      INTEGER   VOPTR            ! Dynamic memory pointer to output
                                 ! variance
      INTEGER   VOSLOT           ! Map slot number for output variance
      LOGICAL   VEXIST           ! True if the variance array exists
      CHARACTER COMMAND*8        ! Actual FIGARO command passed
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get the command name
C
      CALL PAR_COMMAND(COMMAND)
C
C     Get input name
C
      CALL DSA_INPUT ('IMAGE','IMAGE',STATUS)
C
C     Get dimensions of input data
C
      CALL DSA_DATA_SIZE ('IMAGE',10,NDIM,DIMS,NELM,STATUS)
C
C     Get output structure name
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Map data
C
      CALL DSA_MAP_DATA ('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,
     :                                                       STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     See if the variance array exists. If so, map it.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE('IMAGE',VEXIST,STATUS)
      IF (VEXIST) THEN
          CALL DSA_MAP_VARIANCE('OUTPUT','UPDATE','FLOAT',VOPTR,
     :                           VOSLOT,STATUS)
      END IF
      IF (STATUS.NE.0) GOTO 500

C
C     Operate on the data. Note that GEN_LOG and GEN_ALOG can operate
C     on data in situ.
C
      IF ( COMMAND .EQ. 'ILOG' ) THEN
         CALL GEN_LOG(VEXIST,%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(VOPTR)),
     :                NELM,%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(VOPTR)))

      ELSE
         CALL GEN_ALOG(VEXIST,%VAL(CNF_PVAL(OPTR)),
     :                 %VAL(CNF_PVAL(VOPTR)),NELM,
     :                 %VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(VOPTR)))
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE (STATUS)
C
      END
