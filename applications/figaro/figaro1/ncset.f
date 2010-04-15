C+
      SUBROUTINE NCSET
C
C     N C S E T
C
C     Figaro function to set a selected region of a spectrum to a
C     constant value.  This is a non-interactive version of CSET,
C     and has the possible advantage of allowing the region to be
C     specified precisely in terms of the X values of the data.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The spectrum to be modified.
C     XSTART      (Numeric) The X-value of the start of the region.
C     XEND        (Numeric) The X-value of the end of the region.
C     VALUE       (Numeric) The value to use for the selected region.
C     OUTPUT      (Character) The name of the output file to
C                 be created.  If this is the same as the input
C                 spectrum, the data will be modified in situ.
C
C     Command keywords - None
C
C     User variables used -  None
C
C                                              KS / CIT 27th March 1985
C
C     Modified:
C
C     31st Jul 1987  DJA/AAO. Revised DSA_ routines - some specs
C                    changed. Dynamic memory handling now though DYN_
C                    routines.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                    in mapping calls.
C     22nd Sep 1992  HME / UoE, Starlink.  TAB removed, INCLUDE changed.
C     2005 June 10 MJC / Starlink  Use CNF_PVAL for pointers to
C                  mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Limits for VALUE - close to the VAX number limits
C
      REAL FMAX,FMIN
      PARAMETER (FMAX=1.7E38,FMIN=-1.7E38)
C
C     Local variables
C
      INTEGER      DIMS(10)     ! Sizes of dimensions of data
      INTEGER      IXEN         ! Last pixel to be set constant
      INTEGER      IXST         ! First  "   "  "   "      "
      INTEGER      NDIM         ! Number of dimensions in data
      INTEGER      NX           ! Size of 1st dimension
      INTEGER      OPTR         ! Dynamic-memory pointer to output data
                                ! array
      INTEGER      OSLOT        ! Map slot number outputdata array
      INTEGER      STATUS       ! Running status for DSA_ routines
      REAL         VALUE        ! Temporary real number
      REAL         VMAX         !
      REAL         VMIN         !
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of spectrum file, and open it.
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get XSTART and XEND
C
      CALL DSA_AXIS_RANGE('SPECT',1,' ',.FALSE.,VMIN,VMAX,IXST,
     :                    IXEN,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get value for VALUE
C
      CALL PAR_RDVAL('VALUE',FMIN,FMAX,1.,' ',VALUE)
C
C     Get name for output file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data array to receive fitted spectrum
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GOTO 500
C
C     Set the region to the constant value
C
      CALL GEN_CFILL(IXST,IXEN,VALUE,%VAL(CNF_PVAL(OPTR)))
C
C     Tidy up
C
  500 CONTINUE
      CALL DSA_CLOSE(STATUS)
C
      END
