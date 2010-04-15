C+
      SUBROUTINE SFIT
C
C     S F I T
C
C     Figaro function to generate a spectrum by fitting a polynomial
C     to an input spectrum.  An orthogonal polynomial of specified
C     order is fitted either to the spectrum or to its logarithm, and
C     the result is used to generate a new spectrum.
C
C     Command parameters -
C
C     SPECTRUM    (Character) The name of the spectrum to be fitted.
C
C     ORDER       (Numeric) The order to be used for the fit.
C
C     OUTPUT      (Character) The name of the output file to
C                 be created.  If this is the same as the input
C                 spectrum, the data in the input spectrum will
C                 be modified.
C
C     Command keywords -
C
C     LOGS        Indicates that the logs of the pixel values
C                 of the input spectrum are to be fitted.
C
C                                              KS / CIT 5th June 1983
C     Modified:
C
C     25th Mar 1985  KS / AAO.  Workspace used and call to FIG_SPFIT
C                    modified in order to use the NAG library.
C     11th Aug 1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic memory
C                    handling.
C     26th Mar 1991  KS / AAO.  Use of 'UPDATE' and 'WRITE' corrected
C                    in mapping calls.
C     29th Sep 1992  HME / UoE, Starlink.  INCLUDE changed, TABs
C                    removed.
C     17th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C     24th May 1998  ACD / UoE, Starlink.  Increased max. order to 30.
C      4th Jul 2001  VGG / RAL, Starlink.  Error Propogation included.
C      7th Jul 2001  ACD / UoE, Starlink.  Small amount of cosmetic
C                    tidying.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Maximum allowed degree (plus 1)
C
      INTEGER MAXDP1
      PARAMETER (MAXDP1=21)
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      INTEGER      DXPTR         ! Dynamic-memory pointer to workspace
      INTEGER      DYPTR         ! Dynamic-memory pointer to workspace
      LOGICAL      LOGS          ! See above
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      ORDER         ! The order of the polynomial fit
      INTEGER      OSLOT         ! Map slot number for output data array
      INTEGER      OVPTR         ! Dynamic-memory pointer to output
                                 ! variance array
      INTEGER      OVSLOT        ! Map slot number for output variance
                                 ! array
      INTEGER      STATUS        ! Running status for DSA_ routines
      REAL         VALUE         ! Temporary real number
      LOGICAL      VEXIST        ! TRUE if a variance array exists
      INTEGER      WKPTR         ! Dynamic-memory pointer to workspace
      INTEGER      WPTR          ! Dynamic-memory pointer to workspace
      INTEGER      WSLOT         ! Map slot number of workspace
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Open spectrum file
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data
C
      CALL DSA_DATA_SIZE('SPECT',1,NDIM,DIMS,NX,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get ORDER parameter and LOGS keyword
C
      CALL PAR_RDVAL('ORDER',1.,30.,5.,' ',VALUE)
      ORDER=NINT(VALUE)
      CALL PAR_RDKEY('LOGS',.FALSE.,LOGS)
C
C     Get name for output file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data array to receive fitted spectrum
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C
C     Check for the existence of errors and map the variance array if
C     one exists.
C
      VEXIST = .FALSE.
      CALL DSA_SEEK_VARIANCE ('SPECT',VEXIST,STATUS)
      IF (VEXIST) THEN
         CALL DSA_MAP_VARIANCE ('OUTPUT','UPDATE','FLOAT',OVPTR,
     :                           OVSLOT, STATUS)
      END IF
C
C     Get workspace for the fitting routine
C
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',DXPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',DYPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'DOUBLE',WPTR,WSLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(4*NX+3*MAXDP1,'DOUBLE',WKPTR,WSLOT,STATUS)
C
C     Do the fit and generate the new spectrum
C
      CALL FIG_SPFIT(%VAL(CNF_PVAL(OPTR)),NX,ORDER,LOGS,
     :               %VAL(CNF_PVAL(DXPTR)),%VAL(CNF_PVAL(DYPTR)),
     :               %VAL(CNF_PVAL(WPTR)),%VAL(CNF_PVAL(WKPTR)),
     :               %VAL(CNF_PVAL(OPTR)))
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
      SUBROUTINE FIG_SPFIT (INPUT,NX,ORDER,LOGS,DX,DY,W,WK,OUTPUT)
C
C     F I G _ S P F I T
C
C     Fits a polynomial to a spectrum and generates a fitted
C     spectrum from that polynomial.
C
C     Parameters -  (">" input, "<" output, "W" workspace)
C
C     (>) INPUT     (Real array INPUT(NX)) The spectrum to
C                   be fitted.
C     (>) NX        (Integer) The number of points in INPUT.
C     (>) ORDER     (Integer) The order of the polynomial to be
C                   fitted to INPUT. Note that this routine has
C                   workspace for ORDER<=30.
C     (>) LOGS      (Logical) If true, the fit will be to the
C                   logarithms of the values in INPUT.  If false,
C                   the fit will be to the actual values.
C     (W) DX        (Double precision DX(NX)) Workspace - used to hold
C                   the X values for the fit.
C     (W) DY        (Double precision DY(NY)) Workspace - used to hold
C                   the Y values for the fit.
C     (W) W         (Double precision array W(NX)) Workspace - used to
C                   hold the weights for the fit.
C     (W) WK        (Double precision WK(4*NX+3*MAXDP1)) Workspace.
C     (<) OUTPUT    (Real array OUTPUT(NX)) The resulting generated
C                   spectrum.
C
C                                              KS / CIT 5th June 1983
C     Modified:
C
C     25th Mar 1985.  KS / AAO.  Modified to use NAG routines.
C     17th Apr 1995.  HME / UoE, Starlink.  No longer use NAG.
C     24th May 1998   ACD / UoE, Starlink.  Increased max. order to 30.
C+
      IMPLICIT NONE
C
C     Maximum allowed degree (plus 1)
C
      INTEGER MAXDP1
      PARAMETER (MAXDP1=31)
C
C     Parameters
C
      LOGICAL LOGS
      INTEGER NX,ORDER
      REAL INPUT(NX),OUTPUT(NX)
      DOUBLE PRECISION DX(NX),DY(NX),W(NX),WK(4*NX+3*MAXDP1)
C
C     Local variables
C
      INTEGER DEGP1,I,IFAIL,IFAIL2,NDEG
      DOUBLE PRECISION XVAL,EPS
C
C     Check value of ORDER
C
      DEGP1=MIN(MAX(ORDER+1,2),MAXDP1)
C
      IF (.NOT.LOGS) THEN
C
C        If LOGs are not to be used, the procedure is quite simple.
C
C        First, fill up the DX,DY and W  arrays.
C
         XVAL=0.
         DO I=1,NX
            XVAL=XVAL+1.
            DX(I)=XVAL
            DY(I)=INPUT(I)
            W(I)=1.0
         END DO
C
C        Perform the fit
C
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(NX,DX,DY,W,DEGP1-1,NDEG,EPS,WK,
     :      IFAIL,WK(NX+1),IFAIL2)
         IF (NDEG.NE.DEGP1-1.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
C
C        Generate the result spectrum.
C        This is already in WK(1:NX), but that is double precision.
C
         DO I=1,NX
            OUTPUT(I)=WK(I)
         END DO
C
      ELSE
C
C        If LOGs are to be used, then things are a little more
C        complex, but the sequence is the same, except not all
C        the data values are used in the fit, since there has
C        to be a check to ensure we don't take logs of -ve numbers.
C        What we actually do is set a negligible weight for such
C        points.
C
C        Fill up the arrays
C
         DO I=1,NX
            DX(I)=FLOAT(I)
            IF (INPUT(I).GT.0.) THEN
               DY(I)=LOG(INPUT(I))
               W(I)=1.0
            ELSE
               DY(I)=0.
               W(I)=1.0D-6
            END IF
         END DO
C
C        Perform the fit
C
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(NX,DX,DY,W,DEGP1-1,NDEG,EPS,WK,
     :      IFAIL,WK(NX+1),IFAIL2)
         IF (NDEG.NE.DEGP1-1.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
C
C        Generate the result spectrum.
C        This is already in WK(1:NX), but that is double precision
C        and logarithmised.
C
         DO I=1,NX
            OUTPUT(I)=EXP(WK(I))
         END DO
C
      END IF
C
C     Bail out on NAg errors is to here
C
  400 CONTINUE
C
      END
