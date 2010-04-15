C+
      SUBROUTINE MCFIT
C
C     M C F I T
C
C     Performs a masked fit to a spectrum.  At a given number of
C     equally separated points in the spectrum, the average of the
C     surrounding points is taken and a new spectrum is formed by
C     interpolating between the resulting values.  Any points in the
C     spectrum for which the corresponding element of the mask spectrum
C     is non-zero will be ignored.
C
C     Command parameters -
C
C     SPECTRUM     (Character) The spectrum to be fitted
C     MASK         (Character) The mask spectrum to be used
C     POINTS       (Numeric) The number of points to be used
C     OUTPUT       (Character) The name of the resulting spectrum
C
C     Command keywords - None
C
C     User variables used - None
C
C                                    KS / CIT 8th April 1984
C     Modified:
C
C     28th Feb 1985  KS / AAO.  Now operates on a 2D image as well
C                    as on a 1D spectrum.  - But MASK is always a
C                    1D spectrum.
C     27th Mar 1985  KS / AAO. Workspace usage and call to FIG_
C                    MCFIT modified for NAG version of FIG_MCFIT.
C     11th Aug 1987  DJA/ AAO. Revised DSA_ routines - some specs
C                    changed. Now uses DYN_ routines for dynamic-memory
C                    handling.
C     31st Dec 1987  KS / AAO. RMS calculation added.  Check on match of
C                    dimensions between SPECT and MASK revised to allow
C                    for case where SPECT is 2D but MASK is 1D.  MASK
C                    can now be 2D.
C     29th Sep 1992  HME / UoE, Starlink.  TABs removed, INCLUDE
C                    changed. Map output data for update access.
C      5th Apr 1995  HME / UoE, Starlink.  No longer use NAG.
C     2005 June 14   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      INTEGER ICH_ENCODE,ICH_LEN
C
C     Maximum number of points to be used
C
      INTEGER MAXSPL
      PARAMETER (MAXSPL=50)
C
C     Local variables
C
      INTEGER      DIMS(2)       ! Sizes of dimensions of data
      DOUBLE PRECISION DUMMY     ! Dummy numeric value for data info
      INTEGER      IAX           ! Index through axes
      INTEGER      IGNORE        ! Ignored status value
      INTEGER      INVOKE        ! Dummy function value
      INTEGER      IY            ! Index through spectra in 2D data
      LOGICAL      ISNEWM        ! Is MPTR address new to CNF?
      LOGICAL      ISNEWO        ! Is OPTR address new to CNF?
      INTEGER      MDIM          ! Number of MASK dimensions
      INTEGER      MDIMS(2)      ! MASK dimensions
      INTEGER      MPTR          ! Dynamic-memory pointer to mask data
                                 ! array
      INTEGER      MSKELM        ! Elements in MASK data
      INTEGER      MSLOT         ! Map slot number of mask data array
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NEXT          ! Next character in STRING
      INTEGER      NSPL          ! Number of splines
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      NY            ! Size of 2nd dimension (if present)
      INTEGER      OPTR          ! Dynamic-memory pointer to output data
                                 ! array
      INTEGER      OSLOT         ! Map slot number output data array
      LOGICAL      PISNM         ! Previous CNF MPTR pointer new?
      LOGICAL      PISNO         ! Previous CNF OPTR pointer new?
      REAL         RMS           ! RMS value for fits
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*64     ! Used to format RMS values
      INTEGER      TPTR          ! Temporary dynamic mem pointer
      CHARACTER    UNITS*32      ! Data units
      REAL         VALUE         ! Temporary real number
      DOUBLE PRECISION WORK(8*MAXSPL+40) ! Workspace for NAG
      DOUBLE PRECISION X(MAXSPL+2)       !     "      "   "
      DOUBLE PRECISION Y(MAXSPL+2)       !     "      "   "
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get name of spectrum and open it
C
      CALL DSA_INPUT('SPECT','SPECTRUM',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of data array
C
      CALL DSA_DATA_SIZE('SPECT',2,NDIM,DIMS,NELM,STATUS)
      NX=DIMS(1)
      NY=NELM/NX
C
C     Get name of MASK array and open it
C
      CALL DSA_INPUT('MASK','MASK',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get size of mask array and check for compatability with spectrum.
C     Note that if there is an AXIS mismatch but this is not because of
C     the data dimensions (only values, or units), we ignore it.  We
C     cannot use DSA_MATCH_SIZES because it won't allow a 2D spectrum and
C     a 1D mask.
C
      CALL DSA_DATA_SIZE('MASK',2,MDIM,MDIMS,MSKELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      DO IAX=1,MDIM
         CALL DSA_MATCH_AXIS('SPECT',IAX,'MASK',IAX,STATUS)
         IF (MDIMS(IAX).EQ.DIMS(IAX)) STATUS=0
      END DO
      IF (STATUS.NE.0) GO TO 500
C
C     Get number of points to be used
C
      CALL PAR_RDVAL('POINTS',4.,FLOAT(MAXSPL),10.,' ',VALUE)
      NSPL=NINT(VALUE)
C
C     Get name of OUTPUT file
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','SPECT',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map the data
C
      CALL DSA_MAP_DATA('OUTPUT','UPDATE','FLOAT',OPTR,OSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
      CALL DSA_MAP_DATA('MASK','READ','FLOAT',MPTR,MSLOT,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get data units (for RMS output)
C
      CALL DSA_GET_DATA_INFO('SPECT',1,UNITS,0,DUMMY,STATUS)
C
C     Generate the fitted spectrum
C
      PISNM = .FALSE.
      PISNO = .FALSE.
      DO IY=1,NY
         CALL FIG_MCFIT(NX,%VAL(CNF_PVAL(OPTR)),%VAL(CNF_PVAL(MPTR)),
     :                  NSPL,X,Y,WORK,%VAL(CNF_PVAL(OPTR)),RMS)
         IF (NY.EQ.1) THEN
            STRING='RMS for fit'
            NEXT=12
         ELSE
            STRING='RMS for fit to spectrum '
            INVOKE=ICH_ENCODE(STRING,FLOAT(IY),25,0,NEXT)
         END IF
         STRING(NEXT:)=' = '
         INVOKE=ICH_ENCODE(STRING,RMS,NEXT+3,4,NEXT)
         STRING(NEXT:)=' '//UNITS
         CALL PAR_WRUSER(STRING(:ICH_LEN(STRING)),IGNORE)

         CALL DYN_INCAD(OPTR,'FLOAT',NX,TPTR,ISNEWO,STATUS)
         IF (PISNO) CALL CNF_UNREGP(OPTR)
         OPTR = TPTR
         PISNO = ISNEWO

         IF (MDIM.GT.1) THEN
            CALL DYN_INCAD(MPTR,'FLOAT',NX,TPTR,ISNEWM,STATUS)
            IF (PISNM) CALL CNF_UNREGP(MPTR)
            MPTR = TPTR
            PISNM = ISNEWM
         END IF
      END DO
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
      SUBROUTINE FIG_MCFIT (NX,SPECT,MASK,NSPL,X,Y,WORK,RESULT,RMS)
C
C     F I G _ M C F I T
C
C     Creates a spectrum using a 'masked spline fit' - ie generates a
C     continuum spectrum by fitting cubic splines to the unmasked
C     portions of a spectrum.  Masked portions are those regions in
C     which the mask spectrum is non-zero.
C
C     Parameters -    (">" input, "W" workspace, "<" output)
C
C     (>) NX      (Integer) The number of elements in the spectra.
C     (>) SPECT   (Real array SPECT(NX)) The spectrum to be fitted.
C     (>) MASK    (Real array MASK(NX)) The mask spectrum.
C     (>) NSPL    (Integer) the number of splines to be used.  The
C                 actual number used may be less if some areas are
C                 masked out.
C     (W) X       (Double precision array X(NSPL+2)) Workspace.
C     (W) Y       (Double precision array Y(NSPL+2)) Workspace.
C     (W) WORK    (Double precision array WORK(8*NSPL+40)) Workspace.
C     (<) RESULT  (Real array RESULT(NX)) The resulting spectrum.
C     (<) RMS     (Real) The RMS variation between the resulting
C                 spectrum and the original spectrum, in the unmasked
C                 regions.
C
C     Common variables used - None
C
C                                             KS / CIT 6th April 1984
C     Modified:
C
C     27th Mar 1985.  KS / AAO.  Modified to use NAG routines.
C     30th Dec 1987.  KS / AAO.  RMS added.
C      5th Apr 1995.  HME / UoE, Starlink.  No longer use NAG.
C     18th May 1995.  HME / UoE, Starlink.  PDA_DBINTK needs NSPL' more
C                     workspace than I thought previously, increased
C                     WORK from 8*... to 9*...
C+
      IMPLICIT NONE
C
C     Functions
C
      DOUBLE PRECISION PDA_DBVALU
C
C     Parameters.  Note use of WORK array, which is actually split up
C     and used as three work arrays for the NAG routines E01BAF and
C     E02BBF.  These are the arrays called K,C and WRK in the NAG
C     documentation, and need to be (NSPL'+4), (NSPL'+4), and
C     (NSPL'*6+16) elements long, respectively, where NSPL'=NSPL+2
C     in order to allow for the two dummy end points that this routine
C     has to insert to ensure the splines cover the whole range.
C     In SLATEC the third needs to be (NSPL'*7+8).
C
      INTEGER NX, NSPL
      REAL    SPECT(NX), MASK(NX)
      REAL    RESULT(NX), RMS
      DOUBLE PRECISION WORK(9*NSPL+40), X(NSPL+2), Y(NSPL+2)
C
C     Local variables
C
      INTEGER CPTR, I, IFAIL, IFAIL2, INVB,
     :   INCR, IPT, IX, IXEN, IXST, IXV, NDEG
      INTEGER KPTR, NPTS, STATUS, WPTR
      REAL    TOTAL, XTOTAL
      DOUBLE PRECISION EPS, RFIT(5), A2(3*5+3*4)
      DOUBLE PRECISION DVAL, WTS(5)
C
C     Offsets into WORK array
C
      CPTR=1
      KPTR=CPTR+(NSPL+2)+4
      WPTR=KPTR+(NSPL+2)+4
C
C     Run through spectrun building up data points to be fitted
C     Note the inclusion of the two dummy points at each end.
C
      INCR=NX/NSPL
      IXV=INCR/2
      IPT=1
      X(1)=1.
      Y(1)=0.
      DO WHILE ((IXV.LT.NX).AND.(IPT.LT.NSPL+1))
         IXST=MAX(1,IXV-5)
         IXEN=MIN(NX,IXV+5)
         TOTAL=0.
         XTOTAL=0.
         NPTS=0
         DO IX=IXST,IXEN
            IF (MASK(IX).EQ.0.) THEN
               NPTS=NPTS+1
               TOTAL=TOTAL+SPECT(IX)
               XTOTAL=XTOTAL+FLOAT(IX)
            END IF
         END DO
         IF (NPTS.GT.0) THEN
            IPT=IPT+1
            Y(IPT)=TOTAL/FLOAT(NPTS)
            X(IPT)=XTOTAL/FLOAT(NPTS)
         END IF
         IXV=IXV+INCR
      END DO
      IPT=IPT+1
      X(IPT)=DBLE(NX)
      Y(IPT)=0.
C
C     Perform spline fit
C
      IF (IPT.LT.6) THEN
         CALL PAR_WRUSER('Insufficient points for a spline fit',STATUS)
      ELSE
C
C        This step is required because the NAG spline evaluator will
C        reject any point outside the X-range originally supplied.  So
C        we need to generate two extra points at the extreme ends of
C        the spectrum.  We do this by fitting a cubic to the end points
C        that we have, including one at zero weight at the actual end
C        point.  We then evaluate the fitted polynomial at that point,
C        doing this once for each end of the spectrum.
C
         WTS(1)=1.0D-6
         WTS(2)=1.
         WTS(3)=1.
         WTS(4)=1.
         WTS(5)=1.
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(5,X,Y,WTS,3,NDEG,EPS,RFIT,IFAIL,A2,IFAIL2)
         IF (NDEG.NE.3.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
         Y(1)=RFIT(1)
         WTS(1)=1.
         WTS(5)=1.0D-6
         IFAIL2=0
         EPS=0D0
         CALL PDA_DPOLFT(5,X(IPT-4),Y(IPT-4),WTS,3,NDEG,EPS,RFIT,
     :      IFAIL,A2,IFAIL2)
         IF (NDEG.NE.3.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) GO TO 400
         Y(IPT)=RFIT(5)
C
C        Spline fit to points
C
         DO I=1,4
            WORK(KPTR-1+I)=X(1)
            WORK(KPTR-1+IPT+I)=X(IPT)
         END DO
         DO I=5,IPT
            WORK(KPTR-1+I)=X(I-2)
         END DO
         IFAIL2=0
         CALL PDA_DBINTK(X,Y,WORK(KPTR),IPT,4,
     :      WORK(CPTR),WORK(WPTR+8),WORK(WPTR),IFAIL2)
         IF (IFAIL2.NE.0) GO TO 400
C
C        Generate interpolated spectrum and calculate RMS (note that
C        if SPECT and RESULT are the same array, the RMS calc must
C        precede the setting of RESULT(I)=DVAL - this caught me first
C        time through)
C
         RMS=0.0
         INVB=1
         IFAIL2=0
         DO I=1,NX
            DVAL=PDA_DBVALU(WORK(KPTR),WORK(CPTR),IPT,4,0,DBLE(I),
     :         INVB,WORK(WPTR),IFAIL2)
            IF (IFAIL2.NE.0) GO TO 400
            IF (MASK(I).EQ.0.0) RMS=RMS+(DVAL-SPECT(I))*(DVAL-SPECT(I))
            RESULT(I)=DVAL
         END DO
         RMS=SQRT(RMS/FLOAT(NX))
C
C        NAG errors bail out to here
C
  400    CONTINUE
C
      END IF
C
      END
