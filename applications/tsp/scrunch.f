C+
      SUBROUTINE SCRUNCH(STATUS)
C
C            S C R U N C H
C
C     Command name:
C        SCRUNCH
C
C     Function:
C        Rebin a Polarization Spectrum.
C
C     Description:
C        SCRUNCH rebins a polarization spectrum onto a linear or
C        logarithmic wavelength scale. SCRUNCH is closely based on
C        the FIGARO program of the same name, and uses the same
C        subroutine to perform the rebinning.
C
C     Parameters:
C    (1) INPUT      (TSP, nD)  The input spectrum to be scrunched.
C    (2) WSTART     (Real)     The wavelength of the center of the first
C                               bin of the resulting scrunched spectrum.
C    (3) WEND       (Real)     The wavelength of the center of the final
C                               bin of the resulting scrunched spectrum.
C                               If WEND is less than WSTART, then SCRUNCH
C                               assumes that it is the increment that is
C                               being specified rather than the final value.
C                               If the scrunch is logarithmic and WSTART
C                               is greater than WEND, SCRUNCH assumes that
C                               the WEND value represents a velocity in
C                               km/s.
C    (4) BINS       (Integer)  The number of bins for the resulting spectrum.
C    (5) OUTPUT     (TSP, nD)  The Output rebinned spectrum.
C        LOG        (Logical)  Bin into logarithmic wavelength bins.
C        MEAN       (Logical)  Conserve mean data level rather than flux.
C        QUAD       (Logical)  Use quadratic (rather than linear)
C                               interpolation.
C
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 10/8/1988
C
C-
C
C  History:
C    10/8/1988   Original Version.   JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Speed of light
      REAL C
      PARAMETER (C = 299792.458)

*  Data pointers
      INTEGER IPTR,XPTR,RXPTR

*  Array dimensions
      INTEGER NDIM, DIMS(7), ACTDIM

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,RXLOC,SILOC,SOLOC
      CHARACTER*64 LABEL,UNITS
      LOGICAL OK,LOGWR,INCREM
      INTEGER SIZE,NBINR,NUM,NY
      REAL WSTART,WEND,DELTA
      LOGICAL Q,U,V,FLUX,QUAD,MEAN

*  Get the Input data

      CALL DAT_ASSOC('INPUT','READ',ILOC,STATUS)

*  Get the data size

      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_SIZE(ILOC,7,DIMS,NDIM,STATUS)
         SIZE = DIMS(1)

*  Map the wavelength axis

         CALL TSP_MAP_LAMBDA(ILOC,'READ',XPTR,XLOC,STATUS)

*  Get Label and Units for the wavelength axis

         CALL TSP_RLU_LAMBDA(ILOC,LABEL,UNITS,STATUS)

*  Get LOG parameter
         CALL PAR_GET0L('LOG',LOGWR,STATUS)

*  Get wavelength start, end and number of bins parameters
         CALL PAR_DEF0R('WSTART',%VAL(XPTR),STATUS)
         CALL PAR_DEF0R('WEND',%VAL(XPTR+4*(SIZE-1)),STATUS)
         CALL PAR_GET0R('WSTART',WSTART,STATUS)
         CALL PAR_GET0R('WEND',WEND,STATUS)
         CALL PAR_DEF0I('BINS',SIZE,STATUS)
         CALL PAR_GET0I('BINS',NBINR,STATUS)

*  If WEND is less than WSTART it is actually the wavelength increment
         INCREM = WSTART .GT. WEND

*  In this case calculate what the end wavelength really is
         IF (INCREM) THEN
            DELTA=WEND
            IF (.NOT.LOGWR) THEN
               WEND=WSTART+(NBINR-1)*DELTA
            ELSE
               WEND=EXP(LOG(WSTART)+(NBINR-1)*LOG(DELTA/C+1))
            END IF
         END IF

*  Get MEAN parameter
         CALL PAR_GET0L('MEAN',MEAN,STATUS)
         FLUX = .NOT. MEAN

*  Get Quadratic interpolation parameter
         CALL PAR_GET0L('QUAD',QUAD,STATUS)

*  Create output file
         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output and resize to new number of bins
         IF (NDIM .EQ. 1) THEN
             CALL TSP_COPY(ILOC,OLOC,STATUS)
             CALL TSP_RESIZE(OLOC,1,NBINR,STATUS)
         ELSE
             CALL TSP_COPY(ILOC,OLOC,STATUS)
             DIMS(1)=NBINR
             CALL TSP_RESIZE(OLOC,2,DIMS,STATUS)
         ENDIF

*  Map the wavelength axis of the output data
         CALL TSP_MAP_LAMBDA(OLOC,'WRITE',RXPTR,RXLOC,STATUS)

*  Fill the output wavelength axis with new values
         CALL FIG_WFILL(WSTART,WEND,LOGWR,NBINR,%VAL(RXPTR))

         IF (NDIM .EQ. 2) THEN
            NY = DIMS(2)
         ELSE
            NY = 1
         ENDIF

*  Scrunch Intensity array

         IF (STATUS .EQ. SAI__OK) THEN
            CALL TSP_SCRUNCH(ILOC,OLOC,%VAL(XPTR),%VAL(RXPTR),SIZE,
     :             NBINR,NY,QUAD,LOGWR,FLUX,STATUS)

*  Scrunch the Stokes arrays

            CALL TSP_STOKES(ILOC,NUM,Q,U,V,STATUS)
            IF (Q) THEN

*  Q stokes parameter
               CALL TSP_GET_STOKES(ILOC,'Q',SILOC,STATUS)
               CALL TSP_GET_STOKES(OLOC,'Q',SOLOC,STATUS)

*  Scrunch the Q stokes parameter
               CALL TSP_SCRUNCH(SILOC,SOLOC,%VAL(XPTR),%VAL(RXPTR),
     :             SIZE,NBINR,NY,QUAD,LOGWR,FLUX,STATUS)

*  Annul locators
               CALL DAT_ANNUL(SILOC,STATUS)
               CALL DAT_ANNUL(SOLOC,STATUS)
            ENDIF
            IF (U) THEN

*  U stokes parameter
               CALL TSP_GET_STOKES(ILOC,'U',SILOC,STATUS)
               CALL TSP_GET_STOKES(OLOC,'U',SOLOC,STATUS)

*  Scrunch the U stokes parameter
               CALL TSP_SCRUNCH(SILOC,SOLOC,%VAL(XPTR),%VAL(RXPTR),
     :             SIZE,NBINR,NY,QUAD,LOGWR,FLUX,STATUS)

*  Annul locators
               CALL DAT_ANNUL(SILOC,STATUS)
               CALL DAT_ANNUL(SOLOC,STATUS)
            ENDIF
            IF (V) THEN

*  V stokes parameter
               CALL TSP_GET_STOKES(ILOC,'V',SILOC,STATUS)
               CALL TSP_GET_STOKES(OLOC,'V',SOLOC,STATUS)

*  Scrunch the V stokes parameter
               CALL TSP_SCRUNCH(SILOC,SOLOC,%VAL(XPTR),%VAL(RXPTR),
     :             SIZE,NBINR,NY,QUAD,LOGWR,FLUX,STATUS)

*  Annul locators
               CALL DAT_ANNUL(SILOC,STATUS)
               CALL DAT_ANNUL(SOLOC,STATUS)
            ENDIF
         ENDIF

*  Unmap output arrays and annul locators

         CALL TSP_UNMAP(XLOC,STATUS)
         CALL TSP_UNMAP(RXLOC,STATUS)
         CALL DAT_ANNUL(OLOC,STATUS)
         CALL DAT_ANNUL(ILOC,STATUS)

      ENDIF
      END


      SUBROUTINE TSP_SCRUNCH(LOC1,LOC2,X1,X2,NX1,NX2,NY,
     :                        QUAD,LOG,FLUX,STATUS)
*+
*
*  T S P _ S C R U N C H
*
*  SCRUNCH command
*
*  Subroutine to scrunch a single NDF. It gets called for each Stokes
*  parameter in the polarization dataset.
*
*  Parameters:
*
*   (>)   LOC1     (Char)         Locator to input dataset
*   (>)   LOC2     (Char)         Locator to output dataset
*   (>)   X1       (Real array(NX1))  X array of input dataset
*   (>)   X2       (Real array(NX2))  X array of output dataset
*   (>)   NX1      (Integer)      Size of input dataset
*   (>)   NX2      (Integer)      Size of output dataset
*   (>)   NY       (Integer)      Second dimension of data arrays
*   (>)   QUAD     (Logical)      Use quadratic interpolation
*   (>)   LOG      (Logical)      Scrunch to logarthmic scale
*   (>)   FLUX     (Logical)      Scrunch so as to conserve flux
*   (!)   STATUS   (Integer)      Status value
*
*   Jeremy Bailey   10/8/1988
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      CHARACTER*(DAT__SZLOC) LOC1,LOC2
      INTEGER NX1,NX2,NY
      REAL X1(NX1),X2(NX2)
      LOGICAL QUAD,LOG,FLUX
      INTEGER STATUS

*  Local variables
      INTEGER IPTR,OPTR,VIPTR,VOPTR
      CHARACTER*(DAT__SZLOC) ILOC,OLOC,VILOC,VOLOC
      INTEGER IMODE,IQUAD,NADD,VSTAT,I
      LOGICAL VARIANCE
      REAL SSKEW

*  Map the input data
      CALL TSP_MAP_DATA(LOC1,'READ',IPTR,ILOC,STATUS)

*  Map the output data
      CALL TSP_MAP_DATA(LOC2,'WRITE',OPTR,OLOC,STATUS)

*  Try to map the variance
      VARIANCE=.TRUE.
      VSTAT=SAI__OK
      CALL TSP_MAP_VAR(LOC1,'READ',VIPTR,VILOC,VSTAT)
      VARIANCE = VSTAT .EQ. SAI__OK
      IF (VARIANCE) THEN

*  Map the output varaince
          CALL TSP_MAP_VAR(LOC2,'WRITE',VOPTR,VOLOC,STATUS)
      ENDIF

*  Set up parameters needed by TSP_REBIN
      IMODE=1
      IQUAD=0
      IF (QUAD) IQUAD=1
      NADD=1
      SSKEW=0.

*  Loop over Y values
      DO I=1,NY

*  Do the rebinning
         CALL TSP_REBIN(IMODE,IQUAD,%VAL(IPTR),NX1,%VAL(OPTR),NX2,
     :         NADD,SSKEW,FLUX,X1,X2,.FALSE.,LOG,.FALSE.)
         OPTR = OPTR+NX2*4
         IPTR = IPTR+NX1*4
         IF (VARIANCE) THEN

*  Rebin the variances
            CALL TSP_REBIN(IMODE,IQUAD,%VAL(VIPTR),NX1,%VAL(VOPTR),
     :         NX2,NADD,SSKEW,FLUX,X1,X2,.FALSE.,LOG,.TRUE.)
            VOPTR = VOPTR+NX2*4
            VIPTR = VIPTR+NX1*4
         ENDIF
      ENDDO

*  Unmap arrays
      CALL TSP_UNMAP(ILOC,STATUS)
      CALL TSP_UNMAP(OLOC,STATUS)
      IF (VARIANCE) THEN
         CALL TSP_UNMAP(VILOC,STATUS)
         CALL TSP_UNMAP(VOLOC,STATUS)
      ENDIF
      END



C
      SUBROUTINE TSP_REBIN(IMODE,IQUAD,WDATA,NPIX,RBIN,NRBIN,NADD,
     :                 SSKEW,CFLUX,WAVES,WAVESR,LOGW,LOGWR,VARIANCE)
C+
C     T S P _ R E B I N
C
C     QUICK SUMMARY OF OPERATION
C     --------------------------
C
C     REBIN will rebin data from the first NPIX entries of array WDATA
C into the array RBIN. If IQUAD equals zero the rebinning will be done
C using linear interpolation, otherwise quadratic interpolation will be
C used. The program can be used in two modes: if IMODE equals zero then
C the output bins will be shifted by SSKEW relative to the input bins,
C and each output bin will consis of the sum of NRBIN input bins; otherwise
C the rebinnning will be done from the wavelength scale WAVES to the scale
C WAVESR with NRBIN now meaning the number of output bins.
C
C
C      HISTORY OF DEVELOPMENT
C      ----------------------
C
C      This subroutine was written by Michael Ashley as a fast
C replacement for Peter Young's subroutine of the same name, which had
C been the bottleneck when running the data reduction package LOLITA.
C It was then butchered by Keith Shortridge to make it work with the
C Figaro data reduction system.  The main changes from Lolita to Figaro
C are due to the fact that Figaro works with arrays of wavelength
C values and Lolita uses arrays of polynomial wavelength coefficients.
C This current version is based on Ashley's REBIN version 1.4, dated
C 8th July 1983.  The subroutine REBIN_TFORM has been totally replaced,
C and the initialisation sequence has changed to comply with the new
C version.  The error messages have been removed. The calling sequence
C has had to be changed.  However, the main body of the program is
C unchanged.  The CFLUX argument is new.  For compatibility with other
C Figaro programs, the convention regarding the sign of SSKEW has been
C reversed.
C
C First Fig_Rebin working version  -  20th July 1983
C
C Modifications
C     3rd Oct 1983   KS/CIT Minor modification. Sense of SSKEW reversed to
C                    conform to normal Figaro conventions.
C
C     30th Dec 1985  KS/AAO. Operation of program with regard to data
C                    outside the range of the input data array has been
C                    changed.  If the original version of this routine
C                    had to reference data at wavelengths outside the range
C                    of the input data array, it would use either the first
C                    or last input array value.  The modified version will
C                    use a zero value under these circumstances.
C
C     10th Aug 1988  JAB/AAO. Add Variance argument. Rename to TSP_REBIN
C
C DESCRIPTION OF ARGUMENTS
C ------------------------
C
C   IMODE   - INTEGER -  If equal to zero, selects mode 0, in which input
C			 bins can be grouped and shifted to form the output
C			 bins.
C
C			 If not equal to zero, selects mode 1, in which the
C			 input bins are tranferred to the output bins in
C			 accordance with the wavelength scales ARC1 and ARC2.
C
C   IQUAD   - INTEGER -  If equal to zero, selects linear interpolation when
C			 rebinning.
C
C			 If not equal to zero, selects quadratic interpolation.
C
C   WDATA   - REAL*4  -  Array containing input data.
C
C   NPIX    - INTEGER -  The number of bins (elements) in WDATA.
C
C   RBIN    - REAL*4  -  Output Array, into which the rebinned data will be
C			 placed.
C
C   NRBIN   - INTEGER -  The number of bins in RBIN.
C
C   NADD    - INTEGER -  The number of input bins to be added together to
C                        form one output bin.  (Used in mode 0 only.)
C
C   SSKEW   - REAL*4  -  The number of bins the input array is to shifted (used
C			 in mode 0 only).  A positive value shifts data to
C                        HIGHER pixel numbers - this is a Figaro convention,
C                        and is the OPPOSITE of that used by Lolita.
C
C   CFLUX   - LOGICAL -  True if the program is to conserve flux.  If false
C                        it will maintain the mean height of the data.
C                        See below.
C
C   WAVES   - REAL*4  -  Mode 1 only - Wavelength array, giving the wavelengths
C                        of the centers of each bin in the input array.
C
C   WAVESR  - REAL*4  -  Mode 1 only - Wavelength array, giving the wavelengths
C                        of the centers of each bin in the output array.
C
C   LOGW    - LOGICAL    Mode 1 only - True if the wavelengths given in WAVES
C                        increase logarithmically.
C
C   LOGWR   - LOGICAL    Mode 1 only - true if the wavelengths given in WAVESR
C                        increase logarithmically.
C
C   VARIANCE- LOGICAL    True if the variance of the data is being processed
C                        divide by an extra factor of the size of the output
C                        bin in terms of the input bin.
C
C+
C
C POTENTIAL PROBLEMS
C ------------------
C
C      REBIN assumes a certain degree of responsibility on the part of the
C program that calls it. Only minimal checking is done to ensure sensible
C arguments. Needless to say, if REBIN crashes, a large amount of time can
C be wasted. It is up to the user to ensure that inappropriate arguments
C can never be sent to REBIN.
C
C       Most REBIN crashes will be due to floating point overflow problems
C caused by strange wavelength values, especially if logarithmic values
C are used.  The Figaro version should be slightly more robust than the
C Lolita version in this respect, since the linear interpolation between
C array values is less likely to run berserk than the polynomial evaluation
C of the earlier version.
C
C
C LINEAR/QUADRATIC REBINNING
C --------------------------
C
C      IQUAD is used as a flag to set either linear or quadratic rebinning.
C This refers to the way in which the input data is interpolated to arrive
C at the number of counts in a fraction of a bin. Suppose that one of the
C output bins completely covers one of the input bins, and partially covers
C the bins on either side. Bin <n> is treated as though it collects all the
C counts from <n-0.5> to <n+0.5>. It is not treated as sampling the data
C a position <n> only. In linear rebinning it is assumed that the count
C rate was uniform across each bin. Thus, for example, if an output bin covers
C one quarter of an input bin, then it receives one quarter of the counts
C in that bin.
C
C      In quadratic rebinning it is assumed that, locally, the original data
C followed a parabola, and that the number of counts in bin <n> results
C from integrating this parabola from <n-0.5> to <n+0.5>. When a fraction
C of an input bin is required, the equation of the parabola which fits this
C bin and the neighbouring bins is found, and then integrated appropriately.
C
C      If the program is called with CFLUX=.FALSE. after the number of
C counts in each output bin has been found, these are divided by the width of
C each output bin in terms of input bins. This ensures that the mean level
C of the data doess not change. Hence, after rebinning it is not necessarily
C valid to say that the noise in a bin is the square root of the number of
C counts in that bin.  Generally, if the data is in flux units, flux should
C be conserved, but if it is in magnitude units it is the mean level that
C should be conserved.
C
C
C Declaration of arguments.
      LOGICAL CFLUX,LOGW,LOGWR,VARIANCE
      REAL*4 WDATA(NPIX),RBIN(NRBIN),WAVES(NPIX),WAVESR(NRBIN)
C Declaration of variables.
      character*16 m1,m2,m3        !debug test
      REAL*4 WDATA1,WDATA2,WDATA3
      REAL*8 DX,A,B,C,D,DD,DDD,Y
      LOGICAL LSTART
C For communication with FIG_TFORM
      LOGICAL UP,LOGS,LOGSR
      INTEGER NBIN,NBINR
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
C
C     Copy arguments into common for FIG_TFORM
C
      LOGS=LOGW
      LOGSR=LOGWR
      NBIN=NPIX
      NBINR=NRBIN
C
      LSTART=.TRUE.
      IF(IMODE.NE.0) THEN
            UP=WAVES(NPIX).GT.WAVES(1)
            IF (UP) THEN
               REVFAC=1.
            ELSE
               REVFAC=-1.
            END IF
            IX=1
            RX2=0.5
            CALL FIG_TFORM(RX2,WAVES,WAVESR,IX,X1)
	    NSGN=1
            IF (WAVESR(NRBIN).LT.WAVESR(1)) NSGN=-1
	    NSTOP=NRBIN
      ELSE
	    X1=-SSKEW+0.5
	    DX=NADD
	    NSGN=1
	    NSTOP=(NPIX-1)/NADD+1
            REVFAC=1.
      END IF
      J1=NINT(X1)
      DO K=1,NSTOP
	 IF(IMODE.NE.0) THEN
	       RX2=RX2+1.
	       CALL FIG_TFORM(RX2,WAVES,WAVESR,IX,X2)
	       DX=X2-X1
	 ELSE
	       X2=X1+NADD
	 END IF
	 J2=NINT(X2)
	 D=0
	 IF(IQUAD.NE.0) THEN
	       IF(LSTART) THEN
		     LSTART=.FALSE.
                     IF (((J1-1).LE.0).OR.((J1-1).GT.NPIX)) THEN
                        WDATA1=0.0
                     ELSE
                        WDATA1=WDATA(J1-1)
                     END IF
                     IF ((J1.LE.0).OR.(J1.GT.NPIX)) THEN
                        WDATA2=0.0
                     ELSE
                        WDATA2=WDATA(J1)
                     END IF
                     IF (((J1+1).LE.0).OR.((J1+1).GT.NPIX)) THEN
                        WDATA3=0.0
                     ELSE
                        WDATA3=WDATA(J1+1)
                     END IF
		     A=(WDATA1+WDATA3)*0.5D0
		     B=(A-WDATA1)*0.5D0
		     C=(13.0D0/12.0D0)*WDATA2-A/12.0D0
		     A=(A-WDATA2)/3.0D0
		     Y=X1-J1
		     DD=NSGN*((((A*Y)+B)*Y+C)*Y-B*0.25D0)+
     X		                              A*0.125D0+C*0.5D0
	       END IF
               IF (((J2-1).LE.0).OR.((J2-1).GT.NPIX)) THEN
                  WDATA1=0.0
               ELSE
                  WDATA1=WDATA(J2-1)
               END IF
               IF ((J2.LE.0).OR.(J2.GT.NPIX)) THEN
                  WDATA2=0.0
               ELSE
                  WDATA2=WDATA(J2)
               END IF
               IF (((J2+1).LE.0).OR.((J2+1).GT.NPIX)) THEN
                  WDATA3=0.0
               ELSE
                  WDATA3=WDATA(J2+1)
               END IF
	       A=(WDATA1+WDATA3)*0.5D0
	       B=(A-WDATA1)*0.5D0
	       C=1.083333333333333D0*WDATA2-A*
     X		                  0.08333333333333333D0
	       A=(A-WDATA2)*0.3333333333333333D0
	       Y=X2-J2
	       D=D-DD
	       DD=NSGN*((((A*Y)+B)*Y+C)*Y-B*0.25D0)
	       DDD=A*0.125D0+C*0.5D0
	       D=D+DD-DDD
	       DD=DD+DDD
	 ELSE
	       IF(LSTART) THEN
		     LSTART=.FALSE.
                     IF ((J1.LE.0).OR.(J1.GT.NPIX)) THEN
                        DD=0.0
                     ELSE
   		        DD=(NSGN*(J1-X1)-0.5D0)*WDATA(J1)
                     END IF
	       END IF
               IF ((J2.LE.0).OR.(J2.GT.NPIX)) THEN
                  DDD=0.0
               ELSE
                  DDD=WDATA(J2)
               END IF
	       D=D+DD
	       DD=(NSGN*(J2-X2)-0.5D0)*DDD
	       D=D-DD-DDD
	 END IF
	 DO KK=J1,J2,NSGN
            IF ((KK.GE.1).AND.(KK.LE.NPIX)) D=D+WDATA(KK)
	 END DO
	 IF (.NOT.CFLUX) THEN
            RBIN(K)=D/ABS(DX)*REVFAC
         ELSE
            RBIN(K)=D*REVFAC
         END IF
         IF (VARIANCE) THEN
            RBIN(K) = RBIN(K)/ABS(DX)
         ENDIF
	 X1=X2
	 J1=J2
      END DO
C
      END
C


C+
      SUBROUTINE FIG_TFORM(RX,WAVES,WAVESR,IX,X)
C
C     F I G _ T F O R M
C
C     Rebin utility routine.  Given an x-value (RX) in the
C     rebinned data, calculates the corresponding wavelength
C     and returns (X) the corresponding x-value in the original
C     data.
C
C     History :               This performs the same function
C     as the routine REBIN_TFORM written by M. Ashley for
C     the Lolita system, but uses tables of wavelength values
C     instead of wavelength coefficients.  This makes the routine
C     quite different, and perhaps a little cruder.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (>) RX     (Real) The x-value in the rebinned data.  This
C                is a bin number.
C     (>) WAVES  (Real array) The wavelength values for the centers
C                of the bins of the original data.
C     (>) WAVESR (Real array) The wavelength values for the centers
C                of the bins of the rebinned data.
C     (!) IX     (Integer) Passed as an initial bin number at which to
C                start searching for X, so must be a valid bin number.
C                Returned as the number of one of the bins used in
C                the interpolation for X, and so probably a good
C                starting place for the next call.
C     (<) X      (Real) The x-value in the original data corresponding
C                to the wavelength at bin RX in the rebinned data.
C                This is also a bin number.
C
C     Common variables used -
C
C     (>) UP     (Logical) True if the wavelength values in WAVES
C                increase with bin number.
C     (>) LOGS   (Logical) True if the values in WAVES increase
C                logarithmically.  False otherwise.
C     (>) LOGSR  (Logical) True if the values in WAVESR increase
C                logarithmically.  False otherwise.
C     (>) NBIN   (Integer) The number of bins in the original array
C                - ie the dimension of WAVES
C     (>) NBINR  (Integer) The number of bins in the rebinned array
C                - ie the dimension of WAVESR
C
C     All common variables in
C
C     COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
C+
C     Method -
C
C     The wavelength corresponding to RX is found by linear
C     interpolation between the closest array elements in WAVESR.
C     A search through WAVES, starting from IX, finds the nearest
C     two values to that wavelength and X is then calculated by
C     linear interpolation.  If the values are logarithmic, the
C     interpolation is still linear, but the logs of the array values
C     are used.  This is cruder than the Newton Raphson solution used
C     by Michael Ashley's version, but does not require that the
C     functional form of the wavelength values be known.
C
C                                    KS / CIT 18th July 1983
C
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER IX
      REAL RX,WAVES(*),WAVESR(*),X
C
C     Common with main rebin routine
C
      LOGICAL UP,LOGS,LOGSR
      INTEGER NBIN,NBINR
C
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
C
C     Functions
C
      REAL LOG,EXP
C
C     Local variables
C
      INTEGER IRX,IRX2,IX2
      REAL LAM1,LAMBDA
C
C     Find the wavelength (LAMBDA) corresponding to RX
C
      IRX=RX
      IF (IRX.LT.1) THEN
         IRX=1
         IRX2=2
      ELSE IF (IRX.GE.NBINR) THEN
         IRX=NBINR-1
         IRX2=NBINR
      ELSE
         IRX2=IRX+1
      END IF
      IF (.NOT.LOGSR) THEN
         LAMBDA=WAVESR(IRX)+
     :          (RX-FLOAT(IRX))*(WAVESR(IRX2)-WAVESR(IRX))
      ELSE
         LAMBDA=EXP(LOG(WAVESR(IRX))+
     :           (RX-FLOAT(IRX))*(LOG(WAVESR(IRX2))-LOG(WAVESR(IRX))))
      END IF
C
C     Now find the two elements in WAVES1 that straddle LAMBDA - or
C     the end values, if we run off one end.  The following code is
C     long-winded rather than clever, sacrificing compactness for
C     simplicity and execution speed.
C
      LAM1=WAVES(IX)
      IF (UP) THEN
C
C        Values increase with bin #
C
         IF (LAM1.GT.LAMBDA) THEN
C
C           Initial guess is too high
C
            DO WHILE ((IX.GT.1).AND.(LAM1.GT.LAMBDA))
               IX=IX-1
               LAM1=WAVES(IX)
            END DO
            IX2=IX+1
         ELSE
C
C           Initial guess is too low
C
            DO WHILE ((IX.LT.NBIN).AND.(LAM1.LT.LAMBDA))
               IX=IX+1
               LAM1=WAVES(IX)
            END DO
            IX2=IX-1
         END IF
      ELSE
C
C        Wavelength values decrease with bin #.
C
         IF (LAM1.GT.LAMBDA) THEN
C
C           Initial guess is too high
C
            DO WHILE ((IX.LT.NBIN).AND.(LAM1.GT.LAMBDA))
               IX=IX+1
               LAM1=WAVES(IX)
            END DO
            IX2=IX-1
         ELSE
C
C           Initial guess is too low
C
            DO WHILE ((IX.GT.1).AND.(LAM1.LT.LAMBDA))
               IX=IX-1
               LAM1=WAVES(IX)
            END DO
            IX2=IX+1
         END IF
      END IF
C
C     Check for having run off the end
C
      IF (IX.EQ.1) THEN
         IX2=2
      ELSE IF (IX.EQ.NBIN) THEN
         IX2=NBIN-1
      END IF
C
C     Interpolate to find X
C
      IF (.NOT.LOGS) THEN
         X=(LAMBDA-LAM1)/(WAVES(IX2)-LAM1)*FLOAT(IX2-IX)+FLOAT(IX)
      ELSE
         X=(LOG(LAMBDA)-LOG(LAM1))/(LOG(WAVES(IX2))-LOG(LAM1))
     :                                    *FLOAT(IX2-IX)+FLOAT(IX)
      END IF
C
      END





C+
      SUBROUTINE FIG_WFILL (WSTART,WEND,LOGR,NBINR,ARRAY)
C
C     F I G _ W F I L L
C
C     Fills an array with wavelength values.  SCRUNCH utility.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) WSTART    (Real) The wavelength of the center of the first
C                   bin in the wavelength array.
C     (>) WEND      (Real) The wavelength of the center of the last
C                   bin in the wavelength array.  Note that WEND is
C                   allowed to be less than WSTART.
C     (>) LOGR      (Logical) True if the wavelengths are to increase
C                   logarithmically.  Otherwise, the increase will
C                   be linear.
C     (>) NBINR     (Integer) The number of wavelength bins.
C     (<) ARRAY     (Real array ARRAY(NBINR)) The output wavelength
C                   array.
C
C                                              KS / CIT 7th Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LOGR
      INTEGER NBINR
      REAL WSTART,WEND,ARRAY(NBINR)
C
C     Local variables
C
      INTEGER I
      DOUBLE PRECISION DWAVEL,DWINC
C
C     Fill the array
C
      IF (LOGR) THEN
         DWAVEL=LOG(DBLE(WSTART))
         DWINC=(LOG(DBLE(WEND))-DWAVEL)/DBLE(NBINR-1)
         DO I=1,NBINR
            ARRAY(I)=EXP(DWAVEL)
            DWAVEL=DWAVEL+DWINC
         END DO
      ELSE
         DWINC=DBLE(WEND-WSTART)/DBLE(NBINR-1)
         DWAVEL=WSTART
         DO I=1,NBINR
            ARRAY(I)=DWAVEL
            DWAVEL=DWAVEL+DWINC
         END DO
      END IF
C
      END
