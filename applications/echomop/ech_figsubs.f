C   This file contains near-copies of a number of subroutines from
C   Figaro's figaro3 monolith and the libfig and libgen libraries.  The
C   correspondences are as follows:
C
C       1.  FIG_EDGES     figaro3/sdist.f
C       2.  FIG_WGEN      figaro3/iscrunch.f
C       3.  FIG_WGEN2     figaro3/iscrunch.f
C       4.  FIG_REBIN     fig/fig_rebin.f
C       5.  FIG_REBIND    fig/fig_rebind.f
C       6.  FIG_TFORM     fig/fig_tform.f
C       7.  FIG_TFORMD    fig/fig_tformd.f
C       8.  FIG_WFILL     fig/fig_wfill.f
C       9.  FIG_WFILLD    fig/fig_wfilld.f
C       10. GEN_CENTROID  gen/gen_centroid.f
C       11. GEN_EPOLYD    gen/gen_epolyd.f
C       12. GEN_RANGEF    gen/gen_rangef.f
C
C   Routines 1-9 are not quite exact copies of the corresponding Figaro
C   routines from Figaro's figaro3 monolith and the libfig library.
C   Neither one of these sets is unambiguously derived from the other
C   (though the Figaro ones have spelling typos that the Echomop ones
C   don't), and so it looks as if they've both been slightly tweaked from
C   some earlier source.
C
C   Routine 10- are exact copies of Figaro routines (as of 2004
C   August 19), for which there were no clear kaplibs or PDA
C   replacements.
C
C   10. GEN_CENTROID is not replacable by kaplibs kpg1_loctr, since
C   that works in any number of dimensions greater than one.
C
C   11. GEN_EPOLYD is a very simple little routine to evaluate a
C   polynomial.  It is not the same as pda_dpolft and pda_dp1vlu, which
C   address the more general problem of polynomial fitting.
C
C   12. GEN_RANGEF is similarly simple.  kpg1_statr is an alternative,
C   but mmuch more comprehensive, and so requires a confusing host of
C   extra dummy variables.
C
C   Moving this here, and making reference to a couple of
C   kaplibs routines (kpg1_bmedr), make echomop independent of Figaro.


C+
      REAL FUNCTION FIG_EDGES (ARRAY,NELM,CENT,LEFT,RIGHT)
C
C     F I G _ E D G E S
C
C     Attempts to find the center and right and left edges of a
C     square-ish line profile.  First a three pixel median filter
C     is run through the data, then the program starts to examine
C     the absolute values of the first derivative of the data.
C     This should have a peak at the left edge and a peak at the
C     right edge.  The program looks for the first peaks in the
C     derivative either side of the center in the range within
C     the first minima in the actual data either side of the
C     center.  The positions of the edges are more precisely
C     calculated by looking at the derivative of that absolute
C     first derivative - this should cross zero at the edge of
C     the peak.  The two points where this 2nd derivative crosses
C     zero are returned as the left and right edges of the data.
C     The function value is the position of the center of the data
C     calculated as the center of gravity of the data within
C     the left and right edges.
C
C     Call-
C
C     CENTER = FIG_EDGES(ARRAY,NELM,CENT,LEFT,RIGHT)
C
C     Parameters-
C
C     (!) ARRAY      (Real array,ref) The data for the profile.
C                    The median filtering takes place in situ, so
C                    this is returned as the filtered data.
C     (>) NELM       (Integer,ref) The number of data elements
C     (>) CENT       (Real,ref) An estimate of the profile center
C     (<) LEFT       (Real,ref) The left edge of the data
C     (<) RIGHT      (Real,ref) The right edge of the data
C
C     Returns -
C
C     (<) CENTER     (Real,function value) The calculated center
C
C                                      KS / AAO  26th March 1988
C+
      IMPLICIT NONE
C
      INTEGER NELM
      REAL ARRAY(NELM),CENT,LEFT,RIGHT
C
C     Local variables
C
      INTEGER I,ICENT,IMAX,IMIN
      REAL FIRST,LAST,WORK(3),MAXDIF,MINVAL,DIFF1,DIFF2,DIFF3
      REAL DDIF12,DDIF23,TOTAL,TOTXY
C
C     Center pixel
C
      ICENT=NINT(CENT)
C
C     Apply median filter
C
      FIRST=(ARRAY(1)+ARRAY(2))*0.5
      LAST=(ARRAY(NELM)+ARRAY(NELM-1))*0.5
      DO I=2,NELM-1
C        Get median of three values, leaving it in array(i-1).
C        Not pretty, but neither is calling Figaro's gen_sortf.
         IF (ARRAY(I-1).LT.ARRAY(I)) THEN
            IF (ARRAY(I).LT.ARRAY(I+1)) THEN
               ARRAY(I-1)=ARRAY(I)
            ELSE IF (ARRAY(I-1).LT.ARRAY(I+1)) THEN
               ARRAY(I-1)=ARRAY(I+1)
            ENDIF
         ELSE
            IF (ARRAY(I-1).GE.ARRAY(I+1)) THEN
               IF (ARRAY(I).LT.ARRAY(I+1)) THEN
                  ARRAY(I-1)=ARRAY(I+1)
               ELSE
                  ARRAY(I-1)=ARRAY(I)
               ENDIF
            ENDIF
         ENDIF
      END DO
      DO I=NELM-1,2,-1
         ARRAY(I)=ARRAY(I-1)
      END DO
      ARRAY(1)=FIRST
      ARRAY(NELM)=LAST
C
C     Look for abs diff peak to right within first minimum of data
C
      MINVAL=ARRAY(ICENT)
      IMIN=ICENT
      DO I=ICENT+1,NELM-1
         IF (ARRAY(I).LT.MINVAL) THEN
            MINVAL=ARRAY(I)
            IMIN=I
         END IF
      END DO
      IMAX=0
      MAXDIF=ABS(ARRAY(ICENT)-ARRAY(ICENT+1))
      DO I=ICENT+1,IMIN
         IF (ABS(ARRAY(I)-ARRAY(I+1)).GT.MAXDIF) THEN
            MAXDIF=ABS(ARRAY(I)-ARRAY(I+1))
            IMAX=I
         END IF
      END DO
      IF (IMAX+2.GT.NELM .OR. IMAX .LT. 2) THEN
         RIGHT=0.0
      ELSE
         DIFF1=ABS(ARRAY(IMAX-1)-ARRAY(IMAX))
         DIFF2=MAXDIF
         DIFF3=ABS(ARRAY(IMAX+1)-ARRAY(IMAX+2))
         DDIF12=DIFF2-DIFF1
         DDIF23=DIFF3-DIFF2
         RIGHT=FLOAT(IMAX)+DDIF12/MAX(1.,(DDIF12-DDIF23))
      END IF
C
C     Look for abs diff peak to left within first minimum of data
C
      MINVAL=ARRAY(ICENT)
      IMIN=ICENT
      DO I=ICENT-1,1,-1
         IF (ARRAY(I).LT.MINVAL) THEN
            MINVAL=ARRAY(I)
            IMIN=I
         END IF
      END DO
      IMAX=0
      MAXDIF=ABS(ARRAY(ICENT)-ARRAY(ICENT-1))
      DO I=ICENT,IMIN+1,-1
         IF (ABS(ARRAY(I)-ARRAY(I-1)).GT.MAXDIF) THEN
            MAXDIF=ABS(ARRAY(I)-ARRAY(I-1))
            IMAX=I
         END IF
      END DO
      IF (IMAX-2.LT.1 .OR. IMAX+1 .GT. NELM) THEN
         LEFT=0.0
      ELSE
         DIFF1=ABS(ARRAY(IMAX-2)-ARRAY(IMAX-1))
         DIFF2=MAXDIF
         DIFF3=ABS(ARRAY(IMAX)-ARRAY(IMAX+1))
         DDIF12=DIFF2-DIFF1
         DDIF23=DIFF3-DIFF2
         LEFT=FLOAT(IMAX)-DDIF12/MAX ( 1.,(DDIF12-DDIF23))
      END IF
C
C     Calculate new center
C
      IF ( (LEFT .LT. 1.0) .OR. (RIGHT .LE. 0.0)
     :                     .OR. (RIGHT .GT. FLOAT(NELM-1)) ) THEN
         FIG_EDGES=0.0
      ELSE
         TOTAL=0.0
         TOTXY=0.0
         DO I=INT(LEFT),INT(RIGHT)+1
            TOTAL=TOTAL+ARRAY(I)
            TOTXY=TOTXY+ARRAY(I)*FLOAT(I)
         END DO
         IF (TOTAL.NE.0.0) THEN
            FIG_EDGES=TOTXY/TOTAL
         ELSE
            FIG_EDGES=0.0
         END IF
      END IF
C
      END
C
C+
      SUBROUTINE FIG_WGEN(IY,NX,NY,COEFFS,nwcoeff,WARRAY)
C
C     F I G _ W G E N
C
C     Generates an array of wavelength values from a single set
C     of 2D arc coefficients.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IY     (Integer) The set of coefficients to be used
C     (>) NX     (Integer) The number of wavelength values to be
C                generated - ie the X dimension of the image the
C                coefficients apply to.
C     (>) NY     (Integer) The number of sets of coefficients - ie
C                the Y dimension of the image the coefficients apply to.
C     (>) COEFFS (Double precision COEFFS(11,NY)) The coefficients.
C                For each row, the constant term is the last non-zero
C                term.
C     (<) WARRAY (Double precision array WARRAY(NX)) The resulting
C                wavelengths.
C
C     Common variables used -  None
C
C     Subroutines / functions used - None
C
C                                            KS / CIT 24th June 1984
C     Modified:
C
C     30th March 1987.  KS/AAO.  WARRAY made double precision.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IY,NX,NY,nwcoeff,STATUS
      DOUBLE PRECISION COEFFS(nwcoeff,NY), WARRAY(NX)
C
C     Local variables
C
      DOUBLE PRECISION XVALS( 8192 )
      INTEGER I
C
C     Find out how many coefficients are being used, then
C     evaluate polynomial for all the elements of the array
C
      DO I=1,NX
        XVALS( I ) = DBLE( I )
      END DO
      CALL ECH_DFEVAL( ' ', nwcoeff, coeffs(1,iy), NX,
     :     XVALS, WARRAY, status )
C
      END
C+
      SUBROUTINE FIG_WGEN2(IY,NX,NY,FRACT,COEFFS,COEFFS2,
     :                      nwcoeff,WARRAY)
C
C     F I G _ W G E N 2
C
C     Generates an array of wavelength values from two sets of 2D
C     arc coefficients.  For each element, the value from the first
C     set of coeffients (Val1) and from the second set (Val2) is
C     calculated, and the value used is given by
C     Value = Val1 + (Val2 - Val1) * FRACT
C     where FRACT is the parameter passed to this routine.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IY      (Integer) The set of coefficients to be used
C     (>) NX      (Integer) The number of wavelength values to be
C                 generated - ie the X dimension of the image the
C                 coefficients apply to.
C     (>) NY      (Integer) The number of sets of coefficients - ie
C                 the Y dimension of the image the coefficients apply to.
C     (>) FRACT   (Real) The value used to control the interpolation
C                 between the two sets of coefficients.  See above.
C     (>) COEFFS  (Double precision COEFFS(11,NY)) The first set of
C                 coefficients.  For each row, the constant term is the
C                 last non-zero term.
C     (>) COEFFS2 (Double precision COEFFS2(11,NY)) The second set of
C                 coefficients.  Constant term as for COEFFS.
C     (<) WARRAY  (Double precision array WARRAY(NX)) The resulting
C                 wavelengths.
C
C     Common variables used -  None
C
C     Subroutines / functions used - None
C
C                                            KS / CIT 24th June 1984
C     Modified:
C
C     30th March 1987.  KS/AAO.  WARRAY made double precision.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IY,NX,NY,STATUS,NWCOEFF
      REAL    FRACT
      DOUBLE PRECISION COEFFS(nwcoeff,NY),
     :                 COEFFS2(nwcoeff,NY), WARRAY(NX)
C
C     Functions
C

C
C     Local variables
C
      INTEGER I, NCOEFF, NCOEFF2
      DOUBLE PRECISION VAL1, VAL2
C
C     Find out how many coefficients are being used, then
C     evaluate polynomial for all the elements of the array
C
      NCOEFF=1
      NCOEFF2=1
      DO I=1,NX
          CALL ECH_DFEVAL ( ' ' , nwcoeff , coeffs(1,iy) , 1 ,
     :         DBLE(i) , val1 , status)
          CALL ECH_DFEVAL ( ' ' , nwcoeff , coeffs2(1,iy) , 1 ,
     :         DBLE(i) , val2 , status)
         WARRAY(I)=VAL1+(VAL2-VAL1)*FRACT
      END DO
C
      END
C+

      SUBROUTINE FIG_REBIN(IMODE,IQUAD,WDATA,NPIX,RBIN,NRBIN,NADD,
     :                           SSKEW,CFLUX,WAVES,WAVESR,LOGW,LOGWR)
C
C     F I G _ R E B I N
C
C     QUICK SUMMARY OF OPERATION
C     --------------------------
C
C     REBIN will rebin data from the first NPIX entries of array WDATA
C into the array RBIN. If IQUAD equals zero the rebinning will be done
C using linear interpolation, otherwise quadratic interpolation will be
C used. The program can be used in two modes: if IMODE equals zero then
C the output bins will be shifted by SSKEW relative to the input bins,
C and each output bin will consist of the sum of NRBIN input bins; otherwise
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
*
*     27-JUN-1996 (MJC):
*       Modified to search WAVESR for wavescale limits in IMODE.NE.0.
*
C
C DESCRIPTION OF ARGUMENTS
C ------------------------
C
C   IMODE   - INTEGER -  If equal to zero, selects mode 0, in which input
C                        bins can be grouped and shifted to form the output
C                        bins.
C
C                        If not equal to zero, selects mode 1, in which the
C                        input bins are tranferred to the output bins in
C                        accordance with the wavelength scales ARC1 and ARC2.
C
C   IQUAD   - INTEGER -  If equal to zero, selects linear interpolation when
C                        rebinning.
C
C                        If not equal to zero, selects quadratic interpolation.
C
C   WDATA   - REAL    -  Array containing input data.
C
C   NPIX    - INTEGER -  The number of bins (elements) in WDATA.
C
C   RBIN    - REAL    -  Output Array, into which the rebinned data will be
C                        placed.
C
C   NRBIN   - INTEGER -  The number of bins in RBIN.
C
C   NADD    - INTEGER -  The number of input bins to be added together to
C                        form one output bin.  (Used in mode 0 only.)
C
C   SSKEW   - REAL    -  The number of bins the input array is to shifted (used
C                        in mode 0 only).  A positive value shifts data to
C                        HIGHER pixel numbers - this is a Figaro convention,
C                        and is the OPPOSITE of that used by Lolita.
C
C   CFLUX   - LOGICAL -  True if the program is to conserve flux.  If false
C                        it will maintain the mean height of the data.
C                        See below.
C
C   WAVES   - REAL    -  Mode 1 only - Wavelength array, giving the wavelengths
C                        of the centers of each bin in the input array.
C
C   WAVESR  - REAL    -  Mode 1 only - Wavelength array, giving the wavelengths
C                        of the centers of each bin in the output array.
C
C   LOGW    - LOGICAL    Mode 1 only - True if the wavelengths given in WAVES
C                        increase logarithmically.
C
C   LOGWR   - LOGICAL    Mode 1 only - true if the wavelengths given in WAVESR
C                        increase logarithmically.
C
C
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
C Declaration of arguments.
      LOGICAL CFLUX,LOGW,LOGWR
      REAL  WDATA (NRBIN),waves(npix),wavesr(nrbin),rbin(nrbin)
C Declaration of variables.
      REAL WDATA1,WDATA2,WDATA3
      DOUBLE PRECISION DX,A,B,C,D,DD,DDD,Y
      LOGICAL LSTART
C For communication with FIG_TFORM
      LOGICAL UP,LOGS, LOGSR
      INTEGER NBIN,NBINR
      INTEGER I, J, K
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
            NSTOP=NRBIN

*        Find limits of global wavelength scale.
            I = 1
            DO WHILE( WAVESR( I ) .EQ. 0.0 .AND. I .LT. NRBIN )
               I = I + 1
            END DO
            J = NRBIN
            DO WHILE( WAVESR( J ) .EQ. 0.0 .AND. J .GT. 1 )
               J = J - 1
            END DO

*        Determine orientation of wavelength scale.
            IF ( WAVESR( J ) .LT. WAVESR( I ) .AND.
     :           WAVESR( I ) .NE. 0.0 ) THEN
               NSGN = -1

            ELSE
               NSGN = 1
            END IF

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
     X                                        A*0.125D0+C*0.5D0
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
     X                            0.08333333333333333D0
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
         X1=X2
         J1=J2
      END DO
C
      END
C
      SUBROUTINE FIG_REBIND(IMODE,IQUAD,WDATA,NPIX,RBIN,NRBIN,NADD,
     :                           SSKEW,CFLUX,WAVES,WAVESR,LOGW,LOGWR)
C
C See comments for FIG_REBIN.  This routine is exactly the same,
C except that the wavelength arrays (WAVES, WAVESR) are double precision.
C
C KS / AAO 12th Sept 1985
C
C Modified: 30th Dec 1985 KS/AAO Same change as for FIG_REBIN for data
C           outside the range of the input array.
*
*     27-JUN-1996 (MJC):
*       Modified to search WAVESR for wavescale limits in IMODE.NE.0.
*
C
C Declaration of arguments.
      LOGICAL CFLUX,LOGW,LOGWR
      REAL WDATA(NPIX),RBIN(NRBIN)
      DOUBLE PRECISION WAVES(NPIX),WAVESR(NRBIN)
C Declaration of variables.
      DOUBLE PRECISION WDATA1,WDATA2,WDATA3
      DOUBLE PRECISION DX,A,B,C,D,DD,DDD,Y,RX2,X1,X2
      LOGICAL LSTART
C For communication with FIG_TFORMD
      LOGICAL UP,LOGS, LOGSR
      INTEGER NBIN,NBINR
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
C
C     Copy arguments into common for FIG_TFORMD
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
            CALL FIG_TFORMD(RX2,WAVES,WAVESR,IX,X1)
            NSTOP=NRBIN

*        Find limits of global wavelength scale.
            I = 1
            DO WHILE( WAVESR( I ) .EQ. 0.0 .AND. I .LT. NRBIN )
               I = I + 1
            END DO
            J = NRBIN
            DO WHILE( WAVESR( J ) .EQ. 0.0 .AND. J .GT. 1 )
               J = J - 1
            END DO

*        Determine orientation of wavelength scale.
            IF ( WAVESR( J ) .LT. WAVESR( I ) .AND.
     :           WAVESR( I ) .NE. 0.0 ) THEN
               NSGN = -1

            ELSE
               NSGN = 1
            END IF

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
               CALL FIG_TFORMD(RX2,WAVES,WAVESR,IX,X2)
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
     X                                        A*0.125D0+C*0.5D0
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
     X                            0.08333333333333333D0
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
         X1=X2
         J1=J2
      END DO
C
      END

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
C
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
C+
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
      SUBROUTINE FIG_TFORMD(RX,WAVES,WAVESR,IX,X)
C
C     F I G _ T F O R M D
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
C     quite different, and perhaps a little cruder.  This routine
C     This is a double precision version of the routine FIG_TFORM,
C     intended for use by FIG_REBIND.
C
C     Parameters -    (">" input, "!" modified, "<" output)
C
C     (>) RX     (Double precision) The x-value in the rebinned data.
C                This is a bin number.
C     (>) WAVES  (Double precision  array) The wavelength values for
C                the centers of the bins of the original data.
C     (>) WAVESR (Double precision array) The wavelength values for
C                the centers of the bins of the rebinned data.
C     (!) IX     (Integer) Passed as an initial bin number at which to
C                start searching for X, so must be a valid bin number.
C                Returned as the number of one of the bins used in
C                the interpolation for X, and so probably a good
C                starting place for the next call.
C     (<) X      (Double precision) The x-value in the original data
C                corresponding to the wavelength at bin RX in the
C                rebinned data.  This is also a bin number.
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
C
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
C                                    KS / AAO 13th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER IX
      DOUBLE PRECISION RX,WAVES(*),WAVESR(*),X
C
C     Common with main rebin routine
C
      LOGICAL UP,LOGS,LOGSR
      INTEGER NBIN,NBINR
C
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
C
C     Local variables
C
      INTEGER IRX,IRX2,IX2
      DOUBLE PRECISION LAM1,LAMBDA
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
C+
      SUBROUTINE FIG_WFILLD (WSTART,WEND,LOGR,NBINR,ARRAY)
C
C     F I G _ W F I L L D
C
C     Fills a double precision array with wavelength values.
C     SCRUNCH utility.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) WSTART    (Double precision) The wavelength of the center of
C                   the first bin in the wavelength array.
C     (>) WEND      (Double precision) The wavelength of the center of
C                   the last bin in the wavelength array.  Note that WEND
C                   is allowed to be less than WSTART.
C     (>) LOGR      (Logical) True if the wavelengths are to increase
C                   logarithmically.  Otherwise, the increase will
C                   be linear.
C     (>) NBINR     (Integer) The number of wavelength bins.
C     (<) ARRAY     (Double precision array ARRAY(NBINR)) The output
C                   wavelength array.
C
C                                              KS / AAO 13th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LOGR
      INTEGER NBINR
      DOUBLE PRECISION WSTART,WEND,ARRAY(NBINR)
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
C+
      SUBROUTINE GEN_CENTROID(FARRAY,NO_EL,LSIG,LCENT,
     :                                       LSTRENGTH,STATUS)
C
C (local version of Figaro routine...)
C       G E N _ C E N T R O I D
C
C       Locates a peak in a data array by convolution with the
C       derivative of a Gaussian.
C
C       Parameters -    (">" input, "<" output, "!" modified)
C
C       (>) FARRAY    (Real array FARRAY(NO_EL)) Array holding
C                     the data to be searched for a peak.
C       (>) NO_EL     (Integer) The number of elements in FARRAY.
C       (>) LSIG      (Real) Measure of the expected line width -
C                     in elements - center will be taken over a
C                     block of data 6*LSIG elements wide.
C       (!) LCENT     (Real) Passed as a first guess as to the
C                     position of the line center, returned as the
C                     determined position.  Note that the first
C                     element of FARRAY is #1, not 0.
C                     Note: Assumes that the integer value of the
C                     pixel number refers to the center of the
C                     pixel.  ie if data is all zeros except for
C                     elements 9,10,11 which contain say, 25,50,25
C                     counts, LCENT will return as 10.0
C       (<) LSTRENGTH (Real) Returned as a measure of the line
C                     strength.  If an error occurs, LSTRENGTH
C                     will be set to some negative number.
C       (<) STATUS    (Integer) Returns a status code.
C                     0 => OK
C                     1 => Line too close to end of array.
C                     2 => Measurement does not converge.
C                     3 => Measurement is running away.
C                     4 => LSIG passed with a daft value.
C
C       History -
C
C       Orginally obtained from TYB's FORTH package.
C       Modified by KS: Error messages removed, replaced by
C       STATUS parameter; Integer array option removed.
C
C       This version -             KS / CIT   17th April 1983
C
C       Modified:
C
C       28th Sep 1992  HME / UoE, Starlink.  TABs removed.
C+
        INTEGER STATUS
        REAL FARRAY(NO_EL)
        REAL LCENT,LSIG,LSTRENGTH,LAST_LCENT
        IF(LSIG.EQ.0.)GO TO 3999
        NO_ITER=0
        PRECISION=0.001
        DEN=2.*LSIG**2
C ******************************************************
C   EACH ITERATION STARTS HERE
C ******************************************************
  100   LAST_LCENT = LCENT
        NO_ITER = NO_ITER + 1
C ******************************************************
C   SET 3 SIGMA LIMITS AROUND CENTER
C ******************************************************
        LLLIM = LCENT - 3. * LSIG
        LULIM = LCENT + 3. * LSIG + 0.5      ! TO ROUND
C ******************************************************
C   CHECK TO MAKE SURE ENTIRE REGION IS WITHIN ARRAY
C ******************************************************
        IF (LLLIM.LT.1) GO TO 999
        IF (LULIM.GT.NO_EL) GO TO 999
C ******************************************************
C   NOW CALCULATE G(X) AND G'(X)
C ******************************************************
        GOFX = 0.
        GPOFX = 0.
        DO 202 I = LLLIM,LULIM
        T1 = FLOAT(I) - LCENT - 0.5
        T2 = T1 + 1.
        T3 = FARRAY(I)
        TERM1 = EXP (-(T1**2)/DEN)
        TERM2 = EXP (-(T2**2)/DEN)
        GOFX = GOFX + T3 * (TERM2-TERM1)/2.
        GPOFX = GPOFX + T3 * (T2*2./DEN*TERM2 - T1* 2./DEN*TERM1)/2.
  202   CONTINUE
C ******************************************************
C   CHECK TO SEE IF IT IS RUNNING AWAY
C ******************************************************
        IF(GPOFX.EQ.0.)GO TO 2999
        IF(ABS(GOFX/GPOFX).GT.1.5*LSIG) GO TO 2999
C ******************************************************
C   OR BOUNCING BACK AND FORTH
C ******************************************************
        IF (NO_ITER.GE.10) GO TO 1999
C ******************************************************
C   CALCULATE NEW LCENT
C ******************************************************
        LCENT = LCENT - GOFX/GPOFX
C ******************************************************
C   HAS IT CONVERGED?
C ******************************************************
        IF(ABS(LCENT-LAST_LCENT).LE.PRECISION) GO TO 300
        GO TO 100
C ******************************************************
C   END OF ITERATION
C ******************************************************
  300   LSTRENGTH = GPOFX * DEN
        STATUS=0
        RETURN
  999   CONTINUE
        STATUS=1
        LSTRENGTH = 0.
        RETURN
 1999   CONTINUE
        STATUS=2
        LSTRENGTH = -1.
        RETURN
 2999   CONTINUE
        STATUS=3
        LSTRENGTH = -2
        RETURN
 3999   CONTINUE
        STATUS=4
        LSTRENGTH = -3.
        RETURN
        END
C+
      DOUBLE PRECISION FUNCTION GEN_EPOLYD(VALUE,COEFFS,NCOEFF)
C
C     G E N _ E P O L Y D
C
C     Evaluates a polynomial - this is an interim version.
C     Ideally the VAX POLYD instruction should be used.
C
C     Parameters - (">" input, "<" output)
C
C     (>) VALUE   (Double precision) The value for which the
C                 polynomial is to be evaluated.
C     (>) COEFFS  (Double precision  array COEFFS(NCOEFF)) The
C                 coefficients of the polynomial, with the
C                 constant term last.
C     (>) NCOEFF  (Integer) The number of polynomial coefficients.
C
C     Returns -
C
C     (<) GEN_EPOLYD  (Double precision) The value of the polynomial.
C
C                                  KS / CIT 6th May 1983
C     History:
C
C     5th  Feb 1988  (CKL/CIT) VALUE changed from REAL to DOUBLE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NCOEFF
      DOUBLE PRECISION VALUE,COEFFS(NCOEFF)
C
C     Local variables
C
      INTEGER NORD
C
C     Evaluate poly
C
      GEN_EPOLYD=0.
      DO NORD=1,NCOEFF
         GEN_EPOLYD=GEN_EPOLYD*VALUE+COEFFS(NORD)
      END DO
C
      END
C+
      SUBROUTINE GEN_RANGEF(ARRAY,IST,IEN,VMAX,VMIN)
C
C     G E N _ R A N G E F
C
C     Finds the maximum and minimum values in an array.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Real array ARRAY(IEN) - or more) Array
C                   containing the values to be checked.
C     (>) IST       (Integer) The first element of ARRAY to be
C                   examined.
C     (>) IEN       (Integer) The last element of ARRAY to be
C                   examined.
C     (<) VMAX      (Real) The maximum value of those examined.
C     (<) VMIN      (Real) The minimum value of those examined.
C
C                                      KS / CIT  2nd Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER IST,IEN
      REAL ARRAY(IEN),VMIN,VMAX
C
C     Local variables
C
      INTEGER I
C
      VMIN=ARRAY(IST)
      VMAX=VMIN
      IF (IST.LT.IEN) THEN
         DO I=IST+1,IEN
            VMIN=MIN(VMIN,ARRAY(I))
            VMAX=MAX(VMAX,ARRAY(I))
         END DO
      END IF
C
      END
