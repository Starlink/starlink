      SUBROUTINE REBIN(IMODE,IQUAD,WDATA,NPIX,RBIN,NRBIN,SSKEW,
     X NARC1,ARC1,NARC2,ARC2)
C
C    Please do not make changes to this program without documenting them below.
C
C
* Version 1.5      7 Sep 1984 - Integrating mode (IMODE<0) implemented by KDH
*                               MAXCOEFF boosted from 11 to 20
C Version 1.4      8 Jul 1982 - Minor bug fixed (NSGN had the wrong value if
C                               you were rebinning from decreasing to increasing
C                               wavelength scales (or vice versa) and if, at the
C                               same time the slopes were less than a certain
C                               small number).
C Version 1.3     23 Apr 1982 - Documentation added; some code changes.
C Version 1.2     22 Apr 1982 - MACRO PLY replaced by FORTRAN version.
C Version 1.1     29 Mar 1982 - First "working version".
C
C
C COPYRIGHT 1982 M. ASHLEY.  Use of this program at this stage of
C development is entirely the user's responsibility. Unauthorized
C use or copying of this program is strictly prohibited.
C
C
C HISTORY OF DEVELOPMENT
C ----------------------
C
C      This subroutine was written by Michael Ashley as a fast
C replacement for Peter Young's subroutine of the same name, which had
C been the bottleneck when running the data reduction package LOLITA.
C
C
C QUICK SUMMARY OF OPERATION
C --------------------------
C
C      REBIN will rebin data from the first NPIX entries of array WDATA
C into the array RBIN. If IQUAD equals zero the rebinning will be done
C using linear interpolation, otherwise quadratic interpolation will be
C used. The program can be used in two modes: if IMODE equals zero then
C the output bins will be shifted by SSKEW relative to the input bins,
C and each output bin will consis of the sum of NRBIN input bins; otherwise
C the rebinnning will be done from the wavelength scale ARC1 to the scale
C ARC2, with NRBIN now meaning the number of output bins. In this later
C mode the absolute magnitudes of NARC1 and NARC2 are the number of
C coefficients in ARC1 and ARC2 respectively, and their signs indicate
C whether logarithmic scales are to be used.
* The rebinned data are the average of the original data if IMODE > 0,
* and the integral of the original data if IMODE < 0.
C
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
*
*                   If positive, the original data are averaged.
*                   If negative, the original data are integrated.
C
C   IQUAD   - INTEGER -  If equal to zero, selects linear interpolation when
C                   rebinning.
C
C                   If not equal to zero, selects quadratic interpolation.
C
C   WDATA   - REAL*4  -  Array containing input data.
C
C   NPIX    - INTEGER -  The number of bins (elements) in WDATA.
C
C   RBIN    - REAL*4  -  Output Array, into which the rebinned data will be
C                   placed.
C
C   NRBIN   - INTEGER -  MODE 0: The number of input bins to be added together
C                         to form one output bin.
C
C                   MODE 1: The number of bins in RBIN.
C
C   SSKEW   - REAL*4  -  The number of bins the input array is to shifted (used
C                   in mode 0 only).
C
C   NARC1   - INTEGER -  Used in MODE 1 only. The absolute value of NARC1 is
C                   the number of coefficients in ARC1. If NARC1 is
C                   negative, then a logarithmic scale is assumed.
C
C   ARC1    - REAL*8  -  Used in MODE 1 only. The array of polynomial coeff-
C                   icients defining the wavelength scale of the input
C                   data.
C
C   NARC2   - INTEGER -  As for NARC1 except refers to the output wavelength
C                   scale ARC2.
C
C   ARC2    - REAL*8  -  As for ARC2 except it is the output wavelength scale.
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
C      REBIN may crash in the following ways -
C
C [1] Floating point overflow in REBIN_POLY, due to invalid arc coefficients.
C
C [2] Floating point overflow, or argument out of range in REBIN_TFORM when
C     taking the logarithm and/or exponent of a result returned from
C     REBIN_POLY. This would be caused by having invalid arc coefficients.
C
C [3] In the Newton Raphson solution for output bin edges in terms of the
C     input bins it is assumed that the wavelength scales are fairly smooth,
C     and single valued over the interval of interest. If bizarre coefficients
C     are used, all sorts of difficulties may occur.
C
C [4] Divide by zero in REBIN and/or REBIN_TFORM if the derivative of the
C     ARC1 polynomial equals zero at a tested point.
C
C [5] The user must ensure tha the arrays WDATA and RBIN are at least as
C     large as NPIX and NRBIN respectively (except in the case where IMODE
C     equals zero, when RBIN must be as large as necessary to fit the
C     rebinned data).
C
C [6] ARC1 and ARC2 must be dimensioned at least as large as the absolute
C     values of NARC1 and NARC2 respectively.
C
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
C
* AVERAGING/INTEGRATING MODE
*---------------------------
* In AVERAGING mode (IMODE>0), after the number of counts
C in each output bin has been found, these are divided by the width of
C each output bin in terms of input bins. This ensures that the mean level
C of the data does not change. 
*
* In INTEGRATING mode (IMODE<0), the total number of counts is preserved.
* 
*
* Declaration of parameters.
*
      IMPLICIT NONE
      INTEGER NARC1, NARC2, INARC1, INARC2, IN11, IN12
      INTEGER NPIX, NRBIN, IMODE, I, NSGN, MAXCOEFF
      INTEGER J2, IQUAD, M1, M2, M3, KK, N, IN21, NSTOP
      INTEGER J1, K
      REAL SSKEW
*
      PARAMETER (MAXCOEFF=20)
*
* MAXCOEFF is the maximum number of coefficients allowable in ARC1 and ARC2 
* (if you change this you should ensure that REBIN_POLY can handle the new 
* maximum; it must also be changed in REBIN_TFORM). 
*
      DOUBLE PRECISION ACCBIN
      PARAMETER (ACCBIN = 1.D-4)
*
* ACCBIN is an accuracy factor which is used when the program is 
* interpolating to find the new bin edges in terms of the old bins. 
* The iterative solution stops when the absolute value of the increment 
* to the old bin position is less than ACCBIN.
*
C Declaration of arguments.
      DOUBLE PRECISION ARC1(*), ARC2(*)
      REAL WDATA(*), RBIN(*)
C Declaration of variables.
      DOUBLE PRECISION RNPIX,DX,A,B,C,D,ACC,RNRBIN
      DOUBLE PRECISION ARCONE(MAXCOEFF),ARCTWO(MAXCOEFF)
      DOUBLE PRECISION DARCONE(MAXCOEFF),RSLOPE
      DOUBLE PRECISION RX2, X1, X2, Y, DD, DDD
      LOGICAL LOGARC1,LOGARC2,LSTART
C Declaration of external functions.
      DOUBLE PRECISION PLY
C For communication with REBIN_TFORM
      COMMON /REBIN_COMMON/ARCONE,ARCTWO,DARCONE,RSLOPE,ACC,
     &IN11,IN12,IN21,N,LOGARC1,LOGARC2
C
      LSTART=.TRUE.
      LOGARC1=NARC1.LT.0
      LOGARC2=NARC2.LT.0
      INARC1=ABS(NARC1)
      INARC2=ABS(NARC2)
      IN11=INARC1-1
      IN12=INARC1-2
      IN21=INARC2-1
C A few checks are done on the input arguments, and error messages are
C printed if necessary. No indication is given to the calling program that
C anything went wrong.
      IF(NPIX.LE.0) THEN
          WRITE(*,*) 'REBIN ERROR: The number of input bins was',
     X           ' less than or equal to zero.'
          GOTO 1
      END IF
      IF(NRBIN.LE.0) THEN
          WRITE(*,*) 'REBIN ERROR: NRBIN was less than or equal',
     X           ' to zero.'
          GOTO 1
      END IF
      IF(IMODE.NE.0.AND.(INARC1.LE.1.OR.INARC1.GT.MAXCOEFF.OR.
     X INARC2.LE.1.OR.INARC2.GT.MAXCOEFF)) THEN
          WRITE(*,*) 'REBIN ERROR: Invalid number of arc ',
     X           'coefficients.'
          GOTO 1
      END IF
      ACC=ACCBIN/NPIX
      RNPIX=1.0D0/NPIX
      RNRBIN=1.0D0/NRBIN
      IF(IMODE.NE.0) THEN
          RX2=0.5D0*RNRBIN
          DO I=1,INARC1
             ARCONE(INARC1+1-I)=ARC1(I)
             DARCONE(INARC1+1-I)=(I-1)*ARC1(I)
          END DO
          DO I=1,INARC2
             ARCTWO(INARC2+1-I)=ARC2(I)
          END DO
          RSLOPE=1/PLY(DARCONE,IN12,0.2D0)
          X1=0.2
          CALL REBIN_TFORM(RX2,X1)
          X1=X1*NPIX
          DX=0
          NSGN=1
          IF((PLY(ARCTWO,IN21,1.0D0)-ARC2(1))*RSLOPE.LT.0.0D0)
     X           NSGN=-1
          NSTOP=NRBIN
      ELSE
          X1=SSKEW+0.5
          DX=NRBIN
          NSGN=1
          NSTOP=(NPIX-1)/NRBIN+1
      END IF
      J1=NINT(X1)
      DO K=1,NSTOP
       IF(IMODE.NE.0) THEN
             RX2=RX2+RNRBIN
             X2=(X1+DX)*RNPIX
             CALL REBIN_TFORM(RX2,X2)
             X2=X2*NPIX
             DX=X2-X1
       ELSE
             X2=X1+NRBIN
       END IF
       J2=NINT(X2)
       D=0
       IF(IQUAD.NE.0) THEN
             IF(LSTART) THEN
                 LSTART=.FALSE.
                 M1=MAX(MIN(J1-1,NPIX),1)
                 M2=MAX(MIN(J1,NPIX),1)
                 M3=MAX(MIN(J1+1,NPIX),1)
                 A=(WDATA(M1)+WDATA(M3))*0.5D0
                 B=(A-WDATA(M1))*0.5D0
                 C=(13.0D0/12.0D0)*WDATA(M2)-A/12.0D0
                 A=(A-WDATA(M2))/3.0D0
                 Y=X1-J1
                 DD=NSGN*((((A*Y)+B)*Y+C)*Y-B*0.25D0)+
     X                  A*0.125D0+C*0.5D0
             END IF
             M1=MAX(MIN(J2-1,NPIX),1)
             M2=MAX(MIN(J2,NPIX),1)
             M3=MAX(MIN(J2+1,NPIX),1)
             A=(WDATA(M1)+WDATA(M3))*0.5D0
             B=(A-WDATA(M1))*0.5D0
             C=1.083333333333333D0*WDATA(M2)-A*
     X            0.08333333333333333D0
             A=(A-WDATA(M2))*0.3333333333333333D0
             Y=X2-J2
             D=D-DD
             DD=NSGN*((((A*Y)+B)*Y+C)*Y-B*0.25D0)
             DDD=A*0.125D0+C*0.5D0
             D=D+DD-DDD
             DD=DD+DDD
       ELSE
             IF(LSTART) THEN
                 LSTART=.FALSE.
                 M1=MAX(MIN(J1,NPIX),1)
                 DD=(NSGN*(J1-X1)-0.5D0)*WDATA(M1)
             END IF
             M2=MAX(MIN(J2,NPIX),1)
             D=D+DD
             DDD=WDATA(M2)
             DD=(NSGN*(J2-X2)-0.5D0)*DDD
             D=D-DD-DDD
       END IF
       DO KK=J1,J2,NSGN
          D=D+WDATA(MAX(MIN(KK,NPIX),1))
       END DO
       IF( IMODE.GE.0 ) THEN            ! Change here by KDH
          RBIN(K)=REAL(D/ABS(DX))      
       ELSE                        !
             RBIN(K)=REAL(D)            !
       END IF                        !
       X1=X2
       J1=J2
      END DO
      RETURN
1     WRITE(*,*) 'REBIN ERROR:',IMODE,IQUAD,NPIX,NRBIN,SSKEW,
     X NARC1,NARC2,MAXCOEFF
      RETURN
      END

      SUBROUTINE REBIN_TFORM(RX,X)
C
C COPYRIGHT 1982 M. ASHLEY. See comments with SUBROUTINE REBIN.
C
C Version 1.2        23 Apr 1982      - More arguments put into COMMON.
C Version 1.1        29 Mar 1982      - First working version
C
      IMPLICIT NONE
      INTEGER MAXCOEFF, N, IN21, IN11, IN12
      PARAMETER (MAXCOEFF=20)
      DOUBLE PRECISION ARCONE(MAXCOEFF),ARCTWO(MAXCOEFF)
      DOUBLE PRECISION DARCONE(MAXCOEFF),RX,X,RSLOPE,RL
      DOUBLE PRECISION L,DX,ACC,PLY
      LOGICAL LOGARC1,LOGARC2
      COMMON /REBIN_COMMON/ARCONE,ARCTWO,DARCONE,RSLOPE,ACC,
     &IN11,IN12,IN21,N,LOGARC1,LOGARC2
*
      N=1
      RL=PLY(ARCTWO,IN21,RX)
      IF(LOGARC2) RL=EXP(RL)
      IF(LOGARC1) RL=LOG(RL)
1     L=PLY(ARCONE,IN11,X)
      DX=(RL-L)*RSLOPE
      X=X+DX
      IF(ABS(DX).LT.ACC) RETURN
      IF(N.GT.100) RETURN
      N=N+1
      RSLOPE=1/PLY(DARCONE,IN12,X)
      GOTO 1
      END

      SUBROUTINE SINCBIN(IMODE, DATA, NDATA, RBIN, NRBIN, SSKEW,
     &      NARC1, ARC1, NARC2, ARC2, WORK1, WORK2)
*
* Performs sinc (= (Sin X)/X) function rebinning (="scrunching") or 
* just a simple shift. Similar to REBIN but slower, but gives a superior 
* result when it is important not to smooth the data and to preserve 
* the noise properties (such as little pixel-to-pixel correlation). 
* One has to be more careful with bad data with sinc function interpolation
* compared to linear for example since they can cause ringing over neighbouring
* pixels.
*
* It is impossible to compute Sin X / X over an infinite range, and so it has
* to be terminated somehow. A straight truncation produces pronounced ringing
* and therefore considerable effort has gone into the choice of window to use.
* Eventually the following window was selected:
*
* F(X) = 4/3       - 8 X^2 + 8 X^3        0.0 < X < 0.5
*      = 8/3 - 8 X + 8 X^2 - 8 X^3 / 3    0.5 < X < 1
*
* The last equation is computationally better computed as 8/3 (1-X)^3
*
* and is symmetrical around X=0. This function is the convolution of
* two triangle functions which in turn are the convolution of two top
* hat functions and therefore the FT of the above is of the form
* ((SIN X)/X)**4. This has only positive side-lobes which decrease rapidly
* which is why the function was chosen. The positive side-lobes mean that
* no spatial frequency is actually amplified which can occur for many other
* windows.
*
* After the selection of the window function, the only free parameter is the
* truncation limit, specified in this program by MAXSINC. Tests show that 
* SINCBIN is superior to quadratic rebinning for MAXSINC > 2. MAXSINC is
* set = 15 in this version as a compromise between speed and accuracy. This
* also defines the scale over which bad pixels can cause ringing.
*
* IMODE = 0 simple shift.  N.B. not identical to REBIN version where
*                               pixels can be combined as well.
*
*   The data are simply moved by SSKEW pixels.
*
* Action: the interpolating function is computed at the beginning
* and then convolved onto data points to get shifted version. This
* is very fast by comparison with IMODE = 1 and should be used whenever
* possible. No work array used.
*
* IMODE .NE. 0 rebinning between arbitary ARC scales
*
* Action: This case is complicated. Assume data comes from sampling
* a band-limited function into bins. First we recover the values as if 
* the function had been sampled at points, not integrated into bins. 
* This is done by solving a matrix equation by an iterative method and
* is reasonably fast. We can now get the value of the function at any
* point. To rebin, the bounds of every new pixel are converted to the old 
* pixel scale. The function is integrated between the two limits. This 
* stage requires 2 calls to a NAG routine for the integral of SINC(X)
* for every pixel and is thus very slow. Work arrays are used here to 
* deconvolve the spectrum during the first stage.
*
*
* Arguments:
*
* I*4 IMODE       -- = 0 The data are simply shifted
*                    > 0 The data are averaged (e.g. for flux densities)
*                    < 0 The data are summed (useful for counts)
* R*4 DATA(NDATA) -- The data values in the original array
* I*4 NDATA       -- The number of points in the original array
* R*4 RBIN(NRBIN) -- The data values of the new array
* I*4 NRBIN       -- The number of points in the rebinned array
*                    Normally = NDATA if IMODE= 0 (although this is
*                    not compulsory). Note for IMODE=0, the use of NRBIN
*                    differs from the REBIN usage.
* R*4 SSKEW       -- if IMODE=0 SSKEW = shift in pixels to the left
*
* If IMODE does not equal 0 then you need the following as well:
*
* I*4 NARC1       -- Number of arc coefficients for original array
* R*8 ARC1(NARC1) -- The arc coefficients for the original array 
* I*4 NARC2       -- Number of arc coefficients for new array
* R*8 ARC2(NARC2) -- The arc coefficients for the new array
* R*4 WORK1(NDATA), WORK2(NDATA) - Work space arrays
*
*
* The arc scales define the wavength like so:
* 
* Wavelength at centre of pixel I of original data array
* =   sum J=1,NARC1 of ARC1(J)*(I/NDATA)**(J-1)
* 
* and similarly
*
* Wavelength at centre of pixel I of final data array
* =   sum J=1,NARC2 of ARC2(J)*(I/NRBIN)**(J-1)
*
* History:
*
* Written: January/February 1988 by TRM @RGO
*
* Bug fixed 12/8/88 TRM @RGO in IMODE = 0 section, fractional shift was
*                            applied in wrong direction.
*
* Updated for superior window function by TRM 5/10/93 @Oxford
*
*
* Function DSI used as a shareware replacement for NAG S13ADF (PFLM)
*     **   Verified its operation on 29/08/97 (TRM)
*
      IMPLICIT NONE
      INTEGER NDATA, NRBIN, NARC1, NARC2, INARC1, INARC2
      INTEGER MAXSINC,  IS, JLO, JHI, JADD
      PARAMETER (MAXSINC = 15)
      DOUBLE PRECISION SINC(-MAXSINC:MAXSINC)
      DOUBLE PRECISION SINCD(2*MAXSINC+1)
      REAL DATA(NDATA), RBIN(NRBIN)
      REAL WORK1(NDATA), WORK2(NDATA)
      DOUBLE PRECISION WEIGHT, SUM, ARC1(*), ARC2(*)
      DOUBLE PRECISION Z1, Z2, HALF, PI, X1, X2, RX2
      DOUBLE PRECISION ACCBIN, DX, KEEP, XX1, XX2, XX3
      DOUBLE PRECISION XX4, RNRBIN, SPX1, SPX2, CPX1
      DOUBLE PRECISION CPX2, XSHIFT, SCALE
      INTEGER MAXCOEFF
      PARAMETER (MAXCOEFF=20,ACCBIN=1.D-4)

      DOUBLE PRECISION ARCONE(MAXCOEFF),ARCTWO(MAXCOEFF)
      DOUBLE PRECISION DARCONE(MAXCOEFF), RSLOPE, ACC
      LOGICAL LOGARC1,LOGARC2
      INTEGER IMODE, NSHIFT, I, J, K
      INTEGER IN11, IN12, IN21, N
      REAL SSKEW, RNDATA, XLO, XHI
C
C     Declaration of external functions.
C
      DOUBLE PRECISION PLY, DSI
C
C     For communication with REBIN_TFORM
C
      COMMON /REBIN_COMMON/ARCONE,ARCTWO,DARCONE,RSLOPE,ACC,
     &IN11,IN12,IN21,N,LOGARC1,LOGARC2
      LOGICAL FIRST
      DATA FIRST/.TRUE./

      IF(IMODE.EQ.0) THEN
C
C     Compute interpolation function for simple shift. As new pixels 
C     are the same size as the old, we do not need to perform the 
C     deconvolution iteration done for IMODE .NE. 0. See above for 
C     description of window function.
C
        PI = 4.D0*ATAN(1.D0)
        NSHIFT = NINT(SSKEW)
        XSHIFT = SSKEW - REAL(NSHIFT)
        WEIGHT = 0.D0
        DO I = -MAXSINC, MAXSINC
           XX1 = PI*(XSHIFT-DBLE(I))
           XX2 = ABS((XSHIFT-DBLE(I))/(DBLE(MAXSINC)+0.5))
           IF(ABS(XX1).LT.1.D-4) THEN
              SINC(I) = 1.D0-XX1**2/6.
           ELSE
              SINC(I) = SIN(XX1)/XX1
           END IF
           IF(XX2.LE.0.5) THEN
              SINC(I) = SINC(I)*(4./3.+8.*(XX2-1.)*XX2**2)
           ELSE IF(XX2.GT.0.5) THEN
              SINC(I) = SINC(I)*8./3.*(1.-XX2)**3
           END IF
           WEIGHT = WEIGHT + SINC(I)
        END DO
C
C     Now interpolate. Data beyond ends of old array is set equal to
C     the value at the end of the array.
C
        DO K = 1, NRBIN
           IF(K.LE.-NSHIFT) THEN 
              RBIN(K) = DATA(1)
           ELSE IF(K.GT.NDATA-NSHIFT) THEN
              RBIN(K) = DATA(NDATA)
           ELSE
              J   = NSHIFT + K
              SUM = 0.D0
              DO I = -MAXSINC, MAXSINC
                 SUM = SUM + DATA(MAX(1,MIN(NDATA,J+I)))*SINC(I)
              END DO
              RBIN(K) = REAL(SUM/WEIGHT)
           END IF
        END DO        
      ELSE
C     
C     First evaluate array of integral of sinc function
C
        IF(FIRST) THEN
          PI    = 4.D0*ATAN(1.D0)
          SCALE = DBLE(MAXSINC)+0.5
          XX1   = SCALE/2.
          XX3   = PI*XX1
          CPX1  = COS(XX3)
          SPX1  = SIN(XX3)
          Z2    = DSI(XX3)/PI
          Z1    = 4.*Z2/3. + 8./(PI*SCALE)**2*
     &         ((XX1*CPX1-SPX1/PI)+(2.*(CPX1/PI+XX1*SPX1)/PI-
     &         XX1**2*CPX1)/SCALE)
          HALF  = Z1 - (8./3.*Z2 + 8./PI**2/SCALE*
     &           (CPX1+(-(XX1*CPX1-SPX1/PI)-
     &           (2.*(CPX1/PI+XX1*SPX1)/PI-
     &           XX1**2*CPX1)/SCALE/3.)/SCALE))
          WEIGHT = 0.D0
          DO I = -MAXSINC, MAXSINC
            XX1  = DBLE(I) - 5.D-1
            XX2  = XX1 + 1.D0
            XX3  = PI*XX1
            XX4  = PI*XX2
            CPX1 = COS(XX3)
            CPX2 = COS(XX4)
            SPX1 = SIN(XX3)
            SPX2 = SIN(XX4)
            Z1   = DSI(XX3)/PI
            Z2   = DSI(XX4)/PI
            IF(XX1.GE.0. .AND. XX1.LE.SCALE/2.) THEN
              Z1 = 4.*Z1/3. + 8./(PI*SCALE)**2*
     &             ((XX1*CPX1-SPX1/PI)+(2.*(CPX1/PI+XX1*SPX1)/PI-
     &              XX1**2*CPX1)/SCALE)
            ELSE IF(XX1.LT.0. .AND. XX1.GE.-SCALE/2.) THEN
              Z1 = 4.*Z1/3. + 8./(PI*SCALE)**2*
     &             ((XX1*CPX1-SPX1/PI)-(2.*(CPX1/PI+XX1*SPX1)/PI-
     &              XX1**2*CPX1)/SCALE)
            ELSE IF(XX1.GT.SCALE/2.) THEN
              Z1 = HALF + 8./3.*Z1 + 8./PI**2/SCALE*
     &             (CPX1+(-(XX1*CPX1-SPX1/PI)-
     &             (2.*(CPX1/PI+XX1*SPX1)/PI-
     &              XX1**2*CPX1)/SCALE/3.)/SCALE)
            ELSE IF(XX1.LT.-SCALE/2.) THEN
              Z1 = -HALF + 8./3.*Z1 + 8./PI**2/SCALE*
     &             (-CPX1+(-(XX1*CPX1-SPX1/PI)+
     &             (2.*(CPX1/PI+XX1*SPX1)/PI-
     &              XX1**2*CPX1)/SCALE/3.)/SCALE)
            END IF
            IF(XX2.GE.0. .AND. XX2.LE.SCALE/2.) THEN
              Z2 = 4.*Z2/3. + 8./(PI*SCALE)**2*
     &             ((XX2*CPX2-SPX2/PI)+(2.*(CPX2/PI+XX2*SPX2)/PI-
     &              XX2**2*CPX2)/SCALE)
            ELSE IF(XX2.LT.0. .AND. XX2.GE.-SCALE/2.) THEN
              Z2 = 4.*Z2/3. + 8./(PI*SCALE)**2*
     &             ((XX2*CPX2-SPX2/PI)-(2.*(CPX2/PI+XX2*SPX2)/PI-
     &              XX2**2*CPX2)/SCALE)
            ELSE IF(XX2.GT.SCALE/2.) THEN
              Z2 = HALF + 8./3.*Z2 + 8./PI**2/SCALE*
     &             (CPX2+(-(XX2*CPX2-SPX2/PI)-
     &             (2.*(CPX2/PI+XX2*SPX2)/PI-
     &              XX2**2*CPX2)/SCALE/3.)/SCALE)
            ELSE IF(XX2.LT.-SCALE/2.) THEN
              Z2 = -HALF + 8./3.*Z2 + 8./PI**2/SCALE*
     &             (-CPX2+(-(XX2*CPX2-SPX2/PI)+
     &             (2.*(CPX2/PI+XX2*SPX2)/PI-
     &              XX2**2*CPX2)/SCALE/3.)/SCALE)
            END IF
            SINCD(MAXSINC+1+I)= Z2 - Z1
            WEIGHT = WEIGHT + SINCD(MAXSINC+1+I)
          END DO
C
C     Renormalise
C
          DO I = 1, 2*MAXSINC+1
             SINCD(I) = SINCD(I)/WEIGHT
          END DO
C     
C     Remove identity matrix
C     
          SINCD(MAXSINC+1) = SINCD(MAXSINC+1) - 1.D0
          FIRST = .FALSE.
        END IF
*
* We imagine that the Y values were sampled from some function by integrating
* over a width of 1 pixelat each point. We now find the values at each point 
* before binning. This is done by solving a matrix equation iteratively. 
* (I+B)x = y is solved by putting x = y + e, and then e = - B(y+e). If B is 
* small then this can be iterated. We start with e = 0 x,y,e vectors, I,B 
* matrices. I identity matrix. 8 iterations
*
        DO I = 1, NDATA
          WORK1(I) = 0.
        END DO
        DO  K = 1, 8
          DO IS = 1, NDATA
            JLO = MAX(IS - MAXSINC, 1)
            JHI = MIN(IS + MAXSINC, NDATA)
            JADD = MAXSINC+1-IS
            SUM = 0.D0
            DO J = JLO, JHI
              SUM = SUM - SINCD(JADD+J)*(DATA(J)+WORK1(J))
            END DO
*
* End effect correction by assuming that data continues with
* last value of array 
*
            DO J = IS-MAXSINC, 0
              SUM = SUM - SINCD(JADD+J)*DATA(1)
            END DO
            DO J = NDATA+1, IS+MAXSINC
              SUM = SUM - SINCD(JADD+J)*DATA(NDATA)
            END DO
            WORK2(IS) = REAL(SUM)
          END DO
          DO I = 1, NDATA
            WORK1(I) = WORK2(I)
          END DO
        END DO
*
* Unsmoothed version
* DATA left untouched
*
        DO I = 1, NDATA
          WORK2(I) = DATA(I) + WORK1(I)
        END DO
*
* Now rebin onto new scale
* Section lifted from REBIN to
* determine the X limits of a new
* pixel when cast on to the old array
*
        LOGARC1 = NARC1.LT.0
        LOGARC2 = NARC2.LT.0
        INARC1 = ABS(NARC1)
        INARC2 = ABS(NARC2)
        IN11 = INARC1-1
        IN12 = INARC1-2
        IN21 = INARC2-1
        ACC = ACCBIN/REAL(NDATA)
        RNDATA = REAL(1.D0/DBLE(NDATA))
        RNRBIN = REAL(1.D0/DBLE(NRBIN))
        RX2 = 0.5D0*RNRBIN 
        DO I=1,INARC1
          ARCONE(INARC1+1-I) = ARC1(I)
          DARCONE(INARC1+1-I)= (I-1)*ARC1(I)
        END DO
        DO I=1,INARC2
          ARCTWO(INARC2+1-I) = ARC2(I)
        END DO
        RSLOPE=1/PLY(DARCONE,IN12,0.2D0)
        X1=0.2
        CALL REBIN_TFORM(RX2,X1)
        X1 = X1*NDATA
        DX = 0.D0
*
* Now loop through pixels of the rebinned array
*
        XLO = 0.5
        XHI = REAL(NDATA)+0.5
        DO K = 1, NRBIN
          RX2 = RX2 + RNRBIN
*
* Guess X2 then refine it
*
          X2 = (X1+DX)*RNDATA
          CALL REBIN_TFORM(RX2,X2)
*
          X2 = X2*DBLE(NDATA)
          DX = X2 - X1
*
* Determine if any of old spectrum is included in new pixel
* if not, set = 0. This eliminates spurious propagation
* of data beyond its original limits.
*
          IF((DX.GT.0. .AND. X2.GT.XLO .AND. X1.LT.XHI) .OR.
     &       (DX.LT.0. .AND. X1.GT.XLO .AND. X2.LT.XHI)) THEN
            IF(DX.LT.0.) THEN
              JLO = NINT(X2+5.D-1) - MAXSINC
              JHI = NINT(X1-5.D-1) + MAXSINC
            ELSE
              JLO = NINT(X1+5.D-1) - MAXSINC
              JHI = NINT(X2-5.D-1) + MAXSINC
            END IF
*
            WEIGHT = 0.D0
            SUM = 0.D0
*
* Slowest part. 
*
            DO J = JLO, JHI
              XX1 = X1 - REAL(J)
              XX2 = X2 - REAL(J)
              XX3 = PI*XX1
              XX4 = PI*XX2
              CPX1 = COS(XX3)
              CPX2 = COS(XX4)
              SPX1 = SIN(XX3)
              SPX2 = SIN(XX4)
              Z1 = DSI(XX3)/PI
              Z2 = DSI(XX4)/PI
              IF(XX1.GE.0. .AND. XX1.LE.SCALE/2.) THEN
                Z1 = 4.*Z1/3. + 8./(PI*SCALE)**2*
     &          ((XX1*CPX1-SPX1/PI)+(2.*(CPX1/PI+XX1*SPX1)/PI-
     &          XX1**2*CPX1)/SCALE)
              ELSE IF(XX1.LT.0. .AND. XX1.GE.-SCALE/2.) THEN
                Z1 = 4.*Z1/3. + 8./(PI*SCALE)**2*
     &          ((XX1*CPX1-SPX1/PI)-(2.*(CPX1/PI+XX1*SPX1)/PI-
     &          XX1**2*CPX1)/SCALE)
              ELSE IF(XX1.GT.SCALE/2.) THEN
                Z1 = HALF + 8./3.*Z1 + 8./PI**2/SCALE*
     &          (CPX1+(-(XX1*CPX1-SPX1/PI)-
     &          (2.*(CPX1/PI+XX1*SPX1)/PI-
     &          XX1**2*CPX1)/SCALE/3.)/SCALE)
              ELSE IF(XX1.LT.-SCALE/2.) THEN
                Z1 = -HALF + 8./3.*Z1 + 8./PI**2/SCALE*
     &          (-CPX1+(-(XX1*CPX1-SPX1/PI)+
     &          (2.*(CPX1/PI+XX1*SPX1)/PI-
     &          XX1**2*CPX1)/SCALE/3.)/SCALE)
              END IF
              IF(XX2.GE.0. .AND. XX2.LE.SCALE/2.) THEN
                Z2 = 4.*Z2/3. + 8./(PI*SCALE)**2*
     &          ((XX2*CPX2-SPX2/PI)+(2.*(CPX2/PI+XX2*SPX2)/PI-
     &          XX2**2*CPX2)/SCALE)
              ELSE IF(XX2.LT.0. .AND. XX2.GE.-SCALE/2.) THEN
                Z2 = 4.*Z2/3. + 8./(PI*SCALE)**2*
     &          ((XX2*CPX2-SPX2/PI)-(2.*(CPX2/PI+XX2*SPX2)/PI-
     &          XX2**2*CPX2)/SCALE)
              ELSE IF(XX2.GT.SCALE/2.) THEN
                Z2 = HALF + 8./3.*Z2 + 8./PI**2/SCALE*
     &          (CPX2+(-(XX2*CPX2-SPX2/PI)-
     &          (2.*(CPX2/PI+XX2*SPX2)/PI-
     &          XX2**2*CPX2)/SCALE/3.)/SCALE)
              ELSE IF(XX2.LT.-SCALE/2.) THEN
                Z2 = -HALF + 8./3.*Z2 + 8./PI**2/SCALE*
     &          (-CPX2+(-(XX2*CPX2-SPX2/PI)+
     &          (2.*(CPX2/PI+XX2*SPX2)/PI-
     &          XX2**2*CPX2)/SCALE/3.)/SCALE)
              END IF
              KEEP = Z2 - Z1
*
              WEIGHT = WEIGHT + KEEP
*
* Data off end of array is assumed to be equal to the
* appropriate end value of the array
*
              SUM = SUM + WORK2(MAX(1,MIN(NDATA,J)))*KEEP
            END DO
*
* IMODE<0 Multiply by DX to conserve counts,
* normalise by the sum of the weights.
* IMODE>0 do not multiply by DX, but by its sign to
* account for reversed arrays.
*
            IF(IMODE.GT.0.) THEN
              RBIN(K) = REAL(SUM/WEIGHT)
            ELSE
              RBIN(K) = REAL(SUM/WEIGHT*ABS(DX))
            END IF
          ELSE
            RBIN(K) = 0.
          END IF
*
* X2 becomes X1 the next time round
*
          X1 = X2
        END DO
      END IF
      RETURN
      END
