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
C     28th Sep 1992  HME / UoE, Starlink.  TABs removed.
C     26th Jul 1994  Make common blocks SAVE. HME / UoE, Starlink.
C
C DESCRIPTION OF ARGUMENTS
C ------------------------
C
C   IMODE   - INTEGER -  If equal to zero, selects mode 0, in which input
C                      bins can be grouped and shifted to form the output
C                      bins.
C
C                      If not equal to zero, selects mode 1, in which the
C                      input bins are tranferred to the output bins in
C                      accordance with the wavelength scales ARC1 and ARC2.
C
C   IQUAD   - INTEGER -  If equal to zero, selects linear interpolation when
C                      rebinning.
C
C                      If not equal to zero, selects quadratic interpolation.
C
C   WDATA   - REAL*4  -  Array containing input data.
C
C   NPIX    - INTEGER -  The number of bins (elements) in WDATA.
C
C   RBIN    - REAL*4  -  Output Array, into which the rebinned data will be
C                      placed.
C
C   NRBIN   - INTEGER -  The number of bins in RBIN.
C
C   NADD    - INTEGER -  The number of input bins to be added together to
C                        form one output bin.  (Used in mode 0 only.)
C
C   SSKEW   - REAL*4  -  The number of bins the input array is to shifted (used
C                      in mode 0 only).  A positive value shifts data to
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
      REAL*4 WDATA(NPIX),RBIN(NRBIN),WAVES(NPIX),WAVESR(NRBIN)
C Declaration of variables.
      REAL*4 WDATA1,WDATA2,WDATA3
      REAL*8 DX,A,B,C,D,DD,DDD,Y
      LOGICAL LSTART
C For communication with FIG_TFORM
      LOGICAL UP,LOGS,LOGSR
      INTEGER NBIN,NBINR
      COMMON /REBIN_INFO/ UP,LOGS,LOGSR,NBIN,NBINR
      SAVE /REBIN_INFO/
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
     X                                         A*0.125D0+C*0.5D0
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
     X                                  0.08333333333333333D0
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
