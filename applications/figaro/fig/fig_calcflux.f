C+
      SUBROUTINE FIG_CALCFLUX(NIN,DATIN,XIN,NELM,FLUXDEN,INTERP,
     :                                       STOUT,ENOUT,START,END,FLUX)
C
C     F I G _ C A L C F L U X
C
C     Given that there is some overlap between an input bin and an
C     output bin, this routine calculates the total amount of flux
C     in the input bin that lies in the overlap area, using either
C     linear or quadratic interpolation.
C
C     Parameters -   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) NIN       (Integer) The number of elements in the input arrays
C     (>) DATIN     (Real array, DATIN(NIN)) The data to be redistributed.
C     (>) XIN       (Double precision array XIN(NIN)) The wavelength
C                   values for the centers of the ranges covered by each
C                   of the input data elements.  This routine assumes
C                   that the values in XIN are in ascending order.
C     (>) NELM      (Integer) The number of the element in the input
C                   array that we are looking for overlaps with.
C     (>) FLUXDEN   (Logical) False if the data is to be treated as
C                   being in flux units, true if it to be treated as
C                   being in flux units per unit wavelength.
C     (>) INTERP    (Integer) Controls any interpolation that may be
C                   performed between data values.  0 => no interpolation
C                   (that is, data in bins is treated as being flat),
C                   1 => linear interpolation, 2 => quadratic interpolation.
C     (>) NOUT      (Integer) The number of elements in the output arrays.
C     (>) STOUT     (Double precision) The start wavelength of the output
C                   array element.
C     (>) ENOUT     (Double precision) The end wavelength of the output
C                   array element.
C     (>) START     (Double precision) The start wavelength of the input
C                   array element.
C     (>) END       (Double precision) The end wavelength of the input
C                   array element.
C     (<) FLUX      (Real) The total flux in that part of the input bin
C                   that overlaps the output bin.
C
C     Common variables used: None
C
C     External subroutines / functions used:  None
C
C                                           KS / AAO 17th June 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FLUXDEN
      INTEGER NIN, NELM, INTERP
      REAL DATIN(NIN), FLUX
      DOUBLE PRECISION XIN(NIN), END, ENOUT, START, STOUT
C
C     Local variables
C
      DOUBLE PRECISION A0         ! Constant term in fitted parabola
      DOUBLE PRECISION A1         ! Linear term in fitted parabola
      DOUBLE PRECISION A2         ! Quadratic term in fitted parabola
      DOUBLE PRECISION ALPHA      ! Delta wavelength of left hand bin
      REAL AREA                   ! Area under fit in overlap area
      DOUBLE PRECISION BETA       ! Delta wavelength of right hand bin
      REAL CENDAT                 ! Data value of input bin
      DOUBLE PRECISION CENTRE     ! Wavelength of centre of input bin
      REAL FALPHA                 ! Data in left hand bin
      REAL FBETA                  ! Data in right hand bin
      DOUBLE PRECISION FC         ! Statement function, see below
      DOUBLE PRECISION FL         ! Statement function, see below
      DOUBLE PRECISION FR         ! Statement function, see below
      DOUBLE PRECISION LC         ! Constant value for left hand line
      LOGICAL LINEAR              ! True if linear interpolation used
      DOUBLE PRECISION LM         ! Slope value for left hand line
      INTEGER NLEFT               ! Number of left hand bin
      INTEGER NRIGHT              ! Number of right hand bin
      DOUBLE PRECISION OVERL      ! Left hand wavelength of overlap region
      DOUBLE PRECISION OVERR      ! Right hand wavelength of overlap region
      DOUBLE PRECISION RC         ! Constant value for right hand line
      LOGICAL REPEAT              ! Loop control variable
      DOUBLE PRECISION RM         ! Slope value for right hand line
      REAL TOTAREA                ! Total area under fit in input bin
      DOUBLE PRECISION X1         ! Dummy argument for functions, see below
      DOUBLE PRECISION X2         ! Dummy argument for functions, see below
C
C     Statement functions.  FL and FR calculate the area between
C     the points X1 and X2 under, respectively, the lines
C     Y=LM*X+LC and Y=RM*X+RC.
C
      FL(X1,X2)=(0.5*LM*(X1+X2)+LC)*(X2-X1)
      FR(X1,X2)=(0.5*RM*(X1+X2)+RC)*(X2-X1)
C
C     FC calculates the area under the curve Y=A0+A1*X+A2*X*X
C     between the points X1 and X2.
C
      FC(X1,X2)=(X2*(A0+X2*(0.5*A1+A2*X2/3.0)))-
     :                          (X1*(A0+X1*(0.5*A1+A2*X1/3.0)))
C
C     The following quantities are needed for either linear or
C     quadratic interpolation. Wavelength and data values for bin NELM,
C     and the range of the overlap area.
C
      CENTRE=XIN(NELM)
      CENDAT=DATIN(NELM)
      OVERL=MAX(STOUT,START)
      OVERR=MIN(ENOUT,END)
C
C     We have to be sure that the adjacent bins are not at the same
C     wavelength as the input bin, or the interpolation will go wild.
C     So we look down and up the array until we either run off the
C     ends or find suitable values.  The eventual interpolation involves
C     the input bin and the two numbered NLEFT and NRIGHT, which are
C     centered at wavelengths -ALPHA and BETA respecitvely from the
C     centre of the input bin.  Their data values are, respectively,
C     FALPHA and FBETA.
C
      ALPHA=0.0
      NLEFT=NELM-1
      REPEAT=NLEFT.GT.0
      DO WHILE (REPEAT)
         ALPHA=CENTRE-XIN(NLEFT)
         IF (ALPHA.GT.0.0) THEN
            FALPHA=DATIN(NLEFT)
            REPEAT=.FALSE.
         ELSE
            NLEFT=NLEFT-1
            REPEAT=NLEFT.GT.0
         END IF
      END DO
C
      BETA=0.0
      NRIGHT=NELM+1
      REPEAT=NRIGHT.LE.NIN
      DO WHILE (REPEAT)
         BETA=XIN(NRIGHT)-CENTRE
         IF (BETA.GT.0.0) THEN
            FBETA=DATIN(NRIGHT)
            REPEAT=.FALSE.
         ELSE
            NRIGHT=NRIGHT+1
            REPEAT=NRIGHT.LE.NIN
         END IF
      END DO
C
C     Now do the interpolation. The two cases (linear and quadratic) are
C     treated as separate cases, since one involves the fitting of two
C     independent straight lines, the other uses a single parabola.
C     Linear interpolation is used instead of quadratic at the end
C     points, and all the code for dealing with the end points is in
C     the linear section.
C
      LINEAR=INTERP.EQ.1
      IF (INTERP.EQ.2) THEN
         IF ((NLEFT.LE.0).OR.(NRIGHT.GT.NIN)) LINEAR=.TRUE.
      END IF
C
      IF (LINEAR) THEN
C
C        Linear interpolation.  The data is fitted by two straight
C        lines, one from the centre element to the one on its left,
C        this being the line Y=LM*X+LC, and one from the centre
C        element to the one on its right, this being the line
C        Y=RM*X+RC.   First, determine LM,LC and RM,RC.  For the
C        end bins, the left and right lines are the same - ie the
C        inner line is extrapolated over the outer half of the bin.
C
         IF (NLEFT.GT.0) THEN
            LM=(CENDAT-DATIN(NLEFT))/ALPHA
            LC=CENDAT-CENTRE*LM
         END IF
         IF (NRIGHT.LE.NIN) THEN
            RM=(DATIN(NRIGHT)-CENDAT)/BETA
            RC=CENDAT-CENTRE*RM
         END IF
         IF (NLEFT.EQ.0) THEN
            LM=RM
            LC=RC
         ELSE IF (NRIGHT.GT.NIN) THEN
            RM=LM
            RC=LC
         END IF
C
C        Now get the total area under the fitted lines in the overlap
C        area.  This is calculated in two halves, first for any overlap
C        with the left hand side of the input element, then for any
C        overlap with the right hand side.  The overlap range is from
C        OVERL to OVERR.
C
         AREA=0.0
         IF (OVERL.LT.CENTRE) AREA=AREA+FL(OVERL,MIN(CENTRE,OVERR))
         IF (OVERR.GT.CENTRE) AREA=AREA+FR(MAX(CENTRE,OVERL),OVERR)
C
C        If the data is not flux density data, we will need the
C        area under the fitted lines over the whole bin.
C
         IF (.NOT.FLUXDEN) TOTAREA=FL(START,CENTRE)+FR(CENTRE,END)
C
      ELSE
C
C        Quadratic interpolation.  The curve fitted is
C        Y=A0+A1*X+A2*X*X, where (to make the equations easier)
C        the X=0 point is at the centre of the input bin.  The
C        X values of the centres of the adjacent bins are -ALPHA
C        and +BETA and their data values are FALPHA and FBETA.
C        First calculate the polynomial coefficients.
C
         A0=CENDAT
         A1=(ALPHA*ALPHA*(FBETA-CENDAT)-BETA*BETA*(FALPHA-CENDAT))/
     :                                     (ALPHA*BETA*(ALPHA+BETA))
         A2=(ALPHA*(FBETA-CENDAT)-BETA*(FALPHA-CENDAT))/
     :                                     (ALPHA*BETA*(ALPHA+BETA))
C
C        Now, calculate the area under the fitted curve in the
C        overlap region, and (for flux data) the area under the
C        fitted curve over the whole input bin.
C
         AREA=FC(OVERL-CENTRE,OVERR-CENTRE)
         IF (.NOT.FLUXDEN) TOTAREA=FC(START-CENTRE,END-CENTRE)
C
      END IF
C
C     Now, if the data is in units of flux per unit wavelength, then
C     the total flux that we want is just the area under the fit
C     that we have calculated.  If it is in units of flux, then the
C     total flux that we want is the total flux in the input bin
C     times the ratio of the area under the fit in the overlap region
C     to the area under the fit over the whole input bin.
C
      IF (FLUXDEN) THEN
         FLUX=AREA
      ELSE
         FLUX=CENDAT*AREA/TOTAREA
      END IF
C
      END
