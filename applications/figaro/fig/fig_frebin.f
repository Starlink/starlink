C+
      SUBROUTINE FIG_FREBIN(DATIN,NIN,NOUT,XIN,XOUT,NWID,WIDTHS,
     :         ESET,ERRIN,LOGOUT,INTERP,FLUXDEN,SUMOVER,DATOUT,ERROUT)
C
C     F I G _ F R E B I N
C
C     Rebins data from an input array in which the data elements
C     cover discontinuous (and possibly overlapping) wavelength
C     ranges, into an output array in which the elements cover a
C     continuous range.
C
C     Parameters -   (">" input, "W" workspace, "<" output)
C
C     (>) DATIN     (Real array, DATIN(NIN)) The data to be redistributed.
C     (>) NIN       (Integer) The number of elements in the input arrays
C     (>) NOUT      (Integer) The number of elements in the output arrays.
C     (>) XIN       (Double precision array XIN(NIN)) The wavelength
C                   values for the centers of the ranges covered by each
C                   of the input data elements.  This routine assumes
C                   that the values in XIN are in ascending order.
C     (>) XOUT      (Double precision array XOUT(NOUT)) The wavelength
C                   values of the centres of the output elements.
C                   This routine assumes that the values in XOUT are
C                   in ascending order.
C     (>) NWID      (Integer) The number of elements in the width array.
C     (>) WIDTHS    (Real array WIDTHS(NWID)) The range in wavelength
C                   covered by each of the input bins.  If NWID=1, the
C                   same value applies to each bin.
C     (>) ESET      (Logical) True if error processing is to be performed.
C                   If false, ERRIN and ERROUT will be ignored.
C     (>) ERRIN     (Real array ERRIN(NIN)) The absolute errors on each
C                   of the input data elements.
C     (>) LOGOUT    (Logical) If true, indicates that the values of the
C                   output wavelength array increase logarithmicaly.
C     (>) INTERP    (Integer) Controls any interpolation that may be
C                   performed between data values.  0 => no interpolation
C                   (that is, data in bins is treated as being flat),
C                   1 => linear interpolation, 2 => quadratic interpolation.
C     (>) FLUXDEN   (Logical) False if the data is to be treated as
C                   being in flux units, true if it to be treated as
C                   being in flux units per unit wavelength.
C     (W) SUMOVER   (Real array SUMOVER(NOUT)) Workspace array, used
C                   to hold the total wavelength range contributing to
C                   each output bin.
C     (<) DATOUT    (Real array DATOUT(NOUT)) The output data array.
C     (<) ERROUT    (Real array ERROUT(NOUT)) The absolute errors on
C                   each of the elements of the output array.
C
C     Common variables used: None
C
C     External subroutines / functions used:
C
C     GEN_FILL      (GEN_ package) Set an array of bytes to a constant
C     FIG_FINDFIRST (FIG_FREBIN utility) Find first overlapping element
C     FIG_NEXT      (     "        "    ) See if next output bin overlaps
C     FIG_OUTLIM    (     "        "    ) Get output bin wavelength range
C
C                                          KS / AAO  16th June 1986
C     Modified:
C
C     22nd July 1986.  KS / AAO.  Now expects error values to be
C                      absolute rather than percentage errors.
C     24th April 1987. KS / AAO.  Bug fix.  Was trying to zero ERROUT
C                      even if ESET was false.
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL ESET, LOGOUT, FLUXDEN
      INTEGER NIN, NOUT, NWID, INTERP
      REAL    DATIN(NIN), WIDTHS(NWID), ERRIN(NIN), SUMOVER(NOUT)
      REAL    DATOUT(NOUT), ERROUT(NOUT)
      DOUBLE PRECISION XIN(NIN), XOUT(NOUT)
C
C     Local variables
C
      DOUBLE PRECISION ENOUT         ! End wavelength of output bin
      REAL FLUX                      ! Flux in input bin in overlap range
      INTEGER I                      ! Index variable through output data
      INTEGER NELM                   ! Index variable through input data
      INTEGER OUTELM                 ! Number of element in output data
      REAL OVERLAP                   ! Overlap range in wavelength
      DOUBLE PRECISION STOUT         ! Start wavelength of output bin
      REAL WID                       ! Width in wavelength of input bin
C
C     Set output and work arrays to zero
C
      CALL GEN_FILL(NOUT*4,0,DATOUT)
      IF (ESET) CALL GEN_FILL(NOUT*4,0,ERROUT)
      CALL GEN_FILL(NOUT*4,0,SUMOVER)
C
C     This routine works by first calculating, for each element in
C     the output data array, the total flux in the input data over
C     the range of that output element, and the total wavelength range
C     in the input data over which it was collected.
C
C     This is in fact done as follows:
C
C     For each element of the input array:
C         Get first element in the output array for which there is
C             an overlap in wavelength range with the input element.
C         Calculate the overlap in wavelength and add that to the
C             total wavelength overlap for that output element.
C         Calculate the total flux in the overlap range from the input
C             element and add that to the total flux for that output
C             element, propagating the errors if available.
C         See if the next output element overlaps the input element,
C             and if so, repeat the calculation.
C
      OUTELM=1
      DO NELM=1,NIN
         IF (NWID.LE.1) THEN
            WID=WIDTHS(1)
         ELSE
            WID=WIDTHS(NELM)
         END IF
         CALL FIG_FINDFIRST(NIN,DATIN,XIN,NELM,FLUXDEN,INTERP,WID,
     :                           XOUT,NOUT,LOGOUT,OUTELM,FLUX,OVERLAP)
         DO WHILE (OVERLAP.GT.0.0)
            SUMOVER(OUTELM)=SUMOVER(OUTELM)+OVERLAP
            DATOUT(OUTELM)=DATOUT(OUTELM)+FLUX
            IF (ESET) THEN
               ERROUT(OUTELM)=SQRT(
     :              ERROUT(OUTELM)*ERROUT(OUTELM)+
     :              (ERRIN(NELM)*(FLUX/DATIN(NELM))
     :              *ERRIN(NELM)*(FLUX/DATIN(NELM))))
            END IF
            CALL FIG_FINDNEXT(NIN,DATIN,XIN,NELM,FLUXDEN,INTERP,WID,
     :                           XOUT,NOUT,LOGOUT,OUTELM,FLUX,OVERLAP)
         END DO
      END DO
C
C     Now, for each element of the output array, calculate the
C     final output value required, either the mean flux density
C     for the bin, or the total flux for it.
C
      IF (FLUXDEN) THEN
C
C        For flux per unit wavelength data, required value is the
C        total flux divided by the total wavelength range over
C        which it was gathered.
C
         DO I=1,NOUT
            IF (SUMOVER(I).NE.0.0) THEN
               DATOUT(I)=DATOUT(I)/SUMOVER(I)
               IF (ESET) ERROUT(I)=ABS(ERROUT(I)/SUMOVER(I))
            END IF
         END DO
      ELSE
C
C        For flux data, required value is the total flux divided
C        by the total wavelength range over which it was gathered
C        times the wavelength range of the output element.
C
         DO I=1,NOUT
            IF (SUMOVER(I).NE.0.0) THEN
               CALL FIG_OUTLIM(XOUT,I,NOUT,LOGOUT,STOUT,ENOUT)
               DATOUT(I)=DATOUT(I)*(ENOUT-STOUT)/SUMOVER(I)
               IF (ESET) THEN
                  ERROUT(I)=ABS(ERROUT(I)*(ENOUT-STOUT)/SUMOVER(I))
               END IF
            END IF
         END DO
      END IF
C
      END
