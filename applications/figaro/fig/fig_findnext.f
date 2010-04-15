C+
      SUBROUTINE FIG_FINDNEXT(NIN,DATIN,XIN,NELM,FLUXDEN,INTERP,WID,
     :                           XOUT,NOUT,LOGOUT,OUTELM,FLUX,OVERLAP)
C
C     F I G _ F I N D N E X T
C
C     Given an element in the output wavelength array that
C     overlaps a target element in the input array, sees if the
C     next element also overlaps that target element, and returns
C     the overlap range and the total flux in the overlap region
C     if it does.
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
C     (>) WID       (Real) The width in wavelength of the input element.
C     (>) XOUT      (Double precision array XOUT(NOUT)) The wavelength
C                   values of the centres of the output elements.
C                   This routine assumes that the values in XOUT are
C                   in ascending order.
C     (>) NOUT      (Integer) The number of elements in the output arrays.
C     (>) LOGOUT    (Logical) If true, indicates that the values of the
C                   output wavelength array increase logarithmicaly.
C     (!) OUTELM    (Integer) Number of an element in the output array.
C                   (>) An element that is known to overlap the target bin.
C                   (<) The next element number in the output array.  This
C                       is only an overlapping element if OVERLAP is > 0.
C     (<) FLUX      (Real) The total flux in that part of the input bin
C                   that overlaps the output bin.
C     (<) OVERLAP   (Real) The overlap in wavelength between the two bins.
C                   If the next element does not overlap, OVERLAP is
C                   returned as zero.
C
C     Common variables used: None
C
C     External subroutines / functions used:
C
C     FIG_OUTLIM    (FIG_FREBIN utility) Get output bin wavelength range
C     FIG_CALCFLUX  ( "    "       "   ) Calculate flux in overlap region
C
C                                           KS / AAO 17th June 1986
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL FLUXDEN, LOGOUT
      INTEGER NIN, NELM, INTERP, NOUT, OUTELM
      REAL DATIN(NIN), FLUX, WID, OVERLAP
      DOUBLE PRECISION XIN(NIN), XOUT(NOUT)
C
C     Local variables
C
      DOUBLE PRECISION END          ! End wavelength of the input element
      DOUBLE PRECISION ENOUT        ! End wavelength of the output element
      DOUBLE PRECISION START        ! Start wavelength of the input element
      DOUBLE PRECISION STOUT        ! Start wavelength of the output element
C
C     Set overlap to zero at first, calculate the start and end
C     wavelengths of the target element.
C
      OVERLAP=0.0
      START=XIN(NELM)-WID*0.5
      END=START+WID
C
C     If there is a next element, see if it overlaps
C
      IF (OUTELM.LT.NOUT) THEN
         OUTELM=OUTELM+1
         CALL FIG_OUTLIM(XOUT,OUTELM,NOUT,LOGOUT,STOUT,ENOUT)
         IF ((START.LT.ENOUT).AND.(END.GT.STOUT)) THEN
            OVERLAP=MIN(END,ENOUT)-MAX(START,STOUT)
            IF (INTERP.EQ.0) THEN
               IF (FLUXDEN) THEN
                  FLUX=DATIN(NELM)*OVERLAP
               ELSE
                  FLUX=DATIN(NELM)*OVERLAP/WID
               END IF
            ELSE
               CALL FIG_CALCFLUX(NIN,DATIN,XIN,NELM,FLUXDEN,INTERP,
     :                                       STOUT,ENOUT,START,END,FLUX)
            END IF
         END IF
      END IF
C
      END
