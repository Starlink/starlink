*  History:
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE ASK_INTERP (IFAIL)

*   Routine to get a beam to use in interpolating/convolving map.
*   Interpolation can now be done "on-demand", but can also be
*   done immediately. We need to sort this out...

      IMPLICIT NONE

*     Formal parameter:

      INTEGER*4 IFAIL             ! SPECX error return

*     Include files:

      INCLUDE 'FLAGCOMM'
      INCLUDE 'MAPS'

*     Miscellaneous variables

      INTEGER*4 ISTAT             ! Status return for GEN_ routines

* Ok, go...

      IFAIL = 0

*     Can't do it if cube is already both interpolated and rotated

      IF (MAP_INTERPOLATED .AND. MAP_ROTATED) THEN
        PRINT *, ' -- ask_interp -- Map is already both interpolated'//
     &                           ' and rotated!'
        IFAIL = 72
        RETURN
      END IF

*     Find out what the interpolating function looks like

      CALL GEN_GETR4 ('Beam size FWHM? (arcsecs)',
     &                 FWHM, 'F5.1', FWHM, ISTAT)
      CALL GEN_GETR4 ('Truncate spatial interpolating fn'//
     &                ' @ radius? (arcsecs)',
     &                 WMAX, 'F5.1', WMAX, ISTAT)
      CALL GEN_GETR4 ('Velocity convolving fn FWHM?',
     &                 VWIDHM, 'F5.1', VWIDHM, ISTAT)
      CALL GEN_GETR4 ('Truncate velocity convolving fn @ radius?',
     &                 VFNMAX, 'F5.1', VFNMAX, ISTAT)

*     Check that we aren't trying to turn off the interpolation

      IF ((FWHM.EQ.0.0 .OR. WMAX.EQ.0.0) .AND.
     &    (VWIDHM.EQ.0.0 .OR. VFNMAX.EQ.0.0)) THEN
        IF (MAP_INTERPOLATED) THEN
          CALL RELEASE_NEW_CUBE
          PRINT *, 'Interpolated cube deleted, reset to original'
        ELSE
          PRINT *, 'Interpolation on demand has been switched off'
          INTERP_WAIT = .FALSE.
        END IF

*     Wait to do interpolation?

      ELSE
        INTERP_WAIT = .FALSE.
        CALL GEN_YESNO('Interpolate cube immediately (else on demand)?',
     &                  .NOT.INTERP_WAIT, INTERP_WAIT, ISTAT)
        INTERP_WAIT = .NOT.INTERP_WAIT
      END IF

      RETURN
      END

C-----------------------------------------------------------------------
