*  History:
*     27 Jan 1994 (hme):
*        Use make_map4.
*-----------------------------------------------------------------------

      SUBROUTINE SPECX_FITSMAP (XSCALE, BUF, IFAIL)

*  Routine to create and write a 2-D FITS map to either disk or tape.
*  Only works on X-Y maps -- no provision for position-velocity diagrams.

      IMPLICIT  NONE

*     Formal parameters:

      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

*     Include files

      INCLUDE   'FLAGCOMM'
      INCLUDE   'SPECX_FITS'

*  Ok, go...

      IF (.NOT.FITS_OPEN) THEN
        IFAIL = 111
        RETURN
      END IF

      IF (LINK(2).NE.2 .OR. LINK(3).NE.3) THEN
        WRITE (6, *) 'Can only write RA-Dec or X-Y maps!'
        IFAIL = 110
        RETURN
      END IF

      CALL MAKE_MAP4 (BUF, XSCALE, IFAIL)
      IF (IFAIL.NE.0) RETURN

      CALL SPECX_WRFITSMAP (IFAIL)

      RETURN
      END

*-----------------------------------------------------------------------

