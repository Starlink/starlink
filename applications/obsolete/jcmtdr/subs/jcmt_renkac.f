      SUBROUTINE JCMT_RENKACLINE_REGRID (NPIX, ZIN, FBAD, X, Y,
     :   ZINCOPY, XCOPY, YCOPY, TRIANG, GRADS, NI, NJ, ICEN, JCEN,
     :   XINC, YINC, XCEN, YCEN, ZOUT, STATUS)
*+
*  Name:
*     JCMT_RENKACLINE_REGRID

*  Purpose:
*     Regrid data onto a regular array by interpolation using NAG routines
*     to implement the method of Renka and Cline

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL JCMT_RENKACLINE_REGRID (NPIX, ZIN, FBAD, X, Y,
*    :   ZINCOPY, XCOPY, YCOPY, TRIANG, GRADS, NI, NJ, ICEN, JCEN,
*    :   XINC, YINC, XCEN, YCEN, ZOUT, STATUS)

*  Description:
*     This routine will regrid irregularly sampled data points onto a
*     regular grid. The input data points do not need to be in any
*     particular order.
*
*     The method of resampling is to call the NAG routine E01SAF to generate
*     a 2-d surface interpolating the scattered input data points, using the
*     method of Renka and Cline. The constructed surface is continuous and has
*     continuous first derivatives. The NAG routine E01SBF is then used to
*     calculate the values of the interpolated surface at the positions
*     required in the output mesh.
*
*     Input pixels that are bad are ignored. Output pixels that are further
*     than 1 output pixel spacing from any input pixel are set bad, as are
*     any that would have to be calculated by extrapolating the interpolated
*     surface.
*
*     The routine assumes that the output array has RA increasing to the
*     left, or towards lower `i' index (XINC is negative).
*
*  Arguments:
*     NPIX = INTEGER (Given)
*        the number of input pixels
*     ZIN( NPIX ) = REAL (Given)
*        The input data values
*     FBAD = REAL (Given)
*        Value signalling bad pixel
*     X( NPIX ) = DOUBLE PRECISION (Given)
*        The x coordinates of the input pixels
*     Y( NPIX ) = DOUBLE PRECISION (Given)
*        The y coordinates of the input pixels
*     ZINCOPY (NPIX) = DOUBLE PRECISION (Scratch)
*     XCOPY (NPIX) = DOUBLE PRECISION (Scratch)
*     YCOPY (NPIX) = DOUBLE PRECISION (Scratch)
*     TRIANG (7*NPIX) = INTEGER (Scratch)
*     GRADS (2,NPIX) = DOUBLE PRECISION (Scratch)
*     NI = INTEGER (Given)
*        The number of output pixels in the x direction
*     NJ = INTEGER (Given)
*        The number of output pixels in the y direction
*     ICEN = INTEGER (Given)
*        the x index of the centre of the output array
*     JCEN = INTEGER (Given)
*        the y index of the centre of the output array
*     XINC = DOUBLE PRECISION (Given)
*        the output pixel increment in the x direction
*     YINC = DOUBLE PRECISION (Given)
*        the output pixel increment in the y direction
*     XCEN = DOUBLE PRECISION (Given)
*        the x coordinate of the centre of the output array
*     YCEN = DOUBLE PRECISION (Given)
*        the y coordinate of the centre of the output array
*     ZOUT( NI, NJ ) = REAL (Returned)
*        the regridded data array
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  [optional_subroutine_items]...
*  Authors:
*     REVAD::JFL: John Lightfoot

*  History:
*      26-AUG-1991 (REVAD::JFL): Original version
*       8-OCT-1991 (REVAD::JFL): Bug fixed locating nearest output pixels
*                                to input pixels
*      11-NOV-1991 (REVAD::JFL): MAJOR bug fixed, RA offsets now corrected
*                                for cos(dec) effect.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'ASTRO_PAR'

*  Arguments Given:
      INTEGER NPIX
      REAL ZIN (NPIX)
      REAL FBAD
      DOUBLE PRECISION X (NPIX)
      DOUBLE PRECISION Y (NPIX)
      DOUBLE PRECISION ZINCOPY (NPIX)
      DOUBLE PRECISION XCOPY (NPIX)
      DOUBLE PRECISION YCOPY (NPIX)
      INTEGER TRIANG (7*NPIX)
      DOUBLE PRECISION GRADS (2,NPIX)
      INTEGER NI
      INTEGER NJ
      INTEGER ICEN
      INTEGER JCEN
      DOUBLE PRECISION XINC
      DOUBLE PRECISION YINC
      DOUBLE PRECISION XCEN
      DOUBLE PRECISION YCEN

*  Arguments Returned:
      REAL ZOUT (NI, NJ)

*  Status:
      INTEGER STATUS                             ! Global status

*  External Functions:

*  Local Constants:

*  Local Variables:
      INTEGER IOUT, JOUT                         ! loop counters
      INTEGER ILOW, JLOW                         ! index of nearest output pixel
                                                 ! to bottom left of a given
                                                 ! input pixel
      INTEGER IPIX                               ! current pixel number
      INTEGER PIX                                ! number of valid input pixels
      INTEGER IFAIL                              ! NAG routine IFAIL
      INTEGER IGNORE                             !
      DOUBLE PRECISION XX, YY                    ! co-ords of give output pixel
      DOUBLE PRECISION ZZ                        ! interpolated value of output
                                                 ! pixel

*   local data
*.

*  Check inherited global status.

      IF (STATUS .NE. SAI__OK) RETURN

*  Not yet denagified
      STATUS = SAI__ERROR
      CALL ERR_REP(' ','This routine has not yet been de-NAGged',
     :     STATUS)
      RETURN

*  initialise the output arrays

      DO JOUT = 1, NJ
         DO IOUT = 1, NI
            ZOUT(IOUT,JOUT) = FBAD
         END DO
      END DO

*  copy the input arrays into a double precision copy, ignoring points
*  that are bad

      PIX = 1
      DO IPIX = 1, NPIX
         IF (ZIN(IPIX) .NE. FBAD) THEN
            ZINCOPY(PIX) = DBLE(ZIN(IPIX))
            XCOPY(PIX) = X(IPIX)
            YCOPY(PIX) = Y(IPIX)
            PIX = PIX + 1
         END IF
      END DO
      PIX = PIX - 1

*  make the x-axis values offsets from XCEN, corrected for cos(dec) effect,
*  and the y-axis values just offsets from YCEN

      DO IPIX = 1, PIX
         XCOPY (IPIX) = (XCOPY(IPIX) - XCEN) * DCOS (YCOPY(IPIX))
         YCOPY (IPIX) = YCOPY(IPIX) - YCEN
      END DO

*  go through the valid input pixels, set to 0 those points in the output
*  array that are within 1 output pixel spacing of an input point. The x-axis
*  may be a bit confusing because x decreases with increasing array index.

      DO IPIX = 1, PIX
         ILOW = INT (XCOPY(IPIX)/XINC) + ICEN
         JLOW = INT (YCOPY(IPIX)/YINC) + JCEN
         IF (XCOPY(IPIX) .GT. 0.0) THEN
            ILOW = ILOW - 1
         END IF
         IF (YCOPY(IPIX) .LT. 0.0) THEN
            JLOW = JLOW - 1
         END IF
         ZOUT(ILOW,JLOW) = 0.0
         ZOUT(ILOW+1,JLOW) = 0.0
         ZOUT(ILOW,JLOW+1) = 0.0
         ZOUT(ILOW+1,JLOW+1) = 0.0
      END DO

*  Calculate the interpolating surface

      IFAIL = 0
*     CALL E01SAF (PIX, XCOPY, YCOPY, ZINCOPY, TRIANG, GRADS, IFAIL)
      IF (IFAIL .NE. 0) THEN
         STATUS = SAI__ERROR
         IGNORE = 0
         IF (IFAIL .EQ. 1) THEN
            CALL PAR_WRUSER ('JCMT_RENKACLINE_REGRID - '//
     :        'NAG error, fewer than 3 pixels', IGNORE)
         ELSE IF (IFAIL .EQ. 2) THEN
            CALL PAR_WRUSER ('JCMT_RENKACLINE_REGRID - '//
     :        'NAG error, all x,y pairs are collinear', IGNORE)
         ELSE IF (IFAIL .EQ. 3) THEN
            CALL PAR_WRUSER ('JCMT_RENKACLINE_REGRID - '//
     :        'NAG error, 2 measured points are coincident ', IGNORE)
         ELSE
            CALL PAR_WRUSER ('JCMT_RENKACLINE_REGRID - '//
     :        'Unclassified error in NAG routine E01SAF', IGNORE)
         END IF

      ELSE

*  interpolate values onto output mesh, don't bother for those points that
*  have already been set to FBAD because they were too far from any input
*  points

         DO JOUT = 1, NJ
            YY = DBLE(JOUT-JCEN) * YINC
            DO IOUT = 1, NI
               IF (ZOUT(IOUT,JOUT) .NE. FBAD) THEN
                  XX = DBLE(IOUT-ICEN) * XINC
                  IFAIL = 1
*                  CALL E01SBF (PIX, XCOPY, YCOPY, ZINCOPY, TRIANG,
*     :               GRADS, XX, YY, ZZ, IFAIL)
                  IF (IFAIL .EQ. 0) THEN
                     ZOUT (IOUT, JOUT) = REAL(ZZ)
                  ELSE IF (IFAIL .EQ. 2) THEN
                     ZOUT (IOUT, JOUT) = FBAD
                     IGNORE = 0
                     CALL PAR_WRUSER ('JCMT_RENKACLINE-REGRID - '//
     :                 'NAG routine E01SBF IFAIL = 2, possible '//
     :                 'programming error', IGNORE)
                  ELSE
                     ZOUT (IOUT, JOUT) = FBAD
                  END IF
               END IF

            END DO
         END DO

      END IF


      END
