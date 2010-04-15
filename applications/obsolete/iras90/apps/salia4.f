      SUBROUTINE SALIA4( QUAL, VAR, METHOD, IXLO2, IXHI2, IYLO2, IYHI2,
     :                   IXLO1, IXHI1, IYLO1, IYHI1, DATIN, VARIN,
     :                   QLIN, XMAP, YMAP, DATOUT, VAROUT, QLOUT,
     :                   STATUS )
*+
*  Name:
*     SALIA4

*  Purpose:
*     Re-sample the input image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SALIA4( QUAL, VAR, METHOD, IXLO2, IXHI2, IYLO2, IYHI2, IXLO1,
*                  IXHI1, IYLO1, IYHI1, DATIN, VARIN, QLIN, XMAP, YMAP,
*                  DATOUT, VAROUT, QLOUT, STATUS )

*  Description:
*     Each output data value is found by sampling the input image at
*     the positions specified by the coordinates supplied in arrays
*     XMAP and YMAP. The sample value is calculated either by bi-linear
*     interpolation, or by copying the data value at the nearest input
*     pixel. If nearest neighbour interpolation is used, then QUALITY
*     and VARIANCE values can also be assigned to the output NDF.

*  Arguments:
*     QUAL = LOGICAL (Given)
*        True if a QUALITY array is to be copied.
*     VAR = LOGICAL (Given)
*        True if a VARIANCE array is to be copied.
*     METHOD = CHARACTER * ( * ) (Given)
*        The interpolation method to use when re-sampling the input
*        image; BILINEAR or NEAREST.
*     IXLO2 = INTEGER (Given)
*        The lower X bound of the output image.
*     IXHI2 = INTEGER (Given)
*        The upper X bound of the output image.
*     IYLO2 = INTEGER (Given)
*        The lower Y bound of the output image.
*     IYHI2 = INTEGER (Given)
*        The upper Y bound of the output image.
*     IXLO1 = INTEGER (Given)
*        The lower X bound of the input image.
*     IXHI1 = INTEGER (Given)
*        The upper X bound of the input image.
*     IYLO1 = INTEGER (Given)
*        The lower Y bound of the input image.
*     IYHI1 = INTEGER (Given)
*        The upper Y bound of the input image.
*     DATIN( IXLO1:IXHI1, IYLO1:IYHI1 ) = REAL (Given)
*        The input DATA array.
*     VARIN( IXLO1:IXHI1, IYLO1:IYHI1 ) = REAL (Given)
*        The input VARIANCE array. Ignored if VAR is false.
*     QLIN( IXLO1:IXHI1, IYLO1:IYHI1 ) = BYTE (Given)
*        The input QUALITY array. Ignored if QUAL is false.
*     XMAP( IXLO2:IXHI2, IYLO2:IYHI2 ) = REAL (Given)
*        The input X image coordinate corresponding to the centre of
*        each output pixel.
*     YMAP( IXLO2:IXHI2, IYLO2:IYHI2 ) = REAL (Given)
*        The input Y image coordinate corresponding to the centre of
*        each output pixel.
*     DATOUT( IXLO2:IXHI2, IYLO2:IYHI2 ) = REAL (Returned)
*        The output DATA array.
*     VAROUT( IXLO2:IXHI2, IYLO2:IYHI2 ) = REAL (Returned)
*        The output VARIANCE array. Undefined if VAR is false.
*     QLOUT( IXLO2:IXHI2, IYLO2:IYHI2 ) = REAL (Returned)
*        The output QUALITY array. Undefined if QUAL is false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Uses BYTE arrays.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      LOGICAL QUAL
      LOGICAL VAR
      CHARACTER METHOD*(*)
      INTEGER IXLO2
      INTEGER IXHI2
      INTEGER IYLO2
      INTEGER IYHI2
      INTEGER IXLO1
      INTEGER IXHI1
      INTEGER IYLO1
      INTEGER IYHI1
      REAL DATIN( IXLO1:IXHI1, IYLO1:IYHI1 )
      REAL VARIN( IXLO1:IXHI1, IYLO1:IYHI1 )
      BYTE QLIN( IXLO1:IXHI1, IYLO1:IYHI1 )
      REAL XMAP( IXLO2:IXHI2, IYLO2:IYHI2 )
      REAL YMAP( IXLO2:IXHI2, IYLO2:IYHI2 )

*  Arguments Returned:
      REAL DATOUT( IXLO2:IXHI2, IYLO2:IYHI2 )
      REAL VAROUT( IXLO2:IXHI2, IYLO2:IYHI2 )
      BYTE QLOUT( IXLO2:IXHI2, IYLO2:IYHI2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        IIN,               ! Index of current input column
     :        IOUT,              ! Index of current output column
     :        JIN,               ! Index of current input row
     :        JOUT               ! Index of current output row

      REAL
     :        X,                 ! Input x image coordinate
     :        Y                  ! Input Y image coordinate
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First deal with nearest neightbour interolation.
      IF( METHOD .EQ. 'NEAREST' ) THEN

*  Loop round each pixel in the output NDF.
         DO JOUT = IYLO2, IYHI2
            DO IOUT = IXLO2, IXHI2

*  If there is no corresponding position in the input image, store
*  bad values in the output.
               X = XMAP( IOUT, JOUT )
               Y = YMAP( IOUT, JOUT )

               IF( X .EQ. VAL__BADR .OR. Y .EQ. VAL__BADR ) THEN
                  DATOUT( IOUT, JOUT ) = VAL__BADR
                  IF( QUAL ) QLOUT( IOUT, JOUT ) = 0
                  IF( VAR ) VAROUT( IOUT, JOUT ) = VAL__BADR

*  Otherwise, find the indices of the pixel in the input NDF which is
*  closest to the centre of this output pixel.
               ELSE
                  IIN = NINT( X + 0.5 )
                  JIN = NINT( Y + 0.5 )

*  Check it is within the bounds of the input NDF.
                  IF( IIN .GE. IXLO1 .AND. IIN .LE. IXHI1 .AND.
     :                JIN .GE. IYLO1 .AND. JIN .LE. IYHI1 ) THEN

*  If so, copy the input values to the output NDF.
                     DATOUT( IOUT, JOUT ) = DATIN( IIN, JIN )
                     IF( QUAL ) QLOUT( IOUT, JOUT ) = QLIN( IIN, JIN )
                     IF( VAR ) VAROUT( IOUT, JOUT ) = VARIN( IIN, JIN )

*  If the current ouptut pixel lies outside the bounds of the input NDF,
*  store bad values.
                  ELSE
                     DATOUT( IOUT, JOUT ) = VAL__BADR
                     IF( QUAL ) QLOUT( IOUT, JOUT ) = 0
                     IF( VAR ) VAROUT( IOUT, JOUT ) = VAL__BADR

                  END IF

               END IF

            END DO

         END DO

*  Now deal with bilinear interpolation.
      ELSE

*  Loop round each pixel in the output NDF.
         DO JOUT = IYLO2, IYHI2
            DO IOUT = IXLO2, IXHI2

*  If there is no corresponding position in the input image, store
*  a bad value.
               X = XMAP( IOUT, JOUT )
               Y = YMAP( IOUT, JOUT )

               IF( X .EQ. VAL__BADR .OR. Y .EQ. VAL__BADR ) THEN
                  DATOUT( IOUT, JOUT ) = VAL__BADR

*  Otherwise, interpolate the input data values at the image coordinates
*  corresponding to the centre of the current output pixel. Store the
*  interpolated value in the output DATA array.
               ELSE
                  CALL IRM_BILN1( X, Y, IXLO1, IXHI1, IYLO1, IYHI1,
     :                            DATIN, DATOUT( IOUT, JOUT ), STATUS )
               END IF

            END DO
         END DO

      END IF

      END
