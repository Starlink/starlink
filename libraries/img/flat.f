      SUBROUTINE FLAT( ISTAT )
*+
* Name:
*    FLAT

*  Purpose:
*     Creates a false flatfield.

*  Description:
*     This is a demonstration routine for IMG. It creates a new image
*     and fills it with ones.

*-

*.

*  Create a new image.
      CALL IMG_NEW( 'OUT', 416, 578, IP, ISTAT )

*  Set all its elements to the value 1.0.
      CALL DOFILL( %VAL( IP ), 416, 578, ISTAT )

*  Free the new image.
      CALL IMG_FREE( 'OUT', ISTAT )

      END

      SUBROUTINE DOFILL( IMAGE, NX, NY, ISTAT )
      INCLUDE 'SAE_PAR'
      REAL IMAGE( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Loop over all elements of the image setting them to 1.0.
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            IMAGE( I, J ) = 1.0
 2       CONTINUE
 1    CONTINUE
      END
* $Id$
