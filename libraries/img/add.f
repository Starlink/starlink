      SUBROUTINE ADD( ISTAT )
*+
* Name:
*    ADD

*  Purpose:
*     Adds a constant value to all the elements of an image.

*  Description:
*     This is a demonstration routine for IMG. It creates a copy of an
*     existing image and then adds a specified constant to all the
*     image elements.

*  Notes:
*     This routine could also be implemented to just modify the input
*     image, rather than creating a new copy.
*
*     The PAR routines are described in SUN/114.

*-

*  Access an existing image.
      CALL IMG_IN( 'IN', NX, NY, IPIN, ISTAT )

*  Copy this to an output image.
      CALL IMG_OUT( 'IN', 'OUT', IPOUT, ISTAT )

*  Get the value to add.
      CALL PAR_GET0R( 'CONSTANT', VALUE, ISTAT )

*  And do the work.
      CALL DOADD( %VAL( IPOUT ), NX, NY, VALUE, ISTAT )

*  Free the input and output images.
      CALL IMG_FREE( '*', ISTAT )
      END

      SUBROUTINE DOADD( IMAGE, NX, NY, VALUE, ISTAT )
      INCLUDE 'SAE_PAR'
      REAL IMAGE( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Loop over all elements of the image adding VALUE.
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            IMAGE( I, J ) = IMAGE( I, J ) + VALUE
 2       CONTINUE
 1    CONTINUE
      END
* $Id$
