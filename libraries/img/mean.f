      SUBROUTINE MEAN( ISTAT )
*+
* Name:
*    MEAN

*  Purpose:
*     Calculates and reports the mean value of an image.

*  Description:
*     This is a demonstration routine for IMG. It accesses an existing
*     image and calculates the mean value which it then writes to the
*     terminal.

*-

*  Access an input image.
      CALL IMG_IN( 'IN', NX, NY, IP, ISTAT )

*  Derive the mean and write it out.
      CALL DOSTAT( %VAL( IP ), NX, NY, ISTAT )

*  Free the input image.
      CALL IMG_FREE( 'IN', ISTAT )
      END

      SUBROUTINE DOSTAT( IMAGE, NX, NY, ISTAT )
      INCLUDE 'SAE_PAR'
      REAL IMAGE( NX, NY )

*  Check the global status.
      IF ( ISTAT .NE. SAI__OK ) RETURN

*  Initialise the sum and loop over all elements of the image.
      SUM = 0.0
      DO 1 J = 1, NY
         DO 2 I = 1, NX
            SUM = SUM + IMAGE( I, J )
 2       CONTINUE
 1    CONTINUE

*  Write out the mean value.
      WRITE( *, * ) 'Mean = ', SUM / REAL( NX * NY )

      END
* $Id$
