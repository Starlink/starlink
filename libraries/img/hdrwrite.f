      SUBROUTINE HDRWRITE( ISTAT )
*+
* Name:
*    HDRWRITE

*  Purpose:
*     Writes a new header item.

*  Description:
*     This routine is part of the IMG example suite. It accesses an
*     existing image and creates a copy as an output image. It then
*     writes a new header item to the output image.

*  Notes:
*     This could be simplified by just modifying the input image rather
*     than creating a copy. To do this just comment out the IMG_IN and
*     IMG_OUT lines. You might also want to change the HDR_OUT line to
*     access the image via parameter 'IN'.
*
*     The PAR routines are described in SUN/114.

*-

*  Local Variables:
      CHARACTER * ( 8 ) ITEM
      CHARACTER * ( 30 ) COMMEN, VALUE
*.

*  Access the input image.
      CALL IMG_IN( 'IN', NX, NY, IPIN, ISTAT )

*  Copy this to an output image.
      CALL IMG_OUT( 'IN', 'OUT', IPOUT, ISTAT )

*  Now get the name of the header item.
      CALL PAR_GET0C( 'ITEM', ITEM, ISTAT )

*  And its value.
      CALL PAR_GET0C( 'VALUE', VALUE, ISTAT )

*  And a comment for it.
      CALL PAR_GET0C( 'COMMENT', COMMEN, ISTAT )

*  Now write it.
      CALL HDR_OUT( 'OUT', ' ', ITEM, COMMEN, VALUE, ISTAT )

*  Free all the images.
      CALL IMG_FREE( '*', ISTAT )

      END
* $Id$
