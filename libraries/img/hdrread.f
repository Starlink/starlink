      SUBROUTINE HDRREAD( ISTAT )
*+
* Name:
*    HDRREAD

*  Purpose:
*     Reports the value of a header item.

*  Description:
*     This routine is part of the IMG example suite. It accesses an
*     existing image and reads the value of a named item. The value is
*     then reported.

*  Notes:
*     - The headers accessed by this routine are FITS items.
*
*     - The output is written using MSG routines rather than 
*       print statements. MSG is documented in SUN/104.
*
*     - The PAR routines are described in SUN/114.
      
*-

*  Local Variables:
      CHARACTER * ( 8 ) ITEM
      CHARACTER * ( 40 ) VALUE
*.

*  Get the name of the FITS item we are to read.
      CALL PAR_GET0C( 'ITEM', ITEM, ISTAT )
      
*  See if it exists (this call also accesses the image).
      CALL HDR_NUMB( 'IN', ' ', ITEM, N, ISTAT )
      IF ( N .GT. 0 ) THEN

*  Try to read the value.
         CALL HDR_IN( 'IN', ' ', ITEM, 1, VALUE, ISTAT )

*  And write it out.
         CALL MSG_SETC( 'ITEM', ITEM )
         CALL MSG_SETC( 'VALUE', VALUE )
         CALL MSG_OUT( ' ', 'The header item ''^ITEM'' has a ' //
     :        'value of ^VALUE.', ISTAT )
      ELSE

*  Item doesn't exist so make a report to this effect.
         CALL MSG_SETC( 'ITEM', ITEM )
         CALL MSG_OUT( ' ', 'The header item ''^ITEM'' doesn''t exist.',
     :                 ISTAT )
      END IF   

*  Free the input image.
      CALL IMG_FREE( 'IN', ISTAT )
      END
* $Id$
