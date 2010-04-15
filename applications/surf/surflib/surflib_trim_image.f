      SUBROUTINE SURFLIB_TRIM_IMAGE( TRIM, NX, NY, BITNUM, IN_QUAL,
     :     OUT_QUAL, STATUS)
*+
*  Name:
*     SURFLIB_TRIM_IMAGE

*  Purpose:
*     Sets quality bit for specified distance from exisiting bad data

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SURFLIB_TRIM_IMAGE( TRIM, NX, NY, BITNUM,
*     IN_QUAL, OUT_QUAL, STATUS)

*  Description:
*     Will take the quality array of a 2-D image and set the
*     quality bit of all pixels within the specified trimming
*     distance of a pixel that has bit 0 set. The output pixel
*     bit is only set explicitly if bit 0 is not already set.
*     This is so that the effect of the trim can be removed
*     simply by changing the bad bits mask.


*  Arguments:
*     TRIM = REAL (Given)
*       The distance in pixels inside which quality bits will be
*       set.
*     NX = INTEGER (Given)
*       Number of X pixels in image
*     NY = INTEGER (Given)
*       Number of Y pixels in image
*     BITNUM = INTEGER (Given)
*       Bit number to set to bad if within TRIM distance of a
*       pixel that has bit 0 set. Start counting at 0.
*     IN_QUAL(NX,NY) = UBYTE (Given)
*       Input image.
*     OUT_QUAL(NX,NY) = UBYTE (Returned)
*       Output image quality.
*     STATUS = INTEGER (Given and Returned)
*       Global status

*  Authors:
*     Tim Jenness (t.jenness@jach.hawaii.edu)

*  Notes:


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.3  1999/08/06 02:29:07  timj
*     Tweak headers for use with PROLAT.
*
*     Revision 1.2  1999/08/03 19:32:54  timj
*     Add copyright message to header.
*
*     Revision 1.1  1999/05/12 22:46:19  timj
*     First version
*

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      REAL    TRIM
      INTEGER NX
      INTEGER NY
      INTEGER BITNUM
      BYTE    IN_QUAL(NX,NY)

*  Arguments Returned:
      BYTE    OUT_QUAL(NX,NY)

*  Status
      INTEGER STATUS

*  External References:
      BYTE    SCULIB_BITON

*  Local Constants:
      BYTE    BADBIT            ! Bad bits mask to determine 'badness' of input
      PARAMETER ( BADBIT = 1 )

*  Local Variables:
      LOGICAL CORRECTED         ! Have we corrected this pixel already?
      INTEGER BOXSZ             ! size of search box (pixels)
      REAL    DPIX              ! Distance**2 from cent pix to current pix
      REAL    TRIMSQ            ! Square of trim radius
      INTEGER XOUT              ! X pixel position in box
      INTEGER XPIX              ! X pixel pos in image
      INTEGER YOUT              ! Y pixel position in box
      INTEGER YPIX              ! Y pixel pos in image


*  External functions:
      INCLUDE 'NDF_FUNC'

*.
      IF (STATUS .NE. SAI__OK) RETURN

*     Calculate box size to search over
      BOXSZ = INT(TRIM + 1.0)

*     Square of distance
      TRIMSQ = TRIM**2

*     Need to loop over input, copying to output and if it
*     is near a 'bad' pixel we set the required bit to 1.

      DO XPIX = 1, NX
         DO YPIX = 1, NY

*     Copy input value to output
            OUT_QUAL(XPIX, YPIX) = IN_QUAL(XPIX, YPIX)

*     Do nothing further if this pixel is already bad
*     Note that this will actually tell us whether the next pixel
*     is bad....
            IF (NDF_QMASK(IN_QUAL(XPIX,YPIX),BADBIT)) THEN

*     Set a logical to recognise if we set this bit
               CORRECTED = .FALSE.

*     Now look at surrounding input pixels looking for 'bad'
*     ones (make sure that we dont go out of bounds)

               DO XOUT = MAX(1, XPIX-BOXSZ), MIN(NX, XPIX+BOXSZ)
                  DO YOUT = MAX(1, YPIX-BOXSZ), MIN(NY, YPIX+BOXSZ)

*     Check to see whether we have corrected this already
                     IF (.NOT.CORRECTED) THEN

*     Work out distance**2 of this selected pixel to the current pixel
                        DPIX = REAL((XPIX-XOUT)**2 + (YPIX-YOUT)**2)

*     If the pixel is close enough
                        IF (DPIX .LT. TRIMSQ) THEN

*     Check to see if this pixel is 'bad'
                           IF (.NOT.NDF_QMASK(IN_QUAL(XOUT,YOUT),
     :                          BADBIT)) THEN

*     Set the bit in the output to bad for the current pixel
*     (not the position in the search box)
                              OUT_QUAL(XPIX,YPIX) = SCULIB_BITON(
     :                             OUT_QUAL(XPIX,YPIX), BITNUM)

*     And set a flag to prevent us from checking again (need to use
*     the perl 'last' function here...)

                              CORRECTED = .TRUE.

                           END IF

                        END IF

                     END IF

                  END DO
               END DO

            END IF

         END DO
      END DO

      END
