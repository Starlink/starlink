      SUBROUTINE CON_WRPGM( EL, IMAGE, DIMS, OIMAGE, STATUS )
* +
*+
*  Name:
*     CON_WRPGM

*  Purpose:
*     Writes the PGM file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_WRPGM( EL, IMAGE, DIMS, OIMAGE, STATUS )

*  Description:
*     This routine writes a PGM file for NDF2PGM.  It first checks to
*     find the range of the data and generates a value to scale the
*     data to the 0--255 range required.  Then writes out the file
*     header information.  This includes stuff like the image size and
*     the number of colours to be used.  Finally it writes out the
*     image.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels.
*     IMAGE( EL ) = DOUBLE PRECISION (Given)
*        The mapped image.
*     DIMS( 2 ) = INTEGER (Given)
*        The image dimensions.  For a vector array, just set the second
*        dimension to 1.
*     OIMAGE( EL ) = BYTE (Returned)
*        Space for the output image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995-1996, 2000 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1995 (GJP)
*        Original version.
*     1996 February 12 (MJC):
*        Tidied to standard style.  Renamed from NDF2PGM_WRITE.
*     18-DEC-2000 (AJC):
*        Ensure only one char follows maxval in header
*        Scale evenly over 255 values
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN            ! Length of a string
      INTEGER CON_CWRIT          ! C function
      INTEGER CON_CWRI2          ! C function

*  Arguments Given:
      INTEGER EL                 ! Number of pixels
      DOUBLE PRECISION IMAGE( EL ) ! The image data
      INTEGER DIMS( 2 )          ! Image dimensions

*  Arguments Returned:
      BYTE OIMAGE( EL )          ! Output image data

*  Local variables:
      INTEGER I                  ! Loop variable
      INTEGER INDEX1             ! Array pointer
      INTEGER INDEX2             ! Array pointer
      DOUBLE PRECISION MAXI      ! Biggest pixels value
      DOUBLE PRECISION MINI      ! Smallest pixels value
      INTEGER NC                 ! String length
      DOUBLE PRECISION SCFACT    ! Pixel value range
      INTEGER STATC              ! C routine status
      CHARACTER * ( 255 ) STRING ! Output header string
      CHARACTER * ( 255 ) TEMP   ! Output header string
      BYTE TEMPB( 33000 )        ! Temporary storage
      INTEGER X                  ! Pixel column
      INTEGER Y                  ! Pixel row

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the brightest/faintest non-bad pixels.
      MAXI = VAL__MIND
      MINI = VAL__MAXD
      DO 10 I = 1, EL

*  Ignore bad pixels.
         IF ( IMAGE( I ) .NE. VAL__BADD ) THEN

*  Check for brightest.
            IF ( IMAGE( I ) .GT. MAXI ) MAXI = IMAGE( I )

*  Check for faintest.
            IF ( IMAGE( I ) .LT. MINI ) MINI = IMAGE( I )

         END IF

   10 CONTINUE

      SCFACT = ( MAXI - MINI ) / 256.0D0

*  Read the image and convert the values for the output image.
      DO 20 I = 1, EL

*  Check for bad pixels.
         IF ( IMAGE( I ) .EQ. VAL__BADD ) THEN

*  Fudge bad pixels to the minimum pgm value.
            OIMAGE( I ) = 0

         ELSE IF ( IMAGE( I ) .GE. MAXI ) THEN
*  Fudge high pixels to the maximum pgm value
            OIMAGE( I ) = 255

         ELSE
*  Scale the pixel appropriately.
            OIMAGE( I ) = INT( ( IMAGE( I ) - MINI ) / SCFACT )

         END IF
   20 CONTINUE

*  Write out the header/image data.
      TEMP = 'P5 ^X ^Y    255'
      CALL MSG_FMTI( 'X', 'I6', DIMS( 1 ) )
      CALL MSG_FMTI( 'Y', 'I6', DIMS( 2 ) )
      CALL MSG_LOAD( ' ', TEMP, STRING, NC, STATUS )
      NC = CHR_LEN( STRING )

*  Check for an error.
      STATC = CON_CWRIT( STRING( 1:NC ) )
      IF ( STATC .EQ. 0 ) THEN
         CALL ERR_REP( ' ', 'Failed writing the file header.', STATUS )
         GOTO 999
      END IF

*  Swap the image rows to avoid inverting the image.
      DO 100 Y = 1, DIMS( 2 ) / 2

*  Fill the temporary storage with row y.
         INDEX1 = ( Y - 1 ) * DIMS( 1 )
         DO 200 X = 1, DIMS( 1 )
            TEMPB( X ) = OIMAGE( INDEX1 + X )
  200    CONTINUE

*  Copy the row ymax - y to row y.
         INDEX2 = EL - Y * DIMS( 1 )
         DO 300 X = 1, DIMS( 1 )
            OIMAGE( INDEX1 + X ) = OIMAGE( INDEX2 + X )
  300    CONTINUE


*  Copy the stored version of row y to row ymax - y.
         DO 400 X = 1, DIMS( 1 )
            OIMAGE( INDEX2 + X ) = TEMPB( X )
  400    CONTINUE

  100 CONTINUE

*  Write out the image.
      DO 500 Y = 1, DIMS(2)
         INDEX1 = ( Y - 1 ) * DIMS( 1 ) + 1
         STATC = CON_CWRI2( OIMAGE( INDEX1 ), DIMS(1) )
  500 CONTINUE

      IF ( STATC .EQ. 0 ) THEN
         CALL ERR_REP( ' ', 'Failed writing the image.', STATUS )
         GOTO 999
      END IF

  999 CONTINUE

      END
