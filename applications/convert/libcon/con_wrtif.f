      SUBROUTINE CON_WRTIF( EL, IMAGE, DIMS, FLEN, FIOD, OIMAGE,
     :                      STATUS )
*+ 
*  Name:
*     CON_WRTIF
 
*  Purpose:
*     Writes the TIFF file.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL CON_WRTIF( EL, IMAGE, DIMS, FLEN, FIOD, OIMAGE, STATUS )
 
*  Description:
*     This routine writes a TIFF file for NDF2TIFF.  It first checks to
*     find the range of the data and generates a value to scale the
*     data to the 0--255 range required.  Then it writes out the file
*     header information.  This includes stuff like the image size and
*     the number of bits per pixel.  Finally it writes out the image.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels in the array to be written to the TIFF
*        file.
*     IMAGE( EL ) = DOUBLE PRECISION (Given)
*        The array to write to the TIFF file.
*     DIMS( 2 ) = INTEGER (Given)
*        The image dimensions.  For a vector array, just set the second
*        dimension to 1.
*     FLEN = INTEGER (Given)
*        The dimension of the space to contain the header and image.
*        It should be at least EL + 122.
*     FIOD = INTEGER (Given)
*        The FIO file descriptor for the TIFF file.
*     OIMAGE( FLEN ) = BYTE (Returned)
*        Space for the output image and header.
*     STATUS = INTEGER ( Given and Returned )
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     22-NOV-1995 (GJP):
*        Original version.
*     1996 February 12 (MJC):
*        Tidied to standard style.  Renamed from NDF2TIFF_WRITE.
*        Fixed bug with dimensions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
 
*  Status:     
      INTEGER STATUS             ! Global status
 
*  Arguments Given:
      INTEGER EL                 ! Number of pixels
      INTEGER FLEN               ! Output file length
      DOUBLE PRECISION IMAGE( EL ) ! The image data
      INTEGER DIMS( 2 )          ! Image dimensions
      INTEGER FIOD               ! FIO identifier

*  Arguments Returned:
      BYTE OIMAGE( FLEN )        ! Output image data

*  Local Constants:
      INTEGER TP8                ! Two to the eigth power
      PARAMETER ( TP8 = 256 )
      INTEGER TP16               ! Two to the sixteenth power
      PARAMETER ( TP16 = 65536 )
      INTEGER TP24               ! Two to the twenty-fourth power
      PARAMETER ( TP24 = 16777216 )

*  Local variables:
      INTEGER B1                 ! Temorary storage
      INTEGER B2                 ! Temorary storage
      INTEGER B3                 ! Temorary storage
      INTEGER B4                 ! Temorary storage
      BYTE HD( 122 )             ! The file-header contents
      INTEGER I                  ! Loop variable
      INTEGER INDEX1             ! Pointer to row start
      INTEGER INDEX2             ! pointer to row start
      DOUBLE PRECISION MAXI      ! Biggest pixels value
      DOUBLE PRECISION MINI      ! Smallest pixels value
      DOUBLE PRECISION SCFACT    ! Pixel value range
      BYTE TEMP( 33000 )         ! Temporary storage
      INTEGER X                  ! Row number
      INTEGER Y                  ! Column number
 
*  Local Data:

!  These are essential data for the TIFF header.
      DATA HD /  
     :          73, 73,
!  Magic number
     :          42,  0,
!  Where to find IFD
     :           8,  0,  0,  0,
!  Number of tags
     :           9,  0,
!  Data type
     :         255,  0,  3,  0,  1,  0,  0,  0,  1,  0,  0,  0,
!  Width
     :           0,  1,  3,  0,  1,  0,  0,  0,  0,  0,  0,  0,
!  Length
     :           1,  1,  3,  0,  1,  0,  0,  0,  0,  0,  0,  0,
!  BitsPerSample
     :           2,  1,  3,  0,  1,  0,  0,  0,  8,  0,  0,  0,
!  Compression nil
     :           3,  1,  3,  0,  1,  0,  0,  0,  1,  0,  0,  0,
!  Photometric
     :           6,  1,  3,  0,  1,  0,  0,  0,  1,  0,  0,  0,
!  Strip offsets
     :          17,  1,  4,  0,  1,  0,  0,  0, 122, 0,  0,  0, 
!  SamplesPerPixel
     :          21,  1,  3,  0,  1,  0,  0,  0,  1,  0,  0,  0, 
!  StripBytesCounts
     :          23,  1,  4,  0,  1,  0,  0,  0,  0,  0,  0,  0, 
!  Terminator
     :           0,  0,  0,  0 /

*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transfer the header information.
      DO 5 I = 1, 122
         OIMAGE( I ) = HD( I )
    5 CONTINUE

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
      SCFACT = ( MAXI - MINI ) / 255.0D0
   
*  Calculate the strip size data.
      B1 = EL / TP24
      B2 = ( EL - B1 * TP24 ) / TP16
      B3 = ( EL - B1 * TP24 - B2 * TP16 ) / TP8
      B4 = ( EL - B1 * TP24 - B2 * TP16 - B3 * TP8 )

*  Add to the header.
      OIMAGE( 115 ) = B4
      OIMAGE( 116 ) = B3
      OIMAGE( 117 ) = B2
      OIMAGE( 118 ) = B1

*  Adds the size information to the header.
      OIMAGE( 32 ) = DIMS( 1 ) / TP8
      OIMAGE( 31 ) = DIMS( 1 ) - DIMS( 1 ) * TP8
      OIMAGE( 44 ) = DIMS( 2 ) / TP8
      OIMAGE( 43 ) = DIMS( 2 ) - DIMS( 2 ) * TP8

*  Read the image and convert the values for the output image.
      DO 20 I = 1, EL

*  Check for bad pixels.
         IF ( IMAGE( I ) .EQ. VAL__BADD ) THEN

*  Fudge bad pixels to zero.
            OIMAGE( I + 122 ) = 0

         ELSE

*  Scale the pixel appropriately.
            OIMAGE( I + 122 ) = INT( ( IMAGE( I ) - MINI ) / SCFACT )

         END IF
   20 CONTINUE

*   Swap the image rows to avoid inverting the image.
      DO 100 Y = 1, DIMS( 2 ) / 2
          
*  Fill the temporary storage with row y.
         INDEX1 = 122 + ( Y - 1 ) * DIMS( 1 )
         DO 200 X = 1, DIMS( 1 )
            TEMP( X ) = OIMAGE( INDEX1 + X )
  200    CONTINUE

*  Copy the row ymax - y to row y.
         INDEX2 = FLEN - Y * DIMS( 1 )
         DO 300 X = 1, DIMS( 1 )
            OIMAGE( INDEX1 + X ) = OIMAGE( INDEX2 + X )
  300    CONTINUE

*  Copy the stored version of row y to row ymax - y.
         DO 400 X = 1, DIMS( 1 )
            OIMAGE( INDEX2 + X ) = TEMP( X )
  400    CONTINUE

  100 CONTINUE

*  Write out the header/image data.
      CALL RIO_WRITE( FIOD, 1, FLEN, OIMAGE, STATUS ) 

  999 CONTINUE

      END 
