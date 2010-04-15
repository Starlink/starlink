      SUBROUTINE POL1_STK( NPIX, NROW, DIN, VAR, VIN, BOX, NXBIN, NYBIN,
     :                     DOUT, VOUT, VARS, TR, STATUS )
*+
*  Name:
*     POL1_STBIN

*  Purpose:
*     Create a stack of pixel values suitable for binning.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STK( NPIX, NROW, DIN, VAR, VIN, BOX, NXBIN, NYBIN,
*                    DOUT, VOUT, VARS, TR, STATUS )

*  Description:
*     This routine copies the pixel values in the supplied arrays to
*     the output arrays, re-arranging them so that they can be binned
*     using POL1_CM1RR or POL1_CM3RR. The input pixels in each bin
*     are stored as a single column in the output arrays. The number
*     of columns in the output arrays is equal to the number of bins.

*  Arguments:
*     NPIX = INTEGER (Given)
*        The number of pixels per row in DIN and VIN.
*     NROW = INTEGER (Given)
*        The number of rows in DIN and VIN.
*     DIN( NPIX, NROW ) = REAL (Given)
*        The input data values.
*     VAR = LOGICAL (Given)
*        It is .TRUE. if variance values are to be returned.
*     VIN( NPIX, NROW ) = REAL (Given)
*        The input variance values. It is ignored if VAR is .FALSE..
*     BOX( 2 ) = INTEGER (Given)
*        The dimensions of each bin, in pixels.
*     NXBIN = INTEGER (Given)
*        The number of bins along the X axis.
*     NYBIN = INTEGER (Given)
*        The number of bins along the Y axis.
*     DOUT( * ) = REAL (Returned)
*        The output data valus. This should have NPIX*NROW elements.
*     VOUT( * ) = REAL (Returned)
*        The output variance values. This should have NPIX*NROW elements.
*        Only used if VAR is .TRUE.
*     VARS( * ) = REAL (Returned)
*        An array of BOX(1)*BOX(2) elements which will be set to the
*        value 1.0 on return.
*     TR( 4 ) = DOUBLE PRECISION (Returned)
*        The coefficients of the linear mapping produced by the binning:
*           X' = TR1 + TR2*X
*           Y' = TR3 + TR4*Y
*           Z' = Z
*        where (X,Y,Z) are GRID coordinates in the input cube, and
*        (X',Y',Z') are GRID coordinates in the binned cube.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     13-JUL-2009 (DSB):
*        Changed VARS form DOUBLE PRECISION to REAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER NROW
      REAL DIN( NPIX, NROW )
      LOGICAL VAR
      REAL VIN( NPIX, NROW )
      INTEGER BOX( 2 )
      INTEGER NXBIN, NYBIN

*  Arguments Returned:
      REAL DOUT( * )
      REAL VOUT( * )
      REAL VARS( * )
      DOUBLE PRECISION TR( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER COL                ! Current output column being created
      INTEGER EL                 ! Vectorised index of next output element
      INTEGER IX0                ! X index at bottom left of used input area
      INTEGER IY0                ! Y index at bottom left of used input area
      INTEGER J, I               ! Indices of output bin
      INTEGER JJ, II             ! Indices of input pixel
      INTEGER LBNDX              ! Lower X bound in input image of current bin
      INTEGER LBNDY              ! Lower Y bound in input image of current bin
      INTEGER NBIN               ! The no. of bins (i.e. columns) in the output
      INTEGER UBNDX              ! Upper X bound in input image of current bin
      INTEGER UBNDY              ! Upper Y bound in input image of current bin
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of bins. This is equal to the number of columns in
*  the output array.
      NBIN = NXBIN*NYBIN

*  Choose the pixel indices within the input arrays of the pixel to put
*  at the bottom left corner of the bottom left bin. This is done so
*  that any "spare" input pixels are distributed evenly round the 4 sides
*  of the array.
      IX0 = 1 + ( NPIX - NXBIN*BOX( 1 ) )/2
      IY0 = 1 + ( NROW - NYBIN*BOX( 2 ) )/2

*  Store the coefficients of the linear mapping produced by the binning.
      TR( 1 ) = 0.5D0 - ( DBLE( IX0 ) - 0.5D0 )/DBLE( BOX( 1 ) )
      TR( 2 ) = 1.0D0/DBLE( BOX( 1 ) )
      TR( 3 ) = 0.5D0 - ( DBLE( IY0 ) - 0.5D0 )/DBLE( BOX( 2 ) )
      TR( 4 ) = 1.0D0/DBLE( BOX( 2 ) )

*  Initialise the bounds within the input image of the pixels which fall in
*  the first (bottom left) bin.
      LBNDX = IX0
      UBNDX = IX0 + BOX( 1 ) - 1
      LBNDY = IY0
      UBNDY = IY0 + BOX( 2 ) - 1

*  We want the order of the columns in the output arrays to correspond to
*  the order of binned pixels in the output image. So loop through the
*  rows and columns in the output binned image appropriately.
      COL = 1
      DO J = 1, NYBIN
         DO I = 1, NXBIN

*  Go through each row and column in the input image which contribute to
*  this bin. These pixels are scopied to a single column in the output
*  array.
            EL = COL
            DO JJ = LBNDY, UBNDY
               DO II = LBNDX, UBNDX
                  DOUT( EL ) = DIN( II, JJ )
                  IF( VAR ) VOUT( EL ) = VIN( II, JJ )
                  EL = EL + NBIN
               END DO
            END DO

*  Set up the bounds of the next bin in this row of the input image.
            LBNDX = LBNDX + BOX( 1 )
            UBNDX = UBNDX + BOX( 1 )

*  Move on to the next column in the output array.
            COL = COL + 1

         END DO

*  Set up the bounds of the first bin in the next row of the input image.
         LBNDX = IX0
         UBNDX = IX0 + BOX( 1 ) - 1
         LBNDY = LBNDY + BOX( 2 )
         UBNDY = UBNDY + BOX( 2 )

      END DO

*  Store 1.0 in each element of the VARS array.
      IF( .NOT. VAR ) THEN
         DO I = 1, BOX( 1 )*BOX( 2 )
            VARS( I ) = 1.0
         END DO
      END IF

      END
