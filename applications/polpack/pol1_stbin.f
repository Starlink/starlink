      SUBROUTINE POL1_STBIN( NPIX, NROW, NPLANE, DIN, VAR, VIN, BOX, 
     :                       METH, MINPIX, NSIGMA, NXBIN, NYBIN, DBIN,
     :                       VBIN, TR, STATUS )
*+
*  Name:
*     POL1_STBIN

*  Purpose:
*     Bin Stokes parameters

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STBIN( NPIX, NROW, NPLANE, DIN, VAR, VIN, BOX, METH, 
*                      MINPIX, NSIGMA, NXBIN, NYBIN, DBIN, VBIN, TR,
*                      STATUS )

*  Description:
*     This routine bins the supplied Stokes vectors.

*  Arguments:
*     NPIX = INTEGER (Given)
*        The number of pixels per row in STOKE and VSTOKE.
*     NROW = INTEGER (Given)
*        The number of rows in each plane of STOKE and VSTOKE.
*     NPLANE = INTEGER (Given)
*        The number of planes in STOKE and VSTOKE.
*     DIN( NPIX, NROW, NPLANE ) = REAL (Given)
*        The input Stokes parameters.
*     VAR = LOGICAL (Given)
*        It is .TRUE. if output variance values are to be returned.
*     VIN( NPIX, NROW, NPLANE ) = REAL (Given)
*        The variance on the Stokes parameters. It is ignored if VAR is 
*        .FALSE..
*     BOX( 2 ) = INTEGER (Given)
*        The dimensions of each bin, in pixels.
*     METH = CHARACTER * ( * ) (Given)
*        The binning method; one of 'MEAN', 'MEDIAN' or 'SIGMA'.
*     MINPIX = INTEGER (Given)
*        The minimum number of good input pixels required to make a
*        good output pixel.
*     NSIGMA = REAL (Given)
*        The number of sigmas to clip at. Only used if METH is 'SIGMA'.
*     NXBIN = INTEGER (Given)
*        The number of bins along the X axis.
*     NYBIN = INTEGER (Given)
*        The number of bins along the Y axis.
*     DBIN( NXBIN, NYBIN, NPLANE ) = REAL (Returned)
*        The binned Stokes parameters.
*     VBIN( NXBIN, NYBIN, NPLANE ) = REAL (Returned)
*        The variances of the binned Stokes parameters. Only used if VAR
*        is .TRUE.
*     TR( 4 ) = DOUBLE PRECISION (Returned)
*        The coefficients of the linear mapping produced by the binning:
*           X' = TR1 + TR2*X
*           Y' = TR3 + TR4*Y
*           Z' = Z
*        where (X,Y,Z) are GRID coordinates in the input cube, and
*        (X',Y',Z') are GRID coordinates in the binned cube.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
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
      INTEGER NPLANE
      REAL DIN( NPIX, NROW, NPLANE )
      LOGICAL VAR
      REAL VIN( NPIX, NROW, NPLANE )
      INTEGER BOX( 2 )
      CHARACTER METH*(*)
      INTEGER MINPIX
      REAL NSIGMA
      INTEGER NXBIN, NYBIN

*  Arguments Returned:
      REAL DBIN( NXBIN, NYBIN, NPLANE )
      REAL VBIN( NXBIN, NYBIN, NPLANE )
      DOUBLE PRECISION TR( 4 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BINSZ              ! No. of pixels in each bin
      INTEGER IPCOV              ! Pointer to workspace
      INTEGER IPLANE             ! Index of current plane in input
      INTEGER IPNCON             ! Pointer to workspace
      INTEGER IPPNT              ! Pointer to workspace
      INTEGER IPPP               ! Pointer to workspace
      INTEGER IPSTDI             ! Pointer to workspace
      INTEGER IPSTVI             ! Pointer to workspace
      INTEGER IPUSED             ! Pointer to workspace
      INTEGER IPVAR              ! Pointer to workspace
      INTEGER IPWRK1             ! Pointer to workspace
      INTEGER IPWRK2             ! Pointer to workspace
      INTEGER NBIN               ! No. of bins
      INTEGER NMAT               ! Size of workspace 
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of bins, and the the number of input pixels
*  in each bin.
      NBIN = NXBIN*NYBIN
      BINSZ = BOX( 1 )*BOX( 2 )

*  Get workspace.
      CALL PSX_CALLOC( NBIN*BINSZ, '_REAL', IPSTDI, STATUS )
      CALL PSX_CALLOC( BINSZ, '_REAL', IPWRK1, STATUS )
      CALL PSX_CALLOC( BINSZ, '_REAL', IPWRK2, STATUS )
      CALL PSX_CALLOC( BINSZ, '_DOUBLE', IPNCON, STATUS )
      CALL PSX_CALLOC( BINSZ, '_INTEGER', IPPNT, STATUS )
      CALL PSX_CALLOC( BINSZ, '_LOGICAL', IPUSED, STATUS )

      IF( VAR ) THEN
         CALL PSX_CALLOC( NBIN*BINSZ, '_REAL', IPSTVI, STATUS )
         IPVAR = IPPP

         CALL PSX_CALLOC( BINSZ, '_DOUBLE', IPPP, STATUS )
         NMAT = BINSZ*( BINSZ + 1 )/2 
         CALL PSX_CALLOC( BINSZ*NMAT, '_DOUBLE', IPCOV, STATUS )

      ELSE
         IPSTVI = IPSTDI
         CALL PSX_CALLOC( BINSZ, '_DOUBLE', IPVAR, STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each plane of the supplied data.
      DO IPLANE = 1, NPLANE

*  Copy this plane out of the input arrays, into workspace. Re-arrange
*  the values so that the input pixel values in each bin are stored in
*  a single column, there being NBIN columns in the work arrays. This
*  call also stores the value 1.0 in each element of the array pointed to 
*  by IPVAR (but only if VAR is .FALSE.).
         CALL POL1_STK( NPIX, NROW, DIN( 1, 1, IPLANE ), VAR, 
     :                  VIN( 1, 1, IPLANE ), BOX, NXBIN, NYBIN, 
     :                  %VAL( IPSTDI ), %VAL( IPSTVI ), %VAL( IPVAR ),
     :                  TR, STATUS )

*  Do the binning.
         IF( VAR ) THEN
            CALL POL1_CM1RR( %VAL( IPSTDI ), NBIN, BINSZ, 
     :                       %VAL( IPSTVI ), METH, MINPIX, NSIGMA, 
     :                       DBIN( 1, 1, IPLANE ), 
     :                       VBIN( 1, 1, IPLANE ), 
     :                       %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                       %VAL( IPPP ), %VAL( IPCOV ), NMAT, 
     :                       %VAL( IPNCON ), %VAL( IPPNT ), 
     :                       %VAL( IPUSED ), STATUS )
         ELSE
            CALL POL1_CM3RR( %VAL( IPSTDI ), NBIN, BINSZ, %VAL( IPVAR ),
     :                       METH, MINPIX, NSIGMA, DBIN( 1, 1, IPLANE ), 
     :                       %VAL( IPWRK1 ), %VAL( IPWRK2 ),
     :                       %VAL( IPNCON ), %VAL( IPPNT ), 
     :                       %VAL( IPUSED ), STATUS )
         END IF

      END DO

 999  CONTINUE

*  Free workspace.
      CALL PSX_FREE( IPSTDI, STATUS )
      CALL PSX_FREE( IPWRK1, STATUS )
      CALL PSX_FREE( IPWRK2, STATUS )
      CALL PSX_FREE( IPNCON, STATUS )
      CALL PSX_FREE( IPPNT, STATUS )
      CALL PSX_FREE( IPUSED, STATUS )

      IF( VAR ) THEN
         CALL PSX_FREE( IPSTVI, STATUS )
         CALL PSX_FREE( IPPP, STATUS )
         CALL PSX_FREE( IPCOV, STATUS )
      ELSE 
         CALL PSX_FREE( IPVAR, STATUS )
      END IF

      END
