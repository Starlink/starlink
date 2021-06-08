      SUBROUTINE KPS1_DISCL( INDF, SDIMS, MCOMP, LP, UP, BPCI, WPLBND,
     :                       WPUBND, IP, NX, NY, DLO, DHI, STATUS )
*+
*  Name:
*     KPS1_DISCL

*  Purpose:
*     Create an array of colour indices to represent the values in an NDF
*     array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_DISCL( INDF, SDIMS, MCOMP, LP, UP, BPCI, WPLBND, WPUBND,
*                      IP, NX, NY, DLO, DHI, STATUS )

*  Description:
*     If the number of pixels in the supplied image is much larger than
*     the number of pixels in the current pgplot viewport, then block
*     average the values in the specified NDF array component. Then scale
*     the data using the method specified by parameter MODE to produce
*     the required integer colour indices.
*
*     If the block averaging changes the bounds of he data represented by
*     the returned array of colour indices (due to the NDF not covering a
*     whole number of blocks), then WPLBND and WPUBND (which are supplied
*     holding the pixel co-ordinate bounds of the NDF) are modified to
*     hold the pixel co-ordinate bounds (within the NDF) of the area
*     covered by the returned array of colour indices.

*  Arguments:
*     INDF = INTEGER (Given)
*       An NDF identifier for the section of the NDF which is to be
*       displayed.
*     SDIMS( 2 ) = INTEGER (Given)
*       The dimensions of the two pixel axes within the NDF which are being
*       used.
*     MCOMP = CHARACTER * ( * ) (Given)
*       The NDF component to be used. This can include 'Error'.
*     LP = INTEGER (Given)
*       The lowest colour index to use.
*     UP = INTEGER (Given)
*       The highest colour index to use.
*     BPCI = INTEGER (Given)
*       The colour index to use for bad data.
*     WPLBND( 2 ) = REAL (Given and Returned)
*       On entry, this holds the pixel co-ordinate lower bounds of the
*       supplied NDF. On exit they are modified to represent the bounds
*       of the data spanned by the returned array of colour indices,
*       taking account of the any change in shape or size of the data
*       caused by any block averaging which may have been performed.
*     WPUBND( 2 ) = REAL (Given and Returned)
*       On entry, this holds the pixel co-ordinate upper bounds of the
*       supplied NDF. On exit they are modified to represent the bounds
*       of the data spanned by the returned array of colour indices,
*       taking account of the any change in shape or size of the data
*       caused by any block averaging which may have been performed.
*     IP = INTEGER (Returned)
*       A pointer to a work array holding the colour index values. This
*       should be freed using PSX_FREE when no longer needed. Returned
*       equal to zero if an error occurs.
*     NX = INTEGER (Returned)
*       The number of columns in the returned array of colour indices.
*     NY = INTEGER (Returned)
*       The number of rows in the returned array of colour indices.
*     DLO = DOUBLE PRECISION (Returned)
*       Lower data limit used for scaling the array
*     DHI = DOUBLE PRECISION (Returned)
*       Upper data limit used for scaling the array
*     STATUS = INTEGER (Given and Returned)
*       The global status value.

*  Notes:
*     -  The following parameter names are hard-wired into this routine:
*     MODE, SIGMAS, LOW, HIGH, PERCENTILES, NUMBIN.
*     -  The PGPLOT viewport on entry must correspond to the DATA picture
*     in which the image is to be displayed.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     Copyright (C) 1998-1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1998 (DSB):
*        Original version, based on the V0.12 display.f by MJC.
*     18-MAY-1999 (DSB):
*        Use separate compression factors for the two axes.
*     18-OCT-1999 (DSB):
*        Added arguments DLO and DHI, and moved assignment to output
*        parameters SCALOW and SCAHIGH into display.f.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2004 September 30 (PWD):
*        Move CNF_PAR include into declarations.
*     2006 February 24 (MJC):
*        Added new CUMUL argument set to .FALSE. to KPG1_GHSTx calls.
*     2011-08-22 (TIMJ):
*        Add new WGTS and WEIGHT arguments to KPG1_GHSTx calls.
*     22-APR-2013 (DSB):
*        Add "CURRENT" as a scaling option.
*     8-JUN-2021 (DSB):
*        If using "CURRENT", ensure the conversion of the previous
*        scaling limits to the data type of the new image is successful.
*        Previously, strings representing floating point numbers could
*        be used as defaults for integer valued scaling limits, causing
*        chr_ctoi to report an error. Now the strings are converted to
*        integer form if required before using them as the defaults for
*        the scaling limits.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'        ! Global SSE definitions
      INCLUDE 'PRM_PAR'        ! VAL__ definitions
      INCLUDE 'NDF_PAR'        ! NDF_ public constants
      INCLUDE 'NDF_ERR'        ! NDF_ error constants
      INCLUDE 'SUBPAR_PAR'     ! SUBPAR constants
      INCLUDE 'CNF_PAR'        ! CNF functions

*  Arguments Given:
      INTEGER INDF
      INTEGER SDIMS( 2 )
      CHARACTER MCOMP*(*)
      INTEGER LP
      INTEGER UP
      INTEGER BPCI

*  Arguments Given and Returned:
      REAL WPLBND( 2 )
      REAL WPUBND( 2 )

*  Arguments Returned:
      INTEGER IP
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION DLO
      DOUBLE PRECISION DHI

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL CHR_SIMLR        ! Strings equal apart from case?

*  Local Constants:
      INTEGER MAXBIN           ! Maximum number of histogram bins
      PARAMETER( MAXBIN = 2048 )! should be enough

      INTEGER NPRCTL           ! Maximum number of percentiles
      PARAMETER( NPRCTL = 2 )

*  Local Variables:
      BYTE BHI                 ! Upper limit used for scaling the array
      BYTE BLO                 ! Lower   "     "   "     "     "    "
      BYTE BMAXV               ! Minimum value in the array
      BYTE BMINV               ! Maximum value in the array
      CHARACTER COMP*8              ! Component to be displayed
      CHARACTER DTYPE*( NDF__SZFTP )! Type of the image after processing (not used)
      CHARACTER ITYPE*( NDF__SZTYP )! Processing type of the image
      CHARACTER HITXT*80            ! SCAHIGH parameter string value
      CHARACTER LOTXT*80            ! SCALOW parameter string value
      CHARACTER MODE*72             ! Manner in which the array is to be scaled
      DOUBLE PRECISION DMAXV   ! Minimum value in the array
      DOUBLE PRECISION DMINV   ! Maximum value in the array
      INTEGER ACTHIG           ! The HIGH parameter state
      INTEGER ACTLOW           ! The LOW parameter state
      INTEGER BLAVF( 2 )       ! Block averaging factors
      INTEGER CDIMS( 2 )       ! Dimensions of compressed array
      INTEGER CEL              ! Number of elements in compressed array
      INTEGER CODE             ! SUBPAR identifier for parameter
      INTEGER EL               ! Number of elements in the input and cell arrays
      INTEGER HIST( MAXBIN )   ! Array containing histogram
      INTEGER I                ! General variables
      INTEGER IHI              ! Upper limit used for scaling the array
      INTEGER ILO              ! Lower   "     "   "     "     "    "
      INTEGER IMAXV            ! Minimum value in the array
      INTEGER IMINV            ! Maximum value in the array
      INTEGER MAXPOS           ! Position of the maximum (not used)
      INTEGER MINPOS           ! Position of the minimum (not used)
      INTEGER NC               ! Number of characters used
      INTEGER NINVAL           ! Number of bad values in the input array
      INTEGER NUMBIN           ! Number of bins in histogram
      INTEGER NXP              ! Number of device pixels along X axis
      INTEGER NYP              ! Number of device pixels along Y axis
      INTEGER PNTRI( 1 )       ! Pointer to image data
      INTEGER SAPNT            ! Pointer to the scratch area in which a compressed array is kept
      INTEGER SAPNT1           ! Pointer to a scratch array used to compute a compressed image
      INTEGER SAPNT2           ! Pointer to a scratch array used to compute a compressed image
      INTEGER*2 WHI            ! Upper limit used for scaling the array
      INTEGER*2 WLO            ! Lower   "     "   "     "     "    "
      INTEGER*2 WMAXV          ! Minimum value in the array
      INTEGER*2 WMINV          ! Maximum value in the array
      LOGICAL BAD              ! The array may contain bad pixels?
      LOGICAL BLOCK            ! The image was averaged before display?
      LOGICAL FNDRNG           ! Find the data range for scaling?
      LOGICAL POSTIV           ! The scaling of the array is to be positive?
      LOGICAL USECUR           ! Use the existing current values?
      REAL DUMMY               ! Used to swap percentiles
      REAL PERCNT( NPRCTL )    ! Percentiles
      REAL PERDEF( NPRCTL )    ! Suggested values for percentiles
      REAL PERVAL( NPRCTL )    ! Values at the percentiles
      REAL RHI                 ! Upper limit used for scaling the array
      REAL RLO                 ! Lower   "     "   "     "     "    "
      REAL RMAXV               ! Minimum value in the array
      REAL RMINV               ! Maximum value in the array
      REAL SIGDEF( 2 )         ! Suggested default standard-deviation limits
      REAL SIGRNG( 2 )         ! Standard-deviation limits
      REAL X1, X2, Y1, Y2      ! Viewport size in device pixels

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions
*.

*  Initialise.
      IP = 0

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  If the supplied component name is Error, use Variance for all NDF
*  routines except NDF_MAP.
      IF( CHR_SIMLR( MCOMP, 'Error' ) ) THEN
         COMP = 'Variance'
      ELSE
         COMP = MCOMP
      END IF

*  This application can only process real, double precision, word,
*  integer and byte components directly. Therefore for the given type
*  of the image find in which type it should be processed.
      CALL NDF_MTYPE( '_BYTE,_WORD,_INTEGER,_REAL,_DOUBLE', INDF, INDF,
     :                COMP, ITYPE, DTYPE, STATUS )

*  Check whether or not bad pixels may be present.
      CALL NDF_BAD( INDF, COMP, .FALSE., BAD, STATUS )

*  Block average the array to be displayed.
*  ========================================
*  For images much larger than the current picture size (in device
*  pixels) the resolution of the device will allow only a fraction
*  of the detail in the array to be plotted.  If the ratio of the
*  array size to picture size is greater than two the array can be
*  compressed by averaging.  This saves time scaling the data and
*  transmitting them to the image display.
*
*  First find whether compression is possible. Get the size of the
*  current PGPLOT viewport in pixels.
      CALL PGQVP( 3, X1, X2, Y1, Y2 )
      NXP = NINT( X2 - X1 )
      NYP = NINT( Y2 - Y1 )
      IF( NXP .LE. 0 ) NXP = 1
      IF( NYP .LE. 0 ) NYP = 1

*  Calculate the compression factors. Compression is possible when the factor
*  is two or greater.
      BLOCK = .FALSE.
      BLAVF( 1 ) = MAX( 1, SDIMS( 1 ) / NXP )
      BLAVF( 2 ) = MAX( 1, SDIMS( 2 ) / NYP )

      IF ( BLAVF( 1 ) .GE. 2 .OR. BLAVF( 2 ) .GE. 2 ) THEN

*  The compression routine can only cope with floating-point data, so the
*  image section may be converted during mapping. Find the implementation
*  type to be used.
         CALL ERR_MARK
         CALL NDF_MTYPE( '_REAL,_DOUBLE', INDF, INDF, COMP, ITYPE,
     :                   DTYPE, STATUS )
         IF ( STATUS .EQ. NDF__TYPNI ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE

*  Map the input image.
         CALL NDF_MAP( INDF, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Find the size of the output array.
         CDIMS( 1 ) = SDIMS( 1 ) / BLAVF( 1 )
         CDIMS( 2 ) = SDIMS( 2 ) / BLAVF( 2 )
         CEL = CDIMS( 1 ) * CDIMS( 2 )

*  Create workspace: three are needed---one for the compressed
*  array, and the other two are needed to perform the averaging
*  calculations.
         CALL PSX_CALLOC( CEL, ITYPE, SAPNT, STATUS )
         CALL PSX_CALLOC( SDIMS( 1 ), ITYPE, SAPNT1, STATUS )
         CALL PSX_CALLOC( SDIMS( 1 ), '_INTEGER', SAPNT2, STATUS )

*  Something has gone wrong, so let the user know the context.
*  Tidy the workspace.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'DISPLAY_WSP2',
     :        'DISPLAY: Unable to get workspace to compress the '/
     :        /'array', STATUS )

            CALL PSX_FREE( SAPNT2, STATUS )
            CALL PSX_FREE( SAPNT1, STATUS )
            CALL PSX_FREE( SAPNT, STATUS )
            GO TO 999
         END IF

*  Do the compression calling the appropriate routine for the
*  implementation type.  Any bad pixels are not necessarily
*  propagated to the averaged array unless all pixels in a
*  BLAVF*BLAVF-sized bin are bad.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_CMAVR( 2, SDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       BLAVF, 1,
     :                       %VAL( CNF_PVAL( SAPNT ) ),
     :                       %VAL( CNF_PVAL( SAPNT1 ) ),
     :                       %VAL( CNF_PVAL( SAPNT2 ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_CMAVD( 2, SDIMS, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       BLAVF, 1,
     :                       %VAL( CNF_PVAL( SAPNT ) ),
     :                       %VAL( CNF_PVAL( SAPNT1 ) ),
     :                       %VAL( CNF_PVAL( SAPNT2 ) ), STATUS )

         END IF

*  Tidy the workspace.
         CALL PSX_FREE( SAPNT2, STATUS )
         CALL PSX_FREE( SAPNT1, STATUS )

*  Unmap the input array.
         CALL NDF_UNMAP( INDF, COMP, STATUS )

*  Fool the rest of the application into thinking the work array
*  of averaged pixels is the input array.  However, a flag is
*  retained so that the work array may be tidied at the end.
         PNTRI( 1 ) = SAPNT
         BLOCK = .TRUE.

*  The number of elements in compressed array also has to be
*  adjusted for the trick.
         EL = CDIMS( 1 ) * CDIMS( 2 )
         NX = CDIMS( 1 )
         NY = CDIMS( 2 )

*  If the original dimension is not a multiple of the compression factor,
*  the compressed data will not cover all the pixels in the original NDF.
*  A margin of pixels will have been omitted around the top and right
*  edges. Adjust the bounds of the box holding the displayed data to take
*  account of this. This box is specified in PIXEL coordinates in the original
*  NDF. The position of the bottom left corner is unchanged.
         WPUBND( 1 ) = WPLBND( 1 ) + DBLE( CDIMS( 1 )*BLAVF( 1 ) )
         WPUBND( 2 ) = WPLBND( 2 ) + DBLE( CDIMS( 2 )*BLAVF( 2 ) )

*  No compression.
      ELSE

*  Map the image with the original implementation type.
         CALL NDF_MAP( INDF, MCOMP, ITYPE, 'READ', PNTRI, EL, STATUS )

*  Used the supplied dimensions.
         NX = SDIMS( 1 )
         NY = SDIMS( 2 )

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Create a scratch area in which to put the scaled array.
*  =======================================================
      CALL PSX_CALLOC( EL, '_INTEGER', IP, STATUS )

*  Something has gone wrong, so let the user know the context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DISPLAY_WSP',
     :        'DISPLAY: Unable to get workspace to scale the array.',
     :        STATUS )
         GO TO 999
      END IF

*  Determine the scaling method to use.
      CALL PAR_CHOIC( 'MODE', 'Scale', 'Scale,Flash,Faint,'//
     :                'Percentiles,Range,Sigma,Current', .TRUE., MODE,
     :                STATUS )
      CALL CHR_UCASE( MODE )

*  Faint (positive) display of the array.
*  ======================================
      IF ( MODE(1:2) .EQ. 'FA' .OR. MODE(1:2) .EQ. 'SI' ) THEN

*  Obtain the standard-deviation limits if not predefined.  There
*  is no dynamic default.
         IF ( MODE(1:2) .EQ. 'SI' ) THEN
            SIGDEF( 1 ) = VAL__BADR
            SIGDEF( 2 ) = VAL__BADR
            CALL PAR_GDR1R( 'SIGMAS', 2, SIGDEF, -1000., 10000.,
     :                      .FALSE., SIGRNG, STATUS )

         ELSE IF ( MODE(1:2) .EQ. 'FA' ) THEN

*  Fixed range in standard-deviation units.
             SIGRNG( 1 ) = -1.0
             SIGRNG( 2 ) = 7.0

         END IF

*  Select appropriate routine for the data type chosen and scale
*  the image between the standard-deviation limits into the cell
*  array.  The cell array has values between the colour-index
*  limits LP and UP.
*  =============================================================
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_FAINR( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       SIGRNG, LP, UP,
     :                       BPCI, .FALSE., %VAL( CNF_PVAL( IP ) ), RLO,
     :                       RHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_FAIND( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       SIGRNG, LP, UP,
     :                       BPCI, .FALSE., %VAL( CNF_PVAL( IP ) ), DLO,
     :                       DHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_FAINI( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       SIGRNG, LP, UP,
     :                       BPCI, .FALSE., %VAL( CNF_PVAL( IP ) ), ILO,
     :                       IHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_FAINW( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       SIGRNG, LP, UP,
     :                       BPCI, .FALSE., %VAL( CNF_PVAL( IP ) ), WLO,
     :                       WHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_FAINB( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       SIGRNG, LP, UP,
     :                       BPCI, .FALSE., %VAL( CNF_PVAL( IP ) ), BLO,
     :                       BHI, STATUS )

         END IF

*  Do scaling with extreme values in the array as the limits.
*  ==========================================================
      ELSE IF ( MODE(1:2) .EQ. 'RA' ) THEN

*  Select appropriate routine for the data type chosen and scale
*  the image between user-defined limits.   The cell array
*  has values between the colour-index limits LP and UP.
*  =============================================================
         IF ( ITYPE .EQ. '_REAL' ) THEN

*  Obtain the maximum and minimum values.
            CALL KPG1_MXMNR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       NINVAL,
     :                       RHI, RLO, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Report the scaling limits for future use.
            CALL MSG_SETR( 'MINVAL', RLO )
            CALL MSG_SETR( 'MAXVAL', RHI )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*  Scale the data values using the extreme values into the
*  cell array.

            CALL KPG1_ISCLR( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., RLO, RHI,
     :                       LP, UP, BPCI, %VAL( CNF_PVAL( IP ) ),
     :                       STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Obtain the maximum and minimum values.
            CALL KPG1_MXMND( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       NINVAL,
     :                       DHI, DLO, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Report the scaling limits for future use.
            CALL MSG_SETD( 'MINVAL', DLO )
            CALL MSG_SETD( 'MAXVAL', DHI )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*  Scale the data values using the extreme values into the
*  cell array.
            CALL KPG1_ISCLD( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., DLO, DHI,
     :                       LP, UP, BPCI, %VAL( CNF_PVAL( IP ) ),
     :                       STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Obtain the maximum and minimum values.
            CALL KPG1_MXMNI( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       NINVAL,
     :                       IHI, ILO, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Report the scaling limits for future use.
            CALL MSG_SETI( 'MINVAL', ILO )
            CALL MSG_SETI( 'MAXVAL', IHI )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*  Scale the data values using the extreme values into the
*  cell array.
            CALL KPG1_ISCLI( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., ILO, IHI,
     :                       LP, UP, BPCI, %VAL( CNF_PVAL( IP ) ),
     :                       STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Obtain the maximum and minimum values.
            CALL KPG1_MXMNW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       NINVAL,
     :                       WHI, WLO, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Report the scaling limits for future use.
            CALL MSG_SETI( 'MINVAL', NUM_WTOI( WLO ) )
            CALL MSG_SETI( 'MAXVAL', NUM_WTOI( WHI ) )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*  Scale the data values using the extreme values into the
*  cell array.
            CALL KPG1_ISCLW( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., WLO, WHI,
     :                       LP, UP, BPCI, %VAL( CNF_PVAL( IP ) ),
     :                       STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Obtain the maximum and minimum values.
            CALL KPG1_MXMNB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       NINVAL,
     :                       BHI, BLO, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
            BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Report the scaling limits for future use.
            CALL MSG_SETI( 'MINVAL', NUM_BTOI( BLO ) )
            CALL MSG_SETI( 'MAXVAL', NUM_BTOI( BHI ) )
            CALL MSG_OUT( 'PVLO', 'Data will be scaled from ^MINVAL '/
     :                    /'to ^MAXVAL.', STATUS )

*  Scale the data values using the extreme values into the
*  cell array.
            CALL KPG1_ISCLB( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., BLO, BHI,
     :                       LP, UP, BPCI, %VAL( CNF_PVAL( IP ) ),
     :                       STATUS )
         END IF

*  Do scaling with limits from the environment.
*  ============================================
      ELSE IF ( MODE(1:2) .EQ. 'SC' .OR.
     :          MODE(1:2) .EQ. 'CU' ) THEN

*  If the current scaling is to be re-used...
         USECUR = .FALSE.
         IF( MODE(1:2) .EQ. 'CU' .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the current value of 'SCALOW', if any. An error will be reported
*  if it has no current value, but this is annulled below.
            CALL SUBPAR_FINDPAR( 'SCALOW', CODE, STATUS )
            CALL SUBPAR_CURVAL( CODE, LOTXT, STATUS )

*  We will later set this value as the dynamic default for 'LOW', so
*  we tell SUBPAR to accept the suggested default for 'LOW' without
*  prompting.
            CALL SUBPAR_FINDPAR( 'LOW', CODE, STATUS )
            CALL SUBPAR_ACCPT1( CODE, STATUS )

*  Get the current value of 'SCAHIGH', if any. An error will be reported
*  if it has no current value, but this is annulled below.
            CALL SUBPAR_FINDPAR( 'SCAHIGH', CODE, STATUS )
            CALL SUBPAR_CURVAL( CODE, HITXT, STATUS )

*  Indicate that the suggested default for 'HIGH' should be accepted
*  without prompting.
            CALL SUBPAR_FINDPAR( 'HIGH', CODE, STATUS )
            CALL SUBPAR_ACCPT1( CODE, STATUS )

*  If an error occurred above (e.g. no current value is set for 'SCALOW'
*  or 'SCAHIGH'), then annul it (we will continue to use normal SCALE mode
*  instead of CURRENT mode).
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

*  Otherwise report the scaling limits for future use, converting
*  floating point limits to integer limits if the NDF is of integer type.
            ELSE
               CALL CHR_CTOR( LOTXT, RMINV, STATUS )
               CALL CHR_CTOR( HITXT, RMAXV, STATUS )
               IF( ITYPE .EQ. '_REAL' .OR. ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL MSG_SETR( 'MINVAL', RMINV )
                  CALL MSG_SETR( 'MAXVAL', RMAXV )
               ELSE
                  IMINV = NUM_RTOI(RMINV)
                  CALL CHR_ITOC( IMINV, LOTXT, NC )
                  CALL MSG_SETI( 'MINVAL', IMINV )
                  IMAXV = NUM_RTOI(RMAXV)
                  CALL CHR_ITOC( IMAXV, HITXT, NC )
                  CALL MSG_SETI( 'MAXVAL', IMAXV )
               END IF
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '//
     :                       '^MINVAL to ^MAXVAL.', STATUS )
               USECUR = .TRUE.
            END IF

*  Use SCALE mode from here on.
            MODE = 'SCALE'
         END IF

*  Determine whether or not the scaling parameters have been
*  found, to avoid finding the maximum and minimum values when
*  they are not required.
         CALL LPG_STATE( 'LOW', ACTLOW, STATUS )
         CALL LPG_STATE( 'HIGH', ACTHIG, STATUS )
         FNDRNG = ACTLOW .EQ. SUBPAR__ACTIVE .AND.
     :            ACTHIG .EQ. SUBPAR__ACTIVE

*  Select appropriate routine for the data type chosen and scale
*  the image between user-defined limits.  The cell array
*  has values between the colour-index limits LP and UP.
*  =============================================================
         IF ( ITYPE .EQ. '_REAL' ) THEN

*  Use the previous values as defaults.
            IF( USECUR ) THEN
               CALL CHR_CTOR( LOTXT, RMINV, STATUS )
               CALL CHR_CTOR( HITXT, RMAXV, STATUS )

*  Or set the scaling limits to the extreme values.
            ELSE IF ( FNDRNG ) THEN
               RMINV = VAL__MINR
               RMAXV = VAL__MAXR

*  Or obtain the maximum and minimum values to be used as dynamic defaults.
            ELSE
               CALL KPG1_MXMNR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be possible to
*  save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*  Obtain the scaling limits from the environment and do the scaling into
*  the cell array.
            CALL KPS1_DSCLR( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., 'LOW', 'HIGH',
     :                       LP, UP, BPCI, RMINV, RMAXV, .TRUE.,
     :                       %VAL( CNF_PVAL( IP ) ), RLO, RHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Use the previous values as defaults.
            IF( USECUR ) THEN
               CALL CHR_CTOD( LOTXT, DMINV, STATUS )
               CALL CHR_CTOD( HITXT, DMAXV, STATUS )

*  Set the scaling limits to the extreme values.
            ELSE IF ( FNDRNG ) THEN
               DMINV = VAL__MIND
               DMAXV = VAL__MAXD
            ELSE

*  Obtain the maximum and minimum values to be used as dynamic defaults.
               CALL KPG1_MXMND( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*  Obtain the scaling limits from the environment and do the
*  scaling into the cell array.
            CALL KPS1_DSCLD( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., 'LOW', 'HIGH',
     :                       LP, UP, BPCI, DMINV, DMAXV, .TRUE.,
     :                       %VAL( CNF_PVAL( IP ) ), DLO, DHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Use the previous values as defaults.
            IF( USECUR ) THEN
               CALL CHR_CTOI( LOTXT, IMINV, STATUS )
               CALL CHR_CTOI( HITXT, IMAXV, STATUS )

*  Set the scaling limits to the extreme values.
            ELSE IF ( FNDRNG ) THEN
               IMINV = VAL__MINI
               IMAXV = VAL__MAXI
            ELSE

*  Obtain the maximum and minimum values to be used as dynamic defaults.
               CALL KPG1_MXMNI( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          IMAXV, IMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*  Obtain the scaling limits from the environment and do the
*  scaling into the cell array.
            CALL KPS1_DSCLI( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., 'LOW', 'HIGH',
     :                       LP, UP, BPCI, IMINV, IMAXV, .TRUE.,
     :                       %VAL( CNF_PVAL( IP ) ), ILO, IHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Use the previous values as defaults.
            IF( USECUR ) THEN
               CALL CHR_CTOI( LOTXT, IMINV, STATUS )
               CALL CHR_CTOI( HITXT, IMAXV, STATUS )
               WMINV = IMINV
               WMAXV = IMAXV

*  Set the scaling limits to the extreme values.
            ELSE IF ( FNDRNG ) THEN
               WMINV = VAL__MINW
               WMAXV = VAL__MAXW
            ELSE

*  Obtain the maximum and minimum values to be used as dynamic defaults.
               CALL KPG1_MXMNW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          WMAXV, WMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*  Obtain the scaling limits from the environment and do the
*  scaling into the cell array.
            CALL KPS1_DSCLW( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., 'LOW', 'HIGH',
     :                       LP, UP, BPCI, WMINV, WMAXV, .TRUE.,
     :                       %VAL( CNF_PVAL( IP ) ), WLO, WHI, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Use the previous values as defaults.
            IF( USECUR ) THEN
               CALL CHR_CTOI( LOTXT, IMINV, STATUS )
               CALL CHR_CTOI( HITXT, IMAXV, STATUS )
               BMINV = IMINV
               BMAXV = IMAXV

*  Set the scaling limits to the extreme values.
            ELSE IF ( FNDRNG ) THEN
               BMINV = VAL__MINB
               BMAXV = VAL__MAXB
            ELSE

*  Obtain the maximum and minimum values to be used as
*  dynamic defaults.
               CALL KPG1_MXMNB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          BMAXV, BMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )
            END IF

*  Obtain the scaling limits from the environment and do the
*  scaling into the cell array.
            CALL KPS1_DSCLB( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       .FALSE., 'LOW', 'HIGH',
     :                       LP, UP, BPCI, BMINV, BMAXV, .TRUE.,
     :                       %VAL( CNF_PVAL( IP ) ), BLO, BHI, STATUS )
         END IF

*  Do scaling with percentile limits.
*  ==================================
      ELSE IF ( MODE(1:2) .EQ. 'PE' ) THEN

*  Find the percentiles required.  There is no dynamic default.
         DO I = 1, NPRCTL
            PERDEF( I ) = VAL__BADR
         END DO
         CALL PAR_GDR1R( 'PERCENTILES', NPRCTL, PERDEF, 0.0, 100.0,
     :                   .FALSE., PERCNT, STATUS )

*  Convert percentiles to fractions.
         DO  I = 1, NPRCTL
            PERCNT( I ) = PERCNT( I ) * 0.01
         END DO

*  Record the polarity.
         POSTIV = PERCNT( 2 ) .GT. PERCNT( 1 )

*  Also get the number of histogram bins to be used - suggest
*  the maximum allowable as the default.
         CALL PAR_GDR0I( 'NUMBIN', MAXBIN, 1, MAXBIN, .TRUE., NUMBIN,
     :                   STATUS )

*  Since NUMBIN is passed as an adjustable-array dimension we have
*  to check it has been obtained.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Select appropriate routine for the data type chosen and scale
*  the image between user-defined limits.  The cell array
*  has values between the colour-index limits LP and UP.
*  =============================================================
            IF ( ITYPE .EQ. '_REAL' ) THEN

*  Obtain the maximum and minimum values to define the bounds
*  of the histogram.
               CALL KPG1_MXMNR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          RMAXV, RMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds.
               CALL KPG1_GHSTR( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., RMAXV, RMINV, HIST,
     :                          STATUS )

*  Estimate the values at the percentiles.
               CALL KPG1_HSTFR( NUMBIN, HIST, RMAXV, RMINV, NPRCTL,
     :                          PERCNT, PERVAL, STATUS )

*  Swap the percentiles back if they were flipped.
               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*  Report the scaling limits for future use.
               CALL MSG_SETR( 'MINVAL', PERVAL( 1 ) )
               CALL MSG_SETR( 'MAXVAL', PERVAL( 2 ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*  Scale the data values using the percentile values. Copy the scaling
*  values for output.
               RLO = PERVAL( 1 )
               RHI = PERVAL( 2 )
               CALL KPG1_ISCLR( BAD, NX, NY,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          .FALSE., RLO,
     :                          RHI, LP, UP, BPCI,
     :                          %VAL( CNF_PVAL( IP ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

*  Obtain the maximum and minimum values to define the bounds
*  of the histogram.
               CALL KPG1_MXMND( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          DMAXV, DMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds.
               CALL KPG1_GHSTD( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., DMAXV, DMINV, HIST,
     :                          STATUS )

*  Estimate the values at the percentiles.
               CALL KPG1_HSTFR( NUMBIN, HIST, SNGL( DMAXV ),
     :                          SNGL( DMINV ), NPRCTL, PERCNT, PERVAL,
     :                          STATUS )

*  Swap the percentiles back if they were flipped.
               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*  Report the scaling limits for future use.
               CALL MSG_SETR( 'MINVAL', PERVAL( 1 ) )
               CALL MSG_SETR( 'MAXVAL', PERVAL( 2 ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*  Scale the data values using the percentile values.
*  Copy the scaling values for output.
               DLO = DBLE( PERVAL( 1 ) )
               DHI = DBLE( PERVAL( 2 ) )
               CALL KPG1_ISCLD( BAD, NX, NY,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          .FALSE., DLO,
     :                          DHI, LP, UP, BPCI,
     :                          %VAL( CNF_PVAL( IP ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN

*  Obtain the maximum and minimum values to define the bounds
*  of the histogram.
               CALL KPG1_MXMNI( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          IMAXV, IMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds.
               CALL KPG1_GHSTI( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., IMAXV, IMINV, HIST,
     :                          STATUS )

*  Estimate the values at the percentiles.
               CALL KPG1_HSTFR( NUMBIN, HIST, REAL( IMAXV ),
     :                          REAL( IMINV ), NPRCTL, PERCNT, PERVAL,
     :                          STATUS )

*  Swap the percentiles back if they were flipped.
               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*  Report the scaling limits for future use.
               CALL MSG_SETI( 'MINVAL', IFIX( PERVAL( 1 ) ) )
               CALL MSG_SETI( 'MAXVAL', IFIX( PERVAL( 2 ) ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*  Scale the data values using the percentile values.
*  Copy the scaling values for output.
               ILO = IFIX( PERVAL( 1 ) )
               IHI = IFIX( PERVAL( 2 ) )
               CALL KPG1_ISCLI( BAD, NX, NY,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          .FALSE., ILO,
     :                          IHI, LP, UP, BPCI,
     :                          %VAL( CNF_PVAL( IP ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_WORD' ) THEN

*  Obtain the maximum and minimum values to define the bounds
*  of the histogram.
               CALL KPG1_MXMNW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          WMAXV, WMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds.
               CALL KPG1_GHSTW( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., WMAXV, WMINV, HIST,
     :                          STATUS )

*  Estimate the values at the percentiles.
               CALL KPG1_HSTFR( NUMBIN, HIST, NUM_WTOR( WMAXV ),
     :                          NUM_WTOR( WMINV ), NPRCTL, PERCNT,
     :                          PERVAL, STATUS )

*  Swap the percentiles back if they were flipped.
               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*  Report the scaling limits for future use.
               CALL MSG_SETI( 'MINVAL', IFIX( PERVAL( 1 ) ) )
               CALL MSG_SETI( 'MAXVAL', IFIX( PERVAL( 2 ) ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*  Scale the data values using the percentile values.
*  Copy the scaling values for output.
               WLO = NUM_RTOW( PERVAL( 1 ) )
               WHI = NUM_RTOW( PERVAL( 2 ) )
               CALL KPG1_ISCLW( BAD, NX, NY,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          .FALSE., WLO,
     :                          WHI, LP, UP, BPCI,
     :                          %VAL( CNF_PVAL( IP ) ), STATUS )

            ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN

*  Obtain the maximum and minimum values to define the bounds
*  of the histogram.
               CALL KPG1_MXMNB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          NINVAL,
     :                          BMAXV, BMINV, MAXPOS, MINPOS, STATUS )

*  The number of bad pixels has been counted so it might be
*  possible to save future processing.
               BAD = BAD .OR. ( NINVAL .EQ. 0 )

*  Generate the histogram between those bounds.
               CALL KPG1_GHSTB( BAD, EL, %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ), 0.0D0,
     :                          NUMBIN, .FALSE., BMAXV, BMINV, HIST,
     :                          STATUS )

*  Estimate the values at the percentiles.
               CALL KPG1_HSTFR( NUMBIN, HIST, NUM_BTOR( BMAXV ),
     :                          NUM_BTOR( BMINV ), NPRCTL, PERCNT,
     :                          PERVAL, STATUS )

*  Swap the percentiles back if they were flipped.
               IF ( .NOT. POSTIV ) THEN
                  DUMMY = PERVAL( 1 )
                  PERVAL( 1 ) = PERVAL( 2 )
                  PERVAL( 2 ) = DUMMY
               END IF

*  Report the scaling limits for future use.
               CALL MSG_SETI( 'MINVAL', IFIX( PERVAL( 1 ) ) )
               CALL MSG_SETI( 'MAXVAL', IFIX( PERVAL( 2 ) ) )
               CALL MSG_OUT( 'PVLO', 'Data will be scaled from '/
     :                       /'^MINVAL to ^MAXVAL.', STATUS )

*  Scale the data values using the percentile values.
*  Copy the scaling values for output.
               BLO = NUM_RTOB( PERVAL( 1 ) )
               BHI = NUM_RTOB( PERVAL( 2 ) )
               CALL KPG1_ISCLB( BAD, NX, NY,
     :                          %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                          .FALSE., BLO,
     :                          BHI, LP, UP, BPCI,
     :                          %VAL( CNF_PVAL( IP ) ), STATUS )
            END IF

*  End of status check.
         END IF

*  Flash (as positive) the array without scaling.
*  ==============================================
      ELSE IF ( MODE(1:2) .EQ. 'FL' ) THEN

*  Select appropriate routine for the data type chosen and flash
*  the image via modulo arithmetic.  The cell array has values
*  between the colour-index limits LP and UP.
*  =============================================================
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPG1_FLASR( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       LP, UP, .FALSE.,
     :                       .TRUE., %VAL( CNF_PVAL( IP ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPG1_FLASD( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       LP, UP, .FALSE.,
     :                       .TRUE., %VAL( CNF_PVAL( IP ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPG1_FLASI( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       LP, UP, .FALSE.,
     :                       .TRUE., %VAL( CNF_PVAL( IP ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPG1_FLASW( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       LP, UP, .FALSE.,
     :                       .TRUE., %VAL( CNF_PVAL( IP ) ), STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPG1_FLASB( BAD, NX, NY,
     :                       %VAL( CNF_PVAL( PNTRI( 1 ) ) ),
     :                       LP, UP, .FALSE.,
     :                       .TRUE., %VAL( CNF_PVAL( IP ) ), STATUS )
         END IF

      END IF

*  Return the scaling values.
*  =========================

*  There is no scaling in flash mode.
      IF ( MODE( 1:2 ) .NE. 'FL' ) THEN

*  Convert the limits to double precision.
         IF ( ITYPE .EQ. '_REAL' ) THEN
            DLO = DBLE( RLO )
            DHI = DBLE( RHI )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            DLO = DBLE( ILO )
            DHI = DBLE( IHI )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            DLO = NUM_WTOD( WLO )
            DHI = NUM_WTOD( WHI )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            DLO = NUM_BTOD( BLO )
            DHI = NUM_BTOD( BHI )
         END IF

*  Use the pen limits for flash mode.
      ELSE
         DLO = DBLE( LP )
         DHI = DBLE( UP )
      END IF

 999  CONTINUE

*  Release the memory holding unscaled values.
      IF( BLOCK ) THEN
         CALL PSX_FREE( PNTRI( 1 ), STATUS )
      ELSE
         CALL NDF_UNMAP( INDF, COMP, STATUS )
      END IF

*  Return a pointer value of zero if an error occurs.
      IF( STATUS .NE. SAI__OK .AND. IP .NE. 0 ) THEN
         CALL PSX_FREE( IP, STATUS )
         IP = 0
      END IF

      END
