      SUBROUTINE KPS1_PLINT( INDF, IWCS, MAP, PARAMS, CONTNR, NAXC,
     :                       RETAX, INTERP, RSPARS, TOL, STATUS )
*+
*  Name:
*     KPS1_PLINT

*  Purpose:
*     Extracts and saves to NDFs interpolated slices about given
*     co-ordinates supplied by parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLINT( INDF, IWCS, MAP, PARAMS, CONTNR, NAXC, RETAX,
*                      INTERP, RSPARS, TOL, STATUS )

*  Description:
*     This routine extracts interpolated slices from an input NDF for
*     one or more locations, and stores each slice in its own output 
*     NDF.  Slices run parallel to pixel axes, but are not usually at
*     pixel centres, hence the need to interpolate.  The user provides
*     the slice's fixed co-ordinates in the current Frame through a
*     parameter (PARAM(1)).  The axes of these fixed co-ordinates have
*     single elements in each output NDF.   The axes to be retained
*     are specified through argument RETAX.
*
*     A series of slices can be extracted, the loop ending if a null
*     value is supplied for any of the PARAMS parameters.  Only one
*     slice is extracted if PARAM(1) is specified on the command line
*     to allow easier scripting).
*
*     There is a wide selection of interpolation methods available.
*
*     The output NDFs may be stored in an HDS container file (see
*     argument CONTNR) in an HDS_CONTAINER type top-level structure.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifer of the input NDF.
*     IWCS = INTEGER (Given)
*        The FrameSet associated with the input NDF.
*     MAP = INTEGER (Given)
*        The AST Mapping from the Frame in which the fixed co-ordinates
*        are supplied, to the GRID Frame of the NDF.
*     PARAMS( 3 ) = CHARACTER * ( * ) (Given)
*        The names of environment parameters.  The elements are
*        respectively for acquiring the fixed positions, the name of the
*        output NDF or container file, and the title of the output NDF.
*     CONTNR = LOGICAL (Given)
*        If set to .TRUE., the output NDFs are written as components of
*        an HDS container file specified through parameter PARAMS(2).
*        Otherwise each NDF is written to its own file.
*     NAXC = INTEGER (Given)
*        The number of axes in the current Frame.
*     RETAX( NAXC ) = LOGICAL (Given)
*        A .TRUE. value indicates that an axis is retained in the output
*        NDF.
*     INTERP = INTEGER (Given)
*        The resampling-scheme identifier for AST_RESAMPLE<T>.
*     RSPARS( 2 ) = DOUBLE PRECISION (Given)
*        Qualifying parameters required by the Sinc, SincSinc, SincCos,
*        SincGauss, Somb, SombCos, and Gauss interpolation methods in
*        AST_RESAMPLE<X>.
*
*        RSPARS( 1 ) is required by all the above schemes.  It is used
*        to specify how many pixels are to contribute to the interpolated
*        result on either side of the interpolation in each dimension. 
*        Typically, a value of 2 is appropriate and the minimum allowed
*        value is 1 (i.e. one pixel on each side).  A value of zero or
*        fewer indicates that a suitable number of pixels should be
*        calculated automatically.
*
*        RSPARS( 2 ) is required only by the SombCos, Gauss, SincSinc,
*        SincCos, and SincGauss schemes.  For the SombCos, SincSinc, and
*        SincCos schemes, it specifies the number of pixels at which the
*        envelope of the function goes to zero.  The minimum value is
*        1.0, and the run-time default value is 2.0.  For the Gauss and
*        SincGauss scheme, it specifies the full-width at half-maximum
*        (FWHM) of the Gaussian envelope.  The minimum value is 0.1, and
*        the run-time default is 1.0.  On astronomical images and
*        spectra, good results are often obtained by approximately
*        matching the FWHM of the envelope function, given by RSPARS(2),
*        to the point-spread function of the input data.
*     TOL = DOUBLE PRECISION (Given)
*        The maximum tolerable geometrical distortion which may be
*        introduced as a result of approximating non-linear Mappings
*        by a set of piece-wise linear transforms.  The resampling
*        algorithm uses approximate non-linear co-ordinate
*        transformations in order to improve performance, and this
*        parameter controls how inaccurate the resulting approximation
*        is allowed to be, as a displacement in pixels of the input NDF. 
*        A value of zero will ensure that no such approximation is done,
*        at the expense of increasing execution time.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007, 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 July 17 (MJC):
*        Original version.
*     2010 May 11 (MJC):
*        Still erase the temporary NDF even a bad status has occurred
*        elsewhere.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'DAT_PAR'          ! Data-system error constants
      INCLUDE 'PAR_PAR'          ! PAR constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      INTEGER MAP
      CHARACTER*(*) PARAMS( 3 )
      LOGICAL CONTNR
      INTEGER NAXC
      LOGICAL RETAX( NAXC )
      INTEGER INTERP
      DOUBLE PRECISION RSPARS( 2 )
      DOUBLE PRECISION TOL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CFRM               ! AST pointer Current Frame in WCS
                                 ! FrameSet
      DOUBLE PRECISION DLBNDI( NDF__MXDIM ) ! Lower bounds, input array
      CHARACTER*(NDF__SZFTP) DTYPE ! Data type for output components
      DOUBLE PRECISION DUBNDI( NDF__MXDIM ) ! Upper bounds, input array
      LOGICAL FIRST              ! First exteraction to container file?
      DOUBLE PRECISION FIXPOS( NDF__MXDIM ) ! Representative position
                                 ! for retained axes
      DOUBLE PRECISION FULPOS( NDF__MXDIM ) ! Current-frame co-ordinates
                                 ! of section
      DOUBLE PRECISION GRDPOS( NDF__MXDIM ) ! Grid co-ordinates of
                                 ! section
      INTEGER I                  ! Position index
      DOUBLE PRECISION INPOS( NDF__MXDIM ) ! User-supplied position
      CHARACTER*(NDF__SZTYP) ITYPE ! Data type for processing
      INTEGER J                  ! Axis index
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER LISTAX( NDF__MXDIM ) ! Axes whose co-ordimates are to be
                                 ! supplied
      CHARACTER*( DAT__SZLOC ) LOCN ! HDS locator to dummy NDF
      DOUBLE PRECISION LPO       ! Lower bound in output array
      INTEGER MAPIO              ! Mapping from input to output
      LOGICAL MORE               ! Process another position?
      INTEGER NAXREQ             ! Number of axes required
      INTEGER NDIM               ! Number of pixel axes
      CHARACTER*256 PATH         ! Path to container file
      INTEGER PLEN               ! Length of container filename & path
      INTEGER PMAP               ! AST pointer to PermMap between Base
                                 ! frames
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! Pixel shifts
      LOGICAL SINGLE             ! Process only a single positions?
      INTEGER STATE              ! State of the supplied parameter
      INTEGER SUBFRM             ! AST pointer to subset Base Frame
      INTEGER TNDF               ! Identifier for temporary NDF
      INTEGER TSTAT              ! Temporary status
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      DOUBLE PRECISION UPO       ! Upper bound in output array
      DOUBLE PRECISION XL( NDF__MXDIM ) ! Lower bound co-ordinates
      DOUBLE PRECISION XU( NDF__MXDIM ) ! Upper bound co-ordinates

*.

*  Initialisations.
      DATA INPOS / NDF__MXDIM * AST__BAD /

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied Mappings have the required transformations.
      IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_PLINT_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )

      END IF

*  Choose the data type in which to process the data.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', INDF, INDF, 'Data', ITYPE, DTYPE,
     :                STATUS )


*  Determine representative co-ordinates for the retained axes.
*  ============================================================

*  Obtain the input NDF's bounds.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

*  Find the input bounds in GRID space.
      DO I = 1, NDIM
         DLBNDI( I ) = 1.0D0
         DUBNDI( I ) = DBLE( UBNDI( I ) - LBNDI( I ) + 1 )

*  Obtain the output NDF's bounds.
         IF ( RETAX( I ) ) THEN
            LBNDO( I ) = LBNDI( I )
            UBNDO( I ) = UBNDI( I )
         ELSE
            LBNDO( I ) = 1
            UBNDO( I ) = 1
        END IF
      END DO

*  Work out the bounds of an array which would contain the resampled
*  copy of the whole input array and use the midpoint as representative
*  co-ordinates.
      DO I = 1, NDIM
         IF ( RETAX( I ) ) THEN
            CALL AST_MAPBOX( MAP, DLBNDI, DUBNDI, .FALSE., I, LPO, UPO,
     :                       XL, XU, STATUS )
            FIXPOS( I ) = 0.5D0 * ( LPO + UPO )
         END IF
      END DO

*  Make container file.
*  ====================
      IF ( CONTNR ) THEN

*  As we wish to use the multiple-input processing through LPG, we
*  first make a dummy NDF of the required name,  Also modify the
*  prompt string for PARAMS(2) parameter.
         CALL PAR_PROMT( PARAMS( 2 ), 'Output HDS container file',
     :                   STATUS )
         CALL LPG_CREAT( PARAMS( 2 ), '_UBYTE', 1, 1, 1, TNDF, STATUS )

*  Obtain its name.
         CALL NDF_MSG( 'NDF', TNDF )
         CALL MSG_LOAD( ' ', '^NDF', PATH, PLEN, STATUS )

*  Reset parameter state and prompt string again.
         CALL PAR_CANCL( PARAMS( 2 ), STATUS )
         CALL PAR_PROMT( PARAMS( 2 ), 'Output NDF within container',
     :                   STATUS )
      ELSE
         PATH = ' '
      END IF
      FIRST = .TRUE.

*  Main loop for each section
*  ==========================

*  Determine whether the position was supplied on the command line.
      CALL LPG_STATE( PARAMS( 1 ), STATE, STATUS )
      IF ( STATE .EQ. PAR__ACTIVE ) THEN
         SINGLE = .TRUE.
      ELSE
         SINGLE = .FALSE.
      END IF

*  Form list of WCS axes to be obtained.
      NAXREQ = 0
      DO I = 1, NAXC
         IF ( .NOT. RETAX( I ) ) THEN
            NAXREQ = NAXREQ + 1
            LISTAX( NAXREQ ) = I
         END IF
      END DO

*  Create a new Frame with the chosen axes.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      SUBFRM = AST_PICKAXES( CFRM, NAXREQ, LISTAX, PMAP, STATUS )

*  Loop to extract each interpolated section.
      MORE = .TRUE.
      DO WHILE ( MORE .AND. STATUS .EQ. SAI__OK )

*  Obtain the co-ordinates from the Frame with just the axes we require.
         IF ( .NOT. SINGLE ) CALL MSG_BLANK( STATUS )
         CALL KPG1_GTPOS( PARAMS( 1 ), SUBFRM, .FALSE., INPOS,
     :                    0.0D0, STATUS )
         IF ( .NOT. SINGLE ) CALL MSG_BLANK( STATUS )

*  If a null value was supplied, annul the error and indicate that the 
*  loop should be left.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MORE = .FALSE.

*  If an abort value was supplied, do not annul the error but indicate
*  that the loop should be left.
         ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
            MORE = .FALSE.

         END IF

*  Break out of the loop if no position was obtained.
         IF ( .NOT. MORE ) GO TO 10

*  Combine the fixed representative co-ordinates with the user-selected
*  position.
         J = 1
         DO I = 1, NAXC
            IF ( RETAX( I ) ) THEN
               FULPOS( I ) = FIXPOS( I )
            ELSE
               FULPOS( I ) = INPOS( J )
               J = J + 1
            END IF
         END DO

*  Transform the supplied position to the GRID Frame of the NDF.
         CALL AST_TRANN( MAP, 1, NAXC, 1, FULPOS, .TRUE., NDIM,
     :                   1, GRDPOS, STATUS )

*  Create a ShiftMap of the offsets.
         DO I = 1, NDIM
            IF ( RETAX( I ) ) THEN
               SHIFT( I ) = 0.0D0
            ELSE
               SHIFT( I ) = 1.0D0 - GRDPOS( I )
            END IF
         END DO

         MAPIO = AST_SHIFTMAP( NDIM, SHIFT, ' ', STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Interpolate and extract about the fixed co-ordinates.
*  =====================================================
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_PLRSR( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_PLRSB( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_PLRSD( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_PLRSI( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_PLRSUB( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                        FIRST, INTERP, RSPARS, TOL, INDF,
     :                        NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_PLRSUW( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                        FIRST, INTERP, RSPARS, TOL, INDF,
     :                        NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_PLRSW( MAPIO, PARAMS( 2 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )
         END IF

*  If only a single position is to be processed, set MORE so that
*  the loop is left.
         IF ( SINGLE ) THEN
            MORE = .FALSE.

*  Handle the expected null to end looping.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MORE = .FALSE.

*  Otherwise, cancel the parameter values.
         ELSE
            CALL PAR_CANCL( PARAMS( 1 ), STATUS )
            IF ( .NOT. CONTNR ) CALL PAR_CANCL( PARAMS( 2 ), STATUS )
*            CALL PAR_CANCL( PARAMS( 3 ), STATUS )

         END IF

         FIRST = .FALSE.
      END DO

*  Arrive here once all positions have been processed.
   10 CONTINUE

      IF ( CONTNR ) THEN

*  Status may be bad, but we still want to remove the dummy NDF.
         TSTAT = STATUS
         CALL ERR_MARK
         STATUS = SAI__OK

*  As we needed to make an NDF initially so that we could use LPG repeat
*  invocations for a series of input container files, this makes an
*  undefined NDF structure at the top level.  We need to modify the
*  structure type, and remove the dummy NDF.
         CALL NDF_LOC( TNDF, 'UPDATE', LOCN, STATUS )
         CALL DAT_RETYP( LOCN, 'NDF_CONTAINER', STATUS )
         CALL DAT_ERASE( LOCN, 'DATA_ARRAY', STATUS )
         CALL DAT_ANNUL( LOCN, STATUS )
         CALL ERR_RLSE
         STATUS = TSTAT

*  Since the NDF has been erased, yet we want to release the
*  temporary-NDF identifier, we need to ignore the error generated
*  from the DATA_ARRAY being in an undefined state.
         CALL ERR_MARK
         CALL NDF_ANNUL( TNDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
      END IF

*  Tidy up.
*  =======
 999  CONTINUE

      END
