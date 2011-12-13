      SUBROUTINE KPS1_PLFIL( INDF, IWCS, MAP, NPOS, NAXIN, INPOS, GOTID,
     :                       ID, PARAMS, CONTNR, NAXC, RETAX, INTERP,
     :                       RSPARS, TOL, NDIMS, GRDPOS, STATUS )
*+
*  Name:
*     KPS1_PLFIL

*  Purpose:
*     Extracts and saves to NDFs interpolated slices about given
*     co-ordinates supplied in a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL KPS1_PLFIL( INDF, IWCS, MAP, NPOS, NAXIN, INPOS, GOTID, ID,
*                       PARAMS, CONTNR, NAXC, RETAX, INTERP, RSPARS,
*                       TOL, NDIMS, GRDPOS, STATUS )

*  Description:
*     This routine extracts interpolated slices from an input NDF for
*     one or more locations, and stores each slice in its own output NDF.
*     Slices run parallel to pixel axes, but are not usually at pixel
*     centres, hence the need to interpolate.  The slice's fixed
*     co-ordinates Frame are passed through argument INPOS and the
*     mapping between their system and GRID supplied through argument
*     MAP.  The axes of these fixed co-ordinates have single elements
*     in each output NDF.  The axes to be retained are specified through
*     argument RETAX.
*
*     There is a wide selection of interpolation methods available.
*
*     The output NDFs may be stored in an HDS container file (see
*     argument CONTNR) in an HDS_CONTAINER type top-level structure.

*  Arguments:
*     INDF = INTEGER (Given)
*        The input NDF.
*     IWCS = INTEGER (Given)
*        The FrameSet associated with the input NDF.
*     MAP = INTEGER (Given)
*        The AST Mapping from the Frame in which the fixed positions are
*        supplied, to the GRID Frame of the NDF.
*     NPOS = INTEGER (Given)
*        The number of centroid positions to be found.
*     NAXIN = INTEGER (Given)
*        The number of axes in the Frame in which the initial guess
*        positions are supplied.
*     INPOS( NPOS, NAXIN ) = DOUBLE PRECISION (Given)
*        The initial guesses at the centroid positions.  These should be
*        in the co-ordinate system defined by MAP.
*     GOTID = LOGICAL (Given)
*        If TRUE then the position identifiers supplied in ID are used.
*        Otherwise identifiers equal to the position index are used.
*     ID( NPOS ) = INTEGER (Given)
*        A set of integer identifiers for the supplied positions.  These
*        are displayed with the centroid positions. Only accessed if
*        GOTID is .TRUE.
*     PARAMS( 2 ) = CHARACTER * ( * ) (Given)
*        The names of environment parameters.  The elements are
*        respectively for acquiring the name of the output NDF or
*        container file, and the title of the output NDF.  Otherwise
*        each NDF is written to its own file.
*     CONTNR = LOGICAL (Given)
*        If set to .TRUE., the output NDFs are written as components of
*        an HDS container file specified through parameter PARAMS(1).
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
*        AST_RESAMPLE<T>.
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
*     NDIMS = INTEGER (Given)
*        The number of pixel axes in the NDF.
*     GRDPOS( NPOS, NDIMS ) = DOUBLE PRECISION (Returned)
*        Workspace to store the positions of the slices given in the
*        GRID Frame of the NDF.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 July 13 (MJC):
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
      INCLUDE 'DAT_PAR'          ! Data-system error constants
      INCLUDE 'PAR_ERR'          ! Parameter-system error constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      INTEGER MAP
      INTEGER NPOS
      INTEGER NAXIN
      DOUBLE PRECISION INPOS( NPOS, NAXIN )
      LOGICAL GOTID
      INTEGER ID( NPOS )
      CHARACTER*(*) PARAMS( 2 )
      LOGICAL CONTNR
      INTEGER NAXC
      LOGICAL RETAX( NAXC )
      INTEGER INTERP
      DOUBLE PRECISION RSPARS( 2 )
      DOUBLE PRECISION TOL
      INTEGER NDIMS

*  Arguments Returned:
      DOUBLE PRECISION GRDPOS( NPOS, NDIMS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER * ( 2 ) CHR_NTH  ! Ordinal string st, nd, th

*  Local Variables:
      CHARACTER*22 BUFFER        ! Buffer for up to 99999 values read
      CHARACTER*( NDF__SZFTP ) DTYPE ! Data type for output components
      LOGICAL FIRST              ! First exteraction to container file?
      INTEGER I                  ! Position index
      CHARACTER*( NDF__SZTYP ) ITYPE ! Data type for processing
      INTEGER J                  ! Axis index
      INTEGER K                  ! Missing-axis index
      INTEGER LBNDI( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER LBNDO( NDF__MXDIM ) ! Lower bounds of output NDF
      INTEGER LISTAX( NDF__MXDIM ) ! Axes whose co-ordimates are to be
                                 ! supplied
      CHARACTER*( DAT__SZLOC ) LOCN ! HDS locator to dummy NDF
      INTEGER MAPIO              ! Mapping from input to output
      INTEGER NC                 ! Number of characters in buffer
      INTEGER NDIM               ! Number of pixel axes
      CHARACTER*256 PATH         ! Path to container file
      INTEGER PLEN               ! Length of container filename & path
      DOUBLE PRECISION SHIFT( NDF__MXDIM ) ! Pixel shifts
      INTEGER TNDF               ! Identifier for temporary NDF
      INTEGER TSTAT              ! Temporary status
      INTEGER UBNDI( NDF__MXDIM ) ! Upper bounds of input NDF
      INTEGER UBNDO( NDF__MXDIM ) ! Upper bounds of output NDF
      LOGICAL VERB               ! Flush errors instead of annulling them?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied Mappings have the required transformations.
      IF ( .NOT. AST_GETL( MAP, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_PLFIL_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'GRID Frame of the NDF is not defined.',
     :                 STATUS )

      END IF

*  See if we are running in verbose mode.
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

*  Determine the bounds of the input and output arrays.
*  ====================================================

*  Obtain the input NDF's bounds.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBNDI, UBNDI, NDIM, STATUS )

      DO I = 1, NDIM
         IF ( RETAX( I ) ) THEN
            LBNDO( I ) = LBNDI( I )
            UBNDO( I ) = UBNDI( I )
         ELSE
            LBNDO( I ) = 1
            UBNDO( I ) = 1
        END IF
      END DO

*  Transform the supplied positions to the GRID Frame of the NDF.
      CALL AST_TRANN( MAP, NPOS, NAXIN, NPOS, INPOS, .TRUE., NDIM,
     :                NPOS, GRDPOS, STATUS )

*  Determine a data type which can be used for operations on the
*  DATA and possibly VARIANCE components of the NDF.
      CALL NDF_MTYPN( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_REAL,'//
     :                '_DOUBLE', 1, INDF, 'DATA,VARIANCE', ITYPE,
     :                DTYPE, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Make container file.
*  ====================
      IF ( CONTNR ) THEN

*  As we wish to use the multiple-input processing through LPG, we
*  first make a dummy NDF of the required name,  Also modify the
*  prompt string for PARAMS(1) parameter.
         CALL PAR_PROMT( PARAMS( 1 ), 'Output HDS container file',
     :                   STATUS )
         CALL LPG_CREAT( PARAMS( 1 ), '_UBYTE', 1, 1, 1, TNDF, STATUS )

*  Obtain its name.
         CALL NDF_MSG( 'NDF', TNDF )
         CALL MSG_LOAD( ' ', '^NDF', PATH, PLEN, STATUS )

*  Reset parameter state and prompt string again.
         CALL PAR_CANCL( PARAMS( 1 ), STATUS )
         CALL PAR_PROMT( PARAMS( 1 ), 'Output NDF within container',
     :                   STATUS )

*  Advise the user how many prompts for PARAMS( 1 ) are coming.  If it
*  is large, the container file option may be the better alternative.
*  It should be possible to use GRP wildcards to create the output names
*  automatically.
      ELSE
         CALL MSG_SETI( 'N', NPOS )
         CALL MSG_OUTIF ( MSG__NORM, ' ',
     :                    'Catalogue has ^N positions.', STATUS )
         PATH = ' '
      END IF
      FIRST = .TRUE.

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 10

*  Loop to interpolate the section (in the GRID Frame).
      DO J = 1, NPOS

         NC = 15
         BUFFER = 'Output NDF for '
         CALL CHR_PUTI( J, BUFFER, NC )
         CALL CHR_PUTC( CHR_NTH( J ), BUFFER, NC )
         CALL PAR_PROMT( PARAMS( 1 ), BUFFER, STATUS )

*  Form the mapping.
*  =================

*  The supplied co-ordinates may or may not have values for the axes
*  being retained.  This is not a problem as we are only trying to
*  determine the shifts along the supplied axes.  However, we do need
*  to realise that the GRDPOS array's second dimension is not fully
*  populated, and pick out the the first NAXIN axes.
         K = 1
         DO I = 1, NDIM
            IF ( RETAX( I ) ) THEN
               SHIFT( I ) = 0.0
            ELSE
               SHIFT( I ) = 1.0D0 - GRDPOS( J, K )
               K = K + 1
            END IF
         END DO

*  Create a ShiftMap of the offsets.
         MAPIO = AST_SHIFTMAP( NDIM, SHIFT, ' ', STATUS )

*  Interpolate and extract about the fixed co-ordinates.
*  =====================================================
         IF ( ITYPE .EQ. '_REAL' ) THEN
            CALL KPS1_PLRSR( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_BYTE' ) THEN
            CALL KPS1_PLRSB( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
            CALL KPS1_PLRSD( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
            CALL KPS1_PLRSI( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
            CALL KPS1_PLRSUB( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                        FIRST, INTERP, RSPARS, TOL, INDF,
     :                        NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
            CALL KPS1_PLRSUW( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                        FIRST, INTERP, RSPARS, TOL, INDF,
     :                        NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
            CALL KPS1_PLRSW( MAPIO, PARAMS( 1 ), CONTNR, PATH( :PLEN ),
     :                       FIRST, INTERP, RSPARS, TOL, INDF,
     :                       NDIM, LBNDI, UBNDI, LBNDO, UBNDO, STATUS )

         END IF

*  Cancel the parameter values if we are looping.
         IF ( I .NE. NPOS ) THEN
            IF ( .NOT. CONTNR ) CALL PAR_CANCL( PARAMS( 1 ), STATUS )
*            CALL PAR_CANCL( PARAMS( 2 ), STATUS )
         END IF

         IF ( STATUS .EQ. PAR__ABORT ) THEN
            GO TO 10

         ELSE IF ( STATUS .NE. SAI__OK ) THEN

*  If we are processing more than one position, cancel the error so that
*  remaining positions can be processed.  For a single position, we retain
*  the error status.
            IF ( NPOS .GT. 1 ) THEN

*  If we are in verbose mode, add a context report and flush the message.
*  Otherwise, just annul the message.
               IF ( VERB ) THEN
                  IF ( GOTID ) THEN
                     CALL MSG_SETI( 'ID', ID( J ) )
                  ELSE
                     CALL MSG_SETI( 'ID', J )
                  END IF
                  CALL ERR_REP( 'KPS1_PLFIL_ERR4', 'No extraction '//
     :                          'for position ^ID.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               ELSE
                  CALL ERR_ANNUL( STATUS )
               END IF

             END IF
         END IF

         FIRST = .FALSE.
      END DO

  10  CONTINUE
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
