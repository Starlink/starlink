      SUBROUTINE KPS1_CENSG( INDF, CERROR, MAP1, MAP2, MAP3, RFRM,
     :                       PARAM, CURSOR, MARK, MARKER, NAXR, NAXIN,
     :                       LOGPOS, FDL, QUIET, NSIM, NDIMS, SLBND,
     :                       SUBND, SEARCH, POSTVE, GUESS, MXSHFT,
     :                       MXITER, OUTCO, FDO, TOLER, TITLE, MXPOS,
     :                       OUT, NPOS, PIXSIM, REPSIM, STATUS )
*+
*  Name:
*     KPS1_CENSG

*  Purpose:
*     Loop round processing individual centroids specified interactively.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CENSG( INDF, CERROR, MAP1, MAP2, MAP3, RFRM, PARAM,
*                      CURSOR, MARK, MARKER, NAXR, NAXIN, LOGPOS, FDL,
*                      QUIET, NSIM, NDIMS, SLBND, SUBND, SEARCH, POSTVE,
*                      GUESS, MXSHFT, MXITER, OUTCO, FDO, TOLER, TITLE,
*                      MXPOS, OUT, NPOS, PIXSIM, REPSIM, STATUS )

*  Description:
*     This routine finds the accurate centroids of a set of image
*     features identified interatively by the user, either with a cursor
*     or through an envinoment parameter. It can also estimate the error
*     on these positions, and display the results to a log file and the
*     screen.
*
*     Each centroid is calculated and displayed before getting the next
*     positions from the user. This routine should be used in
*     interactive modes such as "Cursor" or "Interface".

*  Arguments:
*     INDF = INTEGER (Given)
*        The input NDF.
*     CERROR = INTEGER (Given)
*        Should the error on the centroid positions be found and dislayed?
*        This requires the DNF to have a variance component.
*     MAP1 = INTEGER (Given)
*        The AST Mapping from the Frame in which the initial guess
*        positions are supplied, to the PIXEL Frame of the NDF.
*     MAP2 = INTEGER (Given)
*        The AST Mapping from the PIXEL Frame of the NDF to the
*        reporting Frame.
*     MAP3 = INTEGER (Given)
*        The AST Mapping from the Frame in which the initial guess
*        positions are supplied, to the reporting Frame.
*     RFRM = INTEGER (Given)
*        A pointer to the reporting Frame.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use when aquiring initial positions
*        in Interface mode.
*     CURSOR = LOGICAL (Given)
*        Get initial positions using the cursor? If not, get them using
*        the parameter given by PARAM.
*     MARK = CHARACTER * ( * ) (Given)
*        What positions are to be marked? Can be "INITIAL", "CENTROID" or
*        "NONE".
*     MARKER = INTEGER (Given)
*        The PGPLOT number for the marker type to mark the positions
*        specified by MARK.
*     NAXR = INTEGER (Given)
*        The number of axes in the reporting Frame.
*     NAXIN = INTEGER (Given)
*        The number of axes in the Frame in which the initial guess
*        positions are supplied.
*     LOGPOS = LOGICAL (Given)
*        Should the results be written to a log file?
*     FDL = INTEGER (Given)
*        The file descriptor for the log file. Ignored if LOGPOS is
*        .FALSE.
*     QUIET = INTEGER (Given)
*        If .FALSE., the results are written to standard output. If .TRUE.
*        nothing is written to standard output.
*     NSIM = INTEGER (Given)
*        The number of simulated positions to use when estimating the
*        errors on each centroid position.
*     NDIMS = INTEGER (Given)
*        The number of significant axes in the NDF (i.e. axes spanning
*        more than a single pixel).
*     SLBND( NDIMS ) = INTEGER (Given)
*        The lower pixel index bounds of the significant axes of the NDF.
*     SUBND( NDIMS ) = INTEGER (Given)
*        The UPER pixel index bounds of the significant axes of the NDF.
*     SEARCH( NDIMS ) = INTEGER (Given)
*        The dimensions of the search box to use when estimating the
*        centroid position, in pixels. Each value must be odd and lie
*        in the range 3--51.
*     POSTVE = LOGICAL (Given)
*        True if image features are positive above the background.
*     GUESS = LOGICAL (Given)
*        Should the original guess positions be included in the information
*        displayed on the screen and/or written to the log file?
*     MXSHFT( NDIMS ) = REAL (Given)
*        Maximum shifts allowable from the initial position along each
*        dimension.
*     MXITER = INTEGER  (Given)
*        Maximum number of iterations to be used.  At least one
*        iteration will be performed even if this is less than one.
*     OUTCO = LOGICAL (Given)
*        Should the pixel co-ordinates of the centroids be written to an
*        output text file?
*     FDO = INTEGER (Given)
*        Teh file descriptor for the output text file. Ignored if OUTCO
*        is .FALSE.
*     TOLER = REAL (Given)
*        Accuracy required in the centroid position.
*     TITLE = CHARACTER * ( * ) (Given)
*        A title to display before the first position.
*     MXPOS = INTEGER (Given)
*        The first dimension of the OUT array. This is the maximum number
*        of positions which can be returned. If the user supplied more
*        than this number of positions, only the first MXPOS will be returned.
*     OUT( MXPOS, NAXR ) = DOUBLE PRECISION (Returned)
*        The centroid positions in the reporting Frame.
*     NPOS = INTEGER (Returned)
*        The number of position returned in OUT.
*     PIXSIM( NSIM, NDIMS ) = DOUBLE PRECISION (Returned)
*        A work array to hold the PIXEL Frame co-ordinates at each
*        simulated centroid position.
*     REPSIM( NSIM, NAXR ) = DOUBLE PRECISION (Returned)
*        A work array to hold the reporting Frame co-ordinates at each
*        simulated centroid position.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999-2001, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2006 Particle Physics & Astronomy
*     Research Council.
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JUN-1999 (DSB):
*        Original version.
*     3-SEP-1999 (DSB):
*        Added NULL argument to KPG1_GTPOS call.
*     15-FEB-2000 (DSB):
*        KPG1_PGCUR argument list changed.
*     4-DEC-2001 (DSB):
*        Retain error messages if only a single position is being
*        centroided.
*     03-SEP-2004 (TIMJ):
*        Use CNF_PVAL.
*     03-MAY-2006 (TIMJ):
*        Initialse INPOS to AST__BAD.
*     2012 May 11 (MJC):
*        Add support for 64-bit integers.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      LOGICAL CERROR
      INTEGER MAP1
      INTEGER MAP2
      INTEGER MAP3
      INTEGER RFRM
      CHARACTER PARAM*(*)
      LOGICAL CURSOR
      CHARACTER MARK*(*)
      INTEGER MARKER
      INTEGER NAXR
      INTEGER NAXIN
      LOGICAL LOGPOS
      INTEGER FDL
      LOGICAL QUIET
      INTEGER NSIM
      INTEGER NDIMS
      INTEGER SLBND( NDIMS )
      INTEGER SUBND( NDIMS )
      INTEGER SEARCH( NDIMS )
      LOGICAL POSTVE
      LOGICAL GUESS
      REAL MXSHFT( NDIMS )
      INTEGER MXITER
      LOGICAL OUTCO
      INTEGER FDO
      REAL TOLER
      CHARACTER TITLE*(*)
      INTEGER MXPOS

*  Arguments Returned:
      DOUBLE PRECISION OUT( MXPOS, NAXR )
      INTEGER NPOS
      DOUBLE PRECISION PIXSIM( NSIM, NDIMS )
      DOUBLE PRECISION REPSIM( NSIM, NAXR )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER AMES(2)*30       ! Instructions on cursor use.
      CHARACTER AXVAL*50         ! A formatted axis value
      CHARACTER DTYPE*(NDF__SZFTP)! Data type for output components
      CHARACTER ITYPE*(NDF__SZTYP)! Data type for processing
      CHARACTER LINE*128         ! Buffer for output text
      DOUBLE PRECISION REPPOS( NDF__MXDIM )! Reporting Frame position
      DOUBLE PRECISION REPWAS( NDF__MXDIM )! Original position
      DOUBLE PRECISION ERROR( NDF__MXDIM )! Error on each axis
      DOUBLE PRECISION INPOS( NDF__MXDIM )! Supplied position
      DOUBLE PRECISION INCEN( NDF__MXDIM )! Centroid graphics position
      DOUBLE PRECISION PIXPOS( NDF__MXDIM )! Pixel position
      DOUBLE PRECISION SUM( NDF__MXDIM )! Sums of squared axis residuals
      INTEGER ACT                ! Cursor action index
      INTEGER CMARK              ! Marker to use when marking centroid positions
      INTEGER EL                 ! Number of mapped elements
      INTEGER I                  ! Position index
      INTEGER IAT                ! Number of characters currently in buffer
      INTEGER IMARK              ! Marker to use when marking initial positions
      INTEGER IPDIN              ! Pointer to mapped data array
      INTEGER IPVIN              ! Pointer to mapped variance array
      INTEGER IPW1               ! Pointer to work array
      INTEGER IPW2               ! Pointer to work array
      INTEGER J                  ! Axis index
      INTEGER NP                 ! No. of cursor position supplied.
      INTEGER NVAL( NDF__MXDIM ) ! Number of values summed in SUM
      INTEGER SEL                ! No. of elements in a search box
      INTEGER STATE              ! State of the supplied parameter
      INTEGER VLBND( NDF__MXDIM )! Signif. bounds of area used for variance estimation
      INTEGER VUBND( NDF__MXDIM )! Signif. bounds of area used for variance estimation
      INTEGER WDIM( NDF__MXDIM ) ! Dimensions of area used for variance
      LOGICAL INFO               ! Display instructions on cursor use?
      LOGICAL MORE               ! Process another position?
      LOGICAL OK                 ! Is this position OK?
      LOGICAL SINGLE             ! Process only a single positions?
      REAL EFINAL( NDF__MXDIM )  ! Pixel coords at simulated centroid position
      REAL INIT( NDF__MXDIM )    ! Pixel coords at initial guess position
      REAL PFINAL( NDF__MXDIM )  ! Pixel coords at genuine centroid position
      REAL X1, Y1                ! Co-ords. of the upper-right of picture
      REAL X2, Y2                ! Co-ords. of the lower-left of picture
      REAL XIN, YIN              ! Co-ords. of the centre of picture
*.
*  Initialisations
      DATA INPOS / NDF__MXDIM * AST__BAD /

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied Mappings have the required transformations.
      IF( .NOT. AST_GETL( MAP1, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CENSG_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )

      ELSE IF( CURSOR .AND. MARK( 1 : 1 ) .EQ. 'C' .AND.
     :         .NOT. AST_GETL( MAP1, 'TRANINVERSE', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_CENSG_MSG1','The Mapping required '//
     :                 'to map the centroid positions into the '//
     :                 'graphics co-ordinate Frame is not defined.',
     :                 STATUS )
         CALL MSG_OUT( 'KPS1_CENSG_MSG2','Centroid positions will '//
     :                 'not be marked!!', STATUS )
         MARK = 'None'

      END IF

      IF( .NOT. AST_GETL( MAP2, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CENSG_ERR2','The Mapping required '//
     :                 'to map pixel positions into the Current '//
     :                 'co-ordinate Frame is not defined.', STATUS )

      ELSE IF( GUESS .AND.
     :         .NOT. AST_GETL( MAP3, 'TRANFORWARD', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_CENSG_MSG3','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'Current co-ordinate Frame is not defined.',
     :                 STATUS )
         CALL MSG_OUT( 'KPS1_CENSG_MSG4','The supplied positions '//
     :                 'will not be reported!!', STATUS )
         GUESS = .FALSE.

      END IF

*  Choose the data type in which to process the data.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                '_REAL,_DOUBLE', INDF, INDF, 'Data', ITYPE, DTYPE,
     :                STATUS )

*  Map the data array from the NDF.
      CALL NDF_MAP( INDF, 'Data', ITYPE, 'READ', IPDIN, EL, STATUS )


*  Find number of elements in each search box.
      SEL = 1
      DO  I = 1, NDIMS
         WDIM( I ) = 2*MXSHFT( I ) + 2*( SEARCH( I )/2 ) + 2
         SEL = SEL * WDIM( I )
      END DO

*  If errors are required,
      IF( CERROR ) THEN

*  Get workspace for the simulations.
         CALL PSX_CALLOC( SEL, ITYPE, IPW1, STATUS )

*  Map the variance array from the NDF.
         CALL NDF_MAP( INDF, 'Variance', ITYPE, 'READ', IPVIN, EL,
     :                 STATUS )

      END IF

*  Get workspace for sorting pixel values in each box.
      CALL PSX_CALLOC( SEL*51*NDIMS, '_REAL', IPW2, STATUS )

*  The cursor will be positioned initially at the centre of the DATA
*  picture. Get the bounds the PGPLOT window, and store the mid
*  position if we are in cursor mode.
      IF( CURSOR ) THEN
         CALL PGQWIN( X1, X2, Y1, Y2 )
         XIN = 0.5*( X1 + X2 )
         YIN = 0.5*( Y1 + Y2 )

*  Indicate that information describing the use of the pointer and
*  buttons should be given before getting the first position.
         INFO = .TRUE.

*  Store the actions which can be performed using the mouse.
         AMES( 1 ) = 'Select an image feature'
         AMES( 2 ) = 'Exit'

*  Allow more than 1 position to be supplied.
         SINGLE = .FALSE.

*  If we are not in cursor mode, check whether or not the initial
*  co-ordinates are supplied on the command line.  If so, we set a
*  flag to indicate that only one centroid is to be determined.
      ELSE
         CALL LPG_STATE( PARAM, STATE, STATUS )
         IF ( STATE .EQ. SUBPAR__ACTIVE ) THEN
            SINGLE = .TRUE.
         ELSE
            SINGLE = .FALSE.
         END IF

      END IF

*  Initialise the number of positions displayed so far.
      NPOS = 0

*  Store the markers to use.
      IF( MARK( 1 : 1 ) .EQ. 'C' ) THEN
         CMARK = MARKER
         IMARK = -999

      ELSE IF( MARK( 1 : 1 ) .EQ. 'I' ) THEN
         CMARK = -999
         IMARK = MARKER

      ELSE
         CMARK = -999
         IMARK = -999

      END IF

*  Loop to find and display the centroid positions.
      MORE = .TRUE.
      DO WHILE( MORE .AND. STATUS .EQ. SAI__OK )

*  Get the initial position...
         IF( CURSOR ) THEN

*  Get a position using the cursor, in PGPLOT world co-ordinates. This
*  corresponds to the Base (i.e. GRAPHICS) Frame of the Plot (millimetres
*  from the bottom left corner of the view surface). The positions which
*  may be selected are restricted to the current picture.
            CALL KPG1_PGCUR( INFO, 'select an image feature', 2, AMES,
     :                       ' .', X1, X2, Y1, Y2, 0, XIN, YIN, 1, 0,
     :                       0, 0, IMARK, AST__NULL, XIN, YIN, ACT, NP,
     :                       STATUS )

*  Look out for the abort, i.e. the number of points is zero.
            IF( NP .EQ. 0 ) THEN
               MORE = .FALSE.

*  If a position was supplied, convert it to double precision.
            ELSE
               INPOS( 1 ) = DBLE( XIN )
               INPOS( 2 ) = DBLE( YIN )
            END IF

* Indicate that we do not need to see the cursor instructions again.
            INFO = .FALSE.

*  In interactive mode, just get a position using the specified parameter.
         ELSE
            IF( .NOT. ( SINGLE .OR. QUIET ) ) CALL MSG_BLANK( STATUS )
            CALL KPG1_GTPOS( PARAM, RFRM, .FALSE., INPOS, 0.0D0,
     :                       STATUS )
            IF( .NOT. ( SINGLE .OR. QUIET ) ) CALL MSG_BLANK( STATUS )

*  If a null value was supplied, annul the error and indicate that
*  the loop should be left.
            IF( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
               MORE = .FALSE.

*  If an abort value was supplied, do not annul the error but indicate that
*  the loop should be left.
            ELSE IF( STATUS .EQ. PAR__ABORT ) THEN
               MORE = .FALSE.

            END IF

         END IF

*  Break out of the loop if no position was obtained.
         IF( .NOT. MORE ) GO TO 10

*  Transform the supplied position to the PIXEL Frame of the NDF.
         CALL AST_TRANN( MAP1, 1, NAXIN, 1, INPOS, .TRUE., NDIMS,
     :                   1, PIXPOS, STATUS )

*  Copy the initial PIXEL position into a single precision array,
*  checking for bad axis values.
         OK = .TRUE.
         DO J = 1, NDIMS
            IF( PIXPOS( J ) .NE. AST__BAD ) THEN
               INIT( J ) = REAL( PIXPOS( J ) )
            ELSE
               INIT( J ) = VAL__BADR
               OK = .FALSE.
            END IF
         END DO

*  If the initial position is good, find the centroid position.
         IF( OK ) THEN

*  Call the subroutine that does the actual work for the required data type.
*  The position is returned in pixel co-ordinates.
            IF( ITYPE .EQ. '_INTEGER' ) THEN
               CALL KPG1_LOCTI( NDIMS, SLBND, SUBND,
     :                          %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_REAL' ) THEN
               CALL KPG1_LOCTR( NDIMS, SLBND, SUBND,
     :                          %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
               CALL KPG1_LOCTD( NDIMS, SLBND, SUBND,
     :                          %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_INT64' ) THEN
               CALL KPG1_LOCTK( NDIMS, SLBND, SUBND,
     :                          %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_WORD' ) THEN
               CALL KPG1_LOCTW( NDIMS, SLBND, SUBND,
     :                          %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
               CALL KPG1_LOCTUW( NDIMS, SLBND, SUBND,
     :                           %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
               CALL KPG1_LOCTB( NDIMS, SLBND, SUBND,
     :                          %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
               CALL KPG1_LOCTUB( NDIMS, SLBND, SUBND,
     :                           %VAL( CNF_PVAL( IPDIN ) ),
     :                          INIT, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, SEL, PFINAL,
     :                          %VAL( CNF_PVAL( IPW2 ) ),
     :                          STATUS )

            ELSE IF( STATUS .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'T', ITYPE )
               CALL ERR_REP( 'KPS1_CENSG_ERR3', 'KPS1_CENSG: This '//
     :                       'application does not yet support the '//
     :                       '^T data type.', STATUS )
               GO TO 999
            END IF

*  If the position could not be found...
            IF( STATUS .NE. SAI__OK ) THEN
               OK = .FALSE.

*  If multiple positions are being supplied, flush the error so that
*  remaining positions can be processed.
               IF( .NOT. SINGLE ) CALL ERR_FLUSH( STATUS )

*  If a good centroid was found, transform it to the reporting Frame.
            ELSE
               DO J = 1, NDIMS
                  IF( PFINAL( J ) .NE. VAL__BADR ) THEN
                     PIXPOS( J ) = DBLE( PFINAL( J ) )
                  ELSE
                     PIXPOS( J ) = AST__BAD
                  END IF
               END DO

               CALL AST_TRANN( MAP2, 1, NDIMS, 1, PIXPOS, .TRUE., NAXR,
     :                         1, REPPOS, STATUS )
            END IF

         END IF

*  Now estimate the error on this centroid position if required.
*  =============================================================
         IF( CERROR .AND. OK ) THEN

*  Find the bounds of the area which will be used to determine the
*  variance of the centroid position. We need to make a separate copy
*  of this area so that noise can be added into it. The bounds of the
*  area are such as to cover a search box at the maximum allowed shift
*  from the final centroid position found above.
            DO J = 1, NDIMS
               VUBND( J ) = INT( PFINAL( J ) + 0.5 ) + WDIM( J )/2
               VLBND( J ) = VUBND( J ) - WDIM( J ) + 1
            END DO

*  Perform each simulation, storing the simulated centroid positions in a
*  work array.
            DO  I = 1, NSIM

*  Call appropriate routines depending on the data type.
               IF( ITYPE .EQ. '_INTEGER' ) THEN

*  Copy the required area from the data array to the workspace, and
*  add gaussian noise.
                  CALL KPS1_CENAI( NDIMS, SLBND, SUBND,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             %VAL( CNF_PVAL( IPVIN ) ),
     :                             VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             STATUS )

*  Find the new centroid after the noise has been added. The position is
*  returned in pixel co-ordinates.
                  CALL KPG1_LOCTI( NDIMS, VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             PFINAL, SEARCH, POSTVE, MXSHFT,
     :                             MXITER, TOLER, SEL, EFINAL,
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Now do the same for the other data types.
               ELSE IF( ITYPE .EQ. '_REAL' ) THEN
                  CALL KPS1_CENAR( NDIMS, SLBND, SUBND,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             %VAL( CNF_PVAL( IPVIN ) ),
     :                             VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             STATUS )
                  CALL KPG1_LOCTR( NDIMS, VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             PFINAL, SEARCH, POSTVE, MXSHFT,
     :                             MXITER, TOLER, SEL, EFINAL,
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )

               ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
                  CALL KPS1_CENAD( NDIMS, SLBND, SUBND,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             %VAL( CNF_PVAL( IPVIN ) ),
     :                             VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             STATUS )
                  CALL KPG1_LOCTD( NDIMS, VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             PFINAL, SEARCH, POSTVE, MXSHFT,
     :                             MXITER, TOLER, SEL, EFINAL,
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )

               ELSE IF( ITYPE .EQ. '_INT64' ) THEN
                  CALL KPS1_CENAK( NDIMS, SLBND, SUBND,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             %VAL( CNF_PVAL( IPVIN ) ),
     :                             VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             STATUS )
                  CALL KPG1_LOCTK( NDIMS, VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             PFINAL, SEARCH, POSTVE, MXSHFT,
     :                             MXITER, TOLER, SEL, EFINAL,
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )

               ELSE IF( ITYPE .EQ. '_WORD' ) THEN
                  CALL KPS1_CENAW( NDIMS, SLBND, SUBND,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             %VAL( CNF_PVAL( IPVIN ) ),
     :                             VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             STATUS )
                  CALL KPG1_LOCTW( NDIMS, VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             PFINAL, SEARCH, POSTVE, MXSHFT,
     :                             MXITER, TOLER, SEL, EFINAL,
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )

               ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
                  CALL KPS1_CENAUW( NDIMS, SLBND, SUBND,
     :                              %VAL( CNF_PVAL( IPDIN ) ),
     :                              %VAL( CNF_PVAL( IPVIN ) ),
     :                              VLBND, VUBND,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              STATUS )
                  CALL KPG1_LOCTUW( NDIMS, VLBND, VUBND,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              PFINAL, SEARCH,
     :                              POSTVE, MXSHFT, MXITER, TOLER,
     :                              SEL, EFINAL,
     :                              %VAL( CNF_PVAL( IPW2 ) ),
     :                              STATUS )

               ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
                  CALL KPS1_CENAB( NDIMS, SLBND, SUBND,
     :                             %VAL( CNF_PVAL( IPDIN ) ),
     :                             %VAL( CNF_PVAL( IPVIN ) ),
     :                             VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             STATUS )
                  CALL KPG1_LOCTB( NDIMS, VLBND, VUBND,
     :                             %VAL( CNF_PVAL( IPW1 ) ),
     :                             PFINAL, SEARCH, POSTVE, MXSHFT,
     :                             MXITER, TOLER, SEL, EFINAL,
     :                             %VAL( CNF_PVAL( IPW2 ) ), STATUS )

               ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
                  CALL KPS1_CENAUB( NDIMS, SLBND, SUBND,
     :                              %VAL( CNF_PVAL( IPDIN ) ),
     :                              %VAL( CNF_PVAL( IPVIN ) ),
     :                              VLBND, VUBND,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              STATUS )
                  CALL KPG1_LOCTUB( NDIMS, VLBND, VUBND,
     :                              %VAL( CNF_PVAL( IPW1 ) ),
     :                              PFINAL, SEARCH,
     :                              POSTVE, MXSHFT, MXITER, TOLER,
     :                              SEL, EFINAL,
     :                              %VAL( CNF_PVAL( IPW2 ) ),
     :                              STATUS )

               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  CALL MSG_SETC( 'T', ITYPE )
                  CALL ERR_REP( 'KPS1_CENTRD_ERR4', 'KPS1_CENSG: '//
     :                          'this application does not yet '//
     :                          'support the ^T data type.', STATUS )
                  GO TO 999
               END IF

*  If the simulated position was not found, annul the error so that the
*  remaining positions can be processed, and store a bad simulated
*  position.
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  DO J = 1, NDIMS
                     PIXSIM( I, J ) = AST__BAD
                  END DO

*  If the simulated position was found, store it in the work array.
               ELSE
                  DO J = 1, NDIMS
                     IF( EFINAL( J ) .NE. VAL__BADR ) THEN
                        PIXSIM( I, J ) = DBLE( EFINAL( J ) )
                     ELSE
                        PIXSIM( I, J ) = AST__BAD
                     END IF
                  END DO

               END IF

            END DO

*  Transform all the simulated positions from pixel co-ordinates into the
*  reporting Frame. We do them all in a single batch to minimise
*  the time spent transforming points.
            CALL AST_TRANN( MAP2, NSIM, NDIMS, NSIM, PIXSIM, .TRUE.,
     :                      NAXR, NSIM, REPSIM, STATUS )

*  Initialise the sums of the squared residuals on each axis.
            DO J = 1, NAXR
               SUM( J ) = 0.0D0
               NVAL( J ) = 0
            END DO

*  Loop round all the simulated positions.
            DO I = 1, NSIM

*  Do each reporting Frame axis.
               DO J = 1, NAXR

*  Check the simulated and centroid positions are good in the reporting Frame.
                  IF( REPSIM( I, J ) .NE. AST__BAD .AND.
     :                REPPOS( J ) .NE. AST__BAD ) THEN

*  If so, increment the running sums for this axis.
                     NVAL( J ) = NVAL( J ) + 1
                     SUM( J ) = SUM( J ) + ( REPSIM( I, J ) -
     :                                       REPPOS( J ) )**2
                  END IF

               END DO

            END DO

*  Now find the error on each axis.
            DO J = 1, NAXR

*  We can only calculate errors is we have at least three positions.
               IF( NVAL( J ) .GE. 3 ) THEN

*  Calculate the RMS error on this axis.
                  ERROR( J ) = SQRT( SUM( J ) /
     :                               DBLE( NVAL( J ) - 1 ) )

*  Store bad values if there were insufficient simulated positions.
               ELSE
                  ERROR( J ) = AST__BAD
               END IF

            END DO

         END IF

*  Now report the results.
*  ======================
         IF( OK ) THEN

*  Mark the centroid position on the screen if required.
            IF( CURSOR .AND. CMARK .GT. -32 ) THEN

*  First transform it from pixel to graphics co-ordinates.
               CALL AST_TRANN( MAP1, 1, NDIMS, 1, PIXPOS, .FALSE.,
     :                         2, 1, INCEN, STATUS )

*  Replace any bad graphics axis values with the original values.
               IF( INCEN( 1 ) .EQ. AST__BAD ) INCEN( 1 ) = INPOS( 1 )
               IF( INCEN( 2 ) .EQ. AST__BAD ) INCEN( 2 ) = INPOS( 2 )

*  Mark it.
               IF( INCEN( 1 ) .NE. AST__BAD .AND.
     :             INCEN( 2 ) .NE. AST__BAD ) THEN
                  CALL PGPT( 1, REAL( INCEN( 1 ) ), REAL( INCEN( 2 ) ),
     :                       CMARK )
               END IF

            END IF

*  If the user wants to see the original guess position, transform it to
*  the reporting Frame.
            IF( GUESS ) CALL AST_TRANN( MAP3, 1, NAXIN, 1, INPOS,
     :                                  .TRUE., NAXR, 1, REPWAS,
     :                                  STATUS )

*  If not already done so, display the header.
            IF( NPOS .EQ. 0 ) CALL KPS1_CENHD( RFRM, LOGPOS, FDL, QUIET,
     :                                         NAXR, TITLE, STATUS )

*  Increment the number of positions displayed.
            NPOS = NPOS + 1

*  Display this position.
            CALL KPS1_CENSH( CERROR, RFRM, 1, NPOS, LOGPOS, FDL, QUIET,
     :                       NDIMS, NAXR, GUESS, OUTCO, FDO, 1, ERROR,
     :                       PIXPOS, REPPOS, REPWAS, 1, STATUS )

*  Copy the centroid position to the output array, unless the array is
*  already full.
            IF( NPOS .LE. MXPOS ) THEN
               DO J = 1, NAXR
                  OUT( NPOS, J ) = REPPOS( J )
               END DO
            END IF


         ELSE IF( .NOT. QUIET ) THEN
            CALL MSG_OUT( 'KPS1_CENSG_MSG5', 'No centroid found.',
     :                    STATUS )
         END IF

*  If only a single position is to be processed, set MORE so that
*  the loop is left.
         IF( SINGLE ) THEN
            MORE = .FALSE.

*  Otherwise, cancel the parameter value if we are not in cursor mode.
         ELSE IF( .NOT. CURSOR ) THEN
            CALL PAR_CANCL( PARAM, STATUS )

         END IF

      END DO

*  Arrive here once all positions have been processed.
 10   CONTINUE

*  A final blank line.
      IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )
      IF( LOGPOS ) CALL FIO_WRITE( FDL, ' ', STATUS )

*  Limit the number of returned positions to the maximum allowed.
      IF( NPOS .GT. MXPOS ) NPOS = MXPOS

*  Now write the last position out to the output parameters.
*  The formatted axis value for axis 1 is written to XCEN, and
*  the formatted axis value for axis 2 is written to YCEN. The
*  complete set of axis values (separated by spaces) is written to
*  CENTRE.
      IAT = 0
      LINE = ' '

      DO J = 1, NAXR
         AXVAL = AST_FORMAT( RFRM, J, REPPOS( J ), STATUS )

         IF( J .EQ. 1 ) THEN
            CALL PAR_PUT0C( 'XCEN', AXVAL( : CHR_LEN( AXVAL ) ),
     :                      STATUS )

         ELSE IF( J .EQ. 2 ) THEN
            CALL PAR_PUT0C( 'YCEN', AXVAL( : CHR_LEN( AXVAL ) ),
     :                      STATUS )

         END IF

         CALL CHR_APPND( AXVAL, LINE, IAT )
         IAT = IAT + 1

      END DO

      CALL PAR_PUT0C( 'CENTRE', LINE( : IAT ), STATUS )

*  Likewise, write the errors for the last position out to the environment.
      IF( CERROR ) THEN
         IAT = 0
         LINE = ' '

         DO J = 1, NAXR
            AXVAL = AST_FORMAT( RFRM, J, ERROR( J ), STATUS )

            IF( J .EQ. 1 ) THEN
               CALL PAR_PUT0C( 'XERR', AXVAL( : CHR_LEN( AXVAL ) ),
     :                         STATUS )

            ELSE IF( J .EQ. 2 ) THEN
               CALL PAR_PUT0C( 'YERR', AXVAL( : CHR_LEN( AXVAL ) ),
     :                         STATUS )

            END IF

            CALL CHR_APPND( AXVAL, LINE, IAT )
            IAT = IAT + 1

         END DO

         CALL PAR_PUT0C( 'ERROR', LINE( : IAT ), STATUS )

      END IF

*  Tidy up.
*  =======
 999  CONTINUE

*  Free any workspace.
      IF( CERROR ) CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

      END
