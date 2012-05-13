      SUBROUTINE KPS1_CENBT( INDF, CERROR, MAP1, MAP2, MAP3, RFRM, NPOS,
     :                       NAXR, NAXIN, INPOS, GOTID, ID, LOGPOS, FDL,
     :                       QUIET, NSIM, NDIMS, SLBND, SUBND, SEARCH,
     :                       POSTVE, GUESS,MXSHFT, MXITER, OUTCO, FDO,
     :                       TOLER, TITLE, NSIMW, ERROR, PIXPOS, REPPOS,
     :                       PIXSIM, REPSIM, STATUS )
*+
*  Name:
*     KPS1_CENBT

*  Purpose:
*     Find the centroids of a batch of image features.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CENBT( INDF, CERROR, MAP1, MAP2, MAP3, RFRM, NPOS,
*                      NAXR, NAXIN, INPOS, GOTID, ID, LOGPOS, FDL, QUIET,
*                      NSIM, NDIMS, SLBND, SUBND, SEARCH, POSTVE,
*                      GUESS, MXSHFT, MXITER, OUTCO, FDO, TOLER, TITLE,
*                      NSIMW, ERROR, PIXPOS, REPPOS, PIXSIM, REPSIM,
*                      STATUS )

*  Description:
*     This routine finds the accurate centroids of a batch of image
*     features given initial guesses at their positions. It can also
*     estimate the error on these positions, and display the results
*     to a log file and the screen.
*
*     All the centroids are found before any positions are reported.
*     This enables the required AST transformations to be applied to
*     all positions in a single call, minimising the time spent in
*     the mapping routines. This routine should be used in
*     non-interactive modes such as "File" or "Catalogue".

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
*        A pointer to the reporting Frame (i.e. the Frame in which
*        positions are to be reported).
*     NPOS = INTEGER (Given)
*        The number of centroid positions to be found.
*     NAXR = INTEGER (Given)
*        The number of axes in the reporting Frame.
*     NAXIN = INTEGER (Given)
*        The number of axes in the Frame in which the initial guess
*        positions are supplied.
*     INPOS( NPOS, NAXIN ) = DOUBLE PRECISION (Given)
*        The initial guesses at the centroid positions. These should be
*        in the co-ordinate system defined by MAP1 and MAP3.
*     GOTID = LOGICAL (Given)
*        If TRUE then the position identifiers supplied in ID are used.
*        Otherwise identifiers equal to the position index are used.
*     ID( NPOS ) = INTEGER (Given)
*        A set of integer identifiers for the supplied positions. These
*        are displayed with the centroid positions. Only accessed if
*        GOTID is .TRUE.
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
*        errors on the centroid positions.
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
*     NSIMW = INTEGER (Given)
*        The first dimension of the PIXSIM and REPSIM work arrays. This
*        should be at least equal to NSIM*NPOS.
*     ERROR( NPOS, NAXR ) = DOUBLE PRECISION (Returned)
*        The errors on the centroid positions, given in the reporting
*        Frame.
*     PIXPOS( NPOS, NDIMS ) = DOUBLE PRECISION (Returned)
*        The centroid positions, given in the PIXEL Frame of the NDF.
*     REPPOS( NPOS, NAXR ) = DOUBLE PRECISION (Returned)
*        The centroid positions, given in the reporting Frame.
*     PIXSIM( NSIMW, NDIMS ) = DOUBLE PRECISION (Returned)
*        A work array to hold the PIXEL Frame co-ordinates at each
*        simulated centroid position.
*     REPSIM( NSIMW, NAXR ) = DOUBLE PRECISION (Returned)
*        A work array to hold the reporting Frame co-ordinates at each
*        simulated centroid position.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999, 2001, 2004 Central Laboratory of the Research
*     Councils.
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
*     4-DEC-2001 (DSB):
*        Retain error messages if only a single position is being
*        centroided.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      LOGICAL CERROR
      INTEGER MAP1
      INTEGER MAP2
      INTEGER MAP3
      INTEGER RFRM
      INTEGER NPOS
      INTEGER NAXIN
      INTEGER NAXR
      DOUBLE PRECISION INPOS( NPOS, NAXIN )
      LOGICAL GOTID
      INTEGER ID( NPOS )
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
      INTEGER NSIMW

*  Arguments Returned:
      DOUBLE PRECISION ERROR( NPOS, NAXR )
      DOUBLE PRECISION PIXPOS( NPOS, NDIMS )
      DOUBLE PRECISION REPPOS( NPOS, NAXR )
      DOUBLE PRECISION PIXSIM( NSIMW, NDIMS )
      DOUBLE PRECISION REPSIM( NSIMW, NAXR )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER AXVAL*50         ! A formatted axis value
      CHARACTER DTYPE*(NDF__SZFTP)! Data type for output components
      CHARACTER ITYPE*(NDF__SZTYP)! Data type for processing
      CHARACTER LINE*128         ! Buffer for output text
      DOUBLE PRECISION SUM( NDF__MXDIM )! Sums of squared axis residuals
      INTEGER EL                 ! Number of mapped elements
      INTEGER I                  ! Position index
      INTEGER IAT                ! Number of characters currently in buffer
      INTEGER IDENT              ! Position identifier
      INTEGER IPDIN              ! Pointer to mapped data array
      INTEGER IPVIN              ! Pointer to mapped variance array
      INTEGER IPW1               ! Pointer to work array
      INTEGER IPW2               ! Pointer to work array
      INTEGER ISIM               ! Index of next simulated position
      INTEGER J                  ! Axis index
      INTEGER K                  ! Simulation index
      INTEGER NVAL( NDF__MXDIM ) ! Number of values summed in SUM
      INTEGER SEL                ! Size of the IPW1 work array
      INTEGER VLBND( NDF__MXDIM )! Signif. bounds of area used for variance estimation
      INTEGER VUBND( NDF__MXDIM )! Signif. bounds of area used for variance estimation
      INTEGER WDIM( NDF__MXDIM ) ! Dimensions of area used for variance
      LOGICAL OK                 ! Is this position OK?
      LOGICAL VERB               ! Flush errors instead of annulling them?
      REAL EFINAL( NDF__MXDIM )  ! Pixel coords at simulated centroid position
      REAL INIT( NDF__MXDIM )    ! Pixel coords at initial guess position
      REAL PFINAL( NDF__MXDIM )  ! Pixel coords at genuine centroid position
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied Mappings have the required transformations.
      IF( .NOT. AST_GETL( MAP1, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CENBT_ERR1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'pixel Frame of the NDF is not defined.',
     :                 STATUS )

      ELSE IF( .NOT. AST_GETL( MAP2, 'TRANFORWARD', STATUS ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_CENBT_ERR2','The Mapping required '//
     :                 'to map pixel positions into the Current '//
     :                 'co-ordinate Frame is not defined.', STATUS )

      ELSE IF( GUESS .AND.
     :         .NOT. AST_GETL( MAP3, 'TRANFORWARD', STATUS ) ) THEN
         CALL MSG_OUT( 'KPS1_CENBT_MSG1','The Mapping required '//
     :                 'to map the supplied positions into the '//
     :                 'Current co-ordinate Frame is not defined.',
     :                 STATUS )
         CALL MSG_OUT( 'KPS1_CENBT_MSG2','The supplied positions '//
     :                 'will not be reported!!', STATUS )
         GUESS = .FALSE.

      END IF

*  See if we are running in verbose mode.
      CALL KPG1_VERB( VERB, 'KAPPA', STATUS )

*  Find number of elements in each search box.
      SEL = 1
      DO  I = 1, NDIMS
         WDIM( I ) = 2*MXSHFT( I ) + 2*( SEARCH( I )/2 ) + 2
         SEL = SEL * WDIM( I )
      END DO

*  Get workspace for sorting pixel values in each box.
      CALL PSX_CALLOC( SEL * 51 * NDIMS, '_REAL', IPW2, STATUS )

*  Transform the supplied positions to the PIXEL Frame of the NDF.
      CALL AST_TRANN( MAP1, NPOS, NAXIN, NPOS, INPOS, .TRUE., NDIMS,
     :                NPOS, PIXPOS, STATUS )

*  Choose the data type in which to process the data.
      CALL NDF_MTYPE( '_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,'//
     :                '_REAL,_DOUBLE', INDF, INDF, 'Data', ITYPE, DTYPE,
     :                STATUS )

*  Map the data array from the NDF.
      CALL NDF_MAP( INDF, 'Data', ITYPE, 'READ', IPDIN, EL, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the centroid positions.
*  ============================

*  Loop to find the centroid positions (in the PIXEL Frame).
      DO I = 1, NPOS

*  Copy the initial PIXEL position into a single precision array,
*  checking for bad axis values.
         OK = .TRUE.
         DO J = 1, NDIMS
            IF( PIXPOS( I, J ) .NE. AST__BAD ) THEN
               INIT( J ) = REAL( PIXPOS( I, J ) )
            ELSE
               INIT( J ) = VAL__BADR
               OK = .FALSE.
            END IF
         END DO

*  If any axis has a bad value in the initial position, add a bad position
*  to the array of centroid positions.
         IF( .NOT. OK ) THEN
            DO J = 1, NDIMS
               PIXPOS( I, J ) = AST__BAD
            END DO

*  If the initial position is good, find the centroid and add it to the
*  array of centroid positions.
         ELSE

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
               CALL ERR_REP( 'KPS1_CENBT_ERR3', 'KPS1_CENBT: This '//
     :                       'application does not yet support the '//
     :                       '^T data type.', STATUS )
               GO TO 999
            END IF

*  If the position could not be found...
            IF( STATUS .NE. SAI__OK ) THEN

*  If we are processing more than one position, cancel the error so that
*  remaining positions can be processed. For a single position, we retian
*  the error status.
               IF( NPOS .GT. 1 ) THEN

*  If we are in verbose mode, add a context report and flush the message.
*  Otherwise, just annul the message.
                  IF( VERB ) THEN
                     IF( GOTID ) THEN
                        CALL MSG_SETI( 'ID', ID( I ) )
                     ELSE
                        CALL MSG_SETI( 'ID', I )
                     END IF
                     CALL ERR_REP( 'KPS1_CENBT_ERR4', 'No centroid '//
     :                             'found for position ^ID.', STATUS )
                     CALL ERR_FLUSH( STATUS )
                  ELSE
                     CALL ERR_ANNUL( STATUS )
                  END IF

               END IF

*  Store a bad position.
               DO J = 1, NDIMS
                  PIXPOS( I, J ) = AST__BAD
               END DO

*  If a good centroid was found, add it to the list of centroid positions.
            ELSE
               DO J = 1, NDIMS
                  IF( PFINAL( J ) .NE. VAL__BADR ) THEN
                     PIXPOS( I, J ) = DBLE( PFINAL( J ) )
                  ELSE
                     PIXPOS( I, J ) = AST__BAD
                  END IF
               END DO

            END IF

         END IF

      END DO

*  We now have an array holding all the centroid positions in pixel
*  co-ordinates. Transform them to the reporting Frame.
      CALL AST_TRANN( MAP2, NPOS, NDIMS, NPOS, PIXPOS, .TRUE., NAXR,
     :                NPOS, REPPOS, STATUS )

*  Now estimate the error on these centroid positions if required.
*  ===============================================================
      IF( CERROR ) THEN

*  Get workspace for the simulations.
         CALL PSX_CALLOC( SEL, ITYPE, IPW1, STATUS )

*  Map the variance array from the NDF.
         CALL NDF_MAP( INDF, 'Variance', ITYPE, 'READ', IPVIN, EL,
     :                 STATUS )

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Initialise the index of the next simulated position to be stored in
*  the work arrays.
         ISIM = 1

*  Loop round every centroid position found above.
         DO I = 1, NPOS

*  Copy the pixel co-ordinates of the centroid to a single precision
*  array, checking for bad values.
            OK = .TRUE.
            DO J = 1, NDIMS
               IF( PIXPOS( I, J ) .NE. AST__BAD ) THEN
                  PFINAL( J ) = REAL( PIXPOS( I, J ) )
               ELSE
                  PFINAL( J ) = VAL__BADR
                  OK = .FALSE.
               END IF
            END DO

*  If any pixel axis has a bad value at the centroid position, ignore
*  the position.
            IF( OK ) THEN

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
               DO  K = 1, NSIM

*  Call appropriate routines depending on the data type.
                  IF( ITYPE .EQ. '_INTEGER' ) THEN

*  Copy the required area from the data array to the workspace, and
*  add gaussian noise.
                     CALL KPS1_CENAI( NDIMS, SLBND, SUBND,
     :                                %VAL( CNF_PVAL( IPDIN ) ),
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                STATUS )

*  Find the new centroid after the noise has been added. The position is
*  returned in pixel co-ordinates.
                     CALL KPG1_LOCTI( NDIMS, VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                PFINAL, SEARCH, POSTVE, MXSHFT,
     :                                MXITER, TOLER, SEL, EFINAL,
     :                                %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Now do the same for the other data types.
                  ELSE IF( ITYPE .EQ. '_REAL' ) THEN
                     CALL KPS1_CENAR( NDIMS, SLBND, SUBND,
     :                                %VAL( CNF_PVAL( IPDIN ) ),
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                STATUS )
                     CALL KPG1_LOCTR( NDIMS, VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                PFINAL, SEARCH, POSTVE, MXSHFT,
     :                                MXITER, TOLER, SEL, EFINAL,
     :                                %VAL( CNF_PVAL( IPW2 ) ), STATUS )

                  ELSE IF( ITYPE .EQ. '_DOUBLE' ) THEN
                     CALL KPS1_CENAD( NDIMS, SLBND, SUBND,
     :                                %VAL( CNF_PVAL( IPDIN ) ),
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                STATUS )
                     CALL KPG1_LOCTD( NDIMS, VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                PFINAL, SEARCH, POSTVE, MXSHFT,
     :                                MXITER, TOLER, SEL, EFINAL,
     :                                %VAL( CNF_PVAL( IPW2 ) ), STATUS )

                  ELSE IF( ITYPE .EQ. '_INT64' ) THEN
                     CALL KPS1_CENAK( NDIMS, SLBND, SUBND,
     :                                %VAL( CNF_PVAL( IPDIN ) ),
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                STATUS )
                     CALL KPG1_LOCTK( NDIMS, VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                PFINAL, SEARCH,
     :                                POSTVE, MXSHFT, MXITER, TOLER,
     :                                SEL, EFINAL,
     :                                %VAL( CNF_PVAL( IPW2 ) ),
     :                                STATUS )

                  ELSE IF( ITYPE .EQ. '_WORD' ) THEN
                     CALL KPS1_CENAW( NDIMS, SLBND, SUBND,
     :                                %VAL( CNF_PVAL( IPDIN ) ),
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                STATUS )
                     CALL KPG1_LOCTW( NDIMS, VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                PFINAL, SEARCH, POSTVE, MXSHFT,
     :                                MXITER, TOLER, SEL, EFINAL,
     :                                %VAL( CNF_PVAL( IPW2 ) ), STATUS )

                  ELSE IF( ITYPE .EQ. '_UWORD' ) THEN
                     CALL KPS1_CENAUW( NDIMS, SLBND, SUBND,
     :                                 %VAL( CNF_PVAL( IPDIN ) ),
     :                                 %VAL( CNF_PVAL( IPVIN ) ),
     :                                 VLBND, VUBND,
     :                                 %VAL( CNF_PVAL( IPW1 ) ),
     :                                 STATUS )
                     CALL KPG1_LOCTUW( NDIMS, VLBND, VUBND,
     :                                 %VAL( CNF_PVAL( IPW1 ) ),
     :                                 PFINAL, SEARCH,
     :                                 POSTVE, MXSHFT, MXITER, TOLER,
     :                                 SEL, EFINAL,
     :                                 %VAL( CNF_PVAL( IPW2 ) ),
     :                                 STATUS )

                  ELSE IF( ITYPE .EQ. '_BYTE' ) THEN
                     CALL KPS1_CENAB( NDIMS, SLBND, SUBND,
     :                                %VAL( CNF_PVAL( IPDIN ) ),
     :                                %VAL( CNF_PVAL( IPVIN ) ),
     :                                VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                STATUS )
                     CALL KPG1_LOCTB( NDIMS, VLBND, VUBND,
     :                                %VAL( CNF_PVAL( IPW1 ) ),
     :                                PFINAL, SEARCH, POSTVE, MXSHFT,
     :                                MXITER, TOLER, SEL, EFINAL,
     :                                %VAL( CNF_PVAL( IPW2 ) ), STATUS )

                  ELSE IF( ITYPE .EQ. '_UBYTE' ) THEN
                     CALL KPS1_CENAUB( NDIMS, SLBND, SUBND,
     :                                 %VAL( CNF_PVAL( IPDIN ) ),
     :                                 %VAL( CNF_PVAL( IPVIN ) ),
     :                                 VLBND, VUBND,
     :                                 %VAL( CNF_PVAL( IPW1 ) ),
     :                                 STATUS )
                     CALL KPG1_LOCTUB( NDIMS, VLBND, VUBND,
     :                                 %VAL( CNF_PVAL( IPW1 ) ),
     :                                 PFINAL, SEARCH,
     :                                 POSTVE, MXSHFT, MXITER, TOLER,
     :                                 SEL, EFINAL,
     :                                 %VAL( CNF_PVAL( IPW2 ) ),
     :                                 STATUS )

                  ELSE IF( STATUS .EQ. SAI__OK ) THEN
                     CALL MSG_SETC( 'T', ITYPE )
                     CALL ERR_REP( 'KPS1_CENTRD_ERR5', 'KPS1_CENBT: '//
     :                             'this application does not yet '//
     :                             'support the ^T data type.', STATUS )
                     GO TO 999
                  END IF

*  If the simulated position was not found, annul the error so that the
*  remaining positions can be processed, and store a bad simulated position
*  in the work array.
                  IF( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     DO J = 1, NDIMS
                        PIXSIM( ISIM, J ) = AST__BAD
                     END DO

*  If the simulated position was found, store it in the work array.
                  ELSE
                     DO J = 1, NDIMS
                        IF( EFINAL( J ) .NE. VAL__BADR ) THEN
                           PIXSIM( ISIM, J ) = DBLE( EFINAL( J ) )
                        ELSE
                           PIXSIM( ISIM, J ) = AST__BAD
                        END IF
                     END DO

                  END IF

*  Increment the index of the next simulated position to be stored in
*  PIXSIM.
                  ISIM = ISIM + 1

               END DO

            END IF

         END DO

*  Transform all the simulated positions from pixel co-ordinats into the
*  reporting Frame. We do them all in a single batch to minimise
*  the time spent transforming points.
         CALL AST_TRANN( MAP2, ISIM - 1, NDIMS, NSIMW, PIXSIM, .TRUE.,
     :                   NAXR, NSIMW, REPSIM, STATUS )


*  Initialise the index of the next simulated position to be read from
*  the work arrays.
         ISIM = 1

*  Loop round every centroid position.
         DO I = 1, NPOS

*  No simulated positions will have been stored for this position if any
*  of the pixel co-ordinates at this position are bad. See if this is the
*  case.
            OK = .TRUE.
            DO J = 1, NDIMS
               IF( PIXPOS( I, J ) .EQ. AST__BAD ) OK = .FALSE.
            END DO

*  If any axis has a bad pixel value at the centroid position, add bad values
*  to the array of errors.
            IF( .NOT. OK ) THEN
               DO J = 1, NAXR
                  ERROR( I, J ) = AST__BAD
               END DO

*  If the centroid position is good, find the errors on the centroid
*  position and add them to the array of errors.
            ELSE

*  Initialise the sums of the squared residuals on each axis.
               DO J = 1, NAXR
                  SUM( J ) = 0.0D0
                  NVAL( J ) = 0
               END DO

*  Loop round all the simulated positions for this centroid.
               DO K = 1, NSIM

*  Do each reporting Frame axis.
                  DO J = 1, NAXR

*  Check the simulated and centroid positions are good in the reporting
*  Frame.
                     IF( REPSIM( ISIM, J ) .NE. AST__BAD .AND.
     :                   REPPOS( I, J ) .NE. AST__BAD ) THEN

*  If so, increment the running sums for this axis.
                        NVAL( J ) = NVAL( J ) + 1
                        SUM( J ) = SUM( J ) + ( REPSIM( ISIM, J ) -
     :                                          REPPOS( I, J ) )**2
                     END IF

                  END DO

*  Increment the index of the next simulated positions to be read from
*  REPSIM.
                  ISIM = ISIM + 1

               END DO

*  Now find the error on each axis.
               DO J = 1, NAXR

*  We can only calculate errors is we have at least three positions.
                  IF( NVAL( J ) .GE. 3 ) THEN

*  Calculate the RMS error on this axis.
                     ERROR( I, J ) = SQRT( SUM( J ) /
     :                                     DBLE( NVAL( J ) - 1 ) )

*  Store bad values if there were insufficient simulated positions.
                  ELSE
                     ERROR( I, J ) = AST__BAD
                  END IF

               END DO

            END IF

         END DO

      END IF

*  Now report the results.
*  ======================

*  If the user wants to see the original guess positions, transform them to
*  the reporting Frame of the NDF. We no longer neeed the contents of the
*  REPSIM array, so we can put the results in REPSIM.
      IF( GUESS ) THEN
         CALL AST_TRANN( MAP3, NPOS, NAXIN, NPOS, INPOS, .TRUE.,
     :                   NAXR, NSIMW, REPSIM, STATUS )
      END IF

*  Display the header.
      CALL KPS1_CENHD( RFRM, LOGPOS, FDL, QUIET, NAXR, TITLE, STATUS )

*  Loop round displaying each position.
      DO I = 1, NPOS
         IF( GOTID ) THEN
            IDENT = ID( I )
         ELSE
            IDENT = I
         END IF
         CALL KPS1_CENSH( CERROR, RFRM, NPOS, IDENT, LOGPOS, FDL,
     :                    QUIET, NDIMS, NAXR, GUESS, OUTCO, FDO,
     :                    NSIMW, ERROR, PIXPOS, REPPOS, REPSIM, I,
     :                    STATUS )
      END DO

*  A final blank line.
      IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )
      IF( LOGPOS ) CALL FIO_WRITE( FDL, ' ', STATUS )

*  Now write the last position out to the output parameters.
*  The formatted axis value for axis 1 is written to XCEN, and
*  the formatted axis value for axis 2 is written to YCEN. The
*  complete set of axis values (separated by spaces) is written to
*  CENTRE.
      IAT = 0
      LINE = ' '

      DO J = 1, NAXR
         AXVAL = AST_FORMAT( RFRM, J, REPPOS( NPOS, J ), STATUS )

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

*  Likewise, write the errors for th elast position out to the environment.
      IF( CERROR ) THEN
         IAT = 0
         LINE = ' '

         DO J = 1, NAXR
            AXVAL = AST_FORMAT( RFRM, J, ERROR( NPOS, J ), STATUS )

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
