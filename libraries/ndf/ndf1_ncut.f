      SUBROUTINE NDF1_NCUT( IACB1, STR, IACB2, STATUS )
*+
*  Name:
*     NDF1_NCUT

*  Purpose:
*     Cut a section specified by a character string from an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_NCUT( IACB1, STR, IACB2, STATUS )

*  Description:
*     The routine creates a section from an NDF, generating a new ACB
*     entry describing the new NDF.  The dimension bounds defining the
*     section are supplied as a parenthesised character string via the
*     STR argument (e.g.  '(256,256)', '(,,~3)', '(3.5:5,8:)' or
*     '(,7.0~100,,:6)', etc.  If this string is blank, then the routine
*     returns an ACB entry describing the original NDF by cloning the
*     ACB entry given.

*  Arguments:
*     IACB1 = INTEGER (Given)
*        Index to the input ACB entry.
*     STR = CHARACTER * ( * ) (Given)
*        Section bounds expression.
*     IACB2 = INTEGER (Returned)
*        Index to the output ACB entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The syntax of the STR string will be fully validated by this
*     routine. It must contain enclosing parentheses unless it is
*     completely blank.
*     -  If this routine is called with STATUS set, then a value of
*     zero will be returned for IACB2. The same value will also be
*     returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S Berry (JACH, UCLan)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1991 (RFWS):
*        Original version.
*     12-MAR-1991 (RFWS):
*        Installed call to NDF1_AXLIM to enable axis coordinate values
*        to appear in section specifications.
*     13-MAR-1991 (RFWS):
*        Added contextual error reports.
*     22-OCT-1991 (RFWS):
*        Removed unused variable.
*     21-MAY-2007 (DSB):
*        Add support for sections given in terms of WCS coords.
*     5-JUN-2007 (DSB):
*        - Interpret non-integers as AXIS only if there is no explicit WCS.
*        - New interface for ndf1_wclim.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locators to axis structure elements.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB1
      CHARACTER * ( * ) STR

*  Arguments Returned:
      INTEGER IACB2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DFLBND( NDF__MXDIM ) ! Default lower bounds
      DOUBLE PRECISION DFUBND( NDF__MXDIM ) ! Default upper bounds
      DOUBLE PRECISION GLBND( NDF__MXDIM )  ! Lower GRID bound
      DOUBLE PRECISION GUBND( NDF__MXDIM )  ! Upper GRID bound
      DOUBLE PRECISION VALUE1( NDF__MXDIM ) ! First bound specifier
      DOUBLE PRECISION VALUE2( NDF__MXDIM ) ! Second bound specifier
      DOUBLE PRECISION XL( NDF__MXDIM ) ! GRID coords at min WCS axis value
      DOUBLE PRECISION XU( NDF__MXDIM ) ! GRID coords at max WCS axis value
      INTEGER F                  ! First non-blank character position
      INTEGER FRAME1( NDF__MXDIM )! Frame used by VALUE1
      INTEGER FRAME2( NDF__MXDIM )! Frame used by VALUE2
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IWCS               ! AST pointer to WCS FrameSet
      INTEGER L                  ! Last non-blank character position
      INTEGER LBND( DAT__MXDIM ) ! Lower dimension bounds
      INTEGER LBNDD( NDF__MXDIM )! NDF lower pixel bounds
      INTEGER NAX                ! No. of axes in chosen coord system
      INTEGER NDIM               ! Number of section dimensions
      INTEGER NDIMD              ! Input NDF number of dimensions
      INTEGER UBND( DAT__MXDIM ) ! Upper dimension bounds
      INTEGER UBNDD( NDF__MXDIM )! NDF upper pixel bounds
      INTEGER WCSMAP             ! AST pointer to WCS Mapping
      LOGICAL ISBND( NDF__MXDIM )! Are VALUEs explicit bounds?
      LOGICAL ISDEF1( NDF__MXDIM )! Is VALUE1 a defalut value?
      LOGICAL ISDEF2( NDF__MXDIM )! Is VALUE2 a defalut value?
      LOGICAL WCSSEC             ! Use WCS section syntax?
      LOGICAL USEWCS             ! Use WCS instead of AXIS?
*.

*  Initialise the returned ACB index.
      IACB2 = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the dimension bounds
*  string.
      CALL CHR_FANDL( STR, F, L )

*  If the string is blank, then simply clone the ACB entry.
      IF ( F .GT. L ) THEN
         CALL NDF1_CLN( IACB1, IACB2, STATUS )

*  Otherwise, check that the string has enclosing parentheses and report
*  an error if it does not.
      ELSE IF ( ( STR( F : F ) .NE. '(' ) .OR.
     :          ( STR( L : L ) .NE. ')' ) ) THEN
         STATUS = NDF__BNDIN
         CALL MSG_SETC( 'SECTION', STR( F : L ) )
         CALL NDF1_AMSG( 'NDF', IACB1 )
         CALL ERR_REP( 'NDF1_NCUT_BND1',
     :                 'Invalid section ''^SECTION'' specified for ' //
     :                 'the NDF ^NDF -- enclosing parenthesis ' //
     :                 'missing.', STATUS )

*  Otherwise, decide if the section is given in terms of pixel/axis
*  coordinates or WCS coordinates.
      ELSE

*  If the opening parenthesis is followed by an asterisk  "*" the section
*  refers purely to WCS coords (in which case step over the asterisk).
*  Otherwise, it may contain a mix of axis/wcs coords and pixel indices.
         IF( STR( F + 1 : F + 1 ) .EQ. '*' ) THEN
            WCSSEC = .TRUE.
            F = F + 1
         ELSE
            WCSSEC = .FALSE.
         END IF

*  Get the pixel bounds of the NDF.
         CALL ARY_BOUND( ACB_DID( IACB1 ), NDF__MXDIM, LBNDD, UBNDD,
     :                   NDIMD, STATUS )

*  Get the WCS FrameSet.
         CALL NDF1_RDWCS( IACB1, IWCS, STATUS )

*  If we are using WCS syntax, the default bounds are the WCS values at
*  the edges of the bounding box containing the whole NDF.
         IF( WCSSEC ) THEN

*  Store the bounds of the NDF in double precision GRID coords.
            DO I = 1, NDIMD
               GLBND( I ) = 0.5D0
               GUBND( I ) = DBLE( UBNDD( I ) - LBNDD( I ) ) + 1.5D0
            END DO

*  For efficiency, extract the Mapping from the WCS FrameSet and pass it
*  to AST_MAPBOX.
            WCSMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT,
     :                               STATUS )

*  Get the corresponding bounds on each WCS axis. These are used as the
*  defaults for any bounds that are not specified in the supplied string.
            NAX = AST_GETI( IWCS, 'Nout', STATUS )
            DO I = 1, NAX
               CALL AST_MAPBOX( WCSMAP, GLBND, GUBND, .TRUE., I,
     :                          DFLBND( I ), DFUBND( I ), XL, XU,
     :                          STATUS )
            END DO

*  Annul the WCS Mapping
            CALL AST_ANNUL( WCSMAP, STATUS )

*  If we are not using WCS syntax, the default bounds are the pixel
*  indices at the edges of the NDF.
         ELSE
            NAX = NDIMD
            DO I = 1, NAX
               DFLBND( I ) = DBLE( LBNDD( I ) )
               DFUBND( I ) = DBLE( UBNDD( I ) )
            END DO

*  If the current WCS Frame is the AXIS Frame, we interpret non-integer
*  values using the AXIS-based code written by RFWS. Otherwise we use new
*  WCS-based code.
            USEWCS = ( AST_GETC( IWCS, 'Domain', STATUS ) .NE. 'AXIS' )

         END IF

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Remove the enclosing parentheses (supply a blank bounds expression
*  if '()' was specified) and parse the dimension bounds expression.
            IF ( .NOT. WCSSEC .AND. STR( F : L ) .EQ. '()' .OR.
     :           WCSSEC .AND. STR( F : L ) .EQ. '*)' ) THEN
               CALL NDF1_PSNDE( ' ', NAX, DFLBND, DFUBND, IWCS,
     :                          WCSSEC, VALUE1, VALUE2, NDIM,
     :                          FRAME1, FRAME2, ISBND, ISDEF1, ISDEF2,
     :                          STATUS )
            ELSE
               CALL NDF1_PSNDE( STR( F + 1 : L - 1 ),
     :                          NAX, DFLBND, DFUBND, IWCS,
     :                          WCSSEC, VALUE1, VALUE2, NDIM,
     :                          FRAME1, FRAME2, ISBND, ISDEF1, ISDEF2,
     :                          STATUS )
            END IF

*  If an error occurs, then report contextual information.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL NDF1_AMSG( 'NDF', IACB1 )
               CALL ERR_REP( 'NDF1_NCUT_BND2',
     :                       'Unable to select the specified ' //
     :                       'section of the NDF ^NDF', STATUS )

*  Otherwise, calculate the actual lower and upper bounds of each
*  dimension in pixels.
            ELSE

*  Convert any FRACTION values into corresponding pixel indices.
               CALL NDF1_FR2PX( NDIM, NDIMD, LBNDD, UBNDD, ISBND,
     :                          VALUE1, VALUE2, FRAME1, FRAME2, STATUS )

*  If we are using WCS syntax, we do all axes together,
               IF( WCSSEC ) THEN
                  CALL NDF1_WCLIM( IWCS, NDIM, NDIMD, LBNDD, UBNDD,
     :                             ISDEF1, ISDEF2, VALUE1, VALUE2,
     :                             ISBND, LBND, UBND, STATUS )

*  For the old pixel/axis syntax, do each axis independently unless
*  floating point values are being interpreted as WCS values
               ELSE IF( .NOT. USEWCS ) THEN

                  DO 1 I = 1, NDIM
                     CALL NDF1_AXLIM( I, IACB1, VALUE1( I ),
     :                                VALUE2( I ), FRAME1( I ),
     :                                FRAME2( I ), ISBND( I ),
     :                                LBND( I ), UBND( I ), STATUS )

*  If an error occurs, then report context information and quit.
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL MSG_SETI( 'DIM', I )
                        CALL MSG_SETC( 'SECTION', STR( F : L ) )
                        CALL ERR_REP( 'NDF1_NCUT_DIM',
     :                                'Error in dimension ^DIM of the'//
     :                                ' NDF section specification '//
     :                                '''^SECTION''.',
     :                                STATUS )
                        GO TO 2
                     END IF
 1                CONTINUE
 2                CONTINUE

*  If we are using the old syntax, but non-integer values are being
*  interpretde as WCS values...
               ELSE
                  CALL NDF1_WPLIM( IWCS, NDIM, LBNDD, UBNDD, VALUE1,
     :                             VALUE2, FRAME1, FRAME2, ISBND,
     :                             ISDEF1, ISDEF2, LBND, UBND, STATUS )
               END IF

*  Select the section from the NDF.
               CALL NDF1_CUT( IACB1, NDIM, LBND, UBND, IACB2, STATUS )

*  Report further context information if needed.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL NDF1_AMSG( 'NDF', IACB1 )
                  CALL ERR_REP( 'NDF1_NCUT_FAIL',
     :                          'Unable to select the specified ' //
     :                          'section of the NDF ^NDF', STATUS )
               END IF
            END IF
         END IF

*  Free the WCS FrameSet pointer.
         IF( IWCS .NE. AST__NULL ) CALL AST_ANNUL( IWCS, STATUS )

      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_NCUT', STATUS )

      END
