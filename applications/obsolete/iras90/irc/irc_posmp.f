      SUBROUTINE IRC_POSMP( IDC, NEWNDF, PNTR, STATUS )
*+
*  Name:
*     IRC_POSMP

*  Purpose:
*     Get arrays containing sky coordinates and scan angles for
*     every sample.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_POSMP( IDC, NEWNDF, PNTR, STATUS )

*  Description:
*     This routine produces a temporary double precision NDF in which
*     the data array is three dimensional, having three "planes" each
*     matching the 2D data array of the input NDF (a CRDD file). The
*     lower plane (index 1) holds the Right Ascension of the centre of
*     each detector sample (B1950 FK4), the middle plane (index 2) holds
*     the Declination, and the upper plane (index 3) holds the scan
*     angle (see ID1 section 3) at the detector centre. This routine
*     also maps the data array of the created temporary NDF, and returns
*     a pointer to it. The created NDF should be annulled (using
*     NDF_ANNUL) when it is no longer required.
*
*     NOTE, unlike IRC_BPOS and IRC_DPOS extrapolated sample positions
*     cannot be found using IRC_POSMP. The returned arrays only contain
*     information for samples lying within the bounds of the DATA array.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the CRDD file.
*     NEWNDF = INTEGER (Returned)
*        The NDF identifier for the created temporary NDF.
*     PNTR = INTEGER (Returned)
*        A pointer to the mapped double precision array holding the
*        position and orientation of each CRDD sample. The array bounds
*        for the first two dimensions are the same as those of the input
*        NDF. The third dimension has bounds (1:3). Note, many IRC
*        routines allow for "extrapolated" fractional sample number
*        (i.e. sample numbers which are outside the bound of the first
*        NDF dimension and which may not be whole numbers). The array
*        returned by this routine is ONLY defined for integer sample
*        numbers lying within the bounds of the second dimension of the
*        NDF.
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*                (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read)
*           True if the corresponding IRC identifier is valid.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Read)
*           HDS type of the DETAILS component.
*        CCM_SLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest sample number in the DATA array.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Read)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Read)
*           Highest detector index in the DATA array.

*  Arguments Given:
      INTEGER IDC

*  Arguments Returned:
      INTEGER NEWNDF
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LBND( 3 )          ! Lower bounds of the output positions
                                 ! array.
      INTEGER NEL                ! Number of elements in the mapped
                                 ! array.
      INTEGER PLACE              ! Place holder for the temporary NDF.
      INTEGER UBND( 3 )          ! Upper bounds of the output positions
                                 ! array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the supplied IRC identifier is valid. If not, report an error
*  and quit.
      IF( IDC .LE. 0 .OR. IDC .GT. IRC__MAX ) THEN
         STATUS = IRC__INVID

      ELSE IF( .NOT. CCM_VALID( IDC ) ) THEN
         STATUS = IRC__INVID

      END IF

      IF( STATUS .EQ. IRC__INVID ) THEN
         CALL ERR_REP( 'IRC_POSMP_ERR1',
     :                 'IRC_POSMP: Invalid IRC identifier supplied',
     :                 STATUS )
         GO TO 999
      END IF


*  Set up the upper and lower bounds of the positions array.
      LBND(1) = CCM_SLOW( IDC )
      LBND(2) = CCM_DLOW( IDC )
      LBND(3) = 1

      UBND(1) = CCM_SHIGH( IDC )
      UBND(2) = CCM_DHIGH( IDC )
      UBND(3) = 3

*  Create a temporary NDF to hold the positions array.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_DOUBLE', 3, LBND, UBND, PLACE, NEWNDF, STATUS )

*  Map the data array which will hold the position data.
      CALL NDF_MAP( NEWNDF, 'DATA', '_DOUBLE', 'WRITE/ZERO', PNTR, NEL,
     :              STATUS )

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_PMPSB( IDC, LBND(1), UBND(1), LBND(2), UBND(2),
     :                    %VAL(CNF_PVAL(PNTR)), STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_POSMP_ERR2',
     :                 'IRC_POSMP: ^T data not yet supported.',
     :                  STATUS )
      END IF

*  If an error occured, give a contextual message and annul the
*  temporary NDF.
 999  CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRC_POSMP_ERR3',
     :          'IRC_POSMP: Unable to create a map of sample positions',
     :                 STATUS )
         CALL NDF_ANNUL( NEWNDF, STATUS )
      END IF

      END
