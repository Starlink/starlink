      SUBROUTINE KPG1_GTAXI( PARAM, FRAME, NAX, AXES, STATUS )
*+
*  Name:
*     KPG1_GTAXI

*  Purpose:
*     Select Frame axes using an environment parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GTAXI( PARAM, FRAME, NAX, AXES, STATUS )

*  Description:
*     This routine returns the indices of selected axes in a supplied
*     Frame. The axes to select are determined using the supplied
*     environment parameter. Each axis can be specified either by giving 
*     its index within the Frame in the range 1 to the number of axes in 
*     the Frame, or by giving its symbol. If the first value supplied in 
*     AXES is not zero, the supplied axes are used as the dynamic default 
*     for the parameter. The parameter value should be given as a GRP group 
*     expression, with default GRP control characters.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the parameter to use.
*     FRAME = INTEGER (Given)
*        An AST pointer for the Frame from which axes may be chosen.
*     NAX = INTEGER (Given)
*        The number of axes which must be selected.
*     AXES( NAX ) = INTEGER (Given and Returned)
*        On entry, the axes to be specified as the dynamic default for the 
*        parameter (if AXES( 1 ) is not zero). On exit, the indices of the
*        selected axes. If AXES(1) is zero the supplied values are ignored
*        and a PAR__NULL status value is returned if a null (!) value is 
*        supplied for the parameter. Otherwise, the PAR_NULL status is
*        annulled if a null value is supplied, and the supplied axes are 
*        returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Case is insignificant when comparing supplied strings with
*     available axis symbols.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER FRAME
      INTEGER NAX

*  Arguments Given and Returned:
      INTEGER AXES( NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CAXIS( NDF__MXDIM )*20 ! Available axis symbols
      CHARACTER PAXIS*( VAL__SZI ) ! Buffer for new axis number
      INTEGER I                    ! Axis index
      INTEGER NCP                  ! No. of characters in PAXIS text buffer
      INTEGER NFC                  ! No. of axes in original Current Frame
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of axes in the supplied Frame.
      NFC = AST_GETI( FRAME, 'NAXES', STATUS )

*  Get the axis symbols and their lengths for all axes in the Frame.
      DO I = 1, NFC
         NCP = 0
         CALL CHR_PUTI( I, PAXIS, NCP )
         CAXIS( I ) = AST_GETC( FRAME, 'Symbol(' // 
     :                          PAXIS( : NCP ) // ')', STATUS )
         CALL CHR_LDBLK( CAXIS( I ) )
         CALL KPG1_PGESC( CAXIS( I ), STATUS )
      END DO

*  Get the required number of axis selections. A resonable guess should
*  be to assume a one-to-one correspondance between Current and Base axes.
*  Therefore, use the significant axes selected above as the defaults to be
*  used if a null (!) parameter value is supplied.
      CALL KPG1_GTCHV( NFC, CAXIS, PARAM, NAX, AXES, AXES, STATUS )
    
      END
