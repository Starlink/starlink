      SUBROUTINE WCSFRAME( STATUS )
*+
*  Name:
*     WCSFRAME

*  Purpose:
*     Change the current co-ordinate Frame in the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays the current co-ordinate Frame associated
*     with an NDF and then allows the user to specify a new Frame. The
*     current co-ordinate Frame determines the co-ordinate system in
*     which positions within the NDF will be expressed when communicating
*     with the user. 

*  Usage:
*     wcsframe ndf frame epoch

*  ADAM Parameters:
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using 
*        parameter FRAME) for a celestial co-ordinate system, then an epoch 
*        value is needed to qualify it. This is the epoch at which the 
*        supplied sky positions were determined. It should be given as a 
*        decimal years value, with or without decimal places  ("1996.8" for 
*        example). Such values are interpreted as a Besselian epoch if less 
*        than 1984.0 and as a Julian epoch otherwise. 
*     FRAME = LITERAL (Read)
*        A string specifying the new co-ordinate Frame. If a null parameter 
*        value is supplied, then the current Frame is left unchanged. The 
*        string can be one of the following:
*
*        - A domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        component of the NDF does not contain Frames with these domains.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see 
*        section "Sky Co-ordinate Systems" in SUN/95).
*
*     NDF = NDF (Read and Write)
*        The NDF data structure in which the current co-ordinate Frame is to 
*        be modified.

*  Examples:
*     wcsframe m51 pixel
*        This chooses pixel co-ordinates for the current co-ordinate
*        Frame in the NDF m51. 
*     wcsframe m51 sky
*        This chooses celestial co-ordinates for the current co-ordinate
*        Frame in the NDF m51. The specific celestial co-ordinate system
*        will depend on the contents of the WCS component of the NDF.
*     wcsframe m51 equ(J2000) epoch=1998.2
*        This chooses equatorial (RA/DEC) co-ordinates referred to the
*        equinox at Julian epoch 2000.0 for the current co-ordinate
*        Frame in the NDF m51. The positions were determined at the
*        Julian epoch 1998.2 (this is needed to correct positions for
*        the fictitious proper motions which may be introduced when 
*        converting between different celestial co-ordinate systems).
*     wcsframe m51 2
*        This chooses the second co-ordinate Frame in the WCS component
*        of the NDF.
*     wcsframe m51 data
*        This chooses a co-ordinate Frame with domain DATA if one exists, 
*        or the AXIS co-ordinate Frame otherwise.
*     wcsframe m51 world
*        This chooses a co-ordinate Frame with domain WORLD if one exists, 
*        or the PIXEL co-ordinate Frame otherwise.

*  Notes:
*     -  The current co-ordinate Frame in the supplied NDF is not displayed 
*     if a value is assigned to parameter FRAME on the command line.
*     -  This routine may add a new co-ordinate Frame into the WCS component 
*     of the NDF.
*     -  The NDFTRACE command can be used to examine the co-ordinate
*     Frames in the WCS component of an NDF.

*  Related Applications:
*     KAPPA: NDFTRACE, WCSREMOVE, WCSCOPY, WCSATTRIB

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Add TOKEN arg in call to KPG1_ASFRM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! PAR constants 

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER INDF               ! NDF identifier
      INTEGER IWCS               ! AST pointer for WCS FrameSet
      INTEGER STATE              ! Indicates state of FRAME parameter
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Obtain an identifier for the NDF.
      CALL NDF_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Create an AST FrameSet from the WCS component of the NDF.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  If no value was supplied for parameter FRAME on the command line, 
*  display the current co-ordinate Frame.
      CALL PAR_STATE( 'FRAME', STATE, STATUS )
      IF( STATE .NE. PAR__ACTIVE ) THEN
         CALL MSG_BLANK( STATUS )
         CALL KPG1_DSFRM( IWCS, 'Current co-ordinate Frame:', STATUS )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set the new Current Frame using parameter FRAME. If "WORLD" co-ordinates
*  are requested, use PIXEL. If "DATA" co-ordinates are requested, use
*  "AXIS".
      CALL NDF_MSG( 'NDF', INDF )
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS', .TRUE.,
     :                 '^NDF', STATUS )

*  Save a copy of the modified FrameSet in the NDF's WCS component.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

 999  CONTINUE

*  Annul the NDF identifier.
      CALL NDF_ANNUL( INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSFRAME_ERR', 'WCSFRAME: Failed to set '//
     :                 'the current co-ordinate Frame in an NDF.',
     :                 STATUS )
      END IF

      END
