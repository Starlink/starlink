      SUBROUTINE KPG1_ASFRM( PARAM, EPARAM, IWCS, WCDOM, DCDOM, PROMPT,
     :                       STATUS )
*+
*  Name:
*     KPG1_ASFRM

*  Purpose:
*     Set the current Frame in a FrameSet to a Frame specified through
*     the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFRM( PARAM, EPARAM, IWCS, WCDOM, DCDOM, PROMPT, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine allows the user to specify a new Current Frame for a 
*     FrameSet using an environment parameter.

*  ADAM Parameters:
*     %PARAM = LITERAL (Read)
*        A string specifying the new co-ordinate Frame. If a null parameter 
*        value is supplied, then the error is annulled and the current Frame 
*        is left unchanged. The string can be one of the following:
*
*        - A Domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into the values supplied for arguments WCDOM and DCDOM
*        respectively, so long as the FrameSet does not contain Frames with 
*        Domains WORLD or DATA.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - An IRAS90 Sky Co-ordinate System (SCS) values such as 
*        EQUAT(J2000) (see SUN/163).
*     %EPARAM = DOUBLE PRECISION (Read)
*        If a celestial co-ordinate system is supplied (using parameter 
*        %PARAM) then an epoch value is needed to qualify it. This is the 
*        epoch at which the supplied sky positions were determined. It should 
*        be given as a decimal years value, with or without decimal places 
*        ("1996.8" for example). Such values are interpreted as a Besselian 
*        epoch if less than 1984.0 and as a Julian epoch otherwise. 

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get Frame description.
*     EPARAM = CHARACTER * ( * ) (Given)
*        Name of parameter to use to get Epoch value (if needed).
*     IWCS = INTEGER (Given)
*        An AST pointer to the FrameSet. 
*     WCDOM = CHARACTER * ( * ) (Given)
*        The Domain name to use if the user requests "WORLD" co-ordinates.
*        No translation of WORLD takes place if a blank value is supplied.
*     DCDOM = CHARACTER * ( * ) (Given)
*        The Domain name to use if the user requests "DATA" co-ordinates.
*        No translation of DATA takes place if a blank value is supplied.
*     PROMPT = LOGICAL (Given)
*        An error is always reported if the requested Frame is not
*        available in the FrameSet. PROMPT controls what happens after
*        the error has been reported. If .TRUE., then the error is
*        flushed, the parameter is cancelled, and the user is re-prompted 
*        for a new %PARAM value. Otherwise, the error is retained, and the 
*        routine exits with STATUS set to SAI__ERROR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may add a new co-ordinate Frame into the FrameSet.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR error constants 
      INCLUDE 'IRA_PAR'          ! IRAS90 astrometry library constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER EPARAM*(*)
      INTEGER IWCS
      CHARACTER WCDOM*(*)
      CHARACTER DCDOM*(*)
      LOGICAL PROMPT

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL KPG1_ISSCS         ! Is string an IRAS90 SCS?
      LOGICAL KPG1_ASSIR         ! Is string an IRAS90 SCS?

*  Local Variables:
      CHARACTER AVDOMS*128       ! List of the main available Domains
      CHARACTER BJ*1		 ! B for Besselian, or J for Julian equinox
      CHARACTER FRAME*30         ! Co-ordinate Frame specification
      CHARACTER DOM*15           ! Domain name
      CHARACTER SCSNAM*(IRA__SZSCS) ! IRAS90 name for sky coordinate system
      CHARACTER TEXT*50          ! Text 
      DOUBLE PRECISION DEFEP     ! Epoch of observation
      DOUBLE PRECISION EPOCH     ! Epoch of observation
      DOUBLE PRECISION EQU       ! Equinox 
      INTEGER SKYFRM             ! Pointer to SkyFrame
      INTEGER FRM                ! Pointer to matching Frame
      INTEGER IAT	         ! Current length of string
      INTEGER IFRM               ! Sub-Frame index
      INTEGER LSTAT              ! CHR status 
      INTEGER MAP                ! Pointer to mapping
      INTEGER SF2                ! Copy of SkyFrame
      LOGICAL ISSCS              ! Is FRAME an IRAS90 SCS?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Construct a list of the Domains of the Frames in the supplied FrameSet. 
*  This will be used in error messages. This may not be an exhaustive
*  list of the available Domains because some of the Frames may be
*  compound frames containing sub-Frames with other Domains.
      AVDOMS = ' '
      IAT = 0
      DO IFRM = 1, AST_GETI( IWCS, 'NFRAME', STATUS )

*  Get a pointer to the Frame with the current index.
         FRM = AST_GETFRAME( IWCS, IFRM, STATUS )

*  Get its Domain value.
         DOM = AST_GETC( FRM, 'DOMAIN', STATUS )

*  Annul the pointer to this Frame.
         CALL AST_ANNUL( FRM, STATUS )

*  Append the Domain value to the current list if it is not blank.
*  Separate list items by ", ".
         IF( DOM .NE. ' ' ) THEN
            IF( IAT .NE. 0 ) CALL CHR_APPND( ',', AVDOMS, IAT )
            IAT = IAT + 1            
            CALL CHR_APPND( DOM, AVDOMS, IAT )
         END IF

      END DO

*  Loop until a usable frame specificiation is obtained.
      IFRM = AST__NOFRAME
      DO WHILE( IFRM .EQ. AST__NOFRAME .AND. STATUS .EQ. SAI__OK  ) 

*  Get the string describing the required co-ordinate Frame.
         CALL PAR_GET0C( PARAM, FRAME, STATUS )

*  Annul the error and abort, if a null value was supplied.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 999
         END IF

*  Convert to upper case, and remove leading blanks.
         CALL CHR_UCASE( FRAME )
         CALL CHR_LDBLK( FRAME )

*  First of all, attempt to use the supplied string as a Domain name.
*  Search for a Frame with the supplied Domain name.
         CALL KPG1_ASFFR( IWCS, FRAME, IFRM, STATUS )

*  If a matching Frame was found, make it the current Frame.
         IF( IFRM .NE. AST__NOFRAME ) THEN
            CALL AST_SETI( IWCS, 'CURRENT', IFRM, STATUS )

*  Othewise, see if the supplied string is an integer. If so, set the index 
*  of the Current Frame.
         ELSE 
            LSTAT = SAI__OK 
            CALL CHR_CTOI( FRAME, IFRM, LSTAT )
            IF( LSTAT .EQ. SAI__OK ) THEN
               CALL AST_SETI( IWCS, 'CURRENT', IFRM, STATUS )          

*  If the supplied string is not an integer, is it an IRAS90 SCS?
            ELSE IF( KPG1_ISSCS( FRAME, EQU, BJ, SCSNAM, STATUS ) ) THEN
       
*  If so, search for a Frame with Domain SKY.
               CALL KPG1_ASFFR( IWCS, 'SKY', IFRM, STATUS )

*  If one was found, we need to set its attributes so that they describe 
*  the requested sky coordinate system.
               IF( IFRM .NE. AST__NOFRAME ) THEN

*  Get a pointer to the SKY Frame.
                  SKYFRM = AST_GETFRAME( IWCS, IFRM, STATUS )

*  If it is not a SkyFrame, we have no match.
                  IF( .NOT. AST_ISASKYFRAME( SKYFRM, STATUS ) ) THEN     
                     IFRM = AST__NOFRAME

*  If it is a SkyFrame, make it the Current Frame.
                  ELSE
                     CALL AST_SETI( IWCS, 'CURRENT', IFRM, STATUS )

*  Take a deep copy of it.
                     SF2 = AST_COPY( SKYFRM, STATUS )

*  Now set the attributes of the SkyFrame to match the values specified
*  by the SCS. 
                     ISSCS = KPG1_ASSIR( SKYFRM, FRAME, EPARAM, STATUS )

*  Get the Mapping from the original SkyFrame to the modified SkyFrame.
                     MAP = AST_GETMAPPING( AST_FINDFRAME( SF2, SKYFRM, 
     :                                                    ' ', STATUS ),
     :                                 AST__BASE, AST__CURRENT, STATUS )

*  Remap the SkyFrame using this Mapping.                  
                     CALL AST_REMAPFRAME( IWCS, IFRM, MAP, STATUS ) 

                  END IF

               END IF

*  If it not an IRAS90 SCS, see if is a "psuedo-domain".
            ELSE

*  If WORLD is specified, look for WCDOM. If DATA is specified, look
*  for DCDOM.
               IF( FRAME .EQ. 'WORLD' ) THEN
                  DOM = WCDOM
               ELSE IF( FRAME .EQ. 'DATA' ) THEN
                  DOM = DCDOM
               ELSE    
                  DOM = ' ' 
               END IF

*  If a pseudo-domain was given, look for a Frame with the corresponding
*  domain.
               IF( DOM .NE. ' ' ) THEN
                  CALL KPG1_ASFFR( IWCS, DOM, IFRM, STATUS )

*  If a matching Frame was found, make it the current Frame.
                  IF( IFRM .NE. AST__NOFRAME ) THEN
                     CALL AST_SETI( IWCS, 'CURRENT', IFRM, STATUS )
                  END IF
   
               ELSE
                  IFRM = AST__NOFRAME
               END IF
   
            END IF
   
         END IF

*  Report an error if the requested Frame was not found.
         IF( IFRM .EQ. AST__NOFRAME .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR

            IF( AVDOMS .NE. ' ' ) THEN
               CALL MSG_SETC( 'FRAME', FRAME )
               CALL MSG_SETC( 'PAR', PARAM )
               CALL MSG_SETC( 'AVDOMS', AVDOMS )
   
               CALL ERR_REP( 'KPG1_ASFRM_1', 'The co-ordinate system '//
     :                    '(^FRAME) requested using parameter %^PAR '//
     :                    'is not available. The available Frames '//
     :                    'include ^AVDOMS.', STATUS )

            ELSE
               CALL MSG_SETC( 'FRAME', FRAME )
               CALL MSG_SETC( 'PAR', PARAM )
   
               CALL ERR_REP( 'KPG1_ASFRM_1', 'The co-ordinate system '//
     :                    '(^FRAME) requested using parameter %^PAR '//
     :                    'is not available.', STATUS )
            END IF

*  If re-prompting is required, flush the error, cancel the parameters.
            IF( PROMPT ) THEN
               CALL ERR_FLUSH( STATUS )
               CALL PAR_CANCL( PARAM, STATUS )
               CALL PAR_CANCL( EPARAM, STATUS )
            END IF

         END IF

      END DO

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
