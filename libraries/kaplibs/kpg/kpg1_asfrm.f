      SUBROUTINE KPG1_ASFRM( PARAM, EPARAM, IWCS, WCDOM, DCDOM, PROMPT,
     :                       TOKEN, STATUS )
*+
*  Name:
*     KPG1_ASFRM

*  Purpose:
*     Sets the current Frame in a FrameSet to a Frame specified through
*     the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASFRM( PARAM, EPARAM, IWCS, WCDOM, DCDOM, PROMPT, TOKEN,
*                      STATUS )

*  Description:
*     This routine allows the user to specify a new Current Frame for a
*     FrameSet using an environment parameter.

*  Environment Parameters:
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
*     %EPARAM = _DOUBLE (Read)
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
*     TOKEN = CHARACTER * ( * ) (Given)
*        A string containing an MSG message token reference (e.g. "^FRED").
*        The value of the token is used within error messages and should
*        describe the object (NDF, catalogue, etc.) from which the supplied
*        FrameSet is derived. If the token reference string is blank it is
*        ignored.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may add a new co-ordinate Frame into the FrameSet.
*     -  If the FrameSet contains more than one Frame with the requested
*        Domain, then the last matching Frame in the FrameSet will be
*        used (i.e. the one with highest index).

*  Copyright:
*     Copyright (C) 1998, 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1998 (DSB):
*        Original version.
*     16-DEC-1998 (DSB):
*        Ensure each Domain is only included once in the list of available
*        Domains.
*     25-AUG-1999 (DSB):
*        Added argument TOKEN.
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
      CHARACTER TOKEN*(*)

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string
      LOGICAL KPG1_ISSCS         ! Is string an IRAS90 SCS?
      LOGICAL KPG1_ASSIR         ! Is string an IRAS90 SCS?

*  Local Variables:
      CHARACTER AVDOMS*128       ! List of the main available Domains
      CHARACTER BJ*1		 ! B for Besselian, or J for Julian equinox
      CHARACTER DOM*15           ! Domain name
      CHARACTER FRAME*30         ! Co-ordinate Frame specification
      CHARACTER OBJ*255          ! The value of the supplied message TOKEN
      CHARACTER SCSNAM*(IRA__SZSCS) ! IRAS90 name for sky co-ordinate system
      CHARACTER T1*16            ! Terminated domain name
      DOUBLE PRECISION EQU       ! Equinox
      INTEGER FRM                ! Pointer to matching Frame
      INTEGER IAT	         ! Current length of string
      INTEGER IFRM               ! Sub-Frame index
      INTEGER JAT	         ! Current length of string
      INTEGER LSTAT              ! CHR status
      INTEGER MAP                ! Pointer to mapping
      INTEGER OBJLEN             ! The used length of the supplied message TOKEN
      INTEGER SF2                ! Copy of SkyFrame
      INTEGER SKYFRM             ! Pointer to SkyFrame
      LOGICAL ISSCS              ! Is FRAME an IRAS90 SCS?
      LOGICAL THERE              ! Is the Domain already in the list?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the value of any supplied message token. Do it now while we know
*  the token is still defined.
      IF( TOKEN .NE. ' ' ) THEN
         CALL MSG_LOAD( ' ', TOKEN, OBJ, OBJLEN, STATUS )
      ELSE
         OBJ = ' '
         OBJLEN = 0
      END IF

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

*  Append the Domain value to the current list if it is not blank, and
*  if it is not already there. Separate list items by ", ".
         IF( DOM .NE. ' ' ) THEN

*  If this is the first domain, it cannot already be in the list.
            IF( IAT .EQ. 0 ) THEN
               THERE = .FALSE.

*  Otherwise, we do the check.
            ELSE

*  Terminate the existing list with a comma.
               CALL CHR_APPND( ',', AVDOMS, IAT )

*  Get a copy of the Domain name, also terminated with a comma. We need
*  to include the comma to avoid the new Domain name being matched by a
*  sub-string within a Domain already in the list.
               T1 = DOM
               JAT = CHR_LEN( DOM )
               CALL CHR_APPND( ',', T1, JAT )

*  Set the flag if the terminated domain name is already in the list,
*  and remove the comma appended above to the end of the list.
               IF( INDEX( AVDOMS( : IAT ), T1( : JAT ) ) .NE. 0 ) THEN
                  THERE = .TRUE.
                  AVDOMS( IAT : ) = ' '
                  IAT = IAT - 1
               ELSE
                  THERE = .FALSE.
               END IF

            END IF

*  If the domain is not already in the list, append it to the end.
            IF( .NOT. THERE ) THEN
               IAT = IAT + 1
               CALL CHR_APPND( DOM, AVDOMS, IAT )
            END IF

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
*  the requested sky co-ordinate system.
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

               IF( OBJLEN .EQ. 0 ) THEN
                  CALL ERR_REP( 'KPG1_ASFRM_1', 'The co-ordinate '//
     :                    'system (^FRAME) requested using parameter '//
     :                    '%^PAR is not available. The available '//
     :                    'Frames include ^AVDOMS.', STATUS )
               ELSE
                  CALL MSG_SETC( 'N', OBJ( : OBJLEN ) )
                  CALL ERR_REP( 'KPG1_ASFRM_1b', 'The co-ordinate '//
     :                    'system (^FRAME) requested using parameter '//
     :                    '%^PAR is not available in ^N. The '//
     :                    'available Frames include ^AVDOMS.', STATUS )
               END IF

            ELSE
               CALL MSG_SETC( 'FRAME', FRAME )
               CALL MSG_SETC( 'PAR', PARAM )

               IF( OBJLEN .EQ. 0 ) THEN
                  CALL ERR_REP( 'KPG1_ASFRM_1c', 'The co-ordinate '//
     :                    'system (^FRAME) requested using parameter '//
     :                    '%^PAR is not available.', STATUS )
               ELSE
                  CALL MSG_SETC( 'N', OBJ( : OBJLEN ) )
                  CALL ERR_REP( 'KPG1_ASFRM_1d', 'The co-ordinate '//
     :                    'system (^FRAME) requested using parameter '//
     :                    '%^PAR is not available in ^N.', STATUS )
               END IF
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
