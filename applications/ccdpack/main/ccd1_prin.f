      SUBROUTINE CCD1_PRIN( PROMPT, NDFS, NNDF, FTYPES, FILT, DARK,
     :                      FLASH, STATUS )
*+
*  Name:
*     CCD1_PRIN

*  Purpose:
*     Gets a list of NDF names their frame types and filter type
*     or exposure factors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRIN( PROMPT, NDFS, NNDF, FTYPES, FILT, DARK, FLASH,
*                     STATUS)

*  Description:
*     This routine intercepts a GRP group from the user and interprets
*     its contents as a list of NDF names together with its frame type,
*     filter type, dark time and pre-flash exposure. Filters are
*     relevant to target and flatfields, exposures to darks or
*     flashes. A null (!)  character for any of the final three fields
*     is accepted and will mean that they are ignored. The input from
*     the user is a GRP group with all these five fields present for
*     each NDF (hence this is really intended for use from a procedure
*     were constructing such a list is not tedious). The outputs are an
*     NDG group of NDFs, a GRP group of their frame types and a group
*     containing the filters a group containing the dark times and a
*     group containing the flash times.

*  Arguments:
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt to use when getting the GRP group from the user.
*     NDFS = INTEGER (Returned)
*        An NDG group identifier for the NDFs.
*     NNDF = INTEGER (Returned)
*        The number of NDFs given.
*     FTYPES = INTEGER (Returned)
*        A GRP identifier with a list of the NDF frame types.
*     FILT = INTEGER (Returned)
*        Group of the NDF filter types. ! indicates none.
*     DARK = INTEGER (Returned)
*        Group of the NDF dark exposure times. ! indicates none.
*     FLASH = INTEGER (Returned)
*        Group of the NDF pre-flash exposure time. ! indicates none.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2000-2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-MAY-1994 (PDRAPER):
*        Original version.
*     24-MAY-1994 (PDRAPER):
*        Added dark and pre-flash arguments.
*     29-JUN-2000 (MBT):
*        Replaced use of IRH/IRG with GRP/NDG.
*     27-JUN-2001 (MBT):
*        Replaced use of CCD1__MXNDF by CCD1__MXINS (I think the original
*        was a mistake).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      INCLUDE 'FIO_ERR'          ! FIO system error codes.
      INCLUDE 'PAR_ERR'          ! Parameter system error codes
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'NDG_ERR'          ! NDG system error codes

*  Arguments Given:
      CHARACTER * ( * ) PROMPT

*  Arguments Returned:
      INTEGER NDFS
      INTEGER NNDF
      INTEGER FTYPES
      INTEGER FILT
      INTEGER DARK
      INTEGER FLASH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDH2               ! Group of given names
      INTEGER NTRY               ! Number of tries
      INTEGER INDEX              ! Loop index
      LOGICAL RENEW              ! Whether to renew input group
      LOGICAL TERM               ! Input has terminator flagged
      LOGICAL AGAIN              ! Loop again or not.
      INTEGER ADDED              ! How many values have been added
      INTEGER NRET               ! Number of values returned
      CHARACTER * ( GRP__SZNAM ) CURSTR ! Item read from group

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access a number of character strings using GRP. These should
*  represent the NDF the frame type and the associated factor.
      NTRY = 0
      NRET = 0
      CALL GRP_NEW( 'CCD1_PRINT:INPUT', IDH2, STATUS )
      RENEW = .FALSE.
 3    CONTINUE                   ! Start of DO UNTIL

*  Create a new group to associate names with if this isn't the first
*  time and a problem has occurred.
         IF ( RENEW ) THEN
            CALL CCD1_GRDEL( IDH2, STATUS )
            ADDED = -1
            CALL GRP_NEW( 'CCD1_PRINT:INPUT', IDH2, STATUS )
            RENEW = .FALSE.
         END IF

*  Get the user return.
         TERM = .FALSE.
         CALL GRP_GROUP( PROMPT, GRP__NOID, IDH2, NRET, ADDED, TERM,
     :                   STATUS )

*  Get out if a null return has been given or a PAR__ABORT. Also quit
*  after an unreasonble number of attempts.
         IF ( STATUS .EQ. PAR__ABORT )THEN
            GO TO 99
         ELSE IF ( NTRY .GT. 10 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', PROMPT )
            CALL ERR_REP( 'CCD1_PRIN1',
     :      '  Unable to obtain valid list of values using parameter'//
     :      ' %^NAME ' , STATUS )
            GO TO 99
         END IF

*  Check that the number of values returned. This has to be between the
*  the limits 5 and 5*CCD1__MXINS. If the continuation character has
*  been used then reprompt if appropriate. etc.
         AGAIN = .FALSE.
         IF ( NRET .GT. 5 * CCD1__MXINS ) THEN

*  Comment on this and try again.
            CALL MSG_SETI( 'MAXVAL', 5 * CCD1__MXINS )
            CALL MSG_OUT( ' ',
     :'Too many values given only ^MAXVAL allowed - try again', STATUS )

*  Reset everything ready for next attempt.
            RENEW = .TRUE.
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )
         ELSE IF ( NRET .LT. 5 ) THEN

*  Comment on this futile exercise.
            CALL MSG_OUT( ' ',
     :'Too few values given need multiple of 5 - try again', STATUS )

*  Reset everything ready for next attempt.
            RENEW = .TRUE.
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )
         ELSE IF ( TERM ) THEN

*  Continuation character has been issued reprompt, appending to the
*  present group.
            AGAIN = .TRUE.
            CALL PAR_CANCL( PROMPT, STATUS )

*  Status may have been set by NDG for a good reason.. check for this
*  and reprompt.
         ELSE IF ( STATUS .EQ. NDG__NOFIL ) THEN

*  Issue the error.
            CALL ERR_FLUSH( STATUS )

*  Reset everything and try again.
            RENEW = .TRUE.
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )

*  Try to trap a special case of ' ' string return. This should leave
*  added unmodified. This will be taken as a request to exit. The normal
*  stop entry request from an blank line will be `!' .
         ELSE IF ( ADDED .EQ. -1 .AND. .NOT. TERM ) THEN
            AGAIN = .FALSE.

*  If status is set to PAR__NULL then reset status, note that this is
*  only tested if the checks for number of values etc. have been passed.
         ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            AGAIN = .FALSE.

         ELSE IF ( MOD ( NRET, 5 ) .NE. 0 ) THEN

*  Return must be a multiple of 5. Check this.
            CALL MSG_SETI( 'NRET', NRET )
            CALL MSG_OUT( ' ',
     :'Can only give a multiple of 5 fields (^NRET given) - try again',
     :      STATUS )

*  Reset everything ready for next attempt.
            RENEW = .TRUE.
            AGAIN = .TRUE.
            NTRY = NTRY + 1
            CALL PAR_CANCL( PROMPT, STATUS )
         END IF

*  End of DO UNTIL.
      IF ( AGAIN ) GO TO 3
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Extract the fields from the input and try to access the NDFs. The
*  NDF types are entered into their own group as is the final field.

*  Create a group consisting of only the NDF names.
      CALL GRP_NEW( 'CCD1_PRINT:NDFS', NDFS, STATUS )
      NNDF = 0
      DO 4 INDEX = 1, NRET, 5

*  Get the NDF name string.
         CALL GRP_GET( IDH2, INDEX, 1, CURSTR, STATUS )

*  Enter this into the group.
         CALL GRP_PUT( NDFS, 1, CURSTR, 0, STATUS )
         NNDF = NNDF + 1
 4    CONTINUE

*  Now create a group for the NDF frame types.
      CALL GRP_NEW( 'CCD1_PRIN:FTYPES', FTYPES, STATUS )

*  And extract the frame types.
      DO 5 INDEX = 2, NRET, 5
         CALL GRP_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL CHR_UCASE( CURSTR )
         CALL CHR_LDBLK( CURSTR )
         CALL GRP_PUT( FTYPES, 1, CURSTR, 0, STATUS )
 5    CONTINUE

*  Extract the filter types.
      CALL GRP_NEW( 'CCD1_PRIN:FILT', FILT, STATUS )
      DO 6 INDEX = 3, NRET, 5
         CALL GRP_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL CHR_LDBLK( CURSTR )
         CALL GRP_PUT( FILT, 1, CURSTR, 0, STATUS )
 6    CONTINUE

*  Extract the dark times.
      CALL GRP_NEW( 'CCD1_PRIN:DARK', DARK, STATUS )
      DO 7 INDEX = 4, NRET, 5
         CALL GRP_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL GRP_PUT( DARK, 1, CURSTR, 0, STATUS )
 7    CONTINUE

*  Extract the pre-flash times.
      CALL GRP_NEW( 'CCD1_PRIN:FLASH', FLASH, STATUS )
      DO 8 INDEX = 5, NRET, 5
         CALL GRP_GET( IDH2, INDEX, 1, CURSTR, STATUS )
         CALL GRP_PUT( FLASH, 1, CURSTR, 0, STATUS )
 8    CONTINUE

*  Exit with error label.
 99   CONTINUE
      END
