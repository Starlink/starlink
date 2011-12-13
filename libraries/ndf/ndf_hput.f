      SUBROUTINE NDF_HPUT( HMODE, APPN, REPL, NLINES, TEXT, TRANS, WRAP,
     :                     RJUST, INDF, STATUS )
*+
*  Name:
*     NDF_HPUT

*  Purpose:
*     Write history information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HPUT( HMODE, APPN, REPL, NLINES, TEXT, TRANS, WRAP,
*                    RJUST, INDF, STATUS )

*  Description:
*     The routine writes textual information to the history component
*     of an NDF, creating a new history record if necessary. A variety
*     of formatting options are available and values may be substituted
*     for message tokens embedded within the text. The text supplied
*     may either augment or replace the history information normally
*     written by default.

*  Arguments:
*     HMODE = CHARACTER * ( * ) (Given)
*        The priority for the text being written: 'VERBOSE', 'NORMAL'
*        or 'QUIET', where 'VERBOSE' signifies the lowest priority and
*        'QUIET' signifies the highest.  The value given may be
*        abbreviated, to no less then three characters. A blank value
*        is accepted as a synonym for 'NORMAL'.
*     APPN = CHARACTER * ( * ) (Given)
*        Name of the current application. This will only be used if a
*        new history record is created by this routine, otherwise it is
*        ignored. If a blank value is given, then a system-supplied
*        default will be used instead. If the special value '<APPEND>'
*        is supplied, then the text is always appended to the current
*        history record, even if it was created by a previous application.
*     REPL = LOGICAL (Given)
*        Whether the text supplied is intended to replace the history
*        information which is supplied by default. If a .TRUE. value is
*        given and no default history information has yet been written
*        to the NDF, then subsequent writing of this information will
*        be suppressed. If a .FALSE. value is given, then default
*        history information is not suppressed and will later be
*        appended to the text supplied (unless suppressed by another
*        call to NDF_HPUT). The supplied value is ignored, and a value
*        of .TRUE. assumed, if APPN is supplied equal to '<APPEND>'.
*     NLINES = INTEGER (Given)
*        Number of lines of history text supplied.
*     TEXT( NLINES ) = CHARACTER * ( * ) (Given)
*        The lines of history text to be written.  The length of the
*        elements of this array (as returned by the Fortran LEN
*        function) should not exceed the value NDF__SZHMX. The
*        recommended length of TEXT elements is given by the constant
*        NDF__SZHIS. (Both of these constants are defined in the
*        include file NDF_PAR.)
*     TRANS = LOGICAL (Given)
*        If a .TRUE. value is supplied, then any message tokens
*        embedded in the supplied text will be expanded before it is
*        written to the history record (see SUN/104 for a description
*        of how to use message tokens). If a .FALSE. value is given,
*        then the supplied text is taken literally; no special
*        characters will be recognised and no message token expansion
*        will occur.
*     WRAP = LOGICAL (Given)
*        If a .TRUE. value is given, then paragraph wrapping will be
*        performed on the supplied text (after message token expansion
*        if appropriate) so as to make as much text fit on to each line
*        of a history record as possible. Blank input lines may be used
*        to delimit paragraphs. If a .FALSE. value is given, then input
*        lines which exceed the history record's text width will simply
*        be broken (at a space if possible) and continued on a new
*        line.
*     RJUST = LOGICAL (Given)
*        If a .TRUE. value is given, then lines of history text will be
*        padded out with blanks (after message token expansion and
*        paragraph wrapping if appropriate) so as to give a justified
*        right margin. If a .FALSE. value is given, then the right
*        margin will remain ragged.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine will return without action if (a) there is no
*     history component present in the NDF, (b) the priority specified
*     via HMODE is lower than the NDF's current history update mode
*     setting, or (c) the NDF's history update mode is currently set to
*     'DISABLED'.
*     -  It is expected that the APPN argument will usually be left
*     blank. A non-blank value should normally only be given for this
*     argument if a more complete identification of the current
*     application can be given than is supplied by default.
*     -  If no previous history information has been written to the NDF
*     by the current application, then this routine will create a new
*     history record whose text width will be determined by the length
*     of the lines of the TEXT array (as returned by the Fortran LEN
*     function). If history information has already been written, then
*     this routine will append to the existing history record. In this
*     case, the text width will already have been defined, so the
*     supplied text will be re-formatted if necessary (by line breaking
*     or paragraph wrapping) to fit into the available width.
*     -  Paragraph wrapping is recommended, when appropriate, as a
*     means of saving space while retaining a neat appearance in the
*     resulting history record. It is particularly useful when message
*     tokens are used within normal text, since these make it hard to
*     predict the precise length of history lines in advance.  The
*     right justification flag may be used to improve the cosmetic
*     appearance of history text, but it has no effect on the amount of
*     space used.
*     -  On exit from this routine, all message tokens in the current
*     message context are left in an undefined state.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAY-1993 (RFWS):
*        Original version.
*     19-MAY-1993 (RFWS):
*        Added the HMODE argument and changed the argument order.
*     20-MAY-1993 (RFWS):
*        Added support for history update modes.
*     16-JUN-1993 (RFWS):
*        Removed code for defaulting of the application name (now
*        performed at a lower level).
*     17-JUN-1993 (RFWS):
*        Removed calls to ERR_BEGIN and ERR_END so as to confine the
*        effect of MSG_LOAD to the current message context only.
*     16-OCT-2009 (DSB):
*        IF APPN is supplied as "<APPEND>", append the text to the current
*        history record, even if the current history record was created by
*        a previous application.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Write)
*           Whether default history information is to be written.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) HMODE
      CHARACTER * ( * ) APPN
      LOGICAL REPL
      INTEGER NLINES
      CHARACTER * ( * ) TEXT( * )
      LOGICAL TRANS
      LOGICAL WRAP
      LOGICAL RJUST
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) CDUMMY   ! Dummy character variable
      INTEGER HUM                ! History update mode code
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER IDUMMY             ! Dummy integer variable

*.

*  If STATUS is set on entry, then make a dummy call to MSG_LOAD to
*  ensure that message tokens are left in an undefined state.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_LOAD( ' ', ' ', CDUMMY, IDUMMY, STATUS )
      ELSE

*  Otherwise, import the NDF identifier.
         CALL NDF1_IMPID( INDF, IACB, STATUS )

*  If OK, check that WRITE access is available to the NDF.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL NDF1_CHACC( IACB, 'WRITE', STATUS )

*  If a blank HMODE value was given, then supply a default. Otherwise
*  validate this string.
            IF ( HMODE .EQ. ' ' ) THEN
               HUM = NDF__HNORM
            ELSE
               CALL NDF1_CHHUM( HMODE, HUM, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  If 'DISABLED' was specified, then report an error.
                  IF ( HUM .EQ. NDF__HDISA ) THEN
                     STATUS = NDF__HUMIN
                     CALL ERR_REP( 'NDF_HPUT_HUM',
     :                             'The ''DISABLED'' history update ' //
     :                             'mode may not be specified as ' //
     :                             'the priority argument when ' //
     :                             'writing history text to an NDF ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  END IF
               END IF
            END IF

*  IF OK, obtain an index to the data object entry in the DCB and ensure
*  that DCB history component information is available.
            IF ( STATUS .EQ. SAI__OK ) THEN
               IDCB = ACB_IDCB( IACB )
               CALL NDF1_DH( IDCB, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Check whether a history component is present. Otherwise there is
*  nothing to do.
                  IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Check that its update mode permits history text of the priority
*  supplied.  If so, then write the information into the history
*  record.
                     IF ( HUM .LE. DCB_HUMOD( IDCB ) ) THEN
                        CALL NDF1_HFWRT( IDCB, APPN, NLINES, TEXT,
     :                                   TRANS, WRAP, RJUST, STATUS )

*  If history text has been written successfully, then cancel the
*  default history writing flag if required.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           IF ( REPL .OR. APPN .EQ. '<APPEND>' ) THEN
                              DCB_HDEF( IDCB ) = .FALSE.
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF
         END IF

*  Make a dummy call to MSG_LOAD to ensure that message tokens are left
*  in an undefined state.
         CALL MSG_LOAD( ' ', ' ', CDUMMY, IDUMMY, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'NDF_HPUT_ERR',
     :      'NDF_HPUT: Error writing history information to an NDF.',
     :                    STATUS )
            CALL NDF1_TRACE( 'NDF_HPUT', STATUS )
         END IF
      END IF

      END
