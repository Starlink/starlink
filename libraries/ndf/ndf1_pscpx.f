      SUBROUTINE NDF1_PSCPX( STR, MXEXTN, EXTN, NEXTN, CPF, STATUS )
*+
*  Name:
*     NDF1_PSCPX

*  Purpose:
*     Parse an NDF component propagation expression.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_PSCPX( STR, MXEXTN, EXTN, NEXTN, CPF, STATUS )

*  Description:
*     The routine parses an expression specifying which components of
*     an NDF are to be propagated when a new NDF is created based on an
*     existing template. The expression should contain a comma
*     separated list of component names (optionally abbreviated) or
*     component names prefixed with 'NO' (to indicate that the
*     specified component should not be propagated). By default the
*     HISTORY, LABEL and TITLE components and all extensions are
*     propagated. Named extensions may be included or excluded by
*     specifying EXTENSION( ) or NOEXTENSION( ) as one of the list
*     items with a list of the extensions to be affected contained
*     within the parentheses. The same component name may appear more
*     than once in the list, and the effects of each occurrence are
*     cumulative (i.e. the latter occurrence takes precedence).  The
*     routine returns an array of logical component propagation flags
*     and a list of the names of extensions which are not to be
*     propagated.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The expression to be parsed.
*     MXEXTN = INTEGER (Given)
*        Maximum number of names to be returned in the EXTN array (i.e.
*        the declared size of this array).
*     EXTN( MXEXTN ) = CHARACTER * ( DAT__SZNAM ) (Returned)
*        List of the names of NDF extensions which are not to be
*        propagated.
*     NEXTN = INTEGER (Returned)
*        Number of extension names returned in the EXTN array.
*     CPF( NDF__MXCPF ) = LOGICAL (Returned)
*        Array of component propagation flags. Symbolic constants are
*        defined in the include file NDF_CONST to identify the elements
*        of this array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  License:
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program (see SLA_CONDITIONS); if not, write to the
*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,
*    Boston, MA  02111-1307  USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1989 (RFWS):
*        Original version.
*     22-FEB-1990 (RFWS):
*        Changed to prevent the UNITS component being propagated by
*        default.
*     15-APR-1994 (RFWS):
*        Added extra arguments to NDF1_FPARX call.
*     1-JUL-1997 (RFWS):
*        Added support for the WCS component.
*     24-DEC-2005 (TIMJ):
*        Use CHR_FPARX rather than NDF1_FPARX
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
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER MXEXTN

*  Arguments Returned:
      CHARACTER * ( DAT__SZNAM ) EXTN( MXEXTN )
      INTEGER NEXTN
      LOGICAL CPF( NDF__MXCPF )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER NDF1_INDXP         ! Position of unparenthesised character
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER I1                 ! Character position of start of item
      INTEGER I2                 ! Character position of end of item
      INTEGER F                  ! Position of start of name to test
      INTEGER L                  ! Position of end of name to test
      INTEGER J1                 ! Position of opening parenthesis
      INTEGER J2                 ! Position of closing parenthesis
      LOGICAL RECOG              ! Whether item was recognised

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the component propagation flags.
      CPF( NDF__ACPF ) = .FALSE.
      CPF( NDF__DCPF ) = .FALSE.
      CPF( NDF__HCPF ) = .TRUE.
      CPF( NDF__LCPF ) = .TRUE.
      CPF( NDF__QCPF ) = .FALSE.
      CPF( NDF__TCPF ) = .TRUE.
      CPF( NDF__UCPF ) = .FALSE.
      CPF( NDF__VCPF ) = .FALSE.
      CPF( NDF__WCPF ) = .FALSE.
      
*  Initialise the count of excluded extensions.
      NEXTN = 0

*  Initialise a pointer to the start of the "current" item in the
*  component list.
      I1 = 1

*  Loop to process each item in the list.
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( ( I1 .LE. LEN( STR ) ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN

*  Find the end of the current item (the last character before the next
*  unparenthesised comma or end of string).
         I2 = NDF1_INDXP( STR( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( STR )
         ELSE
            I2 = I2 + I1 - 2
         END IF

*  Find the first and last characters in the item (excluding surrounding
*  blanks).
         IF ( I1 .LE. I2 ) THEN
            CALL CHR_FANDL( STR( I1 : I2 ), F, L )

*  Check the item is not completely blank.
            IF ( F .LE. L ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Compare the item with each permitted value in turn, allowing
*  abbreviation. Set the appropriate component propagation flag values
*  and note if the item is recognised.
               RECOG = .FALSE.

*  ...AXIS.
               IF ( NDF1_SIMLR( STR( F : L ), 'AXIS',
     :                          NDF__MINAB ) ) THEN
                  CPF( NDF__ACPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOAXIS.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOAXIS',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__ACPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...DATA.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'DATA',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__DCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NODATA.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NODATA',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__DCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...HISTORY.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'HISTORY',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__HCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOHISTORY.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOHISTORY',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__HCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...LABEL.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'LABEL',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__LCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOLABEL.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOLABEL',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__LCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...QUALITY.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'QUALITY',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__QCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOQUALITY.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOQUALITY',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__QCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...TITLE.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'TITLE',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__TCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOTITLE.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOTITLE',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__TCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...UNITS.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'UNITS',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__UCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOUNITS.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOUNITS',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__UCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...VARIANCE.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'VARIANCE',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__VCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOVARIANCE.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOVARIANCE',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__VCPF ) = .FALSE.
                  RECOG = .TRUE.

*  ...WCS.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'WCS',
     :                               NDF__MINAB ) ) THEN
                  CPF( NDF__WCPF ) = .TRUE.
                  RECOG = .TRUE.

*  ...NOWCS.
               ELSE IF ( NDF1_SIMLR( STR( F : L ), 'NOWCS',
     :                               NDF__MINAB + 2 ) ) THEN
                  CPF( NDF__WCPF ) = .FALSE.
                  RECOG = .TRUE.

*  If the item did not match any of the above, then it may be an
*  EXTENSION specification, followed by a parenthesised list of
*  extension names. Search for a parenthesesed expression.
               ELSE
                  CALL CHR_FPARX( STR( F : L ), '(', ')', J1, J2 )

*  If found, then test the characters lying in front of the opening
*  parenthesis (if there are any).
                  IF ( J1 .LE. J2 ) THEN
                     J1 = J1 + F - 1
                     J2 = J2 + F - 1
                     IF ( J1 .GT. F ) THEN
                        IF ( NDF1_SIMLR( STR( F : J1 - 1 ), 'EXTENSION',
     :                                   NDF__MINAB ) ) THEN

*  If this is an EXTENSION specification, then update the list of
*  excluded extensions accordingly.
                           RECOG = .TRUE.
                           IF ( J1 + 1 .LE. J2 - 1 ) THEN
                              CALL NDF1_PXLST( .TRUE.,
     :                                         STR( J1 + 1 : J2 - 1 ),
     :                                         MXEXTN, EXTN, NEXTN,
     :                                         STATUS )
                           END IF

*  Perform the appropriate updating operation if this is a NOEXTENSION
*  specification.
                        ELSE IF ( NDF1_SIMLR( STR( F : J1 - 1 ),
     :                                        'NOEXTENSION',
     :                                        NDF__MINAB + 2 ) ) THEN
                           RECOG = .TRUE.
                           IF ( J1 + 1 .LE. J2 - 1 ) THEN
                              CALL NDF1_PXLST( .FALSE.,
     :                                         STR( J1 + 1 : J2 - 1 ),
     :                                         MXEXTN, EXTN, NEXTN,
     :                                         STATUS )
                           END IF
                        END IF
                     END IF
                  END IF
               END IF

*  If the list item was not ecognised, then report an error.
               IF ( .NOT. RECOG ) THEN
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', STR( F : L ) )
                  CALL ERR_REP( 'NDF1_PSCPX_BAD',
     :                          'Invalid component name ' //
     :                          '''^BADCOMP'' specified (possible ' //
     :                          'programming error).', STATUS )
               END IF
            END IF
         END IF

*  Increment the pointer to the start of the next list item and return
*  to process it.
         I1 = I2 + 2
         GO TO 1
      END IF
       
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_PSCPX', STATUS )

      END
