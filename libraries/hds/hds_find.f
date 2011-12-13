      SUBROUTINE HDS_FIND( LOC1, NAME, MODE, LOC2, STATUS )
*+
*  Name:
*     HDS_FIND
*
*  Purpose:
*     Find an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL HDS_FIND( LOC1, NAME, MODE, LOC2, STATUS )

*  Description:
*     The routine finds an existing HDS object. Its purpose is similar
*     to that of the HDS routine DAT_FIND, except that it permits the
*     component name to contain a series of component fields separated
*     by '.' and also allows dimension bounds expressions (e.g.
*     '(1:2,3,,:6)') to be appended to non-scalar HDS objects in order
*     to select a cell or slice from them. Thus, if all the necessary
*     HDS objects exist, a component name such as
*     'MYSTRUCT.AXIS(2).DATA_ARRAY.DATA(20:)' could be given for this
*     routine to find. If a blank component name is supplied, then the
*     initial locator supplied is simply cloned. If an input locator
*     value of DAT__ROOT is given, then the NAME argument is assumed to
*     contain a complete object specification, including a container
*     file name. The filename can be specified in " " if a non-standard
*     file extension is being used.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator to an existing HDS object (or DAT__ROOT if NAME
*        contains a complete object specification).
*     NAME = CHARACTER * ( * ) (Given)
*        Relative path name of the component to be found within the
*        structure (may be blank, or set to the complete object
*        specification if a value of DAT__ROOT is given for LOC1).
*     MODE = CHARACTER * ( * ) (Given)
*        Mode of access required to the object: 'READ', 'UPDATE' or
*        'WRITE'. This argument is only used if LOC1 is set to
*        DAT__ROOT, otherwise the mode of access is derived from the
*        input locator.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        Locator to the object found. This will be a primary locator if
*        LOC1 was set to DAT__ROOT, otherwise it will be a secondary
*        locator.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then an invalid
*     locator will be returned via the LOC2 argument. The same value
*     will also be returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David Berry (UCLan, Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1990 (RFWS):
*        Original version.
*     12-DEC-1990 (RFWS):
*        Added an additional status check.
*     25-SEP-1991 (RFWS):
*        Permit a dimension bounds expression without a component name
*        in the first field.
*     16-OCT-1991 (RFWS):
*        Permit a '.' as an optional first character if the path name
*        does not start with a dimension bounds expression.
*     23-JUN-1993 (RFWS):
*        Upgraded to accept DAT__ROOT as an input locator value and to
*        return a primary locator in that case. Added the MODE
*        argument.
*     8-OCT-1993 (RFWS):
*        Added status check to protect against possible invalid string
*        subscripts under error conditions. Removed unnecessary status
*        checks elsewhere.
*     15-APR-1994 (RFWS):
*        Added extra arguments to NDF1_FPARX call.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     23-DEC-2005 (TIMJ):
*        Brought into HDS.
*     19-JAN-2006 (TIMJ):
*        Trap "." in section and trigger correct warning. This was causing
*        problems with NDF AXIS sections.
*     27-JAN-2006 (DSB):
*        Check status after calls to DAT_CUT.
*     27-JAN-2006 (TIMJ):
*        Do not need to trap status before calling DAT_ANNUL since
*        DAT_ANNUL was broken.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'DAT_ERR'          ! DAT_ error codes

*  Arguments Given:
      CHARACTER * ( * ) LOC1
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) MODE

*  Arguments Returned:
      CHARACTER * ( * ) LOC2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Temporary locator
      INTEGER DP                 ! Position of '.' in section specification
      INTEGER EP                 ! End posn for search
      INTEGER F                  ! First non-blank component character
      INTEGER F1                 ! First character in file name
      INTEGER F2                 ! Last character in file name
      INTEGER I1                 ! Start of component field
      INTEGER I2                 ! End of component field
      INTEGER IEND               ! Last non-blank path name character
      INTEGER INAME              ! Last component name character
      INTEGER L                  ! Last non-blank component character
      INTEGER LP                 ! Posn. of left parenthesis
      INTEGER NFIELD             ! Number of component name fields
      INTEGER RP                 ! Posn. of right parenthesis
      INTEGER SP                 ! Start posn for search
      LOGICAL AGAIN              ! Loop to process another field?
      LOGICAL DOTTED             ! Path name begins with a '.'?
      LOGICAL THERE              ! Does required component exist?

*.

*  Initialise the returned locator.
      LOC2 = DAT__NOLOC

*  Fix warnings
      DOTTED = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the input locator is DAT__ROOT, then the NAME value contains a
*  complete object name, including a container file name. Split it into
*  its file name and HDS path fields and open the container file.
      IF ( LOC1 .EQ. DAT__ROOT ) THEN
         CALL HDS_SPLIT( NAME, F1, F2, I1, IEND, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL HDS_OPEN( NAME( F1 : F2 ), MODE, LOC2, STATUS )
         END IF

*  Otherwise, clone the input locator and find the first and last
*  non-blank characters in the HDS path string.
      ELSE
         CALL DAT_CLONE( LOC1, LOC2, STATUS )
         CALL CHR_FANDL( NAME, I1, IEND )
      END IF

*  If the HDS path string is not blank, then check to see if it starts
*  with a '.'. If so, then skip over it.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. IEND ) ) THEN
         DOTTED = ( NAME( I1 : I1 ) .EQ. '.' )
         IF ( DOTTED ) I1 = I1 + 1
      END IF

*  In the following code, we assume that "." is the component separator.
*  Since we would like to tell the difference between a bad subsection
*  and any other error, we need to trap the case where the "." lies
*  inside a section. "." is only allowed within an NDF section and not an
*  HDS section. Look for matching parens, then look inside for ".". We do this
*  in a separate loop to overcome the later INDEX on the whole string.

      IF ((STATUS .EQ. SAI__OK) .AND. (I1 .LE. IEND) ) THEN
         AGAIN = .TRUE.
         SP = I1
         EP = IEND
 2       CONTINUE  ! Start of DO WHILE loop

*     Look for parens
         CALL CHR_FPARX( NAME( SP : EP ), '(', ')', LP, RP )

*     Correct the positions for the offset
         LP = LP + SP - 1
         RP = RP + SP - 1

*     Stop looping if we found none
         IF (LP .GT. RP) THEN
            AGAIN = .FALSE.
         ELSE
*     Found something - look for a '.'
            DP = INDEX( NAME(LP:RP), '.')
            IF (DP .NE. 0) THEN
*     Found a '.' so trigger error condition
               STATUS = DAT__SUBIN
               CALL EMS_SETC( 'SECT', NAME(LP:RP))
               CALL EMS_REP( 'HDS_FIND_DOTSUB',
     :              'Invalid section specification. "." is not '//
     :              'allowed in HDS section in ^SECT',
     :              STATUS)
               AGAIN = .FALSE.
            ELSE
*     Increment search location
               SP = RP + 1
               EP = IEND
               IF ( SP .GE. EP ) AGAIN = .FALSE.
            END IF

         END IF

         IF (AGAIN) GO TO 2
      END IF

*  If the HDS path string is still not blank, then loop to extract each
*  HDS component field from the path. Count the fields as they are
*  processed.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( I1 .LE. IEND ) ) THEN
         NFIELD = 0
         AGAIN = .TRUE.
 1       CONTINUE                ! Start of 'DO WHILE' loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND. AGAIN ) THEN
            NFIELD = NFIELD + 1

*  If we are still within the bounds of the name string, then find the
*  end of the next path name field (the last character before a '.' or
*  end of string). Note if a '.' does not terminate the field, in which
*  case there are no more fields to process.
            IF ( I1 .LE. IEND ) THEN
               I2 = INDEX( NAME( I1 : IEND ), '.' )
               IF ( I2 .EQ. 0 ) THEN
                  I2 = IEND
                  AGAIN = .FALSE.
               ELSE
                  I2 = I2 + I1 - 2
               END IF

*  If we are beyond the end of the string, but are making another pass
*  to process the (blank) field following a final '.', then use the
*  string length as the end of the field and note there are no more
*  fields to process.
            ELSE
               I2 = IEND
               AGAIN = .FALSE.
            END IF

*  If the field is missing (two consecutive '.' characters appear or
*  the string finishes with '.') then report an error.
            IF ( I1 .GT. I2 ) THEN
               STATUS = DAT__NOCMP
               CALL EMS_SETC( 'NAME', NAME )
               CALL EMS_REP( 'HDS_FIND_MSF1',
     :                       'Missing field in HDS component name ' //
     :                       '''^NAME''.', STATUS )

*  Find the first and last non-blank characters in the field. Report an
*  error if the field is entirely blank.
            ELSE
               CALL CHR_FANDL( NAME( I1 : I2 ), F, L )
               IF ( F .GT. L ) THEN
                  STATUS = DAT__NOCMP
                  CALL EMS_SETC( 'NAME', NAME )
                  CALL EMS_REP( 'HDS_FIND_MSF2',
     :                          'Missing field in HDS component ' //
     :                          'name ''^NAME''.', STATUS )

*  Search for a parenthesised expression within the field (the dimension
*  bounds expression).
               ELSE
                  F = F + I1 - 1
                  L = L + I1 - 1
                  CALL CHR_FPARX( NAME( F : L ), '(', ')', LP, RP )
                  IF ( LP .LT. RP ) THEN
                     LP = LP + F - 1
                     RP = RP + F - 1

*  Check if there is a component name in front of the opening
*  parenthesis. If not, then report an error unless this is the first
*  field and the path name string did not begin with a '.' (a dimension
*  bounds expression alone is allowed as the first field in the path
*  name).
                     IF ( ( LP .LE. F ) .AND.
     :                    ( DOTTED .OR. ( NFIELD .NE. 1 ) ) ) THEN
                        STATUS = DAT__NOCMP
                        CALL EMS_SETC( 'FIELD', NAME( F : L ) )
                        CALL EMS_REP( 'HDS_FIND_MSN',
     :                                'Missing name in HDS ' //
     :                                'component field ''^FIELD''.',
     :                                STATUS )

*  Check that there are no characters following the closing
*  parenthesis. Report an error if there are.
                     ELSE IF ( RP .NE. L ) THEN
                        STATUS = DAT__SUBIN
                        CALL EMS_SETC( 'FIELD', NAME( F : L ) )
                        CALL EMS_REP( 'HDS_FIND_JUNK',
     :                                'Unknown character(s) ' //
     :                                'following subset expression ' //
     :                                'in HDS component field ' //
     :                                '''^FIELD''.', STATUS )
                     END IF

*  Note where the component name ends (if present).
                     INAME = LP - 1
                  ELSE
                     INAME = L
                  END IF

*  If a dimension bounds expression exists in the absence of a
*  component name (only permitted in the first field), then select the
*  appropriate subset of the object. Promote the resulting locator if
*  necessary.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     IF ( ( LP .LT. RP ) .AND. ( LP .LE. F ) ) THEN
                        CALL DAT_CUT( LOC2, NAME( LP : RP ), LOC,
     :                                  STATUS )
                        IF ( LOC1 .EQ. DAT__ROOT ) THEN
                           CALL DAT_PRMRY( .TRUE., LOC, .TRUE., STATUS )
                        END IF

*  Annul the object locator and replace it with the subset locator.
                        CALL DAT_ANNUL( LOC2, STATUS )
                        LOC2 = LOC
                        LOC = DAT__NOLOC

*  If a component name exists, then check it for validity and see if
*  the required component exists within the current HDS structure.
                     ELSE
                        CALL DAT_CHSCN( NAME( F : INAME ), STATUS )
                        CALL DAT_THERE( LOC2, NAME( F : INAME ), THERE,
     :                                  STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Report an error if the component does not exist.
                           IF ( .NOT. THERE ) THEN
                              STATUS = DAT__NAMIN
                              CALL EMS_SETC( 'NAME', NAME( F : INAME ) )
                              CALL DAT_MSG( 'STRUCT', LOC2 )
                              CALL EMS_REP( 'HDS_FIND_NF',
     :                                      'There is no ''^NAME'' ' //
     :                                      'component in the HDS ' //
     :                                      'structure ^STRUCT',
     :                                      STATUS )

*  Otherwise, locate the required component. Promote the resulting
*  locator if necessary.
                           ELSE
                              CALL DAT_FIND( LOC2, NAME( F : INAME ),
     :                                       LOC, STATUS )
                              IF ( LOC1 .EQ. DAT__ROOT ) THEN
                                 CALL DAT_PRMRY( .TRUE., LOC, .TRUE.,
     :                                           STATUS )
                              END IF

*  Annul the structure locator and replace it with the new object
*  locator.
                              CALL DAT_ANNUL( LOC2, STATUS )
                              LOC2 = LOC
                              LOC = DAT__NOLOC
                           END IF

*  If a dimension bounds expression exists, then select the appropriate
*  subsection from the object. Promote the resulting locator if
*  necessary.
                           IF ( STATUS .EQ. SAI__OK ) THEN
                              IF ( LP .LT. RP ) THEN
                                 CALL DAT_CUT( LOC2, NAME( LP : RP ),
     :                                           LOC, STATUS )
                                 IF ( LOC1 .EQ. DAT__ROOT ) THEN
                                    CALL DAT_PRMRY( .TRUE., LOC, .TRUE.,
     :                                              STATUS )
                                 END IF

*  Annul the object locator and replace it with the subset locator.
                                 CALL DAT_ANNUL( LOC2, STATUS )
                                 LOC2 = LOC
                                 LOC = DAT__NOLOC
                              END IF
                           END IF
                        END IF
                     END IF
                  END IF
               END IF
            END IF

*  Update the character pointer to the next component name field and
*  return to process it.
            I1 = I2 + 2
            GO TO 1
         END IF
      END IF

*  If an error has occurred, then annul the returned locator.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( LOC2, STATUS )
      END IF

      END
