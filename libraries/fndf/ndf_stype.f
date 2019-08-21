      SUBROUTINE NDF_STYPE( FTYPE, INDF, COMP, STATUS )
*+
*  Name:
*     NDF_STYPE

*  Purpose:
*     Set a new type for an NDF array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_STYPE( FTYPE, INDF, COMP, STATUS )

*  Description:
*     The routine sets a new full type for an NDF array component,
*     causing its storage type to be changed. If the component's values
*     are defined, they will be converted from from the old type to the
*     new one. If they are undefined, then no conversion will be
*     necessary. Subsequent enquiries will reflect the new type.
*     Conversion may be performed between any types supported by the
*     NDF_ routines, including from a non-complex type to a complex
*     type (and vice versa).

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The new full type specification for the NDF component (e.g.
*        '_REAL' or 'COMPLEX_INTEGER').
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the array component whose type is to be set: 'DATA'
*        or 'VARIANCE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine may only be used to change the type of a component
*     of a base NDF. If it is called for an NDF which is not a base
*     NDF, then it will return without action. No error will result.
*     -  A comma-separated list of component names may also be supplied,
*     in which case the type of each component will be set to the same
*     value in turn.
*     -  An error will result if a component being modified, or any
*     part of it, is currently mapped for access (e.g. through another
*     identifier).
*     -  If the type of a component is to be changed without its values
*     being retained, then a call to NDF_RESET should be made
*     beforehand. This will avoid the cost of converting all the
*     values.

*  Algorithm:
*     -  Check the full data type for validity.
*     -  Import the NDF identifier.
*     -  Ensure that TYPE access to the NDF is available.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Initialise the character pointer to the start of the component
*     list. Then loop to extract each component from the list.
*     -  Find the final character of the next element in the list.
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  Compare the component name with each value in turn, taking the
*     appropriate action or reporting an error message if an
*     inappropriate component name has been given.
*     -  If the component name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process it.
*     -  If no error has occurred, but the number of components
*     processed is zero, then report an error.
*     -  If an error has occurred, then report context information.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1989 (RFWS):
*        Original version.
*     27-NOV-1989 (RFWS):
*        Changed to check for TYPE access, rather than WRITE.
*     28-NOV-1989 (RFWS):
*        Minor change to prologue and the extension error message.
*     6-DEC-1989 (RFWS):
*        Added support for the variance component.
*     6-DEC-1989 (RFWS):
*        Changed the name of the NDF1_VFTP routine to NDF1_CHFTP to
*        avoid a name clash.
*     7-DEC-1989 (RFWS):
*        Enhanced to accept a comma-separated list of components.
*     19-DEC-1989 (RFWS):
*        Added character string subscripts for COMP, which had been
*        omitted. Also fixed missing argument to NDF1_SIMLR.
*     23-JAN-1990 (RFWS):
*        Renamed from NDF_SFTYP to NDF_STYPE.
*     24-JAN-1990 (RFWS):
*        Changed to call ARY_STYPE instead of ARY_SFTYP.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     21-MAR-1990 (RFWS):
*        Strengthened checks on whether the data component is mapped.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_NDMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of mappings to the NDF's data component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a section.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data array is mapped for access.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      CHARACTER * ( * ) FTYPE
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      CHARACTER * ( NDF__SZTYP ) TYPE ! Validated numeric data type
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      LOGICAL CMPLX              ! Whether data type is complex

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the full data type specification for validity.
      CALL NDF1_CHFTP( FTYPE, TYPE, CMPLX, STATUS )

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that TYPE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'TYPE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Initialise the component count.
         NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
         I1 = 1
1        CONTINUE
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( COMP ) ) ) THEN

*  Find the final character of the next element in the component list
*  (the last character before a comma or end of string).
            I2 = INDEX( COMP( I1 : ), ',' )
            IF ( I2 .EQ. 0 ) THEN
               I2 = LEN( COMP )
            ELSE
               I2 = I2 + I1 - 2
            END IF
            IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
               CALL CHR_FANDL( COMP( I1 : I2 ), F, L )
               IF ( L .GE. F ) THEN
                  F = F + I1 - 1
                  L = L + I1 - 1

*  Increment the component count.
                  NCOMP = NCOMP + 1

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component:
*  ==============
*  Report an error, since this component has no data type.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                             NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_AXI',
     :               'A new type cannot be set for an AXIS ' //
     :               'component (possible programming error).', STATUS )

*  DATA component:
*  ==============
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                                  NDF__MINAB ) ) THEN

*  Check that the data array is not already mapped for access through
*  the current ACB entry. Report an error if it is.
                     IF ( ACB_DMAP( IACB ) ) THEN
                        STATUS = NDF__ISMAP
                        CALL NDF1_AMSG( 'NDF', IACB )
                        CALL ERR_REP( 'NDF_STYPE_DMAP',
     :                  'The data component in the NDF structure ' //
     :                  '^NDF is already mapped for access through ' //
     :                  'the specified identifier (possible ' //
     :                  'programming error).', STATUS )

*  Only take further action if this is a base NDF. Check that the data
*  component is not mapped at all. Report an error if it is.
                     ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
                        IF ( DCB_NDMAP( IDCB ) .NE. 0 ) THEN
                           STATUS = NDF__ISMAP
                           CALL NDF1_DMSG( 'NDF', IDCB )
                           CALL ERR_REP( 'NDF_STYPE_DBMAP',
     :                     'The data component in the NDF structure ' //
     :                     '^NDF is already mapped for access ' //
     :                     'through another identifier (possible ' //
     :                     'programming error).', STATUS )

*  Use the ARY_ system to set a new full data type for the data array.
                        ELSE
                           CALL ARY_STYPE( FTYPE, ACB_DID( IACB ),
     :                                     STATUS )
                           CALL NDF1_CMPAC( ACB_IDCB( IACB ), 'DATA',
     :                                      STATUS )
                        END IF
                     END IF

*  EXTENSION:
*  ==========
*  Report an error, since the data type of extensions cannot be set
*  with this routine.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                   NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_EXT',
     :               'A new type cannot be set for an ' //
     :               'EXTENSION (possible programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, since this component has no data type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_HIS',
     :               'A new type cannot be set for a HISTORY ' //
     :               'component (possible programming error).', STATUS )

*  LABEL component:
*  ===============
*  Report an error, since this component has no data type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_LAB',
     :               'A new type cannot be set for a LABEL ' //
     :               'component (possible programming error).', STATUS )

*  QUALITY component:
*  =================
*  Report an error, since this component's data type cannot be changed.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_QUA',
     :               'A new type cannot be set for a QUALITY ' //
     :               'component (possible programming error).', STATUS )

*  TITLE component:
*  ===============
*  Report an error, since this component has no data type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_TIT',
     :               'A new type cannot be set for a TITLE ' //
     :               'component (possible programming error).', STATUS )

*  UNITS component:
*  ===============
*  Report an error, since this component has no data type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_STYPE_UNI',
     :               'A new type cannot be set for a UNITS ' //
     :               'component (possible programming error).', STATUS )

*  VARIANCE component:
*  ==================
*  Set a new data type for the variance component.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_VSFTP( FTYPE, IACB, STATUS )

*  If the component name is not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_STYPE_ERR',
     :                             'Invalid array component name ' //
     :                             '''^BADCOMP'' specified ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  END IF
               END IF
            END IF

*  Increment the character pointer to the start of the next element in
*  the component list and return to process the next element.
            I1 = I2 + 2
            GO TO 1
         END IF

*  If no error has occurred, but no non-blank component names have been
*  processed, then report an error.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
            STATUS = NDF__NOCMP
            CALL ERR_REP( 'NDF_STYPE_NONE',
     :                    'No array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_STYPE_ERR',
     :   'NDF_STYPE: Error setting a new type for an NDF array ' //
     :   'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_STYPE', STATUS )
      END IF

      END
