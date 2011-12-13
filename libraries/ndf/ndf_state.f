      SUBROUTINE NDF_STATE( INDF, COMP, STATE, STATUS )
*+
*  Name:
*     NDF_STATE

*  Purpose:
*     Determine the state of an NDF component (defined or undefined).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_STATE( INDF, COMP, STATE, STATUS )

*  Description:
*     The routine returns a logical value indicating whether an NDF
*     component has a defined value (or values).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the component; any NDF component name is valid.
*     STATE = LOGICAL (Returned)
*        Whether the specified component is defined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If a component name of 'EXTENSION' is given, then a .TRUE.
*     result will be returned if one or more extensions are present in
*     the NDF.
*     -  A comma-separated list of component names may also be given,
*     in which case the routine will return the logical "AND" of the
*     states of the specified components (i.e. a .TRUE. result will be
*     returned only if all the components have defined values).

*  Copyright:
*     Copyright (C) 1989-1990, 993, 1997 Rutherford Appleton Laboratory
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-NOV-1989 (RFWS):
*        Original version.
*     8-DEC-1989 (RFWS):
*        Added support for the variance component.
*     11-JAN-1990 (RFWS):
*        Added support for comma-separated component lists.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     16-OCT-1990 (RFWS):
*        Installed support for the axis component.
*     5-MAY-1993 (RFWS):
*        Installed support for the HISTORY component.
*     30-JUN-1997 (RFWS):
*        Installed support for the WCS component.
*     03-MAY-2006 (TIMJ):
*        Initialise the return value.
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
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read)
*           Locators to NDF character components.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      LOGICAL STATE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      INTEGER NEXTN              ! Number of NDF extensions present

*.

*  Make sure that we return an initialised value
      STATE = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the result and the component count.
         STATE = .TRUE.
         NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
         I1 = 1
1        CONTINUE                ! Start of "DO WHILE" loop
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
*  Obtain an index to the data object entry in the DCB and ensure that
*  axis structure information is available.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                             NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        IDCB = ACB_IDCB( IACB )
                        CALL NDF1_DA( IDCB, STATUS )

*  Use the locator to the first axis structure element to determine the
*  state.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           STATE = DCB_ALOC( 1, IDCB ) .NE. DAT__NOLOC
                        END IF
                     END IF

*  DATA component:
*  ==============
*  Use the ARY_ system to enquire about the state of the NDF's data
*  array.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        CALL ARY_STATE( ACB_DID( IACB ), STATE, STATUS )
                     END IF

*  EXTENSION component:
*  ===================
*  Obtain an index to the data object entry in the DCB and ensure that
*  extension (MORE) component information is available.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        IDCB = ACB_IDCB( IACB )
                        CALL NDF1_DX( IDCB, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Test if the extension (MORE) structure exists.
                           STATE = DCB_XLOC( IDCB ) .NE. DAT__NOLOC
                           IF ( STATE ) THEN

*  If so, then see how many components it has. Set STATE to .TRUE. only
*  if there is at least one component.
                              CALL DAT_NCOMP( DCB_XLOC( IDCB ), NEXTN,
     :                                       STATUS )
                              IF ( STATUS .EQ. SAI__OK ) THEN
                                 STATE = NEXTN .GT. 0
                              END IF
                           END IF
                        END IF
                     END IF

*  HISTORY component:
*  =================
*  Obtain an index to the data object entry in the DCB and ensure that
*  history structure information is available.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        IDCB = ACB_IDCB( IACB )
                        CALL NDF1_DH( IDCB, STATUS )

*  Use the component locator to determine the state.
                        IF ( STATUS .EQ. SAI__OK ) THEN
                           STATE = DCB_HLOC( IDCB ) .NE. DAT__NOLOC
                        END IF
                     END IF

*  LABEL component:
*  ===============
*  Obtain an index to the data object entry in the DCB.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        IDCB = ACB_IDCB( IACB )

*  Ensure that character component information for the label is
*  available in the DCB.
                        CALL NDF1_DC( IDCB, NDF__LABEL, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Use the component locator to determine the state.
                           STATE = DCB_CLOC( NDF__LABEL, IDCB ) .NE.
     :                             DAT__NOLOC
                        END IF
                     END IF

*  QUALITY component:
*  ==================
*  Inspect the component to determine its state.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        CALL NDF1_QSTA( IACB, STATE, STATUS )
                     END IF

*  TITLE component:
*  ===============
*  Obtain an index to the data object entry in the DCB.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        IDCB = ACB_IDCB( IACB )

*  Ensure that character component information for the title is
*  available in the DCB.
                        CALL NDF1_DC( IDCB, NDF__TITLE, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Use the component locator to determine the state.
                           STATE = DCB_CLOC( NDF__TITLE, IDCB ) .NE.
     :                             DAT__NOLOC
                        END IF
                     END IF

*  UNITS component:
*  ===============
*  Obtain an index to the data object entry in the DCB.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        IDCB = ACB_IDCB( IACB )

*  Ensure that character component information for the units is
*  available in the DCB.
                        CALL NDF1_DC( IDCB, NDF__UNITS, STATUS )
                        IF ( STATUS .EQ. SAI__OK ) THEN

*  Use the component locator to determine the state.
                           STATE = DCB_CLOC( NDF__UNITS, IDCB ) .NE.
     :                             DAT__NOLOC
                        END IF
                     END IF

*  VARIANCE component:
*  ==================
*  Inspect the component to determine its state.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        CALL NDF1_VSTA( IACB, STATE, STATUS )
                     END IF

*  WCS component:
*  ==============
*  Inspect the component to determine its state.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WCS',
     :                                  NDF__MINAB ) ) THEN
                     IF ( STATE ) THEN
                        CALL NDF1_WSTA( IACB, STATE, STATUS )
                     END IF

*  If the component name was not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_STATE_COMP',
     :                             'Invalid component name ' //
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
            CALL ERR_REP( 'NDF_STATE_NONE',
     :                    'No component name specified (possible ' //
     :                    'programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_STATE_ERR',
     :   'NDF_STATE: Error determining the state of an NDF ' //
     :   'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_STATE', STATUS )
      END IF

      END
