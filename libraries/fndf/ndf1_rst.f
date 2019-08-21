      SUBROUTINE NDF1_RST( IACB, COMP, STATUS )
*+
*  Name:
*     NDF1_RST

*  Purpose:
*     Reset an NDF component to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_RST( IACB, COMP, STATUS )

*  Description:
*     The routine resets a component of an NDF so that its value
*     becomes undefined. It may be used to remove unwanted optional NDF
*     components. The NDF is identified by its ACB entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        ACB entry for the NDF to be reset.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF component to be reset; any NDF component name
*        is valid. No error will result if the component is already
*        undefined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied
*     in which case each component will be reset in turn.
*     -  Specifying a component name of '*' will cause all components,
*     except for HISTORY and extensions, to be reset.

*  Copyright:
*     Copyright (C) 1997 Rutherford Appleton Laboratory

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
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1989 (RFWS):
*        Original version.
*     20-OCT-1989 (RFWS):
*        Added check that write access to the NDF is available and that
*        the DATA component is not mapped for access.
*     8-DEC-1989 (RFWS):
*        Installed proper support for the variance component.
*     11-JAN-1990 (RFWS):
*        Added support for comma-separated component lists and fixed a
*        bug in the logic of the previous version.
*     12-JAN-1990 (RFWS):
*        Re-located error report generation to avoid an invalid
*        character string subscript.
*     30-JAN-1990 (RFWS):
*        Installed proper support for the quality component.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character concatenation.
*     19-MAR-1990 (RFWS):
*        Renamed from NDF_RESET to NDF1_RST and slightly re-structured
*        to use an ACB index to identify the NDF, to facilitate use by
*        other NDF_ routines.
*     21-MAR-1990 (RFWS):
*        Strengthened checks on whether the NDF's data component is
*        mapped for access.
*     16-OCT-1990 (RFWS):
*        Installed proper resetting of the axis component.
*     5-MAY-1993 (RFWS):
*        Installed proper resetting of the history component.
*     2-JUN-1993 (RFWS):
*        Clear DCB history information after resetting the history
*        component.
*     11-JUL-1997 (RFWS):
*        Added support for the WCS component.
*     23-JAN-2009 (DSB):
*        Added DCB_HTIME and DCB_HSORT.
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
      INCLUDE 'AST_PAR'          ! AST_ public interface

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read and Write)
*           Locators to NDF character components.
*        DCB_HDEF( NDF__MXDCB ) = LOGICAL (Write)
*           Whether default history information is to be written.
*        DCB_HEXT( NDF__MXDCB ) = INTEGER (Write)
*           Extension increment for the history records array.
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator for NDF history component.
*        DCB_HSORT( NDF__MXDCB ) = LOGICAL (Write)
*           Do the history records need sorting?
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Write)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator for array of history records.
*        DCB_HTIME( NDF__MXDCB ) = DOUBLE PRECISION (Write)
*           The date/time to attach to the next history record to be
*           created, as a UTC Modified Julian Date. If negative, then
*           the current time will be used.
*        DCB_HTLEN( NDF__MXDCB ) = LOGICAL (Write)
*           History current record text length.
*        DCB_HUMOD( NDF__MXDCB ) = INTEGER (Read)
*           History recording update mode.
*        DCB_IWCS( NDF__MXDCB ) = INTEGER (Read and Write)
*           Pointer to AST_ WCS information.
*        DCB_KC( NDF__MXDCB ) = LOGICAL (Write)
*           Whether character component information is available.
*        DCB_KH( NDF__MXDCB ) = LOGICAL (Write)
*           Whether DCB information is available for the NDF's history
*           component.
*        DCB_KX( NDF__MXDCB ) = LOGICAL (Write)
*           Whether extension (MORE) structure information is available.
*        DCB_LOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.
*        DCB_NDMAP( NDF__MXDCB ) = INTEGER (Read)
*           Number of mappings to the NDF's data component.
*        DCB_XLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read and
*        Write)
*           Locator to extension (MORE) structure.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a section.
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.
*        ACB_DMAP( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF's data component is mapped for access.
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified
      LOGICAL RECOG              ! Whether component name is recognised
      LOGICAL THERE              ! Whether data object component exists

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  Initialise the component count.
      NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
      I1 = 1
1     CONTINUE                   ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( COMP ) ) ) THEN

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

*  Increment the component count and initialise the component
*  recognition flag.
               NCOMP = NCOMP + 1
               RECOG = .FALSE.

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component.
*  ==============
*  Reset the axis component, along with all its sub-components.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_ARST( IACB, STATUS )
               END IF

*  DATA component.
*  ==============
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'DATA',
     :              NDF__MINAB ) ) THEN
                  RECOG = .TRUE.

*  If the DATA component is to be reset, then check that it is not
*  mapped for access through the current ACB entry. Report an error if
*  it is.
                  IF ( ACB_DMAP( IACB ) ) THEN
                     STATUS = NDF__ISMAP
                     CALL NDF1_AMSG( 'NDF', IACB )
                     CALL ERR_REP( 'NDF1_RST_DMAP',
     :               'The data component in the NDF structure ^NDF ' //
     :               'is already mapped for access through the ' //
     :               'specified identifier (possible programming ' //
     :               'error).', STATUS )

*  Take no further action unless this is a base NDF. Check that the data
*  component is not mapped at all. Report an error if it is.
                  ELSE IF ( .NOT. ACB_CUT( IACB ) ) THEN
                     IF ( DCB_NDMAP( IDCB ) .NE. 0 ) THEN
                        STATUS = NDF__ISMAP
                        CALL NDF1_DMSG( 'NDF', IDCB )
                        CALL ERR_REP( 'NDF1_RST_DBMAP',
     :                  'The data component in the NDF structure ' //
     :                  '^NDF is already mapped for access through ' //
     :                  'another identifier (possible programming ' //
     :                  'error).', STATUS )

*  Reset the ARY_ system identifier for the NDF's data array held in
*  the ACB.
                     ELSE
                        CALL ARY_RESET( ACB_DID( IACB ), STATUS )
                        CALL NDF1_CMPAC( ACB_IDCB( IACB ), 'DATA',
     :                                   STATUS )
                     END IF
                  END IF
               END IF

*  EXTENSION (MORE) component.
*  ==========================
*  If the EXTENSION component is to be reset, then ensure that
*  information about it is available in the DCB.
               IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_DX( IDCB, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If an extension (MORE) component exists, then erase it along with all
*  its contents. Note whether information is still available in the DCB.
                     IF ( DCB_XLOC( IDCB ) .NE. DAT__NOLOC ) THEN
                        CALL NDF1_ANTMP( DCB_XLOC( IDCB ), STATUS )
                        DCB_KX( IDCB ) = STATUS .EQ. SAI__OK
                     END IF
                  END IF
               END IF

*  HISTORY component.
*  =================
*  If the HISTORY component is to be reset, then ensure that history
*  structure information is available in the DCB.
               IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_DH( IDCB, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a history component exists, then annul the associated DCB
*  locators and erase the structure.
                     IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN
                        CALL DAT_ANNUL( DCB_HRLOC( IDCB ), STATUS )
                        CALL DAT_ANNUL( DCB_HLOC( IDCB ), STATUS )
                        CALL DAT_ERASE( DCB_LOC( IDCB ), 'HISTORY',
     :                                  STATUS )

*  Clear the DCB history information.
                        DCB_HDEF( IDCB ) = .TRUE.
                        DCB_HEXT( IDCB ) = 5
                        DCB_HSORT( IDCB ) = .FALSE.
                        DCB_HNREC( IDCB ) = 0
                        DCB_HTLEN( IDCB ) = 0
                        DCB_HUMOD( IDCB ) = NDF__HNORM
                        DCB_HTIME( IDCB ) = -1.0D0

*  Note whether information is still available in the DCB.
                        DCB_KH( IDCB ) = STATUS .EQ. SAI__OK
                     END IF
                  END IF
               END IF

*  LABEL component.
*  ===============
*  If the LABEL component is to be reset, then ensure that information
*  about it is available in the DCB.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_DC( IDCB, NDF__LABEL, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component exists, then erase it. Note whether information is
*  still available in the DCB.
                     IF ( DCB_CLOC( NDF__LABEL, IDCB ) .NE.
     :                    DAT__NOLOC ) THEN
                        CALL NDF1_ANTMP( DCB_CLOC( NDF__LABEL, IDCB ),
     :                                   STATUS )
                        DCB_KC( NDF__LABEL, IDCB ) = STATUS .EQ. SAI__OK
                     END IF
                  END IF
               END IF

*  QUALITY component.
*  ==================
*  Reset the component.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_QRST( IACB, STATUS )
               END IF

*  TITLE component.
*  ===============
*  If the TITLE component is to be reset, then ensure that information
*  about it is available in the DCB.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_DC( IDCB, NDF__TITLE, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component exists, than erase it. Note whether information is
*  still available in the DCB.
                     IF ( DCB_CLOC( NDF__TITLE, IDCB ) .NE.
     :                    DAT__NOLOC ) THEN
                        CALL NDF1_ANTMP( DCB_CLOC( NDF__TITLE, IDCB ),
     :                                   STATUS )
                        DCB_KC( NDF__TITLE, IDCB ) = STATUS .EQ. SAI__OK
                     END IF
                  END IF
               END IF

*  UNITS component.
*  ===============
*  If the UNITS component is to be reset, then ensure that information
*  about it is available in the DCB.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_DC( IDCB, NDF__UNITS, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component exists, then erase it. Note whether information is
*  still available in the DCB.
                     IF ( DCB_CLOC( NDF__UNITS, IDCB ) .NE.
     :                    DAT__NOLOC ) THEN
                        CALL NDF1_ANTMP( DCB_CLOC( NDF__UNITS, IDCB ),
     :                                   STATUS )
                        DCB_KC( NDF__UNITS, IDCB ) = STATUS .EQ. SAI__OK
                     END IF
                  END IF
               END IF

*  VARIANCE component.
*  ==================
*  Reset the component.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_VRST( IACB, STATUS )
               END IF

*  WCS component.
*  ==============
*  If the WCS component is to be reset, then ensure that information
*  about it is available in the DCB.
               IF ( ( COMP( F : L ) .EQ. '*' ) .OR.
     :              NDF1_SIMLR( COMP( F : L ), 'WCS',
     :                          NDF__MINAB ) ) THEN
                  RECOG = .TRUE.
                  CALL NDF1_DW( IDCB, STATUS )
                  IF ( STATUS .EQ. SAI__OK ) THEN

*  If a pointer to WCS information is held in the DCB, then annul it
*  and erase the associated data structure component.
                     IF ( DCB_IWCS( IDCB ) .NE. AST__NULL ) THEN
                        CALL AST_ANNUL( DCB_IWCS( IDCB ), STATUS )
                        CALL DAT_ERASE( DCB_LOC( IDCB ), 'WCS', STATUS )
                     END IF
                  END IF
               END IF

*  If the NDF component name was not recognised, then report an error.
               IF ( .NOT. RECOG ) THEN
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                  CALL ERR_REP( 'NDF1_RST_COMP',
     :                          'Invalid component name ' //
     :                          '''^BADCOMP'' specified (possible ' //
     :                          'programming error).', STATUS )
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
         CALL ERR_REP( 'NDF1_RST_NONE',
     :                 'No component name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_RST', STATUS )

      END
