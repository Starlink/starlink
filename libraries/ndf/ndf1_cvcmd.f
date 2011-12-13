      SUBROUTINE NDF1_CVCMD( FORFIL, IFMT, NDFLOC, NDFNAM, FROM, REPORT,
     :                       DEF, CMD, LCMD, STATUS )
*+
*  Name:
*     NDF1_CVFOR

*  Purpose:
*     Get a data format conversion command for a foreign file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVCMD( FORFIL, IFMT, NDFLOC, NDFNAM, FROM, REPORT,
*                      DEF, CMD, LCMD, STATUS )

*  Description:
*     The routine returns the conversion command which converts between
*     a foreign format data file and a native format NDF object (or
*     vice versa) by translating the appropriate environment variable.

*  Arguments:
*     FORFIL = CHARACTER * ( * ) (Given)
*        Name of the foreign format file, optionally containing a foreign
*        extension specifier.
*     IFMT = INTEGER (Given)
*        FCB code identifying the format of the foreign file (must be
*        non-zero).
*     NDFLOC = CHARACTER * ( * ) (Given)
*        Locator which, in conjunction with the NDFNAM argument,
*        identifies the native format NDF object. If a value of
*        DAT__ROOT is given, then NDFNAM should contain the absolute
*        name of this object.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        Relative HDS name of the native format NDF object (or the
*        absolute name if NDFLOC is set to DAT__ROOT).
*     FROM = LOGICAL (Given)
*        If a .TRUE. value is given, conversion is from the foreign
*        format to native NDF format. Otherwise the reverse conversion
*        is performed.
*     REPORT = LOGICAL (Given)
*        If a .TRUE. value is given, then an error is reported if the
*        conversion command is not defined.
*     DEF = LOGICAL (Returned)
*        .TRUE. if the requested conversion command is defined.
*     CMD = CHARACTER * ( ) (Returned)
*        The conversion command. The supplied character variable should be at
*        least NDF__SZCVT characters long.
*     LCMD = INTEGER (Returned)
*        Number of characters written to CMD.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2005 Central Laboratories of the Research Councils

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     9-FEB-2005 (DSB):
*        Original version.
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

*  Global Variables:
      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format name.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format name.

      INCLUDE 'NDF_TCB'          ! NDF_ Tuning Control Block
*        TCB_SHCVT = LOGICAL (Read)
*           Whether to display information about data conversion
*           operations.

*  Arguments Given:
      CHARACTER * ( * ) FORFIL
      INTEGER IFMT
      CHARACTER * ( * ) NDFLOC
      CHARACTER * ( * ) NDFNAM
      LOGICAL FROM
      LOGICAL REPORT

*  Arguments Returned:
      LOGICAL DEF
      CHARACTER * ( * ) CMD
      INTEGER LCMD

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TOPLOC ! Top level locator
      CHARACTER * ( NDF__SZCVT ) CVT ! Translated command text
      INTEGER F1                 ! First format name character position
      INTEGER F2                 ! Last format name character position
      INTEGER LCVT               ! Length of converted text

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the TCB is initialised.
      CALL NDF1_INTCB( STATUS )

*  Obtain the character positions of the foreign format name in the FCB
*  format list string.
      F1 = FCB_FMT1( IFMT )
      F2 = FCB_FMT2( IFMT )

*  Attempt to translate the appropriate environment variable to obtain
*  the conversion command.
      IF ( FROM ) THEN
         CALL NDF1_GTENV( 'NDF_FROM_' // FCB_FMT( F1 : F2 ), DEF,
     :                    CMD, LCMD, STATUS )
      ELSE
         CALL NDF1_GTENV( 'NDF_TO_' // FCB_FMT( F1 : F2 ), DEF,
     :                    CMD, LCMD, STATUS )
      END IF

*  If no command was defined (or it was blank), then we must report an
*  error (but only if requested).
      IF ( STATUS .EQ. SAI__OK .AND. REPORT ) THEN
         IF ( LCMD .EQ. 0 ) THEN

*  Set STATUS and define the required message tokens.
            STATUS = NDF__NOCVT
            CALL MSG_SETC( 'FILE', FORFIL )
            CALL MSG_SETC( 'FMT', FCB_FMT( F1 : F2 ) )
            IF ( NDFLOC .EQ. DAT__ROOT ) THEN
               CALL MSG_SETC( 'NDF', NDFNAM )
            ELSE
               CALL DAT_MSG( 'NDF', NDFLOC )
               IF ( NDFNAM .NE. ' ' ) THEN
                  CALL MSG_SETC( 'NDF', '.' )
                  CALL MSG_SETC( 'NDF', NDFNAM )
               END IF
            END IF

*  Adapt the error message to the direction of format conversion.
            IF ( FROM ) THEN
               CALL ERR_REP( 'NDF1_CVFOR_CMD1',
     :              'Unable to convert the ^FMT format ' //
     :              'file ''^FILE'' to NDF format in the object ^NDF.',
     :              STATUS )
               CALL MSG_RENEW
               CALL ERR_REP( 'NDF1_CVFOR_CMD2',
     :              'The NDF_FROM_^FMT environment variable does ' //
     :              'not contain a suitable conversion command.',
     :              STATUS )
            ELSE
               CALL ERR_REP( 'NDF1_CVFOR_CMD3',
     :              'Unable to convert the NDF format object ^NDF ' //
     :              'to ^FMT format in the file ''^FILE''.',
     :              STATUS )
               CALL MSG_RENEW
               CALL ERR_REP( 'NDF1_CVFOR_CMD4',
     :              'The NDF_TO_^FMT environment variable does not ' //
     :              'contain a suitable conversion command.',
     :              STATUS )
            END IF

         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_CVCMD', STATUS )

      END
