      SUBROUTINE NDF1_NTFOR( FORFIL, IFMT, KEEP, NDFLOC, NDFNAM, LNAM,
     :                       STATUS )
*+
*  Name:
*     NDF1_NTFOR

*  Purpose:
*     Identify a native NDF to be associated with a foreign file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_NTFOR( FORFIL, IFMT, KEEP, NDFLOC, NDFNAM, LNAM,
*                      STATUS )

*  Description:
*     The routine accepts the name of a foreign format file from (or to)
*     which data conversion is to be performed and identifies an NDF
*     object which can be associated with it and used to hold the
*     (converted) native NDF format version of the data. The NDF name is
*     obtained by translating an appropriate environment variable. A
*     default NDF name is supplied if this does not yield a suitable
*     result. The identified NDF is not actually created by this
*     routine.

*  Arguments:
*     FORFIL = CHARACTER * ( * ) (Given)
*        Name of the foreign format file, optionally ending with a foreign
*        extension specifier.
*     IFMT = INTEGER (Given)
*        FCB index of the foreign file format (must be non-zero).
*     KEEP = LOGICAL (Given)
*        If a .TRUE. value is supplied, it indicates that the NDF will
*        be kept. Otherwise it will be a temporary object.
*     NDFLOC = CHARACTER * ( * ) (Returned)
*        Locator which, in conjunction with the NDFNAM value,
*        identifies the NDF. On successful exit, this locator will
*        either have the root locator value DAT__ROOT (in which case
*        NDFNAM contains the full NDF name) or will be an active
*        locator (in which case NDFNAM contains the relative name of
*        the NDF). If active, this locator should be annulled by the
*        caller when no longer required.
*     NDFNAM = CHARACTER * ( * ) (Returned)
*        Absolute or relative HDS name which, in conjunction with the
*        NDFLOC value, identifies the NDF object.
*     LNAM = INTEGER (Returned)
*        Number of characters in the NDFNAM value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then an invalid
*     locator will be returned via the NDFLOC argument. The same value
*     will also be returned if this routine should fail for any reason.

*  Copyright:
*     Copyright (C) 2000 Particle Physics & Astronomy Research Council

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
*     {enter_new_authors_here}

*  History:
*     14-SEP-1994 (RFWS):
*        Original version.
*     17-NOV-1994 (RFWS):
*        Removed code to set message tokens explicitly.
*     {enter_changes_here}

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

*  Global Variables:
      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.
*        FCB_FMT1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of start of each foreign format name.
*        FCB_FMT2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Character positions of end of each foreign format name.

*  Arguments Given:
      CHARACTER * ( * ) FORFIL
      INTEGER IFMT
      LOGICAL KEEP

*  Arguments Returned:
      CHARACTER * ( * ) NDFLOC
      CHARACTER * ( * ) NDFNAM
      INTEGER LNAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( NDF__SZREF ) NAME ! Buffer for raw NDF name
      INTEGER F1                 ! First format name character position
      INTEGER F2                 ! Last format name character position
      LOGICAL DEF                ! Environment variable defined?

*.

*  Set an initial null value for the NDFLOC argument.
      NDFLOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the character positions of the foreign format name in the FCB
*  format list string.
      F1 = FCB_FMT1( IFMT )
      F2 = FCB_FMT2( IFMT )

*  Attempt to translate the appropriate environment variable to obtain
*  the native NDF name.
      IF ( KEEP ) THEN
         CALL NDF1_GTENV( 'NDF_KEEP_' // FCB_FMT( F1 : F2 ), DEF, NAME,
     :                    LNAM, STATUS )
      ELSE
         CALL NDF1_GTENV( 'NDF_TEMP_' // FCB_FMT( F1 : F2 ), DEF, NAME,
     :                    LNAM, STATUS )
      END IF

*  If a non-blank name was obtained, return a root locator value to
*  accompany it.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( LNAM .NE. 0 ) THEN
            NDFLOC = DAT__ROOT

*  Mark the error stack to prevent any interference with previously
*  defined message tokens and define standard message tokens for
*  substitution into the NDF name.
            CALL ERR_MARK
            CALL NDF1_CVTOK( FORFIL, IFMT, DAT__ROOT, ' ', STATUS )

*  Substitute these token values into the NDF name, returning the
*  result and its length. Use a low-level (EMS) routine to ensure the
*  message text supplied is used without change.
            CALL EMS_MLOAD( ' ', NAME( : LNAM ), NDFNAM, LNAM, STATUS )

*  Release the error stack.
            CALL ERR_RLSE
         END IF
      END IF

*  If a (non-blank) NDF name was not obtained, then obtain a default
*  locator and name instead.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( LNAM .EQ. 0 ) THEN
            CALL NDF1_DNFOR( FORFIL, IFMT, KEEP, NDFLOC, NDFNAM, LNAM,
     :                       STATUS )
         END IF
      END IF

*  If an error occurred, then return an invalid locator value.
      IF ( STATUS .NE. SAI__OK ) THEN
         NDFLOC = DAT__NOLOC
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_NTFOR', STATUS )

      END
