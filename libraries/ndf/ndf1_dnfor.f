      SUBROUTINE NDF1_DNFOR( FORFIL, IFMT, KEEP, NDFLOC, NDFNAM, LNAM,
     :                       STATUS )
*+
*  Name:
*     NDF1_DNFOR

*  Purpose:
*     Identify a default NDF to be associated with a foreign file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_DNFOR( FORFIL, IFMT, KEEP, NDFLOC, NDFNAM, LNAM,
*                      STATUS )

*  Description:
*     The routine accepts the name of a foreign format file from (or to)
*     which data conversion is to be performed and identifies a default
*     NDF object which can be associated with it and used to hold the
*     (converted) native NDF format version of the data. The NDF is
*     chosen either to be a new NDF file in the current default
*     directory with a name field matching that of the foreign file
*     itself, or a temporary object in the HDS scratch file. The
*     identified NDF is not actually created by this routine.

*  Arguments:
*     FORFIL = CHARACTER * ( * ) (Given)
*        Name of the foreign format file, optionally including a foreign
*        extension specifier.
*     IFMT = INTEGER (Given)
*        FCB index of the foreign file format (must be non-zero if KEEP
*        is .TRUE.).
*     KEEP = LOGICAL (Given)
*        If a .TRUE. value is supplied, it indicates that the NDF will
*        be kept, so it should reside in a file in the default
*        directory. Otherwise, it will reside in the HDS scratch file.
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
*     Copyright (C) 2000 Science & Engineering Research Council

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-OCT-1993 (RFWS):
*        Original version.
*     15-OCT-1993 (RFWS):
*        Complete re-write to use a scratch file when appropriate.
*     21-OCT-1993 (RFWS):
*        Changed to locate temporary NDFs in the standard HDS scratch
*        file.
*     11-NOV-1993 (RFWS):
*        Do not worry if an NDF of the same name already exists.
*     14-SEP-1994 (RFWS):
*        Simplified, to be used for providing a default NDF name only.
*     4-NOV-1994 (RFWS):
*        Allow for foreign file extensions with '.' characters in them.
*     17-JUL-2000 (DSB):
*        Allow for foreign extension specifiers in FORFIL.
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
      LOGICAL CHR_ISALM          ! Is a character alphanumeric?

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) TMPLOC ! Locator to temporary structure
      CHARACTER * ( NDF__SZREF ) NAME ! Buffer for constructing name
      INTEGER COUNT              ! Number of temporary components
      INTEGER D1                 ! First character of directory field
      INTEGER D2                 ! Last character of directory field
      INTEGER DIM( 1 )           ! Dummy dimension array
      INTEGER I                  ! Character index
      INTEGER N1                 ! First character of name field
      INTEGER N2                 ! Last character of name field
      INTEGER T1                 ! First character of type field
      INTEGER T2                 ! Last character of type field
      INTEGER V1                 ! First character of version field
      INTEGER V2                 ! Last character of version field
      INTEGER X1                 ! First character of for. extension field
      INTEGER X2                 ! Last character of for. extension field

      SAVE COUNT                 ! Remember number of temp. components
      SAVE TMPLOC                ! Save locator to temporary file

*  Local Data:
      DATA COUNT  / 0 /          ! Initially no temporary components
      DATA TMPLOC / DAT__NOLOC / ! Initial null temporary locator

*.

*  Set an initial null value for the NDFLOC argument.
      NDFLOC = DAT__NOLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Permanent NDF.
*  =============
*  If the NDF is to be saved, then it should reside in the default
*  directory, so return a root locator value.
      IF ( KEEP ) THEN
         NDFLOC = DAT__ROOT

*  Split the foreign file name into directory, name, type, version and
*  foreign extension fields.
         CALL NDF1_SPFOR( FORFIL, IFMT, D1, D2, N1, N2, T1, T2, V1, V2,
     :                    X1, X2, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Omit any directory specification (the default will be used) and
*  form the file name from the name and foreign extension fields of the
*  foreign file (if present). Replace any non-alphanumeric characters in
*  the foreign extension specifier with underscores, to ensure the
*  resulting NDF name is legal.
            LNAM = 0
            IF ( N1 .LE. N2 ) THEN
               CALL CHR_PUTC( FORFIL( N1 : N2 ), NAME, LNAM )
            END IF

            IF ( X1 .LE. X2 ) THEN
               DO I = X1, X2
                  IF( CHR_ISALM( FORFIL( I : I ) ) ) THEN
                     CALL CHR_PUTC( FORFIL( I : I ), NAME, LNAM )
                  ELSE
                     CALL CHR_PUTC( '_', NAME, LNAM )
                  END IF
               END DO
            END IF

*  Return the NDF file name and its length, if this is not zero.
            IF ( LNAM .GT. 0 ) THEN
               CALL NDF1_CCPY( NAME( : LNAM ), NDFNAM, STATUS )

*  If the name consists of just a type field (unusual, but possible)
*  then this must be returned enclosed in quotes to prevent a
*  completely blank name resulting.
            ELSE
               LNAM = 6
               CALL NDF1_CCPY( '".sdf"', NDFNAM, STATUS )
            END IF
         END IF

*  Temporary NDF.
*  =============
*  If the NDF is not to be kept, then it can be a temporary object.
      ELSE

*  If no temporary HDS structure has yet been created, then create one
*  to contain the temporary NDF object.
         IF ( TMPLOC .EQ. DAT__NOLOC ) THEN
            CALL DAT_TEMP( 'NDF_CVT_AREA', 0, DIM, TMPLOC, STATUS )
         END IF

*  Clone and return a locator for the temporary structure.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_CLONE( TMPLOC, NDFLOC, STATUS )

*  Increment the count of components within the temporary structure.
            IF ( STATUS .EQ. SAI__OK ) THEN
               COUNT = COUNT + 1

*  Create and return the name of a new component.
               LNAM = 0
               CALL CHR_PUTC( 'NDF_', NAME, LNAM )
               CALL CHR_PUTI( COUNT, NAME, LNAM )
               CALL NDF1_CCPY( NAME( : LNAM ), NDFNAM, STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then return an invalid locator value.
      IF ( STATUS .NE. SAI__OK ) THEN
         NDFLOC = DAT__NOLOC
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_DNFOR', STATUS )

      END
