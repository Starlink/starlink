      SUBROUTINE NDF1_SPFOR( FNAME, IFMT, D1, D2, N1, N2, T1, T2,
     :                       V1, V2, X1, X2, STATUS )
*+
*  Name:
*     NDF1_SPFOR

*  Purpose:
*     Split a foreign format file name into its components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_SPFOR( FNAME, IFMT D1, D2, N1, N2, T1, T2, V1, V2,
*                      X1, X2, STATUS )

*  Description:
*     The routine splits a full foreign format file name into a
*     directory field, a name field, a type field (which contains a
*     leading '.'), a version field and a foreign extension field and
*     returns the character positions of the start and end of each field.

*  Arguments:
*     FNAME = CHARACTER * ( * ) (Given)
*        The file specification.
*     IFMT = INTEGER (Given)
*        The foreign file format code (must be non-zero).
*     D1 = INTEGER (Returned)
*        The position of the first character in the directory field.
*     D2 = INTEGER (Returned)
*        The position of the last character in the directory field.
*     N1 = INTEGER (Returned)
*        The position of the first character in the file name field.
*     N2 = INTEGER (Returned)
*        The position of the last character in the file name field.
*     T1 = INTEGER (Returned)
*        The position of the first character in the type field.
*     T2 = INTEGER (Returned)
*        The position of the last character in the type field.
*     V1 = INTEGER (Returned)
*        The position of the first character in the version field.
*     V2 = INTEGER (Returned)
*        The position of the last character in the version field.
*     X1 = INTEGER (Returned)
*        The position of the first character in the foreign extension field.
*     X2 = INTEGER (Returned)
*        The position of the last character in the foreign extension field.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine should be used in preference to NDF1_FSPLT
*     whenever a foreign format file name is being processed as it will
*     permit the '.' character to occur in the file extension field if
*     necessary (and so long as the actual type field present in the
*     file name matches that expected for the specified foreign format).
*     -  If the supplied file name contains no directory field, then D2
*     is returned less than D1, if it contains no name field, then N2
*     is returned less than N1, if it contains no type field, then
*     T2 is returned less than T1, if it contains no version field,
*     then V2 is returned less than V1, and if it contains no foreign
*     extension field, then X2 is returned less than X1.
*     -  The restrictions that apply to the routine NDF1_FSPLT relating
*     to the expansion of file name components also apply to this
*     routine.

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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-NOV-1994 (RFWS):
*        Original version.
*     15-NOV-1994 (RFWS):
*        Made adjustment of type field limits conditional on the
*        expected type field being present.
*     17-JUL-2000 (DSB):
*        Added arguments X1 and X2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_FCB'          ! NDF_ Format Conversion Block
*        FCB_FEX1( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Starting positions of each foreign format file name
*           extension in the FCB_FMT string.
*        FCB_FEX2( 2 * NDF__MXFMT ) = INTEGER (Read)
*           Ending positions of each foreign format file name extension
*           in the FCB_FMT string.
*        FCB_FMT = CHARACTER * ( 2 * NDF__SZFMT ) (Read)
*           Foreign format list string.

*  Arguments Given:
      CHARACTER * ( * ) FNAME
      INTEGER IFMT

*  Arguments Returned:
      INTEGER D1
      INTEGER D2
      INTEGER N1
      INTEGER N2
      INTEGER T1
      INTEGER T2
      INTEGER V1
      INTEGER V2
      INTEGER X1
      INTEGER X2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F1                 ! First character of file extension
      INTEGER F2                 ! Last character of file extension
      INTEGER TMIN               ! Anticipated start of type field
      LOGICAL FOUND              ! Expected file extension present?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate any foreign extension specifier. X1 is returned equal to one
*  more than the used length of the string if no foreign extension specifier
*  is present.
      CALL NDF1_FORXT( FNAME, X1, X2, STATUS )

*  Split the preceeding file name into its directory, name, type and
*  version fields, using the file name syntax rules for the host machine.
      CALL NDF1_FSPLT( FNAME( : X1 - 1 ), D1, D2, N1, N2, T1, T2, V1,
     :                 V2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If a file type extension appears to be present, then obtain the
*  character string limits for the expected file extension in the FCB
*  format list string.
         IF ( T2 .GE. T1 ) THEN
            F1 = FCB_FEX1( IFMT )
            F2 = FCB_FEX2( IFMT )

*  Since the file extension may contain a '.' character, it may actually
*  be longer than identified above (i.e. the end of the name field may
*  still contain the first part of the file extension). Find the first
*  character position at which the full file extension field could
*  start (allowing it to extend into the name field, if present, but not
*  into the directory field).
            TMIN = T1
            IF ( N2 .GE. N1 ) TMIN = N1

*  Adjust the anticipated starting position for the expected file
*  extension, given by the file's format code.
            TMIN = MIN( MAX( TMIN, T2 - ( F2 - F1 ) ), T1 )

*  Test if the expected file extension is present (extending into the
*  name field if necessary).
            CALL NDF1_CMPFL( FNAME( TMIN : T2 ), FCB_FMT( F1 : F2 ),
     :                       FOUND, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  If so, then correct the name and type field limits to identify it.
               IF ( FOUND ) THEN
                  T1 = TMIN
                  IF ( N2 .GE. N1 ) N2 = MIN( N2, T1 - 1 )
               END IF
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_SPFOR', STATUS )

      END
