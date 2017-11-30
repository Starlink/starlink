      SUBROUTINE NDF1_CVTOK( FORFIL, IFMT, NDFLOC, NDFNAM, STATUS )
*+
*  Name:
*     NDF1_CVTOK

*  Purpose:
*     Define standard message tokens for use in conversion commands.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_CVTOK( FORFIL, IFMT, NDFNAM, NDFLOC, STATUS )

*  Description:
*     The routine defines a standard set of message tokens to hold
*     information about the datasets involved in foreign file conversion
*     operations.

*  Arguments:
*     FORFIL = CHARACTER * ( * ) (Given)
*        The fully-expanded name of the foreign format file, optioanlly
*        ending with a foreign extension specifier.
*     IFMT = INTEGER (Given)
*        FCB foreign format code identifying the file's format. May be
*        zero to indicate the absence of a foreign format file (i.e.
*        when a native format NDF is accessed normally).
*     NDFLOC = CHARACTER * ( * ) (Given)
*        Locator which, in conjunction with the NDFNAM argument,
*        identifies the native format NDF object. If a value of
*        DAT__ROOT is given, then NDFNAM should contain the absolute
*        name of this object.
*     NDFNAM = CHARACTER * ( * ) (Given)
*        Relative HDS name of the native format NDF object (or the
*        absolute name if NDFLOC is set to DAT__ROOT).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Message Tokens:
*     The routine defines the following message tokens:
*        DIR
*           Name of the directory containing the foreign file.
*        NAME
*           The name field of the foreign file.
*        TYPE
*           The type field of the foreign file (including leading '.').
*        VERS
*           The version number field of the foreign file.
*        FXS
*           The foreign extension specifier (including enclosing square
*           brackets).
*        FXSCL
*           A cleaned version of the foreign extension specifier in which
*           all non-alphanumeric characters have been replaced by
*           underscore. This is useful when determining the name of
*           temporary native NDFs in which to store the converted foreign
*           NDFs.
*        FMT
*           Name of the foreign file format (upper case).
*        NDF
*           Full name of the native NDF data structure.
*        NAMECL
*           The name of the foreign file but containing only characters
*           accptable within an HDS name. Any other characters are
*           replaced by an underscore.

*  Notes:
*      -  If IFMT is zero, then the DIR, NAME, TYPE and VERS tokens will
*      not be defined by this routine.
*      -  If IFMT is zero, then the FMT token will be given the value
*      'NDF'.
*      -  If NDFLOC has the value DAT__ROOT and NDFNAM is blank, then
*      the NDF token will not be defined by this routine.
*      -  If any token requires a value but none is available (e.g. the
*      corresponding field of the foreign file name is missing or not
*      supported by the operating system), then a blank value will be
*      assigned.

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
*     17-NOV-1994 (RFWS):
*        Original version.
*     23-DEC-1994 (RFWS):
*        Fixed bug: array index out of bounds if IFMT is zero.
*     30-JAN-1995 (RFWS):
*        Leave tokens that are not required undefined.
*     17-JUL-2000 (DSB):
*        Added foreign extension field, and tokens FXS & FXSCL.
*     11-NOV-2011 (DSB):
*        Added NAMECL token.
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
      CHARACTER * ( * ) NDFLOC
      CHARACTER * ( * ) NDFNAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ISALM          ! Is a character alphanumeric?

*  Local Variables:
      INTEGER D1                 ! First character of directory field
      INTEGER D2                 ! Last character of directory field
      INTEGER I                  ! Character index
      INTEGER N1                 ! First character of name field
      INTEGER N2                 ! Last character of name field
      INTEGER T1                 ! First character of type field
      INTEGER T2                 ! Last character of type field
      INTEGER V1                 ! First character of version field
      INTEGER V2                 ! Last character of version field
      INTEGER X1                 ! First character of foreign extension field
      INTEGER X2                 ! Last character of foreign extension field

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a foreign format is being accessed, we must define tokens for each
*  of the fields in the foreign file name.
      IF ( IFMT .NE. 0 ) THEN

*  Initialise.
         D1 = 0
         D2 = -1
         N1 = 0
         N2 = -1
         T1 = 0
         T2 = -1
         V1 = 0
         V2 = -1
         X1 = 0
         X2 = -1

*  Split the foreign file name into its directory, name, type,
*  version and foreign extension fields
         IF ( FORFIL .NE. ' ' ) THEN
            CALL NDF1_SPFOR( FORFIL, IFMT, D1, D2, N1, N2, T1, T2,
     :                       V1, V2, X1, X2, STATUS )
         END IF
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Define the foreign file directory token.
            IF ( D1 .LE. D2 ) THEN
               CALL MSG_SETC( 'DIR', FORFIL( D1 : D2 ) )
            ELSE
               CALL MSG_SETC( 'DIR', ' ' )
            END IF

*  Define the foreign file name token.
            IF ( N1 .LE. N2 ) THEN
               CALL MSG_SETC( 'NAME', FORFIL( N1 : N2 ) )
            ELSE
               CALL MSG_SETC( 'NAME', ' ' )
            END IF

*  Define the cleaned foreign file name token.
            IF ( N1 .LE. N2 ) THEN
               DO I = N1, N2
                  IF( CHR_ISALM( FORFIL( I : I ) ) ) THEN
                     CALL MSG_SETC( 'NAMECL', FORFIL( I : I ) )
                  ELSE
                     CALL MSG_SETC( 'NAMECL', '_' )
                  END IF
               END DO
            ELSE
               CALL MSG_SETC( 'NAMECL', ' ' )
            END IF

*  Define the foreign file type token.
            IF ( T1 .LE. T2 ) THEN
               CALL MSG_SETC( 'TYPE', FORFIL( T1 : T2 ) )
            ELSE
               CALL MSG_SETC( 'TYPE', ' ' )
            END IF

*  Define the foreign file version token.
            IF ( V1 .LE. V2 ) THEN
               CALL MSG_SETC( 'VERS', FORFIL( V1 : V2 ) )
            ELSE
               CALL MSG_SETC( 'VERS', ' ' )
            END IF

*  Define the foreign extension token, and a cleaned version containing
*  only alphanumeric (and underscore) characters.
            IF ( X1 .LE. X2 ) THEN
               CALL MSG_SETC( 'FXS', FORFIL( X1 : X2 ) )

               DO I = X1, X2
                  IF( CHR_ISALM( FORFIL( I : I ) ) ) THEN
                     CALL MSG_SETC( 'FXSCL', FORFIL( I : I ) )
                  ELSE
                     CALL MSG_SETC( 'FXSCL', '_' )
                  END IF
               END DO

            ELSE
               CALL MSG_SETC( 'FXS', ' ' )
               CALL MSG_SETC( 'FXSCL', ' ' )
            END IF

         END IF
      END IF

*  Define the foreign format name.
      IF ( IFMT .NE. 0 ) THEN
         CALL MSG_SETC( 'FMT', FCB_FMT( FCB_FMT1( IFMT ) :
     :                                  FCB_FMT2( IFMT ) ) )
      ELSE
         CALL MSG_SETC( 'FMT', 'NDF' )
      END IF

*  If necessary, define the native format NDF name.
      IF ( NDFLOC .EQ. DAT__ROOT ) THEN
         IF ( NDFNAM .NE. ' ' ) CALL MSG_SETC( 'NDF', NDFNAM )
      ELSE
         CALL DAT_MSG( 'NDF', NDFLOC )
         IF ( NDFNAM .NE. ' ' ) THEN
            CALL MSG_SETC( 'NDF', '.' )
            CALL MSG_SETC( 'NDF', NDFNAM )
         END IF
      END IF

      END
