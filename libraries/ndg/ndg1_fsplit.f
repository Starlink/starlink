      SUBROUTINE NDG1_FSPLIT( SPEC, DIR, BN, TYP, HPATH, SEC, STATUS )
*+
*  Name:
*     NDG1_FSPLIT

*  Purpose:
*     Extracts all fields from a full NDF specification.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_FSPLIT( SPEC, DIR, BN, TYP, HPATH, SEC, STATUS )

*  Description:
*     This routine extracts and returned all fields from the supplied NDF
*     specification.

*  Arguments:
*     SPEC = CHARACTER * ( * ) (Given)
*        The full NDF file spec.
*     DIR = CHARACTER * ( * ) (Returned)
*        The directory path (ending at the final "\" in the spec).
*     BN = CHARACTER * ( * ) (Returned)
*        The file base name. Ends with the character preceeding the first
*        "." or "(" or "[" following the directory path.
*     TYP = CHARACTER * ( * ) (Returned)
*        The file type. This will be ".sdf" or one of the file types
*        specified by the NDF_FORMATS_OUT environment variable. If it is
*        ".sdf"
*     HPATH = CHARACTER * ( * ) (Returned)
*        Any string following the file base name, and preceeding any
*        opening parenthesis. SUF will either be blank, or begin with a
*        dot or a "[".
*     SEC = CHARACTER * ( * ) (Returned)
*        Any parenthesised string following the suffix.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-OCT-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PSX_ERR'          ! PSX error constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'NDG_CONST'        ! NDG constants.

*  Arguments Given:
      CHARACTER SPEC*(*)

*  Arguments Returned:
      CHARACTER DIR*(*)
      CHARACTER BN*(*)
      CHARACTER TYP*(*)
      CHARACTER HPATH*(*)
      CHARACTER SEC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of string

*  Local Constants:
      INTEGER MXTYP              ! Max. number of foreign data formats
      PARAMETER ( MXTYP = 50 )
      INTEGER SZTYP              ! Max. length of a foreign file type
      PARAMETER ( SZTYP = 15 )

*  Local Variables:
      CHARACTER FMTOUT*(NDG__SZFMT)! List of output NDF formats
      CHARACTER SUF*(GRP__SZNAM) ! File typ and HDS path combined
      CHARACTER TYPS( MXTYP )*(SZTYP)! Known foreign file types
      INTEGER ITYP               ! Index into TYPS array
      INTEGER LTYP               ! Length of the file type
      INTEGER MXLTYP             ! Max length of a matching file type
      INTEGER NTYP               ! No. of known foreign data formats

*.

*  Initialise
      DIR = ' '
      BN = ' '
      TYP = ' '
      HPATH = ' '
      SEC = ' '

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Split the path into directory, base name, suffix and slice. The
*  suffix may be a combination of file type and hds path. Assume there
*  are no dots in the bane name.
      CALL NDG1_FPARS( SPEC, 0, DIR, BN, SUF, SEC, STATUS )

*  Split the suffix up into file type and HDS path. First get the
*  current value of environment variable NDF_FORMATS_OUT. If it is
*  not defined, annull the error and use ".sdf" as the file type. If
*  the suffix starts with ".sdf" remove it and use the remained as the
*  HDS Path.
      CALL PSX_GETENV( 'NDF_FORMATS_OUT', FMTOUT, STATUS )
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         TYP = NDG__NDFTP
         LTYP = CHR_LEN( TYP )
         IF( SUF( : LTYP ) .EQ. TYP ) THEN
            HPATH = SUF( LTYP + 1 : )
         ELSE
            HPATH = SUF
         END IF

*  If NDF_FORMATS_OUT is defined, extract the file types into an array.
      ELSE
         CALL CHR_RMBLK( FMTOUT )
         CALL NDG1_GTYPS( MXTYP, FMTOUT, NTYP, TYPS, STATUS )

*  Loop round the formats.
         MXLTYP = 0
         DO ITYP = 1, NTYP

*  If the type is ".", we used ".sdf" instead.
            IF( TYPS( ITYP ) .EQ. '.' ) TYPS( ITYP ) = NDG__NDFTP

*  If the type is "*" (i.e. use the format of the corresponding input
*  file), we pass on to the next since we do not know the type of the
*  corresponding input file.
            IF( TYPS( ITYP ) .NE. '*' ) THEN

*  If the suffix starts with the current file type, and the file type is
*  longer than any previously matching file type,  use it as the file type.
*  The remained of the suffix is assigned to the HDS path...
               LTYP = CHR_LEN( TYPS( ITYP ) )
               IF( SUF( : LTYP ) .EQ. TYPS( ITYP ) .AND.
     :             LTYP .GT. MXLTYP ) THEN
                  TYP = TYPS( ITYP )
                  HPATH = SUF( LTYP + 1 : )
                  MXLTYP = LTYP
               END IF

            END IF

         END DO

*  If no matching file type was found, assume it is an HDS file. Use
*  ".sdf" as the file type and the entire suffix as the HDS path.
         IF( MXLTYP .EQ. 0 ) THEN
            TYP = NDG__NDFTP
            HPATH = SUF
         END IF

      END IF

      END
