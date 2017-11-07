      SUBROUTINE ATL_CREAT( FPARAM, IAST, STATUS )
*+
*  Name:
*     ATL_CREAT

*  Purpose:
*     Write an AST Object to a text file or NDF specified using an environment
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_CREAT( FPARAM, IAST, STATUS )

*  Description:
*     Write an AST Object to a text file or NDF specified using an environment
*     parameter.

*  Arguments:
*     FPARAM = CHARACTER * ( * ) (Given)
*        The parameter name. If the supplied string contains a colon,
*        then the parameter name is taken to be the string following
*        the colon. The string before the colon indicates the format
*        required for the output text file:
*
*        "AST:"      - AST_SHOW format
*        "STCS:"     - STCS format
*        "XML:"      - AST XML format
*        "FITS-xxx:" - FITS, using the specified encoding
*        "NATIVE:"   - FITS, using NATIVE encoding
*
*        The default (i.e. used if the string does not contain a
*        colon) is "AST". Attribute values for the Channel (of whatever
*        class) can be specified using the environment variable
*        ATOOLS_CHATT_OUT.
*     IAST = INTEGER (Given)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001, 2003 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     15-SEP-2003 (DSB):
*        Changed call to FIO_ANNUL to FIO_CLOSE. Previously the output
*        text file was not completely flushed when being used as a
*        monolith (e.g. from ICL), resulting in incomplete output files.
*     11-MAY-2006 (DSB):
*        Increase maximum line length to 300 characters.
*     30-MAY-2006 (DSB):
*        Moved into ATL library and changed prefix from "ATL1_" to "ATL_".
*     8-JUN-2012 (DSB):
*        Allow required format to specified via PARAM argument (doing it
*        this way provides backward compatibility in the API).
*     25-OCT-2017 (DSB):
*        Allow Channel attributes to be set using environment variable
*        ATOOLS_CHANATTRS.
*     7-NOV-2017 (DSB):
*        - Annul the error if the Channel attributes supplied via
*        ATOOLS_CHANATTRS are inappropriate for the type of channel in
*        use.
*        - Rename env. variable from ATOOLS_CHANATTRS to ATOOLS_CHATT_OUT
*        preparatory to allowing input channels to be configured using
*        ATOOLS_CHATT_IN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'AST_ERR'          ! AST error constants

*  Arguments Given:
      CHARACTER FPARAM*(*)
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SNK/ FD

*  External References:
      EXTERNAL ATL_SNK
      EXTERNAL ATL_FITSSNK

*  Local Variables:
      CHARACTER FMT*50
      CHARACTER PARAM*50
      CHARACTER FNAME*255
      CHARACTER ATTRS*500
      INTEGER CHAN
      INTEGER COLON
      INTEGER NOBJ
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract any format specified from the PARAM value.
      FMT = 'AST'
      PARAM = FPARAM

      COLON = INDEX( FPARAM, ':' )
      IF( COLON .GT. 0 ) THEN
         IF( COLON .GT. 1  ) FMT = FPARAM( : COLON - 1 )
         PARAM =  FPARAM( COLON + 1 : )
      END IF

*  Get the name of the output file.
      CALL PAR_GET0C( PARAM, FNAME, STATUS )

*  We delete any pre-existing file first.
      CALL ATL_RM( FNAME, STATUS )

*  Open a new file and get an FIO identifier for it.
      CALL FIO_OPEN( FNAME, 'WRITE', 'LIST', 300, FD, STATUS )

*  Create an AST Channel of the appropriate class to write to the file.
      IF( FMT .EQ. 'AST' ) THEN
         CHAN = AST_CHANNEL( AST_NULL, ATL_SNK, ' ', STATUS )

      ELSE IF( FMT .EQ. 'STCS' ) THEN
         CHAN = AST_STCSCHAN( AST_NULL, ATL_SNK, 'Indent=1,'//
     :                        'StcsLength=200 ', STATUS )

      ELSE IF( FMT .EQ. 'XML' ) THEN
         CHAN = AST_XMLCHAN( AST_NULL, ATL_SNK, 'XmlLength=200',
     :                       STATUS )

      ELSE IF( FMT( :5 ) .EQ. 'FITS-' .OR. FMT .EQ. 'NATIVE' ) THEN
         CHAN = AST_FITSCHAN( AST_NULL, ATL_FITSSNK, ' ',  STATUS )
         CALL AST_SETC( CHAN, 'Encoding', FMT, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FPARAM )
         CALL ERR_REP( ' ', 'ATL_CREAT: Unknown format specified '//
     :                'in ''^F''.', STATUS )
      END IF

*  See if any attributes should be set in the channel.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PSX_GETENV( 'ATOOLS_CHATT_OUT', ATTRS, STATUS )
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            CALL AST_SET( CHAN, ATTRS, STATUS )
            IF( STATUS .EQ. AST__BADAT ) CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  Write the Object to the Channel.
      NOBJ = AST_WRITE( CHAN, IAST, STATUS )

*  Report an error if no Object was written.
      IF( STATUS .EQ. SAI__OK .AND. NOBJ .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FNAME )
         CALL ERR_REP( 'ATL_CREAT_ERR1', 'Failed to write an AST '//
     :                 'Object to file ''^F''.', STATUS )

      ELSE
         CALL MSG_SETC( 'F', FNAME )
         CALL ATL_NOTIF( '   AST data written to text file ''^F''.',
     :                    STATUS )
      END IF

*  Annul the channel.
      CALL AST_ANNUL( CHAN, STATUS )

*  Close the file.
      CALL FIO_CLOSE( FD, STATUS )

      END


      SUBROUTINE ATL_SNK( STATUS )

      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'

*  Arguments:
      INTEGER STATUS

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SNK/ FD

*  Local Variables:
      CHARACTER BUF*300
      INTEGER NC

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read a line from the Channel.
      CALL AST_GETLINE( BUF, NC, STATUS )

*  If succesful, write it to the file.
      IF( NC .GT. 0 ) CALL FIO_WRITE( FD, BUF( : NC ), STATUS )

      END



      SUBROUTINE ATL_FITSSNK( CARD, STATUS )

      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'

*  Arguments:
      CHARACTER CARD*80
      INTEGER STATUS

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SNK/ FD

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER NC

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the supplied card to the file.
      NC = CHR_LEN( CARD )
      IF( NC .GT. 0 ) THEN
         CALL FIO_WRITE( FD, CARD( : NC ), STATUS )
      ELSE
         CALL FIO_WRITE( FD, '       ', STATUS )
      END IF

      END
