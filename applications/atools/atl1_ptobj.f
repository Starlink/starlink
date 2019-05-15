      SUBROUTINE ATL1_PTOBJ( PARAM, PREF, IAST, STATUS )
*+
*  Name:
*     ATL1_PTOBJ

*  Purpose:
*     Put an AST Object into an NDF or text file using an environment
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_PTOBJ( PARAM, PREF, IAST, STATUS )

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     PREF = CHARACTER * ( * ) (Given)
*        The parameter name associated with the input Object. Ignored
*        if blank.
*     IAST = INTEGER (Given)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - FrameSets can be written out as the WCS FrameSet of an NDF
*     - Mocs can be written out as FITS binary tables.
*     - This routine uses an ADAM parameter with hard-wired name "FMT" to
*     get the format for the output text file. It may be one of:
*
*        "AST:"      - AST_SHOW format (the default)
*        "STCS:"     - STCS format
*        "MOC-JSON:" - MOC JSON format
*        "MOC"       - MOC "string" format
*        "XML:"      - AST XML format
*        "FITS-xxx:" - FITS, using the specified encoding
*        "NATIVE:"   - FITS, using NATIVE encoding

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2011 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     16-JAN-2001 (DSB):
*        Original version.
*     8-JUN-2012 (DSB):
*        Allow output to be created in a variety of formats.
*     10-DEC_2018 (DSB):
*        Allow Moc's to be written out to FITS binary tables.
*     9-MAY-2019 (DSB):
*        Allow Moc's to be written out as text files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CNF_PAR'          ! CNF functions etc
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'DAT_ERR'          ! HDS error constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'NDF_ERR'          ! NDF error constants

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER PREF*(*)

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER CARD*80
      CHARACTER CLASS*30
      CHARACTER DOM*50
      CHARACTER ERRTEXT*30
      CHARACTER ERRMESS*80
      CHARACTER FILE*255
      CHARACTER FPARAM*100
      INTEGER DIM( NDF__MXDIM )
      INTEGER FC
      INTEGER FLEN
      INTEGER FSTAT
      INTEGER INDF
      INTEGER IP
      INTEGER IPAR
      INTEGER LN
      INTEGER NAX
      INTEGER NB
      INTEGER NC
      INTEGER NDIM
      INTEGER PLACE
      INTEGER UNIT
      LOGICAL DONE
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If an input Object parameter was supplied, use the same name as
*  the dynamic default.
      IF( PREF .NE. ' ' ) THEN
         CALL SUBPAR_FINDPAR( PREF, IPAR, STATUS )
         CALL SUBPAR_GETNAME( IPAR, FILE, STATUS )
         CALL PAR_DEF0C( PARAM, FILE, STATUS )
      END IF

*  Get the name of the output file or NDF.
      CALL PAR_GET0C( PARAM, FILE, STATUS )
      FLEN = CHR_LEN( FILE )

*  Indicate we have not yet stored the object.
      DONE = .FALSE.

*  If the object is a Moc, it can be stored in a FITS file.
      IF( AST_ISAMOC( IAST, STATUS ) .AND. STATUS .EQ. SAI__OK ) THEN
         IF( CHR_SIMLR( FILE( FLEN-3 : ), '.fit' ) .OR.
     :       CHR_SIMLR( FILE( FLEN-4 : ), '.fits' ) ) THEN
            CALL ATL_MOCFT( IAST, FILE, STATUS )
         END IF

*  If the object is a FrameSet, it can be stored in an NDF.
      ELSE IF( AST_ISAFRAMESET( IAST, STATUS ) ) THEN

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to access the file as an NDF.
         CALL NDF_OPEN( DAT__ROOT, FILE, 'UPDATE', 'OLD', INDF, PLACE,
     :                  STATUS )

*  If successfull..
         IF( STATUS .EQ. SAI__OK ) THEN

*  Check that the Base Frame in the FrameSet has Domain GRID.
            DOM = AST_GETC( AST_GETFRAME( IAST, AST__BASE, STATUS ),
     :                      'Domain', STATUS )

            IF( .NOT. CHR_SIMLR( DOM, 'GRID' ) .AND.
     :          STATUS .EQ. SAI__OK ) THEN

               CALL NDF_ANNUL( INDF, STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'D', DOM )
               CALL ERR_REP( 'ATL_PTOBJ_ERR2', 'The Base Frame in the'//
     :                       ' FrameSet has Domain name ''^D''. This '//
     :                       'should be ''GRID'' if the FrameSet is '//
     :                       'to be stored in an NDF.', STATUS )
               GO TO 999
            END IF

*  Check the Base Frame in the FrameSet has the correct number of axes.
            CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )
            NAX = AST_GETI( IAST, 'Nin', STATUS )

            IF( NAX .NE. NDIM .AND. STATUS .EQ. SAI__OK ) THEN
               CALL NDF_ANNUL( INDF, STATUS )
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'NAX', NAX )
               CALL MSG_SETI( 'NDIM', NDIM )
               CALL ERR_REP( 'ATL_PTOBJ_ERR3', 'The Base Frame in the'//
     :                       ' FrameSet has ^NAX axes, but the NDF '//
     :                       'has ^NDIM pixel axes. These numbers '//
     :                       'must be the same.', STATUS )
               GO TO 999
            END IF

*  Store the FrameSet in the NDF, and annul the identifier.
            CALL NDF_PTWCS( IAST, INDF, STATUS )
            CALL NDF_MSG( 'NDF', INDF )
            CALL ATL_NOTIF( '   AST data written to NDF ''^NDF''.',
     :                       STATUS )
            CALL NDF_ANNUL( INDF, STATUS )

*  Indicate we have stored the object.
            DONE = .TRUE.

*  If no NDF was found, annul the error and store the AST Object in a text
*  file.
         ELSE IF( STATUS .EQ. NDF__FILNF .OR.
     :            STATUS .EQ. DAT__OBJNF .OR.
     :            STATUS .EQ. DAT__FILNF ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
      END IF

*  If the AST Object was not a FrameSet, or the output was not an NDF,
*  store the AST Object in a text file.
      IF( .NOT. DONE ) THEN

*  Get the format to use.
         CALL PAR_CHOIC( 'FMT', 'AST', 'AST,STCS,XML,NATIVE,FITS-WCS,'//
     :                   'FITS-PC,FITS-IRAF,FITS-AIPS,FITS-AIPS++,'//
     :                   'FITS-CLASS,MOC,MOC-JSON', .TRUE., FPARAM,
     :                   STATUS )

*  Construct a string of the form "<fmt>:<param>".
         NC = CHR_LEN( FPARAM )
         CALL CHR_APPND( ':', FPARAM, NC )
         CALL CHR_APPND( PARAM, FPARAM, NC )

*  Write out the object using the specified format.
         CALL ATL_CREAT( FPARAM, IAST, STATUS )

      END IF

 999  CONTINUE

      END
