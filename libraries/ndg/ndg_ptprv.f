      SUBROUTINE NDG_PTPRV( INDF1, INDF2, ID, ISROOT, STATUS )
*+
*  Name:
*     NDG_PTPRV

*  Purpose:
*     Add provenance information to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_PTPRV( INDF1, INDF2, ID, ISROOT, STATUS )

*  Description:
*     This routine stores information in the "PROVENANCE" extension in
*     INDF1 indicating that INDF2 was used in the creation of INDF1. 
*     The provenance information is stored in an NDF extension call 
*     "PROVENANCE". It consists of text identifiers for the NDFs that
*     were used (directly) in the creation of NDF1. It also stores the
*     provenance information for these "parent" NDFs so that the entire
*     "family tree" of the NDF can be determined. If a particular NDF in
*     this family tree does not itself have any parents, then it is called
*     a "root" NDF.
*
*     Provenance information can be retrieved using NDG_GTPRV.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        An identifier for a newly created NDF.
*     INDF2 = INTEGER (Given)
*        An identifier for an NDF that was used in the creation of INDF1.
*     ID = CHARACTER * ( * ) (Given)
*        A string that will be used to identify INDF2 within the
*        provenance information. If this is blank, then the NDF file name
*        and HDS path (bit not the file system path) for INDF2 will be used.
*     ISROOT = LOGICAL (Given)
*        If TRUE, then INDF2 is always treated as a root NDF. That is,
*        any provenance information in INDF2 is ignored. If FALSE, then
*        any provenance information in INDF2 is copied into INDF1. INDF2
*        is then only a root NDF if it contains no provenance information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The PROVENANCE extension in an NDF contains a table with 2 
*     columns. Each row describers a single NDF - column 1 is the
*     text identifier for the NDF, and column 2 holds an array of 
*     integers that are the row indices within the table of the 
*     immediate parents of the NDF. The first row in the table always
*     describes the NDF containing the PROVENANCE extension, and its
*     identifier will be blank. The table is implemented as an array
*     of HDS structures with type "PROV". Each element in the array 
*     corresponds to a row in the table. A PROV structure has two 
*     elements (corresponding to the two columns in the table); the
*     first is a string called "ID", and the second is an 1D integer
*     array called "PARENTS" holding the row indices of the parent 
*     NDFs.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*     
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-NOV-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      CHARACTER ID*(*)
      LOGICAL ISROOT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Constnats:
      INTEGER MXPRNT             ! Max no. of parents for an NDF
      PARAMETER (MXPRNT = 255 )

      INTEGER IDLEN              ! Max length for a text identifier
      PARAMETER ( IDLEN = 255 )

*  Local Variables:
      CHARACTER BN*255           ! File basename
      CHARACTER DIR*255          ! Directory field
      CHARACTER KEY*( IDLEN )    ! Text identifier for an NDF
      CHARACTER NDFNAM*255       ! Full path for NDF
      CHARACTER PRNTS( MXPRNT )*( IDLEN ) ! Text id.s for parents of an NDF
      CHARACTER SEC*255          ! File NDF/HDS section
      CHARACTER SUF*255          ! File suffix
      CHARACTER USEID*( IDLEN )  ! File basename
      INTEGER I                  ! Loop count
      INTEGER NDFLEN             ! Used length of NDFNAM
      INTEGER NNDF               ! No of entries in PROV2
      INTEGER NPRNT              ! No. of parents for an NDF
      INTEGER PROV1              ! KeyMap holding provenance from INDF1
      INTEGER PROV2              ! KeyMap holding provenance from INDF2
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context
      CALL AST_BEGIN( STATUS )

*  If a blank identifier has been supplied, get the NDF basename and use
*  it as the text identifier for INDF2.
      IF( CHR_LEN( ID ) .EQ. 0 ) THEN
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL MSG_LOAD( ' ', '^NDF', NDFNAM, NDFLEN, STATUS )
         CALL NDG1_FPARS( NDFNAM( : NDFLEN ), DIR, BN, SUF, SEC, 
     :                    STATUS )
         USEID = BN
         USEID( CHR_LEN( BN ) + 1 : ) = SUF
      ELSE
         USEID = ID
      END IF

*  Get the provenance information from INDF1. This information is stored in 
*  the form of an AST KeyMap in which each entry describes a single NDF (an 
*  ancestor of INDF1). The entry key is the text identifier used to identify 
*  the ancestor NDF, and the entry value is a list of text identifiers for 
*  the immediate parents of the ancestor NDF.
      CALL NDG1_RDPRV( INDF1, PROV1, STATUS )      

*  We now add the text identifier for INDF2 to the list of direct parents 
*  of INDF1. This list is held in the entry of PROV1 that has a blank key. 
*  Get the value of this entry.
      IF( .NOT. AST_MAPGET1C( PROV1, ' ', MXPRNT, NPRNT, PRNTS, 
     :                        STATUS ) ) NPRNT = 0

*  If there is room, add the text identifier for INDF2 to the list, and
*  store the modified list back in the KeyMap.
      IF( NPRNT .LT. MXPRNT ) THEN
         NPRNT = NPRNT + 1
         PRNTS( NPRNT ) = USEID
         CALL AST_MAPPUT1C( PROV1, ' ', NPRNT, PRNTS, ' ', STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MX', MXPRNT )
         CALL ERR_REP( ' ', 'Too many parent NDFs (>^MX).', STATUS )
      END IF

*  If INDF2 is not a root NDF, we need to add the ancestors of INDF2 into
*  the list of ancestors for INDF1.
      IF( .NOT. ISROOT ) THEN

*  Get the provenance information from INDF2.
         CALL NDG1_RDPRV( INDF2, PROV2, STATUS )

*  Ancestors of INDF2 become ancestors of INDF1. So loop round every 
*  ancestor of INDF2, adding it into the provenance KeyMap for INDF1.
         NNDF = AST_MAPSIZE( PROV2, STATUS )
         DO I = 1, NNDF

*  Get the key associated with this entry. This is the text identifier
*  for an ancestor of INDF2, or a blank string (if the entry holds the
*  direct parents of INDF2).
            KEY = AST_MAPKEY( PROV2, I, STATUS )

*  If the key is blank, we use the identifier supplied for INDF2.
            IF( KEY .EQ. ' ' ) KEY = USEID

*  Get the value of this entry. This is a list of text identifiers for
*  the direct parents of the ancestor NDF described by the KeyMap entry.
            IF( AST_MAPGET1C( PROV2, KEY, MXPRNT, NPRNT, PRNTS, 
     :                        STATUS ) ) THEN

*  Append a new entry describing this ancestor to the KeyMap holding the 
*  provenance information for INDF1. If this ancestor is already included
*  in the KeyMap, its existing list of parents will be over-written by the
*  new list of parents.
               CALL AST_MAPPUT1C( PROV1, KEY, NPRNT, PRNTS, ' ', 
     :                            STATUS )
            END IF
         END DO
      END IF

*  Store the extended provenance in INDF1.
      CALL NDG1_WRPRV( PROV1, INDF1, STATUS )

*  End the AST context
      CALL AST_END( STATUS )

*  If anything went wrong, issue a context message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF1', INDF1 )
         CALL ERR_REP( ' ', 'Cannot modify provenance in ^NDF1.',
     :                 STATUS )
      END IF

      END
