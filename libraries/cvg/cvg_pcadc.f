      SUBROUTINE CVG_PCADC( IPROV, FUNIT, STATUS )
*+
*  Name:
*     CVG_PCADC

*  Purpose:
*     Writes CADC-style provenance records to the current FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_PCADC( IPROV, FUNIT, STATUS )

*  Description:
*     This creates headers in the current FITS header that record the
*     number and names of all the immediate parents in the supplied
*     NDG provenance structure. It also records the number of root
*     parents---those without ancestors---and their observation
*     identifiers from component OBIDSS within the MORE component of
*     the supplied provenance structure. These are the observations.
*
*     The names follow CADC convention as follows.  For the immediate
*     parents:
*
*     PRVCNT  =             _INTEGER / Number of parents
*     PRV1    = _CHAR                / Name of the first parent
*     PRV2    = _CHAR                / Name of the second parent
*         :        :        :        :        :        :
*     PRVn    = _CHAR                / Name of the PRVCNTth parent
*
*     for the root provenance:
*     OBSCNT  =             _INTEGER / Number of root-ancestor headers
*     OBS1    = _CHAR                / First observation identifier
*         :        :        :        :        :        :
*     OBSn    = _CHAR                / OBSCNTth observation identifier
*
*     and the output file name:
*     FILEID  = _CHAR                / Filename
*
*     The above headers are prefaced by a blank header and a title
*     "Provenance:" comment.
*
*     The PRODUCT keyword's value is modified for FITS extensions.  It
*     has '_<extname>' appended where <extname> is the lowercase name of
*     the standard EXTNAME keyword.

*  Arguments:
*     IPROV = INTEGER (Given)
*        The identifier of the PROVENANCE that is to be written to
*        the FITS headers. If NDG__NULL is supplied, no provenance is
*        stored in the header, but the PRODUCT keyword is still updated.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Ancestors that have been flagged as "hidden" are ignored.
*     - A warning is issued if the OBSIDSS component cannot be found
*     for a root ancestor.  The value of OBSCNT gives the number of
*     ancestors with an OBSIDSS value.

*  Prior Requirements:
*     The FITS file must already be open.  The current HDU in the FITS
*     file should be the primary and the standard headers should be
*     present.

*  Copyright:
*     Copyright (C) 2008-2013 Science & Technology Facilities Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S. Berry (JAC, Hawaii)
*     BRADC: Brad Cavanagh (JAC, Hawaii)
*     GSB: Graham Bell (EAO)
*     {enter_new_authors_here}

*  History:
*     2008 January 11 (MJC):
*        Original version.
*     2008 February 4 (MJC):
*        Use indexed keyword names more in keeping with the FITS
*        standard.  Fix bug from a misunderstanding of KeyMap returned
*        by NDG_GTPRV.  Look for OBSIDSS in ANCESTORS structure, not
*        ANCESTOR as in the specification.
*     2008 February 5 (MJC):
*        Modify OBSCNT to reflect number of OBSn headers written.
*     2008 March 6 (MJC):
*        Check for existence of PARENTS component.  Remove the limit on
*        the number of parents.
*     2008 May 19 (MJC):
*        Write FILEID keyword.
*     2008 June 12 (TIMJ):
*        Fix some valgrind warnings. FNAME used incorrectly for PATH.
*     2008 August 6 (MJC):
*        Only record unique OBSIDSS values.
*     2008 October 2 (MJC):
*        Edit the PRODUCT keyword for extensions.
*     29-JUN-2009 (DSB):
*        Use new NDG provenance API.
*     7-JUL-2009 (DSB):
*        Ignore hidden ancestors.
*     22-DEC-2009 (BRADC):
*        Handle periods in ancestor path when no file extension is
*        given.
*     19-SEP-2012 (DSB):
*        Incorporate changes to the NDG API introduced at version 7 of
*        NDG.
*     14-NOV-2013 (DSB):
*        Renamed from COF_PCADC to CVG_PCADC and moved from CONVERT to
*        CVG, so that CUPID can use it. The old NDF argument has
*        been replaced by IPROV.
*     26-NOV-2018 (GSB):
*        Retained file extensions in provenance headers as CADC are
*        apparently moving to a new storage system and only supporting
*        full filenames.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system public constants
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDG_PAR'          ! Provenance structure constants
      INCLUDE 'CVG_PAR'          ! CVG constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER IPROV
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER*2 CHR_NTH        ! Ordinal string
      INTEGER CHR_LEN            ! Effective string length

*  Local Variables:
      CHARACTER*47 ANCCOM        ! Ancestor header comment
      INTEGER ANCKM              ! KeyMap describing an ancestor
      CHARACTER CARD*(CVG__HEDLEN) ! FITS header card
      CHARACTER*47 COMENT        ! FITS header comment
      INTEGER CPOS               ! Current string position
      CHARACTER*47 EXTCOM        ! EXTNAM header comment
      CHARACTER*68 EXTNAM        ! Name of the extension
      CHARACTER FNAME*(CVG__MXPTH)! Output file name
      INTEGER FSTAT              ! FITSIO status
      LOGICAL HASID              ! Is the OBSIDSS key already present?
      LOGICAL IDPRS              ! Index to root present?
      INTEGER ID                 ! Index to a root ancestor
      INTEGER IDP                ! Index to current parent
      INTEGER IPROV2             ! Id. for cleansed provenance structure
      INTEGER IREC               ! Loop counter for provenance records
      CHARACTER*( AST__SZCHR ) KEY ! Current key in KeyMap of root anc.
      INTEGER KEYMAP             ! AST KeyMap of root ancestors
      CHARACTER*8 KEYWRD         ! Header keyword
      INTEGER KMIDSS             ! AST KeyMap of OBSIDSS
      INTEGER L                  ! Used line length
      INTEGER MOREKM             ! KeyMap holding MORE information
      CHARACTER*68 NAME          ! Path to ancestor
      INTEGER NCNAME             ! Character length of the name
      INTEGER NHDU               ! HDU number
      INTEGER NIDS               ! Number of indices
      INTEGER NOBSID             ! Number of OBSn headers written
      INTEGER NPAR               ! Number of parents
      INTEGER NROOT              ! Number of root ancestors
      CHARACTER*30 OBIDSS        ! MORE.OBSIDSS value
      LOGICAL OBIPRS             ! OBSIDSS present?
      CHARACTER*256 PATH         ! Path to ancestor
      INTEGER PIPNTR             ! Pointer to indices of the parents
      CHARACTER*68 PRODUC        ! Value PRODUCT keyword
      INTEGER PRVKM              ! KeyMap holding PROVENANCE info
      LOGICAL THERE              ! Component is present

*  The PRODUCT keyword may only exist in the primary HDU, so save its
*  value and comment when this routine is called for writing the FITS
*  extension headers.
      SAVE COMENT, PRODUC

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = CVG__FITSOK

* Check a Provenance structure was supplied.
      IF ( IPROV .NE. NDG__NULL ) THEN

*  Get a copy of the provenance info excluding hidden ancestors.
         CALL NDG_COPYPROV( IPROV, .TRUE., IPROV2, STATUS )

*  Direct parents
*  ==============

*  Meet the direct parents.  There may not be any. Annul any MORE
*  locator immediately since we do not need it.
         CALL NDG_GETPROV( IPROV2, 0, PRVKM, STATUS )
         IF ( AST_MAPHASKEY( PRVKM, 'PARENTS', STATUS ) ) THEN

*  Find the number of parent NDFs.
            NPAR = AST_MAPLENGTH( PRVKM, 'PARENTS', STATUS )

*  Obtain workspace for the indices.
            CALL PSX_CALLOC( NPAR, '_INTEGER', PIPNTR, STATUS )

*  Obtain the array of parents' indices.
            THERE = AST_MAPGET1I( PRVKM, 'PARENTS', NPAR, NPAR,
     :                            %VAL( CNF_PVAL( PIPNTR ) ),
     :                            STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Write a blank header and a title for the block of provenance headers.
            CARD = ' '
            CALL FTPREC( FUNIT, CARD, FSTAT )

            CPOS = 31
            CALL CHR_APPND( '/ Provenance:', CARD, CPOS )
            CALL FTPREC( FUNIT, CARD, FSTAT )

*  Write the PRVCNT header.
            CALL FTPKYJ( FUNIT, 'PRVCNT', NPAR, 'Number of parents',
     :                   FSTAT )

            DO IREC = 1, NPAR

*  Extract the next index from the mapped array.
               CALL CVG_RETRI( NPAR, IREC, %VAL( CNF_PVAL( PIPNTR ) ),
     :                         IDP, STATUS )

*  Obtain the path of the current immediate ancestor. Annul any MORE
*  locator immediately since we do not need it.
               CALL NDG_GETPROV( IPROV2, IDP, ANCKM, STATUS )
               IF ( .NOT. AST_MAPGET0C( ANCKM, 'PATH', PATH, L,
     :                                  STATUS ) ) THEN
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', IDP )
                     CALL ERR_REP( ' ', 'No path found for ancestor '//
     :                             '^I in provenance extension '//
     :                             '(programming error).', STATUS )
                  END IF
               END IF
               IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Extract the name.
*  *** Assume UNIX for the moment. ***
               CALL CHR_LASTO( PATH, '/', CPOS )

               NAME = PATH( CPOS + 1 : )
               NCNAME = CHR_LEN( NAME )

*  Form keyword without leading zeroes (the FITS Standard says it shall
*  be done this way).
               KEYWRD = 'PRV'
               CPOS = 3
               CALL CHR_PUTI( IREC, KEYWRD, CPOS )

*  Form comment.
               ANCCOM = 'Name of the '
               CPOS = 12
               CALL CHR_PUTI( IREC, ANCCOM, CPOS )
               CALL CHR_APPND( CHR_NTH( IREC ), ANCCOM, CPOS )
               CALL CHR_APPND( ' parent', ANCCOM, CPOS )

*  Write the PRVnnnnn header.
               CALL FTPKYS( FUNIT, KEYWRD, NAME( :NCNAME ),
     :                      ANCCOM( :CPOS ), FSTAT )

*  Free the information for the current parent.
               CALL AST_ANNUL( ANCKM, STATUS )
            END DO

*  Complete the tidying of resources.
            CALL PSX_FREE( PIPNTR, STATUS )
         END IF
         CALL AST_ANNUL( PRVKM, STATUS )

*  Root ancestors
*  ==============

*  Obtain the root ancestors (ones with no parents) via an AST KeyMap.
         CALL NDG_ROOTPROV( IPROV2, KEYMAP, STATUS )
         NROOT = AST_MAPSIZE( KEYMAP, STATUS )

*  Write the OBSCNT header with the expected value, so that it's in the
*  desired location immediately before the OBSn headers.  Its value may
*  be corrected if there are fewer than NROOT OBSIDSS values found.
         CALL FTPKYJ( FUNIT, 'OBSCNT', NROOT,
     :                'Number of root-ancestor headers', FSTAT )
         NOBSID = 0

*  Create a KeyMap to record the OBSIDSS values as keys.  The default
*  is 300 elements, which ought to be plenty by a large factor, hence
*  to resizing.
         KMIDSS = AST_KEYMAP( ' ', STATUS )

         DO IREC = 1, NROOT
            KEY = AST_MAPKEY( KEYMAP, IREC, STATUS )
            IDPRS = AST_MAPGET0I( KEYMAP, KEY, ID, STATUS )

*  Obtain the identifier of the current immediate ancestor.
            CALL NDG_GETPROV( IPROV2, ID, ANCKM, STATUS )

*  Attempt to find the .MORE.OBSIDSS component.
            OBIPRS = .FALSE.
            IF ( AST_MAPGET0A( ANCKM, 'MORE', MOREKM, STATUS ) ) THEN
               IF ( AST_MAPGET0C( MOREKM, 'OBSIDSS', OBIDSS, L,
     :                            STATUS ) ) THEN
                  IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Test whether or not this OBSIDSS is unique within this NDF.
                  HASID = AST_MAPHASKEY( KMIDSS, OBIDSS, STATUS )
                  IF ( .NOT. HASID ) THEN
                     NOBSID = NOBSID + 1
                     CALL AST_MAPPUT0I( KMIDSS, OBIDSS, NOBSID, ' ',
     :                                  STATUS )

*  Form keyword without leading zeroes (the FITS Standard says it shall
*  not done this way).
                     KEYWRD = 'OBS'
                     CPOS = 3
                     CALL CHR_PUTI( NOBSID, KEYWRD, CPOS )

*  Form comment.
                     ANCCOM = 'Name of the '
                     CPOS = 12
                     CALL CHR_PUTI( NOBSID, ANCCOM, CPOS )
                     CALL CHR_APPND( CHR_NTH( NOBSID ), ANCCOM, CPOS )
                     CALL CHR_APPND( ' root ancestor', ANCCOM, CPOS )

*  Write the OBSnnnnn header.
                     CALL FTPKYS( FUNIT, KEYWRD, OBIDSS,
     :                            ANCCOM( :CPOS ), FSTAT )
                  END IF
                  OBIPRS = .TRUE.
               END IF

*  Free the identifier for the MORE keymap.
               CALL AST_ANNUL( MOREKM, STATUS )
            END IF

*  Issue warning if OBSIDSS is absent.
            IF ( .NOT. OBIPRS ) THEN
               CALL MSG_SETI( 'I', IREC )
               CALL MSG_OUTIF( MSG__NORM, 'CVG_PCADC_NOOBSIDSS',
     :                         'Root ancestor ^I has no OBSIDSS.',
     :                         STATUS )
            END IF

*  Free the locator for the current parent.
            CALL AST_ANNUL( ANCKM, STATUS )

         END DO
         CALL AST_ANNUL( KMIDSS, STATUS )

*  Free the KeyMap holding root ancestor information.
         CALL AST_ANNUL( KEYMAP, STATUS )

*  Free the structure holding provenance information.
         CALL NDG_FREEPROV( IPROV2, STATUS )

*  Correct the OBSCNT header value to allow for missing OBSIDSS values.
         IF ( NOBSID .LT. NROOT ) THEN
            CALL FTMKYJ( FUNIT, 'OBSCNT', NOBSID,
     :                   'Number of root-ancestor headers', FSTAT )
         END IF

*  FILEID header
*  =============

*  Inquire the filename.
         FNAME = ' '            ! valgrind warnings
         CALL FTFLNM( FUNIT, FNAME, STATUS )

*  *** Alert!  UNIX assumption. *** We need a generic routine to extract
*  the filename, path, and extension. ***
         CALL CHR_LASTO( FNAME, '/', CPOS )

         CALL FTPKYS( FUNIT, 'FILEID', FNAME( CPOS + 1 : ),
     :                'Filename', FSTAT )

*  Write a blank header.
         CARD = ' '
         CALL FTPREC( FUNIT, CARD, FSTAT )
      END IF

*  PRODUCT header
*  ==============

*  The PRODUCT keyword retains its value in the primary HDU, but
*  has the extension name appended in extensions, of the form
*  <PRODUCT>_<extname>, where <extname> is the value iof EXTNAM in
*  lowercase.
      CALL FTGHDN( FUNIT, NHDU )

      IF ( NHDU .EQ. 1 ) THEN
         CALL FTGKYS( FUNIT, 'PRODUCT', PRODUC, COMENT, FSTAT )

         IF ( FSTAT .EQ. 202 .AND. STATUS .EQ. SAI__OK ) THEN
            FSTAT = CVG__FITSOK
            CALL FTCMSG
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'CVG_PCADC: No ''PRODUCT'' keyword '//
     :                    'found in supplied FITS header.', STATUS )
         END IF

      ELSE IF ( NHDU .GT. 1 ) THEN

* Obtain the two relevant headers.
         CALL FTGKYS( FUNIT, 'EXTNAME', EXTNAM, EXTCOM, FSTAT )

* Form the required string.
         CALL CHR_LCASE( EXTNAM )
         CPOS = CHR_LEN( PRODUC )
         CALL CHR_APPND( '_', PRODUC, CPOS )
         CALL CHR_APPND( EXTNAM, PRODUC, CPOS )

*  Write the PRODUCT keyword's new value leaving the comment unchanged.
*  A new keyword is written should the PRODUCT keyword not exist in
*  the extension.
         CALL FTUKYS( FUNIT, 'PRODUCT', PRODUC( :CPOS ), COMENT,
     :                 FSTAT )
      END IF

*  Come here is there has been an error.
  999 CONTINUE

*  Check for an error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. CVG__FITSOK ) THEN
         CALL CVG_FIOER( FSTAT, 'CVG_PCADC_ERR', 'FTPKYJ',
     :                   'Error writing CADC provenance header cards.',
     :                   STATUS )

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error writing CADC provenance header '//
     :                 'cards.', STATUS )
      END IF

      END
