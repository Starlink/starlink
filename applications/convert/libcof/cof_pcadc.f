      SUBROUTINE COF_PCADC( NDF, FUNIT, STATUS )
*+
*  Name:
*     COF_PCADC

*  Purpose:
*     Writes CADC-style provenance records to the current FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_PCADC( NDF, FUNIT, STATUS )

*  Description:
*     This creates headers in the current FITS header that record the
*     number and names of all the immediate parents of the NDF being
*     converted.  It also records the number of root parents---those 
*     without ancestors---and their observation identifiers from
*     component OBIDSS within the PROVENANCE's own extension.  These
*     are the observations.
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
*     FILEID  = _CHAR                / Filename without extension

*     The above headers are prefaced by a blank header and a title
*     "Provenance:" comment.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF whose PROVENANCE is to be written to
*        the FITS headers.
*     FUNIT = INTEGER (Given)
*        The logical unit number of the output FITS file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - A warning is issued if the OBSIDSS component cannot be found
*     for a root ancestor.  The value of OBSCNT gives the number of
*     ancestors with an OBSIDSS value.

*  Prior Requirements:
*     The NDF and the FITS file must already be open.  The current
*     HDU in the FITS file should be the primary and the standard
*     headers should have been written.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2008 January 11 (MJC):
*        Original version.
*     2008 February 4 (MJC):
*        Use indexed keyword names more in keeping with the FITS 
*        standard.  Fix bug from a misunderstanding of KeyMap returned
*        by NDG_GTPRV.  Look for OBSIDSS is ANCESTORS structure, not
*        ANCESTOR as in the specification.
*     2008 February 5 (MJC):
*        Modify OBSCNT to reflect number of OBSn headers written.
*     2008 March 6 (MJC):
*        Check for existence of PARENTS component.  Remove the limit on
*        the number of parents.
*     2008 May 19 (MJC):
*        Write FILEID keyword.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system public constants      
      INCLUDE 'MSG_PAR'          ! Message-system constants    
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER NDF
      INTEGER FUNIT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      CHARACTER*2 CHR_NTH        ! Ordinal string
      INTEGER CHR_LEN            ! Effective string length

*  Local Constants:
      INTEGER FITSOK             ! Good status for FITSIO library
      PARAMETER ( FITSOK = 0 )

*  Local Variables:
      CHARACTER*47 ANCCOM        ! Ancestor header comment
      CHARACTER*( DAT__SZLOC ) ANCLOC ! Locator to an ancestor
      CHARACTER*80 CARD          ! FITS header card
      INTEGER CPOS               ! Current string position
      CHARACTER*256 FNAME        ! Output file name
      INTEGER FSTAT              ! FITSIO status
      LOGICAL IDPRS              ! Index to root present?
      INTEGER ID                 ! Index to a root ancestor
      INTEGER IDP                ! Index to current parent
      INTEGER IREC               ! Loop counter for provenance records
      CHARACTER*( AST__SZCHR ) KEY ! Current key in KeyMap of root anc.
      INTEGER KEYMAP             ! AST KeyMap of root ancestors
      CHARACTER*8 KEYWRD         ! Header keyword
      CHARACTER*( DAT__SZLOC ) MORLOC ! Locator to MORE component
      CHARACTER*68 NAME          ! Path to ancestor
      INTEGER NCNAME             ! Character length of the name
      INTEGER NIDS               ! Number of indices
      INTEGER NOBSID             ! Number of OBSn headers written
      INTEGER NPAR               ! Number of parents
      INTEGER NROOT              ! Number of root ancestors
      CHARACTER*30 OBIDSS        ! MORE.OBSIDSS value
      LOGICAL OBIPRS             ! OBSIDSS present?
      INTEGER PIPNTR             ! Pointer to indices of the parents
      LOGICAL PRVPRS             ! PROVENANCE present?
      CHARACTER*( DAT__SZLOC ) PARLOC ! Locator to PARENTS component
      CHARACTER*256 PATH         ! Path to ancestor
      CHARACTER*( DAT__SZLOC ) PRVLOC ! Locator to PROVENANCE component
      INTEGER SOE                ! Character position of file extension
      LOGICAL THERE              ! Component is present

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  First check that there is provenance to record.
      CALL NDF_XSTAT( NDF, 'PROVENANCE', PRVPRS, STATUS )
      IF ( PRVPRS ) THEN

*  Direct parents
*  ==============

*  Meet the direct parents.  There may not be any.
         CALL NDG_GTPRV( NDF, 0, PRVLOC, STATUS )
         CALL DAT_THERE( PRVLOC, 'PARENTS', THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND( PRVLOC, 'PARENTS', PARLOC, STATUS )

*  Find the number of parent NDFs.
            CALL DAT_SIZE( PARLOC, NPAR, STATUS )

*  Obtain workspace for the indices.
            CALL PSX_CALLOC( NPAR, '_INTEGER', PIPNTR, STATUS )

*  Obtain the array of parents' indices.
            CALL DAT_GETVI( PARLOC, NPAR, %VAL( CNF_PVAL( PIPNTR ) ), 
     :                      NIDS, STATUS )
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
               CALL KPG1_RETRI( NPAR, IREC, %VAL( CNF_PVAL( PIPNTR ) ),
     :                          IDP, STATUS )

*  Obtain the path of the current immediate ancestor.
               CALL NDG_GTPRV( NDF, IDP, ANCLOC, STATUS )
               CALL CMP_GET0C( ANCLOC, 'PATH', PATH, STATUS )
               IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Extract the name excluding the file extension. 
*  *** Assume UNIX for the moment. ***
               CALL CHR_LASTO( PATH, '/', CPOS )
               CALL CHR_LASTO( FNAME, '.', SOE )
               IF ( SOE .EQ. 0 ) SOE = CHR_LEN( PATH ) + 1

               NAME = PATH( CPOS + 1 : SOE - 1 )
               NCNAME = SOE - CPOS - 1

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

*  Free the locator for the current parent.
               CALL DAT_ANNUL( ANCLOC, STATUS )
            END DO

*  Complete the tidying of resources.
            CALL PSX_FREE( PIPNTR, STATUS )
            CALL DAT_ANNUL( PARLOC, STATUS )
         END IF
         CALL DAT_ANNUL( PRVLOC, STATUS )

*  Root ancestors
*  ==============

*  Obtain the root ancestors (ones with no parents) via an AST KeyMap.
         CALL NDG_RTPRV( NDF, KEYMAP, STATUS )
         NROOT = AST_MAPSIZE( KEYMAP, STATUS )

*  Write the OBSCNT header with the expected value, so that it's in the
*  desired location immediately before the OBSn headers.  Its value may
*  be corrected if there are fewer than NROOT OBSIDSS values found.
         CALL FTPKYJ( FUNIT, 'OBSCNT', NROOT,
     :                'Number of root-ancestor headers', FSTAT )
         NOBSID = 0

         DO IREC = 1, NROOT
            KEY = AST_MAPKEY( KEYMAP, IREC, STATUS )
            IDPRS = AST_MAPGET0I( KEYMAP, KEY, ID, STATUS )

*  Obtain the identifier of the current immediate ancestor.
            CALL NDG_GTPRV( NDF, ID, ANCLOC, STATUS )

*  Attempt to find the .MORE.OBSIDSS component.
            OBIPRS = .FALSE.
            CALL DAT_THERE( ANCLOC, 'MORE', THERE, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 999

            IF ( THERE ) THEN
               CALL DAT_FIND( ANCLOC, 'MORE', MORLOC, STATUS )
               CALL DAT_THERE( MORLOC, 'OBSIDSS', THERE, STATUS )

               IF ( THERE ) THEN
                  CALL CMP_GET0C( MORLOC, 'OBSIDSS', OBIDSS, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GOTO 999
                  NOBSID = NOBSID + 1
                  
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
                  CALL FTPKYS( FUNIT, KEYWRD, OBIDSS, ANCCOM( :CPOS ), 
     :                         FSTAT )
                  OBIPRS = .TRUE.
               END IF

*  Free the locator for the MORE component.
               CALL DAT_ANNUL( MORLOC, STATUS )
            END IF

*  Issue warning if OBSIDSS is absent.
            IF ( .NOT. OBIPRS ) THEN
               CALL MSG_SETI( 'I', IREC )
               CALL MSG_OUTIF( MSG__NORM, 'COF_PCADC_NOOBSIDSS',
     :                         'Root ancester ^I has no OBSIDSS.',
     :                         STATUS )
            END IF

*  Free the locator for the current parent.
            CALL DAT_ANNUL( ANCLOC, STATUS )

         END DO

*  Correct the OBSCNT header value to allow for missing OBSIDSS values.
         IF ( NOBSID .LT. NROOT ) THEN
            CALL FTMKYJ( FUNIT, 'OBSCNT', NOBSID,
     :                   'Number of root-ancestor headers', FSTAT )
         END IF

*  FILEID header
*  =============

*  Inquire the filename.
         CALL FTFLNM( FUNIT, FNAME, STATUS )

*  *** Alert!  UNIX assumption. *** We need a generic routine to extract
*  the filename, path, and extension. ***
         CALL CHR_LASTO( FNAME, '/', CPOS )
         
         CALL CHR_LASTO( FNAME, '.', SOE )
         IF ( SOE .EQ. 0 ) SOE = CHR_LEN( FNAME ) + 1

         CALL FTPKYS( FUNIT, 'FILEID', FNAME( CPOS + 1 : SOE - 1 ), 
     :                'Filename minus extension', FSTAT )

*  Write a blank header.
         CARD = ' '
         CALL FTPREC( FUNIT, CARD, FSTAT )
      END IF


*  Come here is there has been an error.
  999 CONTINUE

*  Check for an error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_PCADC_ERR', 'FTPKYJ',
     :                   'Error writing provenance header card.',
     :                   STATUS )
      END IF

      END
