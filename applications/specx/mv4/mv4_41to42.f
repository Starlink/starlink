      SUBROUTINE MV4_41TO42( NAMEMP, STATUS )
*+
*  Name:
*     MV4_41TO42

*  Purpose:
*     Convert v4.1 map file to v4.2.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MV4_V41TO42(  )

* Description:
*     Converts an old HDS format map to the new format.
*

*  Arguments:
*     Old filename (changed to the new one on return) and STATUS

*  Prior Requirements:
*     The HDS system must have been started and the NDF system must have
*     been begun.

*  Authors:
*     timj: Tim Jenness (JACH)
*     {enter_new_authors_here}

*  History:
*     11 Oct 1995 (timj):
*         First attempt
*     16 Aug 2004 (timj):
*         Use CNF_PVAL
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'DAT_ERR'          ! Standard ERR constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL

*  Global Variables:
*     none


*  Functions
      INTEGER CHR_LEN            ! Used length of string

*  Local variables
      INTEGER   ONE( 2 )         ! Itself
      INTEGER   DIM( 2 )         ! Index dimensions
      INTEGER   CNT              ! Loop counter
      INTEGER   STATUS           ! Starlink status
      INTEGER   OLDNDF           ! Index of old file
      INTEGER   NEWNDF           ! New NDF identifier
      INTEGER   PNDF             ! NDF identifier of NEW.POSN
      INTEGER   INDF             ! NDF identifier of OLD.POSN
      INTEGER   MPLACE           ! Temporary placeholder
      INTEGER   SPLACE           ! Temporary placeholder
      INTEGER   NUMSPEC          ! Temporay number of spectra
      INTEGER   NUMPTS           ! Temporary number of points
      INTEGER   DUMMY            ! Dummy variable
      INTEGER   PPTR             ! Temporary pointer to NEWNDF.POSN
      INTEGER   IPTR             ! Temporary pointer to OLDNDF.POSN
      REAL      MAPVERS          ! Map version number (4.2)
      CHARACTER * ( 80 ) NAMEMP  ! Name of old file
      CHARACTER * ( 80 ) NEWNAME ! New map name so dont overwrite old one
      CHARACTER * ( DAT__SZLOC ) TLOC( 2 ) ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) PLOC      ! Temporary locator
      CHARACTER * ( DAT__SZLOC ) ILOC      ! Temporary locator

* Variables from mapopen.f
      INTEGER JDEF
      CHARACTER * ( 3 ) MAPFORM
      INTEGER GEN_ILEN
      INTEGER LNAME




*  History variables
      CHARACTER * ( NDF__SZHUM ) HMODE         ! History mode
      CHARACTER * ( NDF__SZHIS ) HISTORY       ! History text
      CHARACTER * ( NDF__SZAPP ) APPN          ! Application name
      LOGICAL  REPL, TRANS, WRAP, RJUST
      INTEGER HLEN               ! Length of history string

*  Local Data:
      DATA ONE / 1, 1 /
      DATA MAPVERS / 4.2 /

*.

*  Starlink error system
      STATUS = SAI__OK
      CALL ERR_MARK


*  Default filename is conv_map
      NEWNAME = 'conv'

*  Get file name, append .map extension.
*  Code stolen from start of mapopen.f
      WRITE( MAPFORM, '(''A'',I2.2)' ) MAX( 10, GEN_ILEN(NEWNAME) )
      CALL GEN_GETSTR( 'File name? (extension will be _map.sdf)',
     :   NEWNAME(:LNAME(NEWNAME)), MAPFORM, NEWNAME, JDEF )
      IF ( JDEF .GE. 0 ) THEN
         NEWNAME = NEWNAME(:LNAME(NEWNAME))//'_map'
      END IF

*  Steal this bit from mv4_cnv321.f
*  Before we seriously try to create the new map file we try to open it
*  for read-only. This should fail.
      CALL HDS_OPEN( NEWNAME, 'READ', ILOC, STATUS )
      IF ( STATUS .NE. DAT__FILNF ) THEN
         CALL DAT_ANNUL( ILOC, STATUS )
         WRITE( *, * ) 'Output file exists, no conversion!'
         STATUS = SAI__ERROR
         RETURN
      END IF
      CALL ERR_ANNUL( STATUS )


*  Open the old file for READ access.
      CALL NDF_OPEN( DAT__ROOT, NAMEMP, 'READ', 'OLD', OLDNDF,
     :               MPLACE, STATUS )

*  Need to open the new file for WRITE access

      CALL NDF_OPEN( DAT__ROOT, NEWNAME, 'WRITE', 'NEW', NEWNDF,
     :               MPLACE, STATUS )

*  Copy information into it from the old map

      PRINT *, '    Copying map structures to new format file...'

      CALL NDF_SCOPY( OLDNDF, 'DATA,NOEXTENSION(POSN)', MPLACE,
     :     NEWNDF, STATUS )

*  Modify the version number

      CALL NDF_XPT0R( MAPVERS, NEWNDF, 'SPECX_MAP', 'VERSION',
     :                STATUS )

*  Put information into the HISTORY structure

      HMODE = 'NORMAL'
      APPN = 'Specx: open-map'
      REPL = .FALSE.
      HISTORY = 'Map v4.1 original =   '
      HLEN = CHR_LEN(HISTORY)
      HISTORY(HLEN+1:) = NAMEMP
      TRANS = .FALSE.
      WRAP  = .FALSE.
      RJUST = .FALSE.

*  Create history field
      CALL NDF_HCRE( NEWNDF, STATUS )

*  Write information about old file
      CALL NDF_HPUT( HMODE, APPN, REPL, 1, HISTORY, TRANS, WRAP,
     :               RJUST, NEWNDF, STATUS )

*  Disable HISTORY before continuing
      CALL NDF_HSMOD( 'DISABLED', NEWNDF, STATUS )

*  Copy POSN

*  First need to find out how big the POSN array should be
*   ie need NPTS1 and NSPEC

      CALL NDF_XGT0I( OLDNDF, 'SPECX_MAP', 'NSPEC', NUMSPEC,
     :                STATUS )
      CALL NDF_XGT0I( OLDNDF, 'SPECX_MAP', 'NPTS1', NUMPTS,
     :                STATUS )

*  Then need to setup the array to receive these spectra
      DIM( 1 ) = NUMPTS
      DIM( 2 ) = NUMSPEC

      CALL NDF_XNEW( NEWNDF, 'POSN', 'NDF', 0, 1, TLOC(1), STATUS )
      CALL NDF_PLACE( TLOC(1), ' ', SPLACE, STATUS )
      CALL NDF_NEW( '_REAL', 2, ONE, DIM, SPLACE, PNDF, STATUS )
      CALL NDF_MAP(   PNDF, 'DATA', '_REAL', 'WRITE/BAD',
     :   PPTR, DUMMY, STATUS )

*  Then need to go through all 1..NSPEC and find a new pointer
*  to copy each spectra in turn into the array

*  Find where the extension is
      CALL NDF_XLOC( OLDNDF, 'POSN', 'READ', PLOC, STATUS )

*     Map the data of the spectrum in question.

      PRINT *, '    Copying spectra...'
      DO CNT = 1, NUMSPEC
         CALL DAT_CELL( PLOC, 1, CNT, TLOC(2), STATUS )
         CALL NDF_FIND( TLOC(2), ' ', INDF, STATUS )
         CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ',
     :        IPTR, DUMMY, STATUS )
*  Copy the data through the pointers
         CALL MV4_41COPY( %VAL(CNF_PVAL(PPTR)), %VAL(CNF_PVAL(IPTR)),
     :        CNT, NUMPTS, NUMSPEC )
         CALL NDF_UNMAP( INDF, 'DATA', STATUS )
         CALL NDF_ANNUL( INDF, STATUS )
      END DO

*  Close the files

      CALL NDF_UNMAP( PNDF, 'DATA', STATUS )
      CALL NDF_ANNUL( PNDF, STATUS )
      CALL NDF_ANNUL( OLDNDF, STATUS )
      CALL NDF_ANNUL( NEWNDF, STATUS )

*  Need to select the new map now so that mapopn.f can continue
      NAMEMP = NEWNAME

      END


      SUBROUTINE MV4_41COPY( NEWDAT, OLDDAT, IDXNUM, NUMPTS, NUMSPEC )

      IMPLICIT NONE

      INTEGER IDXNUM, NUMPTS, NUMSPEC
      REAL NEWDAT( NUMPTS, NUMSPEC )
      REAL OLDDAT( NUMPTS )
      INTEGER K


      DO K = 1, NUMPTS
         NEWDAT( K, IDXNUM ) = OLDDAT( K )
      END DO

      END
