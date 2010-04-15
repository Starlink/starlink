      SUBROUTINE FLA_NWILD( PNIN, PNOUT, GRPI, GRPO, NONDF, STATUS )
*+
*  Name:
*     FLA_NWILD

*  Purpose:
*     Obtains a list of input and output NDFs as GRP groups.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FLA_NWILD( PNIN, PNOUT, GRPI, GRPO, NONDF, STATUS )

*  Description:
*     This routine uses the parameter system to obtain wildcarded lists
*     of input and output NDFs stored in GRP groups.  The output names
*     can be modified input names.  The number of members of each group
*     must be the same.

*  Arguments:
*     PNIN = CHARACTER * ( * ) (Given)
*        Parameter name for the input list of NDFs.
*     PNOUT = CHARACTER * ( * ) (Given)
*        Parameter name for the output list of NDFs.
*     GRPI = INTEGER (Returned)
*        The GRP identifier of the group of input NDFs.  When it is no
*        longer needed this should be annulled with routine GRP_DELET.
*     GRPO = INTEGER (Returned)
*        The GRP identifier of the group of output NDFs.  When it is no
*        longer needed this should be annulled with routine GRP_DELET.
*     NONDF = INTEGER (Returned)
*        Number of NDFs in each group.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (UKATC)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     1998 October 16 (MJC):
*        Original version.
*     1998 October 26 (ACD)
*        Removed commented out subroutine and unused variables.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! Data-system error constants
      INCLUDE 'NDF_PAR'          ! NDF_ constants
      INCLUDE 'GRP_PAR'          ! GRP_ constants
      INCLUDE 'GRP_ERR'          ! GRP_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      CHARACTER * ( * ) PNIN
      CHARACTER * ( * ) PNOUT

*  Arguments Returned:
      INTEGER GRPI
      INTEGER GRPO
      INTEGER NONDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER ADDED              ! Number of items added to a group
      LOGICAL CFLAG              ! True if a group requires further
                                 ! input via continuation lines
      CHARACTER * ( 255 ) FSPEC  ! File specification
      CHARACTER * ( DAT__SZLOC ) HLOC ! Locator to HDS input file
      INTEGER IFILE              ! Loop counter for each input NDF
      INTEGER IGRP1              ! Group identifier of input files
      INTEGER IGRP2              ! Group identifier of input NDFs
      INTEGER IGRP3              ! Group identifier of input purged NDFs
      INTEGER IWILD              ! Counter of the wild-carded files
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      LOGICAL LEAVE              ! True if the NDF-testing is finished
      INTEGER NDF                ! NDF identifier
      INTEGER NDIM               ! NDF dimensions
      INTEGER NIFILE             ! Number of NDF files
      INTEGER NGLIST             ! No. of items in input list
      INTEGER NOFILE             ! Number of output files
      INTEGER OGROUP             ! Group identifier of output NDFs
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise returned arguments
      NONDF = 0
      GRPI = GRP__NOID
      GRPO = GRP__NOID

*  Get file list and check the number of specifications.
*  =====================================================
*
*  Use GRP to get a list of wildcarded filenames.

*  Create a new group to contain the input file names.
      CALL GRP_NEW( 'Input files', IGRP1, STATUS )

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of file names from the environment.
         CALL GRP_GROUP( PNIN, GRP__NOID, IGRP1, NGLIST, ADDED,
     :                   CFLAG, STATUS )

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( PNIN, STATUS )
      END DO

*  Tidy and exit if there has been an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP1, STATUS )
         GOTO 999
      END IF

*  Create a second group to hold the filenames including expanded
*  wildcards.
      CALL GRP_NEW( 'Expanded wild card files', IGRP2, STATUS )

*  Expand the wildcards.
*  =====================
*
*  Initialise the count of the number of files and the index to the
*  expanded file.
      NIFILE = 0
      IWILD = DAT__NOWLD
      DO IFILE = 1, NGLIST

*  Get a file specification from the input group.
         CALL GRP_GET( IGRP1, IFILE, 1, FSPEC, STATUS )

*  Find the files which match this specification.
         LEAVE = .FALSE.

*  Start new error context.
         CALL ERR_MARK

*  Loop for all the files in the wildcard specification.
         DO WHILE ( .NOT. LEAVE )

*  Get a single HDS file that matches this specification.  This assumes
*  a file extension of ".sdf".   However, it does not discriminate
*  between NDFs (there is no NDF_WILD yet).
            CALL HDS_WILD( FSPEC, 'READ', IWILD, HLOC, STATUS )

*  Check if a file has been found and can be read.
            IF ( HLOC .NE. DAT__NOLOC .AND. STATUS .EQ. SAI__OK ) THEN

*  Next validate it as an NDF.
               CALL NDF_FIND( HLOC, ' ', NDF, STATUS )

*  Call something to validate it (up to a point).
               CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM,
     :                         STATUS )

*  Take a bad status to mean that this is not an NDF.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
               ELSE

*  Add this NDF into the output group.  NFILE keeps a count of the
*  number of files in the output group.
                  CALL GRP_GRPEX( FSPEC, GRP__NOID, IGRP2, NIFILE,
     :                            ADDED, CFLAG, STATUS )
               END IF

*  Tidy the NDF.
               CALL NDF_ANNUL( NDF, STATUS )

*  Tidy the HDS file.
               CALL DAT_ANNUL( HLOC, STATUS )

*  Annul a bad status as we want to read files from subsequent entries
*  in the list, but not leave the cycle.  This might have resulted from
*  a file protection, or it has just been deleted.  There is an
*  exception when no HDS files were found on the first call to
*  HDS_WILD.
            ELSE IF ( STATUS .NE. DAT__FILNF .AND.
     :                STATUS .NE. SAI__OK ) THEN
               CALL ERR_ANNUL( STATUS )

            ELSE
*  Go to the next GRP expression.
               LEAVE = .TRUE.

            END IF

         END DO

*  Release the resources assoicated with the wild-card search.
         CALL HDS_EWILD( IWILD, STATUS )
      END DO

*  Release the error context.
      CALL ERR_RLSE

*  Finished with the first group so delete it.
      CALL GRP_DELET( IGRP1, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP2, STATUS )
         GOTO 999
      END IF

*  Purge any duplication from the NDFs.
      CALL GRP_PURGE( IGRP2, IGRP3, STATUS )

*  Finished with the second group so delete it.
      CALL GRP_DELET( IGRP2, STATUS )

*  Find the number of NDFs after the purge.
      CALL GRP_GRPSZ( IGRP3, NIFILE, STATUS )

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  At this point the output group contains the paths and names of the
*  NDFs to be processed.  Tell the user how many have been found to help
*  them supply the appropriate number of BITPIX and output file names.
      CALL MSG_SETI( 'NF', NIFILE )
      IF ( NIFILE .NE. 1 ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'NOFILES', '^NF NDFs selected.',
     :                   STATUS )
      ELSE
         CALL MSG_OUTIF( MSG__NORM, 'NOFILES', '^NF NDF selected.',
     :                   STATUS )
      END IF


*  Use GRP to get a list of wildcarded output NDFs.
*  ================================================

*  Create a new group to contain the output file names.
      CALL GRP_NEW( 'Output files', OGROUP, STATUS )

*  Allow for continuation lines.
      CFLAG = .TRUE.
      DO WHILE ( CFLAG .AND. STATUS .EQ. SAI__OK )

*  Get the list of output file names from the environment.  Allow
*  modification of the input file names.
         CALL GRP_GROUP( PNOUT, IGRP3, OGROUP, NOFILE, ADDED, CFLAG,
     :                   STATUS )

*  Cancel the parameter association in order to get more group values
*  through the parameter, unless there are no more to obtain.
         IF ( CFLAG ) CALL PAR_CANCL( PNOUT, STATUS )
      END DO

*  Tidy up and exit if something went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL GRP_DELET( OGROUP, STATUS )
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Check that the number of input files matches the number of input
*  files.
      IF ( NOFILE .NE. NIFILE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NI', NIFILE )
         CALL MSG_SETI( 'NO', NOFILE )
         CALL ERR_REP( 'FLA_NWILD_FILECOUNT',
     :     'The number of output NDFs (^NO) does not '/
     :     /'equal the number of input NDFs (^NI).', STATUS )

*  Tidy up and exit.
         CALL GRP_DELET( OGROUP, STATUS )
         CALL GRP_DELET( IGRP3, STATUS )
         GOTO 999
      END IF

*  Set the returned arguments.
      NONDF = NIFILE
      GRPI = IGRP3
      GRPO = OGROUP

  999 CONTINUE

      END
