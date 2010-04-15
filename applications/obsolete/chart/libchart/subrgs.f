      SUBROUTINE SUB_RGS( IPARI, STATUS)
*+
*  Name:
*     SUBRGS

*  Purpose:
*     This program reads the intermediate data file and creates a
*     formatted file for output to a printer.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUB_RGS( IPARI, STATUS )

*  Description:
C      This program reads the intermediate data file and creates a
C      formatted file for output to a printer.
C
C      The output contains information relevant to the RGO
C      26 and 13 inch refracting telescopes and the 1 metre camera.
C
C
C      Most of the work is done by subroutines adapted from those
C      in the much earlier ICL 1903T version of Chart!
*     {routine_description}

*  Arguments:
*     IPARI = INTEGER (Given and Returned)
*        {argument_description}
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     ANO: Someone (Somewhere)
*     {enter_new_authors_here}

*  History:
*     {date} ({author_identifier}):
*        Original version.
*     23-FEB-1993 (AJJB):
*        Conversion to ADAM, and proper commenting
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'MAIN'             ! CHART control common blocks
*
*  Globals used from MAIN.FOR:
*
*        CATRUN = LOGICAL (Read)
*           IF '.TRUE.' THEN SELECT STARS FROM PARTICULAR CATALOGUES
*        AO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE RA AT EQUINOX 'EQUOUT'
*        DO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE DEC AT EQUINOX 'EQUOUT'
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'CONVF'            ! /CONVF/ common block
*
*  No globals from CONVF common seem to be used, but it was in the
*  subroutine already so I left it in when converting it to ADAM.
*
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'CHT_ERR'          ! CHART error constants
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given and Returned:
      INTEGER IPARI

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*3 FLDNUM, TEXT * 60
      INTEGER IFLD, IEND

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

C
C   There may be several sets of parameters, corresponding to
C   several fields.
C
C   So - keep restoring until an error occurs.
C
      IFLD=1
  100 CONTINUE
         CALL RESTORE(IEND,IPARI, STATUS )
         IF (IEND.GT.0) GO TO 899
C
C      This program is only designed to run using the astrometric
C      catalogues, so exit if this is not the case.
C
         IF (.NOT.CATRUN) THEN
            CALL MSG_OUT( ' ', ' ', STATUS )
            CALL MSG_OUT(' ',  'This program only works in astrometric'
     :      //' mode', STATUS)
            CALL MSG_OUT(' ', '**************************************'
     :      //'*****', STATUS)
            CALL MSG_OUT(' ', ' ', STATUS)
            GO TO 899
         END IF
         WRITE  (FLDNUM,'(I3)') IFLD
         TEXT='Field number '//FLDNUM//' processed'
         CALL MSG_OUT( ' ', TEXT, STATUS )
         CALL MSG_OUT( ' ', 'Output written to RGOGS.LIS', STATUS )
         IFLD=IFLD+1
         CALL SORT( STATUS )
         CALL HDNG( STATUS )
C
C      Now print the telescope headings
C
         CALL PRT26( STATUS )
         CALL CONST(AO,DO, STATUS )
         CALL GSOUT( STATUS )
         GO TO 100

  899 CONTINUE
      END
