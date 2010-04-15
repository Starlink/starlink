      SUBROUTINE SUB_SRCH(FILENAME, STATUS )

*  Name:
*     SUB_SRCH

*  Purpose:
C      This program reads the current parameter values and searches
C      the specified catalogues for all the stars within the
C      selected area, subject to any selection criteria which have been
C      laid down.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUB_SRCH( FILENAME, STATUS )

*  Description:
C      The subroutines called are all minor modifications of
C      those in the earlier version of CHART, written by P B Taylor.
C      They in turn were derived from earlier work by Roger Wood
C      and Bill Nicholson, all at RGO. Several other people at RGO
C      also worked on various aspects of these programs at various
C      times.
*     {routine_description}

*  Arguments:
*     FILENAME = CHARACTER * ( 55 ) (Given)
*        {argument_description}
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     {author_identifier}: K F Hartley (RGO)
*     {enter_new_authors_here}

*  History:
*     11-1-83 (K F Hartley):
*        Original version.
*     23-FEB-1993 (AJJB):
*        Conversion to ADAM.
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CONST call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
*     22-MAR-1993 (AJJB):
*        Replaced OPEN statement, with a call to
*        a new routine, FILEOPEN, which was found to be necessary when
*        porting, as the READONLY specifier is used which is
*        necessary on the Vax but unsupported on the Sun machines, so
*        that we can just have a different version of FILEOPEN for the
*        different machines to take care of this.
*     21-APR-1993 (AJJB):
*        Changed STATUS specifier in OPEN statement which opens
*        chartint.dat from NEW to UNKNOWN, so that on Unix machines if
*        chartint.dat already exists it will use the existing version
*        instead of just complaining about it, and if it doesn't it will
*        create a new one.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Variables:
      INCLUDE 'MAIN'             ! CHART control common blocks
*        AO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE RA AT EQUINOX 'EQUOUT'
*        DO = DOUBLE PRECISION (Read and Write)
*           FIELD CENTRE DEC AT EQUINOX 'EQUOUT'
*        {global_name}[dimensions] = {data_type} ({global_access_mode})
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'CONVF'            ! /CONVF/ common block
*
*  The globals in /CONVF/ common don't appear to be used in the code,
*  but the author saw fit to include this common so I left it in.(AJJB)
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

      INCLUDE 'CHT_ERR'          ! CHART error constants
*        {descriptions_of_global_variables_referenced}...

*  Arguments Given:
      CHARACTER * ( 55 ) FILENAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IFC, IPARO
      LOGICAL EOI
*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

C
C   If the field centres are to come from a file, then open it.
C
      IFC=3
      IF (BATCH) THEN

* This statement :
*
*        OPEN (UNIT=IFC,FILE=FILENAME,ERR=800,STATUS='OLD',READONLY)
*
* is replaced by this call ( see History ):

         CALL FILEOPEN( IFC, FILENAME, 'OLD', ' ', ' ', .FALSE., 0,
     :                  .TRUE., STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 800

      END IF

C
C   Open a file for the intermediate output.
C
      IPARO=4
      OPEN (UNIT=IPARO,FILE='chartint.dat',FORM='UNFORMATTED',
     :      STATUS='UNKNOWN',ERR=810)
      REWIND IPARO
C
C   Return to here until end-of-file or null response
C
  100 CONTINUE
         CALL FINPUT( IFC, EOI, STATUS )
C
C      Check that a valid position has been defined.
C
         IF ( EOI ) GO TO 899
C
C      Go ahead and search the catalogues.
C
         CALL SELECT('NORMAL', STATUS)
         IF (STATUS .EQ. CHT__NOCAT) GOTO 899
C
C      Set up plate constants to be stored with the data.
C
         CALL CONST(AO,DO, STATUS )
C
C      Sort them (WHY???)
C
C        CALL SORT( STATUS )
C
C      Store the results in an intermediate file.
C
         CALL STORE(IPARO, STATUS )
         GO TO 100
C
C   Come here if there is an error opening the file.
C
  800 CONTINUE
      STATUS = CHT__NOFIL
      CALL ERR_REP(' ', 'Error opening the file of field centres',
     :              STATUS)
      CALL ERR_FLUSH( STATUS )
      GO TO 899
C
C   Come here if there is an error opening the intermediate file.
C
  810 CONTINUE
      STATUS = CHT__NOFIL
      CALL ERR_REP(' ', 'Failed to open the intermediate file',  STATUS)
      CALL ERR_FLUSH( STATUS )
      GO TO 899
C
C   This is the normal exit point.
C
  899 CONTINUE
      CLOSE (UNIT=IPARO)
      CLOSE (UNIT=IFC)
      CLOSE (UNIT=7)
      END
