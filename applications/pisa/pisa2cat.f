      SUBROUTINE PISA2CAT( STATUS )
*+
*  Name:
*     PISA2CAT

*  Purpose:
*     Converts a PISA formatted data file into a catalogue.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISA2CAT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     PISA2CAT converts PISAFIND and PISAPEAK results files into
*     catalogues. The output catalogues can be used by CURSA
*     or CATPAC applications.

*  Usage:
*     pisa2cat datatype data cat

*  ADAM Parameters:
*     DATATYPE = LITERAL (Read)
*        The type of PISA data. Either FIND, SIZE or PEAK. FIND
*        indicates that the input data is a results file produced by
*        PISAFIND (RESULTS parameter) and that it contains the main
*        object parameterisations (ie.  position, total intensity, peak
*        intensity etc. ). SIZE indicates that the input file is a
*        results file from PISAFIND, but which contains the pixel sums
*        within different intensity thresholds (from the SIZES
*        parameter). Finally PEAK indicates that the input is a results
*        file from the PISAPEAK routine. [FIND]
*     DATA = FILENAME (Read)
*        Name of the file containing the PISA results to be converted
*        into a catalogue.
*        [PISAFIND.DAT]
*     CAT = FILENAME (Write)
*        Name of the output catalogue.
*        [FIND]

*  Notes:
*     -  The output format of the catalogue can be manipulated by
*        changing the file extension. Without a file extension
*        a binary FITS table is produced. If you add a ".sdf" extension
*        then an HDS catalogue that can be used by CATPAC will
*        be created.

*  Column_names:
*     The column names used in the output catalogues are.
*
*        - DATATYPE = FIND
*             INDEX XPOS YPOS INTENSITY NPIX PEAK ELLIPT ANGLE SXX SYY SXY
*        - DATATYPE = SIZE
*             INDEX A1 A2 A3 A4 A5 A6 A7 A8
*        - DATATYPE = PEAK
*             INDEX RRATIO IRATIO ELLIP ABSSXY

*  Examples:
*     pisa2cat find pisafind.dat find
*        This example converts the results file containing the object
*        parameterisations from PISAFIND to FITS table format. It writes
*        the results to the file find.FIT
*     pisa2cat peak pisapeak.dat peak.sdf
*        This example converts the output from a run of PISAPEAK into
*        an HDS catalogue that can be used with the CATPAC applications.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-NOV-1995 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system status codes.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CI                !  Catalogue identifier
      INTEGER IFS               ! FIO file descriptor
      CHARACTER DTYPE * ( 4 )   ! The type of the file to be
                                ! converted.
      LOGICAL OPNF1             ! Input PISA-type file is open.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the type of PISA file we're dealing with.
      CALL PAR_CHOIC( 'DATATYPE', 'FIND', 'FIND,SIZE,PEAK', .FALSE.,
     :                DTYPE, STATUS )

*  Open the pisa data file.
      CALL PSA1_ASFIO( 'DATA', 'READ', 'LIST', 0, IFS, OPNF1,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Open the catalogue.
      CALL CAT_CREAT( 'CAT', CI, STATUS )

*  And set up catalogue for the PISA type, then transfer the data.
      IF ( DTYPE .EQ. 'FIND' ) THEN
         CALL PSA1_CCATF( CI, IFS, STATUS )
      ELSE IF ( DTYPE .EQ. 'SIZE' ) THEN
         CALL PSA1_CCATS( CI, IFS, STATUS )
      ELSE IF ( DTYPE .EQ. 'PEAK' ) THEN
         CALL PSA1_CCATP( CI, IFS, STATUS )
      END IF

*  Release the catalogue.
      CALL CAT_TRLSE( CI, STATUS )

*  Close the PISA data file
 999  CONTINUE
      IF ( OPNF1 ) THEN
         CALL FIO_CLOSE( IFS, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISA2CAT_ERR',
     :   'PISA2CAT: Error converting PISA file into catalogue',
     :   STATUS )
      END IF

      END
* @(#)pisa2scar.f	1.1     07 Oct 1993     1
