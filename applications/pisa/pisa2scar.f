      SUBROUTINE PISA2SCAR( STATUS )
*+
*  Name:
*     PISA2SCAR

*  Purpose:
*     Converts PISA formatted data files to SCAR/CHI format.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISA2SCAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     PISA2SCAR converts PISAFIND and PISAPEAK results files to SCAR
*     format. The output catalogues can be used either by SCAR or by
*     the CATPAC catalogue manipulation applications. Note that the
*     output catalogues have a DSCFxxxxx file associated with them this
*     should not be deleted as this invalidates the catalogue.

*  Usage:
*     PISA2SCAR DATATYPE DATA SCARCAT

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
*        Name of the file containing the PISA results to be modified to
*        SCAR format. [PISAFIND.DAT]
*     SCARCAT = FILENAME (Write)
*        Name of the catalogue to contain the SCAR formatted data. Note
*        that a file DSCFfilename is also produced, this should not be
*        deleted. [FIND.DAT]

*  Notes:
*     -  VMS specific. Only supplied on VMS.

*  Output_fields:
*     The catalogue output from PISA2SCAR has an associated file
*     DSCFfilename, this contains a description of what the catalogue
*     contains and what the catalogue field names are. Briefly they are
*
*        o DATATYPE = FIND
*           INDEX XPOS YPOS INTENSITY NPIX PEAK ELLIPT ANGLE SXX SYY SXY
*        o DATATYPE = SIZE
*           INDEX A1 A2 A3 A4 A5 A6 A7 A8
*        o DATATYPE = PEAK
*           INDEX RRATIO IRATIO ELLIP ABSSXY

*  Examples:
*     PISA2SCAR FIND PISAFIND.DAT SCARFIND
*        This example converts the results file containing the object
*        parameterisations from PISAFIND to SCAR format. It writes the
*        results to the file SCARFIND and DSCFSCARFIND.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-MAY-1991 (PDRAPER):
*        Original version.
*     3-SEP-1992 (PDRAPER):
*        Changed to CHI version 1.1.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO system status codes.
      INCLUDE 'CHI_PAR'          ! CHI system buffer sizes.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHI__SZNAME ) NAME ! Catalogue name
      INTEGER IFS                      ! FIO file descriptor
      LOGICAL OPNF1                    ! Flag set if file is open
      CHARACTER DTYPE * ( 4 )          ! The type of the file to be
                                       ! converted.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the type of PISA file we're dealing with.
      CALL AIF_CHOIC( 'DATATYPE', 'FIND,SIZE,PEAK', DTYPE, STATUS )

*  Open the pisa data file.
      CALL PSA1_ASFIO( 'DATA', 'READ', 'LIST', 0, IFS, OPNF1,
     :                 STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Open CHI.
      CALL CHI_OPEN( STATUS )

*  Get a name for the output SCAR/CHI catalogue.
      CALL PAR_GET0C( 'SCARCAT', NAME, STATUS )

*  Create a an appropriate PISA type catalogue, and transfer the data.
      IF ( DTYPE .EQ. 'FIND' ) THEN
         CALL PSA1_CCATF( NAME, STATUS )
         CALL PSA1_TCATF( NAME, IFS, STATUS )
      ELSE IF ( DTYPE .EQ. 'SIZE' ) THEN
         CALL PSA1_CCATS( NAME, STATUS )
         CALL PSA1_TCATS( NAME, IFS, STATUS )
      ELSE IF ( DTYPE .EQ. 'PEAK' ) THEN
         CALL PSA1_CCATP( NAME, STATUS )
         CALL PSA1_TCATP( NAME, IFS, STATUS )
      END IF

*  Close down CHI and release the catalogue.
      CALL CHI_CLOSE( STATUS )

*  Close the PISA data file
999   CONTINUE
      IF ( OPNF1 ) THEN
         CALL FIO_CLOSE( IFS, STATUS )
         CALL PAR_CANCL( 'DATA', STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISA2SCAR_ERR',
     :   'PISA2SCAR: Error converting PISA file to SCAR format',
     :   STATUS )
      END IF

      END
* $Id$
