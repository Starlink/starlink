      SUBROUTINE POL1_RDTCL( FILE, CIREF, CIOUT, STATUS )
*+
*  Name:
*     POL1_RDTCL

*  Purpose:
*     Reads data from a text file holding a Tcl code fragment describing a
*     catalogue and copies the data into a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     POL1_RDTCL( FILE, CIREF, CIOUT, STATUS )

*  Description:
*     This routine reads a Tcl list holding the column data in a 
*     catalogue.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the input text file.
*     CIREF = INTEGER (Given)
*        A CAT identifier for the reference catalogue.
*     CIOUT = INTEGER (Given)
*        A CAT identifier for the output catalogue.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-DEC-2000 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'CAT_PAR'          ! CAT__ constants

*  Arguments Given:
      CHARACTER FILE*(*)
      INTEGER CIREF
      INTEGER CIOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCOL
      PARAMETER ( MXCOL = 30 )

*  Local Variables:
      CHARACTER CNAM( 30 )*(MXCOL)! Column names in Tcl file
      CHARACTER CNAMCI( 30 )*(MXCOL)! Column names in ref catalogue
      CHARACTER TEXT*(MXCOL*(VAL__SZD+3)+5)  ! A text buffer
      CHARACTER WORDS( MXCOL )*(VAL__SZD+3) ! Words in a row of data 
      INTEGER GI( MXCOL )        ! Column identifier in ref catalogue
      INTEGER GOTWCS             ! Non-zero if Tcl file has RA/DEC columns 
      INTEGER I                  ! Column index in ref catalogue
      INTEGER II                 ! CAT identifier for a parameter
      INTEGER J                  ! Column index in tcl file
      INTEGER JCOLCI( MXCOL )    ! Col no. in tcl file for eahc ref cat col
      INTEGER K                  ! Row index
      INTEGER LSTAT              ! Secondary status value
      INTEGER NC                 ! Length of string
      INTEGER NCOL               ! No of columns in Tcl file
      INTEGER NCOLCI             ! No of columns in ref catalogue
      INTEGER NROW               ! No of rows in tcl file 
      INTEGER NWRD               ! No. of words in a row of data from Tcl file 
      INTEGER START( MXCOL )     ! Word starts in a row of data from Tcl file 
      INTEGER STOP( MXCOL )      ! Word ends in a row of data from Tcl file 
      REAL VAL                   ! A data value
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of columns in the reference catalogue.
      CALL CAT_TCOLS( CIREF, CAT__GPHYS, NCOLCI, STATUS ) 
      IF( NCOLCI .GT. MXCOL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', NCOLCI )
         CALL MSG_SETI( 'M', MXCOL )
         CALL ERR_REP( 'POL1_RDTCL_ERR1', 'Too many columns (^N) in '//
     :                 'the original catalogue. Must be ^M or less.',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the column names and identifiers from the output catalogue
      DO I = 1, NCOLCI
         CALL CAT_TNDNT( CIOUT, CAT__FITYP, I, GI( I ), STATUS ) 
         CALL CAT_TIQAC( GI( I ), 'NAME', CNAMCI( I ), STATUS ) 
      END DO

*  Create a Tcl interpreter and execute the script in the supplied file.
      CALL POL1_TCLEX( FILE, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Extract the values from the TCL interpreter for the required variables.
      CALL POL1_TCLGT( 'nrow_', -1, TEXT, NC, STATUS )
      CALL CHR_CTOI( TEXT, NROW, STATUS )

      CALL POL1_TCLGT( 'ncol_', -1, TEXT, NC, STATUS )
      CALL CHR_CTOI( TEXT, NCOL, STATUS )

      CALL POL1_TCLGT( 'gotwcs_', -1, TEXT, NC, STATUS )
      CALL CHR_CTOI( TEXT, GOTWCS, STATUS )

      IF( STATUS .EQ. SAI__OK ) THEN 
         DO J = 1, NCOL
            CALL POL1_TCLGT( 'headings_', J - 1, CNAM( J ), NC, 
     :                       STATUS )
         END DO
      END IF

*  If the input file contains WCS, save the epoch and equinox values in
*  the output catalogue.
      IF( GOTWCS .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

         CALL POL1_TCLGT( 'equinox_', -1, TEXT, NC, STATUS )
         IF( TEXT .NE. ' ' ) THEN
            CALL CAT_PPTSC( CIOUT, 'EQUINOX', TEXT( : NC ), 'Epoch of'//
     :                      ' reference equinox', II, STATUS )
            CALL CAT_TATTI( II, 'CSIZE', NC, STATUS ) 
         END IF

         CALL POL1_TCLGT( 'epoch_', -1, TEXT, NC, STATUS )
         IF( TEXT .NE. ' ' ) THEN
            CALL CAT_PPTSC( CIOUT, 'EPOCH', TEXT( : NC ), 'Epoch of '//
     :                      'observation', II, STATUS )
            CALL CAT_TATTI( II, 'CSIZE', NC, STATUS ) 
         END IF

      END IF

*  If any of the strings could not be converted by CHR, STATUS will not be
*  SAI__OK but no call to ERR_REP wil have been made. CHeck for this and
*  call ERR_REP now if required.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_STAT( LSTAT )
         IF( LSTAT .EQ. SAI__OK ) THEN
            CALL MSG_SETC( 'T', TEXT )
            CALL ERR_REP( 'POL1_RDTCL_ERR2', 'Unable to interpret '//
     :                    'string ''^T''.', STATUS )
         END IF
         GO TO 999
      END IF

*  Find the index within the tcl catalogue of each column in the CAT
*  catalogue.
      DO I = 1, NCOLCI
         JCOLCI( I ) = 0
         DO J = 1, NCOL
            IF( CNAM( J ) .EQ. CNAMCI( I ) ) THEN
               JCOLCI( I ) = J 
            END IF
         END DO

         IF( JCOLCI( I ) .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', CNAMCI( I ) )
            CALL ERR_REP( 'POL1_RDTCL_ERR3', 'Column ''^C'' is '//
     :                    'required, but is not available in the '//
     :                    'supplied data.', STATUS )
            GO TO 999
         END IF

      END DO

*  Copy the data from the Tcl file into the CAT catalogue.
      DO K = 0, NROW - 1

*  Get a string holding the column data for this row.
         CALL POL1_TCLGT( 'data_', K, TEXT, NC, STATUS )

*  Split it up into consitituent words.
         CALL CHR_DCWRD( TEXT, NCOL, NWRD, START, STOP, WORDS, LSTAT ) 

*  Check the number of columns.
         IF( NWRD .NE. NCOL .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'N', NWRD )
            CALL MSG_SETI( 'M', NCOL )
            CALL MSG_SETI( 'R', I )
            CALL ERR_REP( 'POL1_RDTCL_ERR4', 'Only ^N values found in'//
     :                    ' row ^R. ^M are required.', STATUS )
            GO TO 999
         END IF

*  Loop round each column in the output catalogue.
         DO I = 1, NCOLCI

*  Abort if an error has occurred.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the corresponding column in the Tcl catalogue.
            J = JCOLCI( I )

*  Decode the value for this column.
            CALL CHR_CTOR( WORDS( J ), VAL, STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETC( 'T', WORDS( J ) )
               CALL ERR_REP( 'POL1_RDTCL_ERR5', 'Unable to interpret '//
     :                       'string ''^T''.', STATUS )
               GO TO 999
            END IF

*  Store it in the current row buffer for the output catalogue.
            CALL CAT_PUT0R( GI( I ), VAL, .FALSE., STATUS )

         END DO

*  Append the current row buffer to the output catalogue.
         CALL CAT_RAPND( CIOUT, STATUS )

* Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999  

      END DO

*  Arrive here if an error occurs.
 999  CONTINUE      

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Delete the Tcl interpreter.
      CALL POL1_TCLDL( STATUS )

*  Release column identifiers.
      DO I = 1, NCOLCI
         CALL CAT_TRLSE( GI( I ), STATUS )
      END DO

*  End the current error reporting environment.
      CALL ERR_END( STATUS )

      END 
