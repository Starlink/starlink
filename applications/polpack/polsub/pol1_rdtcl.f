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
      CHARACTER CNAM(MXCOL)*30   ! Column names in Tcl file
      CHARACTER CNAMCI(MXCOL)*30 ! Column names in ref catalogue
      CHARACTER DECNAM*30        ! Column names for DEC values
      CHARACTER RANAM*30         ! Column names for RA values
      CHARACTER TEXT*(MXCOL*(VAL__SZD+3)+5)  ! A text buffer
      CHARACTER WORDS(MXCOL)*(VAL__SZD+3) ! Words in a row of data
      INTEGER CNAMTY(MXCOL)      ! Column types in ref catalogue
      INTEGER COLID( MXCOL )     ! Output column ID for for each tcl col
      INTEGER COLTYP( MXCOL )    ! Output column data type for for each tcl col
      INTEGER DECCOL             ! Zero based index of DEC column in Tcl file
      INTEGER DECGID             ! Column id for DEC col
      INTEGER GI( MXCOL )        ! Column identifier in ref catalogue
      INTEGER GOTWCS             ! Non-zero if Tcl file has RA/DEC columns
      INTEGER II                 ! CAT identifier for a parameter
      INTEGER J                  ! Column index in tcl file
      INTEGER JCOLCI( MXCOL )    ! Col no. in tcl file for eahc ref cat col
      INTEGER K                  ! Row index
      INTEGER L                  ! Column index in ref catalogue
      INTEGER LSTAT              ! Secondary status value
      INTEGER NC                 ! Length of string
      INTEGER NCOL               ! No of columns in Tcl file
      INTEGER NCOLCI             ! No of columns in ref catalogue
      INTEGER NROW               ! No of rows in tcl file
      INTEGER NWRD               ! No. of words in a row of data from Tcl file
      INTEGER RACOL              ! Zero-based index of RA column in Tcl file
      INTEGER RAGID              ! Column id for RA col
      INTEGER START( MXCOL )     ! Word starts in a row of data from Tcl file
      INTEGER STOP( MXCOL )      ! Word ends in a row of data from Tcl file
      LOGICAL USEWCS             ! Add equinox/epoch parameters to output?
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

*  Get the column names and identifiers from the output catalogue.
      DO L = 1, NCOLCI
         CALL CAT_TNDNT( CIOUT, CAT__FITYP, L, GI( L ), STATUS )
         CALL CAT_TIQAC( GI( L ), 'NAME', CNAMCI( L ), STATUS )
         CALL CAT_TIQAI( GI( L ), 'DTYPE', CNAMTY( L ), STATUS )
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
      DO J = 1, NCOL
         COLID( J ) = -1
      END DO

      DO L = 1, NCOLCI
         JCOLCI( L ) = 0
         DO J = 1, NCOL
            IF( CNAM( J ) .EQ. CNAMCI( L ) ) THEN
               JCOLCI( L ) = J
               COLID( J ) = GI( L )
               IF( CNAMTY( L ) .EQ. CAT__TYPEI ) THEN
                  COLTYP( J ) = 0
               ELSE IF( CNAMTY( L ) .EQ. CAT__TYPER ) THEN
                  COLTYP( J ) = 1
               ELSE IF( CNAMTY( L ) .EQ. CAT__TYPED ) THEN
                  COLTYP( J ) = 2
               ELSE
                  COLTYP( J ) = 3
               END IF
            END IF
         END DO

         IF( JCOLCI( L ) .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'C', CNAMCI( L ) )
            CALL ERR_REP( 'POL1_RDTCL_ERR3', 'Column ''^C'' is '//
     :                    'required, but is not available in the '//
     :                    'supplied data.', STATUS )
            GO TO 999
         END IF

      END DO

*  If the input file contains WCS, see if we need to save the equinox and
*  epoch in the output catalogue.
      RAGID = -1
      DECGID = -1
      IF( GOTWCS .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the ra and dec column indices in the Tcl file.
         CALL POL1_COLNM( 'RA', .FALSE., RANAM, STATUS )
         CALL POL1_COLNM( 'DEC', .FALSE., DECNAM, STATUS )
         DO J = 1, NCOL
            IF( CNAM( J ) .EQ. RANAM ) THEN
               RACOL = J
               RAGID = COLID( J )
            ELSE IF( CNAM( J ) .EQ. DECNAM ) THEN
               DECCOL = J
               DECGID = COLID( J )
            END IF
         END DO

*  See if the RA and DEC columns are needed in the output catalogue.
         USEWCS = .FALSE.
         DO L = 1, NCOLCI
            IF( JCOLCI( L ) .EQ. RACOL .OR.
     :          JCOLCI( L ) .EQ. DECCOL ) THEN
               USEWCS = .TRUE.
            END IF
         END DO

*  If the WCS columns are used, copy the EQUINOX and EPOCH to the output
*  catalogue. First get the value from the Tcl file.
         IF( USEWCS ) THEN
            CALL POL1_TCLGT( 'equinox_', -1, TEXT, NC, STATUS )

*  Check a value was obtained OK.
            IF( NC .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Try to get a CAT identifier for a pre-existing EQUINOX parameter.
               CALL CAT_TIDNT( CIOUT, 'EQUINOX', II, STATUS )

*  If this was succesful, set the parameter value.
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CAT_TATTC( II, 'VALUE', TEXT( : NC ), STATUS )

*  If an error occurred looking for a pre-existing parameter, creata a
*  new one, setting its value.
               ELSE
                  CALL CAT_PPTSC( CIOUT, 'EQUINOX', TEXT( : NC ),
     :                           'Epoch of reference equinox', II,
     :                           STATUS )
               END IF

*  Relase the identifier
               CALL CAT_TRLSE( II, STATUS )

            END IF

*  Do the same for the epoch.
            CALL POL1_TCLGT( 'epoch_', -1, TEXT, NC, STATUS )
            IF( NC .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN
               CALL CAT_TIDNT( CIOUT, 'EPOCH', II, STATUS )
               IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CAT_TATTC( II, 'VALUE', TEXT( : NC ), STATUS )
               ELSE
                  CALL CAT_PPTSC( CIOUT, 'EPOCH', TEXT( : NC ),
     :                            'Epoch of observation', II, STATUS )
               END IF
               CALL CAT_TRLSE( II, STATUS )
            END IF

         END IF

      END IF

*  Read the row/column data into the catalogue.
      CALL POL1_RDTDT( FILE, NCOL, COLID, COLTYP, RAGID, DECGID, CIOUT,
     :                 STATUS )

*  Arrive here if an error occurs.
 999  CONTINUE

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Delete the Tcl interpreter.
      CALL POL1_TCLDL( STATUS )

*  Release column identifiers.
      DO L = 1, NCOLCI
         CALL CAT_TRLSE( GI( L ), STATUS )
      END DO

*  End the current error reporting environment.
      CALL ERR_END( STATUS )

      END
