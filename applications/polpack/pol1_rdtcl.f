      SUBROUTINE POL1_RDTCL( FILE, CIREF, CIOUT, TRANS, I, Q, U, V, 
     :                       DI, DQ, DU, DV, STATUS )
*+
*  Name:
*     POL1_RDTCL

*  Purpose:
*     Reads data from a text file holding a Tcl code fragment describing a
*     catalogue and copies the data into a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     POL1_RDTCL( FILE, CIREF, CIOUT, TRANS, I, Q, U, V, DI, DQ, DU, 
*                 DV, STATUS )

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
*     TRANS = LOGICAL (Read)
*        Translate column names given by parameters I, DI, etc, into the
*        equivalent standard POLPACK column names?
*     I = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing I values.
*     Q = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing Q values.
*     U = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing U values.
*     V = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing V values.
*     DI = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing DI values.
*     DQ = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing DQ values.
*     DU = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing DU values.
*     DV = CHARACTER * ( * ) (Given) 
*        The name of the column within CI1 containing DV values.
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
      LOGICAL TRANS
      CHARACTER I*(*)
      CHARACTER Q*(*)
      CHARACTER U*(*)
      CHARACTER V*(*)
      CHARACTER DI*(*)
      CHARACTER DQ*(*)
      CHARACTER DU*(*)
      CHARACTER DV*(*)

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
      INTEGER DECCOL             ! Zero based index of DEC column in Tcl file
      INTEGER GI( MXCOL )        ! Column identifier in ref catalogue
      INTEGER GOTWCS             ! Non-zero if Tcl file has RA/DEC columns 
      INTEGER L                  ! Column index in ref catalogue
      INTEGER II                 ! CAT identifier for a parameter
      INTEGER J                  ! Column index in tcl file
      INTEGER COLID( MXCOL )     ! Output column ID for for each tcl col
      INTEGER JCOLCI( MXCOL )    ! Col no. in tcl file for eahc ref cat col
      INTEGER K                  ! Row index
      INTEGER LSTAT              ! Secondary status value
      INTEGER NC                 ! Length of string
      INTEGER NCOL               ! No of columns in Tcl file
      INTEGER NCOLCI             ! No of columns in ref catalogue
      INTEGER NROW               ! No of rows in tcl file 
      INTEGER NWRD               ! No. of words in a row of data from Tcl file 
      INTEGER RACOL              ! Zero-based index of RA column in Tcl file
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
*  Translate them into the corresponding column names in the reference
*  catalogue.
      DO L = 1, NCOLCI
         CALL CAT_TNDNT( CIOUT, CAT__FITYP, L, GI( L ), STATUS ) 
         CALL CAT_TIQAC( GI( L ), 'NAME', CNAMCI( L ), STATUS ) 

         IF( TRANS ) THEN 
            IF( CNAMCI( L ) .EQ. 'I' ) THEN
               CNAMCI( L ) = I
            ELSE IF( CNAMCI( L ) .EQ. 'Q' ) THEN
               CNAMCI( L ) = Q
            ELSE IF( CNAMCI( L ) .EQ. 'U' ) THEN
               CNAMCI( L ) = U
            ELSE IF( CNAMCI( L ) .EQ. 'V' ) THEN
               CNAMCI( L ) = V
            ELSE IF( CNAMCI( L ) .EQ. 'DI' ) THEN
               CNAMCI( L ) = DI
            ELSE IF( CNAMCI( L ) .EQ. 'DQ' ) THEN
               CNAMCI( L ) = DQ
            ELSE IF( CNAMCI( L ) .EQ. 'DU' ) THEN
               CNAMCI( L ) = DU
            ELSE IF( CNAMCI( L ) .EQ. 'DV' ) THEN
               CNAMCI( L ) = DV
            END IF
         END IF

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
      IF( GOTWCS .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the ra and dec column indices in the Tcl file, convert from zero
*  to one based.
         CALL POL1_TCLGT( 'ra_col_', -1, TEXT, NC, STATUS )
         CALL CHR_CTOI( TEXT, RACOL, STATUS )
         RACOL = RACOL + 1

         CALL POL1_TCLGT( 'dec_col_', -1, TEXT, NC, STATUS )
         CALL CHR_CTOI( TEXT, DECCOL, STATUS )
         DECCOL = DECCOL + 1

*  Check that the conversions from text to integer went OK.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_STAT( LSTAT )
            IF( LSTAT .EQ. SAI__OK ) THEN
               CALL MSG_SETC( 'T', TEXT )
               CALL ERR_REP( 'POL1_RDTCL_ERR4', 'Unable to interpret '//
     :                       'string ''^T''.', STATUS )
            END IF
            GO TO 999
         END IF

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
      CALL POL1_RDTDT( FILE, NCOL, COLID, CIOUT, STATUS )

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
