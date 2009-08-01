      SUBROUTINE POL1_IMPRT( NFITS, FITS, FD, FNAME, XLOC,
     :                       STATUS )
*+
*  Name:
*     POL1_IMPRT

*  Purpose:
*     Imports FITS information into the POLPACK extension of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_IMPRT( NFITS, FITS, FD, FNAME, XLOC, STATUS )

*  Description:
*   This routine reads and interprets the contents of the import control 
*   table using the supplied file descriptor. Values are stored in the 
*   supplied HDS structure for any valid POLPACK extenson items defined
*   by the control table. References to FITS keywords within the control
*   table are resolved by searching the supplied array of FITS header
*   cards.

*  Arguments:
*     NFITS = INTEGER (Given)
*        The number of FITS cards (array elements) in FITS.
*     FITS( NFITS )= CHARACTER * ( * ) (Given)
*        Character array containing the FITS information stored in NDF
*        NDF (this is probably the mapped character array in the NDF
*        extension).
*     FD = INTEGER (Given)
*        A file descriptor for the file containing the import control table.
*     FNAME = CHARACTER * ( * ) (Given)
*        The name of the control table.
*     XLOC = CHARACTER * ( * ) (Given)
*        The locator for the POLPACK extension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-APR-1999 (DSB):
*        Original version.
*     31-JUL-2009 (TIMJ):
*        QUIET handling is done via MSG_IFGET now.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP parameters
      INCLUDE 'DAT_PAR'          ! HDS/DAT parameters
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER NFITS
      CHARACTER FITS( NFITS )*(*)
      INTEGER FD
      CHARACTER FNAME*(*)
      CHARACTER XLOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXBUF              ! Max length of a logical control table line
      PARAMETER ( MXBUF = 512 )

      INTEGER NTYPE              ! No. of supported HDS data-types
      PARAMETER ( NTYPE = 6 )

*  Local Variables:
      INTEGER FCHAN              ! AST FitsChan containing FITS keywords
      INTEGER I                  ! Loop variable
      INTEGER ILINE              ! Index of previous logical line
      INTEGER IGRP1              ! GRP group identifier for HDS data-types
      INTEGER IGRP2              ! GRP group identifier for FITS keywords
      INTEGER BUFLEN             ! Used length of BUFFER
      INTEGER SPACE              ! Index of first space in BUFFER
      LOGICAL EOF                ! Has end of file been reached?
      LOGICAL OK                 ! Is the value recognised?
      CHARACTER BUFFER*( MXBUF ) ! A logical line read from the control table
      CHARACTER HDSTYP( NTYPE )*(DAT__SZTYP)! Supported HDS data-types
      CHARACTER TYPE*(DAT__SZTYP)! HDS data-type

*  Local Data:
      DATA HDSTYP / '_INTEGER', '_REAL', '_DOUBLE', '_WORD', '_BYTE',
     :              '_CHAR' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the FITS header cards into an AST FitsChan. A FitsChan provides a
*  convenient tool for searching FITS cards, determining their data types, 
*  and converting between data types.
      FCHAN = AST_FITSCHAN( AST_NULL, AST_NULL, ' ', STATUS )
      DO I = 1, NFITS
         CALL AST_PUTFITS( FCHAN, FITS( I ), .TRUE., STATUS )
      END DO

*  Loop round reading lines from the control table until the end-of-file
*  is reached.
      EOF = .FALSE.
      ILINE = 0
      IGRP1 = GRP__NOID
      IGRP2 = GRP__NOID
      DO WHILE( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) 

*  Read a logical line from the file. This may be made up of several
*  physical lines concatenated together. Concatenation is indicated by a
*  physical line which ends with a minus sign. This call also ignores
*  comments, blank lines, etc, and generally tidies up the returned string.
         IF( FNAME .NE. ' ' ) THEN
            CALL CCD1_RDLIN( FD, MXBUF, BUFFER, BUFLEN, ILINE, EOF,
     :                       STATUS )
         ELSE
            CALL POL1_DFTAB( MXBUF, BUFFER, BUFLEN, ILINE, EOF, STATUS )
         END IF

*  Check a new line was ontained succesfully.
         IF ( STATUS .EQ. SAI__OK .AND. .NOT. EOF ) THEN

*  CCD1_RDLIN removes leading spaces, so the first space in the line
*  marks the end of the first word.
            SPACE = INDEX( BUFFER, ' ' ) 

*  Ignore blank lines (shouldn't be any of these).
            IF( SPACE .GT. 1 ) THEN

*  Report an error if the rest of the line is blank.
               IF( SPACE + 1 .GT. BUFLEN .AND. 
     :             STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'POL1_IMPR_ERR1','Incomplete '//
     :                          'line found.', STATUS )
                  GO TO 999
               END IF

*  Convert the first word to upper case.
               CALL CHR_UCASE( BUFFER( : SPACE - 1 ) )

*  See if the first word is the name of a legal POLPACK extension item.
               CALL POL1_KNEXT( BUFFER( : SPACE - 1 ), OK, TYPE, 
     :                          STATUS )

*  If so, evaluate the expression and store a value for the extension item.
               IF( OK ) THEN
                  CALL POL1_XEVAL( BUFFER( : SPACE - 1 ), 
     :                             BUFFER( SPACE + 1 : BUFLEN ),
     :                             TYPE, XLOC, FCHAN, IGRP1,
     :                             IGRP2, STATUS )

*  Delete any groups holding explicit FITS keyword declarations for the
*  expression just evaluated.
                  IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, 
     :                                                       STATUS )
                  IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, 
     :                                                       STATUS )

*  If the first word is not the name of a legal POLPACK extension item,
*  see if it is a legal HDS data type string.
               ELSE

                  OK = .FALSE.
                  DO I = 1, NTYPE
                     IF( BUFFER( : SPACE - 1 ) .EQ. HDSTYP( I ) ) THEN
                        OK = .TRUE.
                        TYPE = HDSTYP( I )
                        GO TO 10
                     END IF
                  END DO
 10               CONTINUE

*  If so, assume the line is an explicit FITS keyword declaration to be
*  used in the next expression. The remainder of the line should correspond 
*  to a FITS keyword name. Store the keyword and data type in two GRP groups.
                  IF( OK ) THEN

                     IF( IGRP1 .EQ. GRP__NOID ) THEN
                        CALL GRP_NEW( 'HDS data types', IGRP1, STATUS )
                        CALL GRP_NEW( 'FITS keywords', IGRP2, STATUS )
                     END IF                        

                     CALL CHR_RMBLK( BUFFER( : SPACE - 1 ) )
                     CALL CHR_UCASE( BUFFER( : SPACE - 1 ) )
                     CALL GRP_PUT( IGRP1, 1, BUFFER( : SPACE - 1 ), 0, 
     :                             STATUS )

                     CALL CHR_RMBLK( BUFFER( SPACE + 1 : BUFLEN ) )
                     CALL CHR_UCASE( BUFFER( SPACE + 1 : BUFLEN ) )
                     CALL GRP_PUT( IGRP2, 1, 
     :                             BUFFER( SPACE + 1 : BUFLEN ), 0, 
     :                             STATUS )

*  If the first word is not a legal HDS data type, report an error.
                  ELSE IF( STATUS .EQ. SAI__OK ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'POL1_IMPR_ERR2','Line does not '//
     :                             'start with a supported HDS data'//
     :                             ' type or POLPACK extension item.', 
     :                             STATUS )
                     GO TO 999

                  END IF

               END IF

            END IF

         END IF

      END DO

*  Tidy up.
 999  CONTINUE

*  Give some context if an error occurred reading a line of the control
*  table.
      IF( STATUS .NE. SAI__OK .AND. ILINE .GT. 0 ) THEN
         CALL MSG_SETI( 'LINE', ILINE )

         IF( FNAME .NE. ' ' ) THEN
            CALL MSG_SETC( 'TAB', FNAME )
            CALL ERR_REP( 'POL1_IMPRT_ERR3', 'Error at or before line'//
     :                    ' ^LINE of import control table ''^TAB'':', 
     :                    STATUS )

         ELSE
            CALL ERR_REP( 'POL1_IMPRT_ERR4', 'Error at or before line'//
     :                    ' ^LINE of the default import control table:', 
     :                    STATUS )
         END IF

         CALL MSG_SETC( 'BUF', BUFFER )
         CALL ERR_REP( 'POL1_IMPRT_ERR5', '   ^BUF', STATUS )

      END IF

*  Delete any groups holding FITS keyword declarations.
      IF( IGRP1 .NE. GRP__NOID ) CALL GRP_DELET( IGRP1, STATUS )
      IF( IGRP2 .NE. GRP__NOID ) CALL GRP_DELET( IGRP2, STATUS )

*  Delete the AST FitsChan holding the FITS headers.
      CALL AST_ANNUL( FCHAN, STATUS )

      END
