      SUBROUTINE CCDIMP( STATUS )
*+
*  Name:
*     CCDIMP

*  Purpose:
*     Re-imports FITS information into the CCDPACK extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDIMP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application extracts the values of FITS keywords from a FITS
*     extension in an NDF and uses them to re-construct a CCDPACK
*     extension.  The list of new extension components required, their
*     data types and the names of the FITS keywords from which to
*     derive their values are specified in a "keyword translation
*     table" held in a separate text file.
*
*     Note this is a "private" CCDPACK application and is only
*     supported for use in transferring extension information
*     during NDF library foreign data conversion processes. It is
*     not a general user application.

*  Usage:
*     ccdimp ndf table

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF in which the new extension is to be created.
*     TABLE = FILE (Read)
*        The text file containing the CCDPACK keyword translation table.
*        The format of this file is described under "Table Format".

*  Notes:
*     If a TRANSFORM structure is located in the CCDPACK extension
*     already then it will not be overwritten. This is based on the
*     assumption that the FITS2NDF program is most likely to have
*     restored it (and it will be complete already). Could really extend
*     this principle to whole of CCDPACK extension... at least for FITS.

*  Table Format:
*     The keyword translation table should be held in a text file, with
*     one extension component specified per line.  Each line should
*     contain 3 fields, separated by spaces and/or tabs, as follows.
*
*     -  Field 1:
*        The name of the component in the output extension for which a
*        value is to be obtained.
*
*     -  Field 2:
*        The data type of the output component, to which the keyword
*        value will be converted (one of _INTEGER, _REAL, _DOUBLE,
*        _LOGICAL or _CHAR).
*
*     -  Field 3:
*        The name of the FITS keyword from which the value is to be
*        obtained.
*
*     Comments may appear at any point in the table and should begin
*     with an exclamation mark. The remainder of the line will then be
*     ignored.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     4-MAR-1997 (PDRAPER):
*        Original version, heavily based on KAPPA:FITSIMP.
*     12-NOV-1997 (PDRAPER):
*        Modified to switch off NDF conversion.
*     14-NOV-1997 (PDRAPER):
*        Stopped existing TRANSFORM structures from being overwritten.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! Data-system constants
      INCLUDE 'NDF_PAR'         ! NDF_ public constants
      INCLUDE 'FIO_ERR'         ! FIO_ error codes
      INCLUDE 'TRN_PAR'         ! TRANSFORM constants
      INCLUDE 'CCD1_PAR'        ! CCDPACK constants
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Significant length of a string

*  Local Variables:
      CHARACTER * ( 1 ) C       ! Single number
      CHARACTER * ( 1 ) TOK( 3 ) ! Buffer for parsing lines
      CHARACTER * ( 132 ) LINE  ! Buffer for file reading
      CHARACTER * ( 16 ) KEYWRD ! FITS keyword
      CHARACTER * ( 80 ) CVAL   ! Character FITS value
      CHARACTER * ( CCD1__SZTRN ) TRNC( 6 ) ! TRANSFORM workspace
      CHARACTER * ( DAT__SZLOC ) CCDLOC ! CCDPACK extension locator
      CHARACTER * ( DAT__SZLOC ) LOC ! FITS extension locator
      DOUBLE PRECISION DVAL     ! Double precision FITS value
      INTEGER CARD              ! Fits header element (card) found
      INTEGER DOCVT             ! Current NDF foreign conversion status
      INTEGER EL                ! Number of FITS header records mapped
      INTEGER FIRST             ! First value of range
      INTEGER HIC               ! Position of comment character
      INTEGER I                 ! Loop variable
      INTEGER I1( 3 )           ! Pointer to start of tokens
      INTEGER I2( 3 )           ! Pointer to end of tokens
      INTEGER IDIN              ! NDF identifier
      INTEGER IFIL              ! File descriptor
      INTEGER ILINE             ! Input line counter
      INTEGER INSERT            ! Range character position
      INTEGER ITMP              ! Dummy integer
      INTEGER IVAL              ! Integer FITS value
      INTEGER LAST              ! Last value of range
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER LENGTH            ! Length of a character being mapped
      INTEGER LSTAT             ! Local status variable
      INTEGER NC                ! Number of characters
      INTEGER NDIM              ! Number of NDF dimensions
      INTEGER NTOK              ! Number of tokens on line
      INTEGER PNTR( 1 )         ! Pointer to mapped FITS headers
      INTEGER TRNI( 2 )         ! TRANSFORM workspace
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF
      LOGICAL FOUND             ! Was a value found?
      LOGICAL LVAL              ! Logical FITS value
      LOGICAL SEETRN            ! TRANSFORM keywords are located
      LOGICAL SPLIT             ! TRANSFORM elements may be split across lines
      LOGICAL THERE             ! TRANSFORM structure already exists
      LOGICAL TRNL( TRN__MXCLS + 2 ) ! TRANSFORM workspace
      REAL RVAL                 ! Real FITS value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Tune NDF to not convert the input data from a foreign format. This
*  routine should always be passed an NDF and conversion on release
*  of the NDF isn't desirable. Note we store the current state of DOCVT
*  and restore it on exit.
      CALL NDF_GTUNE( 'DOCVT', DOCVT, STATUS )
      CALL NDF_TUNE( 0, 'DOCVT', STATUS )

*  Obtain the NDF to be updated.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'UPDATE', IDIN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Open the CCDPACK extension (or a new one).
      CALL CCD1_CEXT( IDIN, .TRUE., 'UPDATE', CCDLOC, STATUS )

*  Find the FITS extension and map it.
      CALL NDF_XLOC( IDIN, 'FITS', 'READ', LOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         GO TO 98
      END IF
      CALL DAT_MAPV( LOC, '_CHAR*80', 'READ', PNTR( 1 ), EL, STATUS )
      LENGTH = 80

*  Open the translation table file and defer error reporting.
      CALL FIO_ASSOC( 'TABLE', 'READ', 'LIST', 0, IFIL, STATUS )
      CALL ERR_MARK

*  Initialise TRANSFORM structure workspace.
      DO 3 I = 1, TRN__MXCLS + 2
         TRNL( I ) = .FALSE.
 3    CONTINUE
      DO 4 I = 1, 6
         TRNC( I ) = ' '
 4    CONTINUE
      TRNI( 1 ) = 0
      TRNI( 2 ) = 0
      SEETRN = .FALSE.

*  Loop to read from the file, counting the lines read.
      ILINE = 0
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      CALL FIO_READ( IFIL, LINE, NC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         ILINE = ILINE + 1

*  If the line is not blank, then look for a comment delimiter. If
*  found, adjust the line length to omit the comment and remove any
*  resulting trailing blanks.
         IF ( NC. NE. 0 ) THEN
            HIC = INDEX( LINE( : NC ), '!' )
            IF ( HIC .NE. 0 ) THEN
               NC = HIC - 1
               IF ( NC .GT. 0 ) NC = CHR_LEN( LINE( : NC ) )
            END IF
         END IF

*  Ignore lines which are now blank. Remove non-printable characters
*  (e.g. tabs) from the rest and convert to upper case.  Decompose the
*  line into tokens.
         IF ( NC .NE. 0 ) THEN
            CALL CHR_CLEAN( LINE( : NC ) )
            CALL CHR_UCASE( LINE( : NC ) )
            CALL CHR_DCWRD( LINE( : NC ), 3, NTOK, I1, I2, TOK, LSTAT )

*  Report an error if more than 3 tokens were found.
            IF ( LSTAT .NE. 0 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCDIMP_JUNK',
     :                       'Extra information at end of line.',
     :                       STATUS )

*  Report an error if fewer than 3 tokens were found.
            ELSE IF ( NTOK .LT. 3 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCDIMP_2FEW',
     :                       'Insufficient information on line.',
     :                       STATUS )
            ELSE

*  Need to check if the item to be written is part of a TRANSFORM
*  structure. If so then use special code to deal with this (the
*  TRANSFORM package is fussy about the types etc. of this so we cannot
*  just unpack blindly).
               IF ( LINE( I1( 1 ): I1( 1 ) + 8 ) .EQ.  'TRANSFORM' )
     :              THEN

*  Some parts of a TRANSFORM structure may extend over several cards, we
*  need to trap these and deal with them correctly.
                  KEYWRD = LINE( I1( 3 ) : I2( 3 ) )
                  SPLIT = .FALSE.
                  IF ( INDEX( LINE( I1(3): I2(3) ), '[' ) .NE. 0 ) THEN
                     SPLIT = .TRUE.
                     CALL CCD1_HDRRN( KEYWRD, FIRST, LAST, INSERT,
     :                                STATUS  )
                     KEYWRD( INSERT: ) = ' '
                  ELSE
                     FIRST = 1
                     LAST = 1
                  END IF
                  DO 5 I = FIRST, LAST

*  Construct the keyword if dealing with a range.
                     IF ( SPLIT ) THEN
                        ITMP = INSERT - 1
                        CALL CHR_PUTI( I, KEYWRD, ITMP )
                     END IF
                     CALL FTS1_GKEYC( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                1,
     :                                KEYWRD, FOUND, CVAL, CARD,
     :                                STATUS,
     :                                %VAL( CNF_CVAL( LENGTH ) ) )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If a TRANSFORM value was found, then inform the necessary routine
*  to store it ready for reconstructing the whole TRANSFORM.
                        IF ( FOUND ) THEN
                           SEETRN = .TRUE.
                           CALL CCD1_ENTRN( CCDLOC, .TRUE.,
     :                                      LINE( I1( 1 ): I2( 1 ) ),
     :                                      CVAL, TRNI, TRNC,
     :                                      TRNL, STATUS )
                        END IF
                     END IF
 5                CONTINUE
               ELSE

*  Handle each data type separately....
*  Character:
*  =========
*  Obtain a value for the keyword from the FITS header. Note
*  extra %VAL(LENGTH) used to pass length of mapped character array.
                  IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_CHAR' ) THEN
                     CALL FTS1_GKEYC( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                1, LINE( I1( 3 ):I2( 3 ) ), FOUND,
     :                                CVAL, CARD, STATUS,
     :                                %VAL( CNF_CVAL( LENGTH ) ) )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then find its length and store it in the
*  extension.
                        IF ( FOUND ) THEN
                           NC = MAX ( 1, CHR_LEN( CVAL ) )
                           CALL CCG1_STOCC( IDIN,
     :                                      LINE( I1( 1 ):I2( 1 ) ),
     :                                      CVAL( : NC ), STATUS )
                        END IF
                     END IF

*  Double precision:
*  ================
*  Obtain a value for the keyword from the FITS header.
                  ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_DOUBLE' )
     :                    THEN
                     CALL FTS1_GKEYD( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                1, LINE( I1( 3 ):I2( 3 ) ), FOUND,
     :                                DVAL, CARD, STATUS,
     :                                %VAL( CNF_CVAL( LENGTH ) ) )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                        IF ( FOUND ) THEN
                           CALL CCG1_STOCD( IDIN,
     :                                      LINE( I1( 1 ):I2( 1 ) ),
     ;                                      DVAL, STATUS )
                        END IF
                     END IF

*  Integer:
*  =======
*  Obtain a value for the keyword from the FITS header.
                  ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_INTEGER' )
     :                    THEN
                     CALL FTS1_GKEYI( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                1, LINE( I1( 3 ):I2( 3 ) ), FOUND,
     :                                IVAL, CARD, STATUS,
     :                                %VAL( CNF_CVAL( LENGTH ) ) )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                        IF ( FOUND ) THEN
                           CALL CCG1_STOCI( IDIN,
     :                                      LINE( I1( 1 ):I2( 1 ) ),
     :                                      IVAL, STATUS )
                        END IF
                     END IF

*  Logical:
*  =======
*  Obtain a value for the keyword from the FITS header.
                  ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_LOGICAL' )
     :                    THEN
                     CALL FTS1_GKEYL( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                1, LINE( I1( 3 ):I2( 3 ) ), FOUND,
     :                                LVAL, CARD, STATUS,
     :                                %VAL( CNF_CVAL( LENGTH ) ) )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                        IF ( FOUND ) THEN
                           CALL CCG1_STOCL( IDIN,
     :                                      LINE( I1( 1 ):I2( 1 ) ),
     :                                      LVAL, STATUS )
                        END IF
                     END IF

*  Real:
*  ====
*  Obtain a value for the keyword from the FITS header.
                  ELSE IF ( LINE( I1( 2 ):I2( 2 ) ) .EQ. '_REAL' ) THEN
                     CALL FTS1_GKEYR( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                                1, LINE( I1( 3 ):I2( 3 ) ), FOUND,
     :                                RVAL, CARD, STATUS,
     :                                %VAL( CNF_CVAL( LENGTH ) ) )
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  If a value was found, then store it in the extension.
                        IF ( FOUND ) THEN
                           CALL CCG1_STOCR( IDIN,
     :                                      LINE( I1( 1 ):I2( 1 ) ),
     :                                      RVAL, STATUS )
                        END IF
                     END IF

*  If the data type was not recognised, then report an error.
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETC( 'TYPE', LINE( I1( 2 ):I2( 2 ) ) )
                     CALL ERR_REP( 'CCDIMP_TYPE',
     :                    'Invalid data type ''^TYPE'' specified.',
     :                    STATUS )
                  END IF
               END IF
            END IF

*  If a fatal error has occurred, report which line of the translation
*  table it occurred in and display the line contents.
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'ILINE', ILINE )
               CALL ERR_REP( 'CCDIMP_ILINE',
     :              'Error occurred in line ^ILINE of the '/
     :              /'keyword translation table $TABLE.', STATUS )
               CALL MSG_SETC( 'LINE', LINE )
               CALL ERR_REP( 'CCDIMP_LINE',
     :              'Line read was: ''^LINE''.', STATUS )
            END IF
         END IF
         GO TO 1
      END IF

*  Annul end-of-file errors, end the deferral of error reporting and
*  close the translation table file.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
      CALL ERR_RLSE
      CALL FIO_CLOSE( IFIL, STATUS )

*  If necessary write the new TRANSFORM structure (only do this if one
*  doesn't exist already).
      IF ( SEETRN ) THEN
         CALL DAT_THERE( CCDLOC, 'TRANSFORM', THERE, STATUS )
         IF ( .NOT. THERE ) THEN
            CALL CCD1_ENTRN( CCDLOC, .FALSE., LINE, CVAL,
     :                       TRNI, TRNC, TRNL, STATUS )
         END IF
      END IF

*  See if the LBOUND1 and LBOUND2 keywords are present. If so use these
*  to set the NDF origin information --- hack as IRAF2NDF does not do
*  this 13-11-1997 (FITS does this correctly).
      CALL ERR_MARK
      CALL NDF_BOUND( IDIN, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      DO 6 I = 1, NDIM
         KEYWRD = ' '
         CALL CHR_ITOC( I, C, NC )
         KEYWRD = 'LBOUND'//C( 1 : NC )
         CALL FTS1_GKEYI( EL, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 1,
     :                    KEYWRD, FOUND, IVAL, CARD, STATUS,
     :                    %VAL( CNF_CVAL( LENGTH ) ) )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            GO TO 7
         END IF
         IF ( FOUND ) THEN
            LBND( I ) = IVAL - LBND( I )
         ELSE
            GO TO 7
         END IF
 6    CONTINUE
      CALL NDF_SHIFT( NDIM, LBND, IDIN, STATUS )

*  Exit from above loop when in error.
 7    CONTINUE
      CALL ERR_RLSE

*  Annul (thereby unmapping) the FITS extension locator and the NDF
*  identifier. Jump to 98 when FITS extension doesn't exist.
      CALL DAT_ANNUL( LOC, STATUS )
 98   CONTINUE
      CALL DAT_ANNUL( CCDLOC, STATUS )
      CALL NDF_END( STATUS )

*  Restore the NDF tuning parameter.
 99   CONTINUE
      CALL NDF_TUNE( DOCVT, 'DOCVT', STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CCDIMP_ERR',
     :   'CCDIMP: Error importing FITS information into an NDF ' //
     :   'extension.',
     :   STATUS )
      END IF

      END
