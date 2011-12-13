      SUBROUTINE CCDEXP( STATUS )
*+
*  Name:
*     CCDEXP

*  Purpose:
*     Exports CCDPACK extension information to FITS

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CCDEXP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application places the values of components of the CCDPACK
*     extension into the FITS extension within the same NDF.  This
*     operation is needed if auxiliary data are to appear in the header
*     of a foreign data file converted from the NDF.  The extension
*     items, FITS keyword names and optional FITS inline comment
*     are specified in a "keyword translation table" held in a separate
*     text file.
*
*     Note this is a "private" CCDPACK application and is only
*     supported for use in transferring extension information
*     during NDF library foreign data conversion processes. It is
*     not a general user application.

*  Usage:
*     ccdexp ndf table

*  ADAM Parameters:
*     NDF = NDF (Read and Write)
*        The NDF whose CCDPACK extension is to be exported to
*        the FITS extension.
*     TABLE = FILE (Read)
*        The text file containing the keyword translation table. The
*        format of this file is described under "Table Format".

*  Notes:
*     Before an export occurs all existing CCDPACK items are removed
*     from the headers. This may cause the occasional unexpected
*     behaviour (if for instance you wanted to run this in sequence
*     rather than just once for each export), but is generally the
*     correct thing to do.

*  Table Format:
*     The keyword translation table should be held in a text file, with
*     one extension component specified per line.  Each line should
*     contain three or four fields, separated by spaces and/or tabs, as
*     follows.
*
*     -  Field 1:
*        The name of the input extension component.
*
*     -  Field 2:
*        The data type of the FITS value.
*
*     -  Field 3:
*        The name of the FITS keyword to which the value is to be
*        copied.
*
*     -  Field 4:
*        The comment to appear in the FITS header card for the chosen
*        keyword.  This field is optional.
*
*     Comments may appear at any point in the table and should begin
*     with an exclamation mark.  The remainder of the line will then be
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
*     5-MAR-1997 (PDRAPER):
*        Original version.
*     12-NOV-1997 (PDRAPER):
*        Modified to switch off NDF format conversion.
*     14-NOV-1997 (PDRAPER):
*        Added changes to clear any existing CCDPACK parameters from the
*        headers, before adding the new set.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! Primitive data constants
      INCLUDE 'FIO_ERR'         ! FIO error constants
      INCLUDE 'CCD1_PAR'        ! CCDPACK parameters

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Significant length of a string

*  Local constants:
      INTEGER FITLEN
      PARAMETER ( FITLEN = 43 ) ! Number of characters in FITS value

*  Local Variables:
      CHARACTER * ( 1 ) TOK( 3 ) ! Buffer for parsing lines
      CHARACTER * ( 132 ) LINE  ! Buffer for file reading
      CHARACTER * ( 80 ) CVAL   ! Character FITS value
      CHARACTER * ( CCD1__SZTRN ) TRANS ! Transform string
      CHARACTER KEYWRD * ( 16 ) ! Keyword when includes range
      DOUBLE PRECISION DVAL     ! Double precision FITS value
      INTEGER FD                ! FIO file identifier
      INTEGER FIRST             ! First value in range
      INTEGER HIC               ! Position of comment character
      INTEGER I                 ! Loop variable
      INTEGER I1( 4 )           ! Pointer to start of tokens
      INTEGER I2( 4 )           ! Pointer to end of tokens
      INTEGER I3                ! Position in string
      INTEGER I4                ! Position in string
      INTEGER ILINE             ! Input line counter
      INTEGER INSERT            ! Insertion point in string
      INTEGER ITMP              ! Dummy
      INTEGER IVAL              ! Integer FITS value
      INTEGER LAST              ! Last value in range
      INTEGER LSTAT             ! Local status variable
      INTEGER NC                ! Number of characters
      INTEGER NTOK              ! Number of tokens on line
      INTEGER N                 ! Number of header items
      INTEGER DOCVT             ! Current foreign data conversion state
      LOGICAL LVAL              ! Logical FITS value
      LOGICAL SKIP              ! Skip over rest
      REAL RVAL                 ! Real FITS value
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Tune NDF to not convert the input data from a foreign format. This
*  routine should always be passed an NDF and conversion on release
*  of the NDF isn't desirable. Note we store the current state of DOCVT
*  and restore it on exit.
      CALL NDF_GTUNE( 'DOCVT', DOCVT, STATUS )
      CALL NDF_TUNE( 0, 'DOCVT', STATUS )

*  Open the NDF, so we can modify the extension.
      CALL HDR_MOD( 'IN', STATUS )

*  Open the export table.
      CALL FIO_ASSOC( 'TABLE', 'READ', 'LIST', 0, FD, STATUS )

*  Clear any CCDPKnnn keywords from the headers before adding new ones.
      CALL HDR_NUMB( 'IN', 'FITS', '*', N, STATUS )
      IF ( N .GT. 0 ) THEN
         I = 1
 4       CONTINUE                   ! Do until loop

*  Get the name of the I'th header item (note that deleting a header
*  reorders the remaining ones).
            CALL HDR_NAME( 'IN', ' ', I, KEYWRD, STATUS )
            IF ( KEYWRD .NE. ' ' ) THEN
               IF ( KEYWRD( 1 : 5 ) .EQ. 'CCDPK' ) THEN
                  CALL HDR_DELET( 'IN', 'FITS', KEYWRD, 1, STATUS )
               ELSE
                  I = I + 1         ! Next header, if deleted reshuffle
                                    ! keeps same I
               END IF
            ELSE
               GO TO 5              ! No more items
            END IF
            IF ( I .GT. N ) GO TO 5 ! No more items
         GO TO 4                    ! Next item
      END IF
 5    CONTINUE

*  Read the table and copy over the items.
      ILINE = 0
 1    CONTINUE
      CALL FIO_READ( FD, LINE, NC, STATUS )
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
            CALL CHR_DCWRD( LINE( : NC ), 3, NTOK, I1, I2, TOK, LSTAT )


*  Report an error if fewer than 3 tokens were found.
            IF ( NTOK .LT. 3 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CCDIMP_2FEW',
     :                       'Insufficient information on line.',
     :                       STATUS )
            ELSE

*  If more than 3 tokens where found then the last are the comments.
               IF ( LSTAT .NE. 0 ) THEN
                  CALL CHR_FANDL( LINE( I2( 3 ) + 1: NC ), I1( 4 ),
     :                            I2( 4 ) )
                  I1( 4 ) = I1( 4 ) + I2( 3 )
                  I2( 4 ) = I2( 4 ) + I2( 3 )
               ELSE
                  I1( 4 ) = MIN( NC + 1, 132 )
                  I2( 4 ) = I1( 4 )
               END IF

*  Extract the CCDPACK extension item according to type and transfer it
*  to the FITS extension. Deal with the special case of a range of FITS
*  keywords reserved for a long string (in CCDPACK these are parts of
*  the TRANSFORM structure).
               SKIP = .FALSE.
               IF ( INDEX( LINE( I1( 3 ): I2( 3 ) ), '[' ) .NE. 0 ) THEN

*  Extract the value and prepare to store this using a range of
*  FITS keywords.
                  SKIP = .TRUE.
                  TRANS = '<unknown>'
                  CALL ERR_MARK
                  CALL HDR_INC( 'IN', 'CCDPACK',
     :                         LINE( I1( 1 ) : I2( 1 ) ), 1, TRANS,
     :                         STATUS )
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                  CALL ERR_RLSE
                  IF ( TRANS .NE. '<unknown>' ) THEN

*  Determine the range of keywords and break string down into
*  bits.
                     KEYWRD = LINE( I1( 3 ) : I2( 3 ) )
                     NC = CHR_LEN( TRANS )
                     CALL CCD1_HDRRN( KEYWRD, FIRST, LAST, INSERT,
     :                            STATUS  )
                     KEYWRD( INSERT: ) = ' '
                     I3 = 1
                     I4 = FITLEN
                     DO 2 I = FIRST, LAST

*  Construct keyword.
                        ITMP = INSERT - 1
                        CALL CHR_PUTI( I, KEYWRD, ITMP )
                        CALL HDR_OUTC( 'IN' ,'FITS',
     :                               KEYWRD, LINE( I1( 4 ):I2( 4 ) ),
     :                               TRANS( I3 : I4 ), STATUS )

*  Get the next piece of the value to store.
                        IF ( I4 .LT. NC ) THEN
                           I3 = I4 + 1
                           I4 = MIN( I4 + FITLEN, CCD1__SZTRN )
                        ELSE

*  No more parts to store, so stop.
                           GO TO 3
                        END IF
 2                   CONTINUE
 3                   CONTINUE
                  END IF
               END IF

*  Double precision.
               IF ( .NOT. SKIP ) THEN
                  IF ( LINE( I1( 2 ) : I2( 2 ) ) .EQ. '_DOUBLE' ) THEN
                     DVAL = VAL__BADD
                     CALL ERR_MARK
                     CALL HDR_IND( 'IN', 'CCDPACK',
     :                            LINE( I1( 1 ) : I2( 1 ) ), 1, DVAL,
     :                            STATUS )
                     IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                     IF ( DVAL .NE. VAL__BADD ) THEN
                        CALL HDR_OUTD( 'IN' ,'FITS',
     :                               LINE( I1( 3 ) : I2( 3 ) ),
     :                               LINE( I1( 4 ) : I2( 4 ) ), DVAL,
     :                               STATUS )
                     END IF

*  Single precision
                  ELSE IF ( LINE( I1( 2 ) : I2( 2 ) ) .EQ. '_REAL' )
     :                            THEN
                     RVAL = VAL__BADR
                     CALL ERR_MARK
                     CALL HDR_INR( 'IN', 'CCDPACK',
     :                            LINE( I1( 1 ) : I2( 1 ) ), 1, RVAL,
     :                            STATUS )
                     IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                     IF ( RVAL .NE. VAL__BADR ) THEN
                        CALL HDR_OUTR( 'IN' ,'FITS',
     :                               LINE( I1( 3 ) : I2( 3 ) ),
     :                               LINE( I1( 4 ) : I2( 4 ) ), RVAL,
     :                               STATUS )
                     END IF

*  Integer
                  ELSE IF ( LINE( I1( 2 ) : I2( 2 ) ) .EQ. '_INTEGER' )
     :                            THEN
                     IVAL = VAL__BADI
                     CALL ERR_MARK
                     CALL HDR_INI( 'IN', 'CCDPACK',
     :                            LINE( I1( 1 ) : I2( 1 ) ), 1, IVAL,
     :                            STATUS )
                     IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                     IF ( IVAL .NE. VAL__BADI ) THEN
                        CALL HDR_OUTI( 'IN' ,'FITS',
     :                               LINE( I1( 3 ) : I2( 3 ) ),
     :                               LINE( I1( 4 ) : I2( 4 ) ), IVAL,
     :                               STATUS )
                     END IF

*  Logical (special case as cannot set special value).
                  ELSE IF ( LINE( I1( 2 ) : I2( 2 ) ) .EQ. '_LOGICAL' )
     :                            THEN
                     CALL ERR_MARK
                     CALL HDR_NUMB( 'IN', 'CCDPACK',
     :                            LINE( I1( 1 ) : I2( 1 ) ), IVAL,
     :                            STATUS )
                     IF ( IVAL .NE. 0 ) THEN
                        CALL HDR_INL( 'IN', 'CCDPACK',
     :                               LINE( I1( 1 ) : I2( 1 ) ), 1,
     :                               LVAL, STATUS )
                        CALL HDR_OUTL( 'IN' ,'FITS',
     :                               LINE( I1( 3 ) : I2( 3 ) ),
     :                               LINE( I1( 4 ) : I2( 4 ) ), LVAL,
     :                               STATUS )
                     END IF
                     IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE

*  Character
                  ELSE IF ( LINE( I1( 2 ) : I2( 2 ) ) .EQ. '_CHAR' )
     :                            THEN
                     CVAL = '<unknown>'
                     CALL ERR_MARK
                     CALL HDR_INC( 'IN', 'CCDPACK',
     :                            LINE( I1( 1 ) : I2( 1 ) ), 1, CVAL,
     :                            STATUS )
                     IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
                     CALL ERR_RLSE
                     IF ( CVAL .NE. '<unknown>' ) THEN
                        CALL HDR_OUTC( 'IN' ,'FITS',
     :                               LINE( I1( 3 ) : I2( 3 ) ),
     :                               LINE( I1( 4 ) : I2( 4 ) ), CVAL,
     :                               STATUS )
                     END IF
                  ELSE

*  Unknown type.
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'LINE', ILINE )
                     CALL ERR_REP( 'CCDEXP_BADTYPE',
     :                    '  Line ^LINE contains an unknown data type',
     :                            STATUS )
                     GO TO 1
                  END IF
               END IF
            END IF
         END IF

*  Return for next line.
         GO TO 1
      END IF

*  Release the data file and the table.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )
      CALL FIO_CLOSE( FD, STATUS )
      CALL IMG_FREE( 'IN', STATUS )

*  Restore the NDF tuning parameter.
      CALL NDF_TUNE( DOCVT, 'DOCVT', STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CCDEXP_ERR',
     :        'CCDEXP: Error exporting CCDPACK extension information',
     :        STATUS )
      END IF

      END
* $Id$
