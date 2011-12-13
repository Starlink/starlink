      SUBROUTINE FTS1_GVALC( HEADER, CSTART, CEND, VALUE, STATUS )
*+
*  Name:
*     FTS1_GVALC

*  Purpose:
*     Extracts a string value from a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_GVALC( HEADER, CSTART, CEND, VALUE, STATUS )

*  Description:
*     This routines determines the location of a character value within
*     a FITS header and hence the value itself.  The location of the
*     delimiting quote marks and string value are returned.

*  Arguments:
*     HEADER = CHARACTER * ( * ) (Given)
*        The FITS header 'card' whose value is to be extractd.
*     CSTART = INTEGER (Given)
*        The column containing the leading quote of the string value.
*        This is set to zero if no string value could be found.
*     CEND = INTEGER (Given)
*        The column containing the trailing quote of the string value.
*        This is set to zero if no string value could be found.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The character value.  CARD should contain at least 68
*        characters.  A blabnnk string is returned if the header failed
*        to contain a character value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  It is assumed that the header conforms to the FITS Standard,
*     with one exception.  Single quotes within a string---these should
*     be doubled---are detected provided that the next solidus after a
*     single quote will be a comment delimiter rather than literal text
*     in the string.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2009 January 11 (MJC):
*        Original version derived from FTS1_VHEAD.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) HEADER

*  Arguments Returned:
      INTEGER CSTART
      INTEGER CEND
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER CFIXED             ! Standard column to which to right
                                 ! justify values for fixed format
      PARAMETER ( CFIXED = 30 )

      INTEGER FITSLN             ! Length of a FITS card image
      PARAMETER ( FITSLN = 80 )

      INTEGER VALLN              ! Maximum length of a FITS value
      PARAMETER ( VALLN = 70 )

*  Local Variables:
      CHARACTER*( VALLN ) CDUMMY ! Work string for value
      INTEGER COLEQS             ! Character pointer to the equals sign
      INTEGER CPOS               ! Integer pointer for appending
                                 ! right-justified non-character values
      INTEGER ENDW               ! Column position of the end of the
                                 ! header value (w.r.t. = sign)
      CHARACTER*( VALLN ) FITDAT ! FITS value
      INTEGER NCCOM              ! Number of characters in FITS
                                 ! COMMENT or HISTORY card
      INTEGER NCCQ               ! Column of FITS trailing quote for
                                 ! character value
      INTEGER NCCQ2              ! Column of FITS trailing quote for
                                 ! character value (work variable)
      INTEGER NCDQ               ! Column of FITS double quote for
                                 ! character value
      INTEGER NCFD               ! Column position in output FITS
                                 ! character value (final double quote)
      INTEGER NCFS               ! Column position in output FITS
                                 ! character value (final single quote)
      INTEGER NCSTQ              ! Column from where to start search
                                 ! for a trailing quote for a FITS
                                 ! character value
      LOGICAL TRAILQ             ! Does the value have a trailing quote?

*.

*  Initialise returned values.
      CSTART = 0
      CEND = 0
      VALUE = ' '

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the first equals
      COLEQS = INDEX( HEADER, '=' )

*  Copy the buffer from the equals sign and remove the leading blanks.
      CDUMMY = HEADER( COLEQS + 1: )
      CALL CHR_LDBLK( CDUMMY )

*  Columns of 11:30 should contain FITS item value if numeric or
*  logical.  Character values may extend to column 80.  Column 10
*  should be a space immediately following the equals sign.  To protect
*  against non-fatal errors in the input buffer this coding should
*  allow for additional or no spaces between the value and equals sign.
*  Search for the leading quote.
       IF ( CDUMMY( 1:1 ) .EQ. '''' ) THEN

*  Initialise the character value.
         FITDAT = ' '

*  Determine where the initial quote is located.
*  =============================================
         NCSTQ = INDEX( HEADER( COLEQS+1: ), '''' ) + COLEQS
         CSTART = NCSTQ

*  Determine where the trailing quote is located.
*  ==============================================

*  First look for any double quotes.  These mean a single quote, so
*  O'Hara appears as O''Hara in a FITS card image.  Start the search
*  immediately after the leading quote, and subsequently immediately
*  following a double quote.  Prevent the search extending beyond the
*  end of the value.  Keep a count of the absolute column.  Enclosed
*  quotes that should be double but are single will be tested for
*  later.
         NCDQ = 1
         NCFD = 1
         NCSTQ = NCSTQ + 1
         DO WHILE ( NCDQ .NE. 0 )
            NCDQ = INDEX( HEADER( NCSTQ: ), '''''' )

*  If there is no double quote no action is necessary.  When there is,
*  we form part of the value, removing the second quote.
            IF ( NCDQ .GT. 0 ) THEN
               FITDAT( NCFD: NCFD + NCDQ ) =
     :                 HEADER( NCSTQ: NCSTQ + NCDQ )

*  Increment the counter to where to append to the value.
               NCFD = NCFD + NCDQ

*  Increment the counter in the FITS card, skipping over the double
*  quote.
               NCSTQ = NCSTQ + NCDQ + 1
            END IF
         END DO

*  We need some method for detecting single quotes within the string.
*  Any that lie before the last correctly doubled quote will be in the
*  string.  The single quotes get doubled later.  The following
*  algorithm is far from foolproof, but it will catch some errors in
*  the input card.
*
*  The method assumes that the next slash will be a comment rather than
*  literal text in the string.  Find the location of the comment
*  delimiter.  Start just before the normal position just in case some
*  characters have been removed before it, but not too far in case we
*  confuse a slash in the value with the comment delimiter when there
*  is no closing quote.  When there is no comment delimiter, the search
*  for the closing quote continues to the end of the card.  Specify the
*  position of the notional closing quote.
         NCCOM = INDEX( HEADER( MAX( CFIXED - 1, NCSTQ ): ), '/' )
         IF ( NCCOM .EQ. 0 ) THEN
            NCCOM = FITSLN
            NCCQ2 = FITSLN
         ELSE
            NCCOM = MAX( CFIXED - 1, NCSTQ ) + NCCOM - 1
            NCCQ2 = NCCOM - 2
         END IF

*  The last of these is taken to be the actual final quote, and earlier
*  single quotes in the single as part of the string.
         NCCQ = 1
         NCFS = 1
         TRAILQ = .FALSE.
         DO WHILE ( NCCQ .NE. 0 )
            NCCQ = INDEX( HEADER( NCSTQ:NCCOM ), '''' )

*  If there is no single quote no action is necessary.  When there is,
            IF ( NCCQ .GT. 0 ) THEN

*  Record the trailing quote and its position.
               TRAILQ = .TRUE.

*  Append the remainder or all of the value (the latter when there is
*  no double quote in the FITS card image) to the value, and including
*  the quote.  If it is the last quote, later it will be removed from
*  the value.
               FITDAT( NCFD:NCFD + NCCQ - 1 ) =
     :                       HEADER( NCSTQ:NCSTQ + NCCQ - 1 )

*  Increment the counter to where to append to the value.
               NCFD = NCFD + NCCQ

*  Increment the counter in the FITS card, skipping over the single
*  quote.
               NCSTQ = NCSTQ + NCCQ

            END IF
         END DO

*  Extract the character value.
*  ============================

*  There is no trailing quote so assume that the character value
*  extends to the end of the card image.  Copy the value.
         IF ( .NOT. TRAILQ ) THEN
            FITDAT = HEADER( NCSTQ:NCCQ2 )

*  Assign the position of the closing quote.
            NCCQ = NCCQ2 + 2
         ELSE
            NCCQ = NCSTQ - 1

*  Remove the trailing quote from the value.
            FITDAT( NCFD-1:NCFD-1 ) = ' '
         END IF

         VALUE = FITDAT
         CEND = NCCQ
      END IF

      END
