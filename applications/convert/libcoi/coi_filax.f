      SUBROUTINE COI_FILAX( NVALUE, VALUE, Z, EL, CENTRE, STATUS )
*+
*  Name:
*     COI_FILAX

*  Purpose:
*     Creates axis centres within an NDF from Multispec ftype=5 axis
*     information in an IRAF header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_FILAX( NVALUE, VALUE, Z, EL, CENTRE, STATUS )

*  Description:
*     The routine takes the part of the spec1 value string containing
*     the axis values, extracts the values and fills the axis-centre
*     array with them.  The axis centres have type double-precision
*     The spec1 value is derived from the WAT2_nnn headers which define
*     an axis structure stored using the IRAF Mini World Co-ordinate
*     System (MWCS) in the Multispec format.  The redshift correction is
*     applied.
*
*     INDEF values are converted to the bad value.

*  Arguments:
*     NVALUE = INTEGER (Given)
*        The number of axis values in the VALUE buffer. (Word 12 from
*        the spec1 parameter.)  This should be no more than the value of
*        argument EL.
*     VALUE = CHARACTER * ( * ) (Given)
*        Buffer containing the space-separated values of the axis
*        centres derived from the extract spec1 parameter in the
*        WAT2_nnn headers.  The first word should be the first value.
*     Z = DOUBLE PRECISION (Returned)
*        Redshift.  This is normally 0.  The axis centres are scaled
*        by 1/(1+Z).
*     EL = INTEGER (Given)
*        The number of axis values in the axis centre-array.
*     CENTRE( EL ) = DOUBLE PRECISION (Returned)
*        The array axis centres.  When NVALUE is less than EL, the array
*        is padded to EL elements with the bad value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 July 25 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER NVALUE
      CHARACTER * ( * ) VALUE
      DOUBLE PRECISION Z
      INTEGER EL

*  Arguments Returned:
      DOUBLE PRECISION CENTRE( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXWRD             ! Maximum number of words
      PARAMETER( MAXWRD = 16 )

*  Local Variables:
      INTEGER CPOS               ! Current column position
      INTEGER CSTAT              ! CHR status
      INTEGER END( MAXWRD )      ! End column of each word
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      INTEGER MWORD              ! Number of words extracted
      INTEGER NLOOP              ! Number of loops
      INTEGER NWORD              ! Number of words to extract
      INTEGER NX                 ! Pixel index
      INTEGER START( MAXWRD )    ! Start column of each word
      CHARACTER * ( VAL__SZD ) WORDS( MAXWRD ) ! spec1 sub parameters
      DOUBLE PRECISION ZCORR     ! Redshift correction

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Evaluate the redshift correction factor, ensuring that it is
*  positive.
      ZCORR = MAX( VAL__EPSD, 1.0D0 + Z )

*  Using the number of values to extract and convert to numeric form,
*  find the number of loops using the finite size of word buffer.
      NLOOP = ( NVALUE - 1 ) / MAXWRD + 1

*  Set the initial position and pixel index.
      CPOS = 1
      NX = 0

      DO I = 1, NLOOP

*  See how many words to extract this time.  This will be MAXWRD until
*  the loop which may have fewer.
         MWORD = MIN( MAXWRD, NVALUE - ( I - 1 ) * MAXWRD )

*  Split the axis values into words.  Length is at least 8 characters
*  fewer than the concatenated header value.  Use a local status as we
*  expect more words in the buffer than elements of the WORDS array.
         CALL CHR_DCWRD( VALUE( CPOS: ), MWORD, NWORD, START, END,
     :                   WORDS, CSTAT )

*  Convert each word.
         DO J = 1, NWORD
            NX = NX + 1

            CALL CHR_UCASE( WORDS( J ) )
            IF ( WORDS( J ) .NE. 'INDEF' ) THEN
               CALL CHR_CTOD( WORDS( J ), CENTRE( NX ), STATUS )

*  Apply the redshift correction.
               CENTRE( NX ) = CENTRE( NX ) / ZCORR

*  Set INDEF values to be bad.
            ELSE
               CENTRE( NX ) = VAL__BADD
            END IF
         END DO

*  Shift the origin of the character position to pick up the next batch
*  of values.
         CPOS = CPOS + END( MAXWRD )
      END DO

*  Fill any missing values with the bad value.
      IF ( NVALUE .LT. EL ) THEN
         DO I = 1, EL - NVALUE
            CENTRE( I ) = VAL__BADD
         END DO
      END IF

  999 CONTINUE

      END
