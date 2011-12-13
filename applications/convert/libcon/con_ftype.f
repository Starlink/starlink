      SUBROUTINE CON_FTYPE( BITPIX, SCARD, NCARD, HEADER, BSCALE, BZERO,
     :                      BLANK, BADPIX, UNSIGN, STATUS )
*+
*  Name:
*     CON_FTYPE

*  Purpose:
*     Obtains the input data type, scales and offsets for a simple
*     FITS-like header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FTYPE( BITPIX, SCARD, NCARD, HEADER, BSCALE, BZERO,
*                     BLANK, BADPIX, UNSIGN, STATUS )

*  Description:
*     This is a server routine for ASCII2NDF and UNF2NDF.  It packages
*     up the operations required to define the input data types, the
*     scale factor and offset, the data-blank value, the number of
*     bytes per data values.

*  Arguments:
*     BITPIX = INTEGER (Given)
*        The value of the BITPIX keyword in the FITS header, i.e. the
*        number of bits per data value.  If it is negative this
*        indicates an IEEE-format file.
*     SCARD = INTEGER (Given)
*        The number of the card from where the searches of the header
*        will begin.  This is needed because the headers make contain a
*        dummy header prior to an extension.
*     NCARD = INTEGER (Given)
*        The number of card images in the header.
*     HEADER( NCARD ) = CHARACTER * 80 (Given)
*        The FITS headers in 80-character records.
*     BSCALE = REAL (Returned)
*        The scale factor of the FITS integer data for their conversion
*        to the true floating-point values.
*     BZERO = REAL (Returned)
*        The offset of the FITS integer data for their conversion to
*        the true floating-point values.
*     BLANK = INTEGER (Returned)
*        The data-blank value equivalent to the bad-pixel flag.  It
*        should be ignored if %BADPIX is false.
*     BADPIX = LOGICAL (Returned)
*        If true the data-blank was defined in the FITS header.
*     UNSIGN = LOGICAL (Returned)
*        If true the data are unsigned as given by the UNSIGNED being
*        true in the FITS header.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The non-standard case of a floating-point data-blank value (BLANK)
*     is handled assuming it is the true blank value as opposed to the
*     blank value in the FITS data.  This is achieved by subtracting the
*     offset and dividing by the scale factor and taking the nearest
*     integer.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     AJC: Alan J. Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 September 17 (MJC):
*        Original version based on FTS1_DTYPE.
*     2001 August 30 (AJC):
*        Correct CON_GKEYx arguments
*     2009 June 29 (MJC):
*        Replace cloned CON_GKEYx with KAPLIBS FTS1_GKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER BITPIX
      INTEGER SCARD
      INTEGER NCARD

      CHARACTER*80 HEADER( NCARD )

*  Arguments Returned:
      INTEGER BLANK

      REAL BSCALE
      REAL BZERO

      LOGICAL BADPIX
      LOGICAL UNSIGN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*80 COM           ! Header-card comment
      INTEGER DCARD              ! Header card number of BLANK
      LOGICAL IEEE               ! Data type is floating?
      INTEGER NKC                ! Number of the header card containing
                                 ! the requested FITS keyword
      REAL RBLANK                ! Floating-point data blank
      LOGICAL THERE              ! Selected keyword is present in
                                 ! FITS header?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the unsigned flag.
      UNSIGN = .FALSE.

*  If the data are integers.
      IF ( BITPIX .GT. 0 ) THEN

*  Get the UNSIGNED flag.
         CALL FTS1_GKEYL( NCARD, HEADER, SCARD, 'UNSIGNED', 1, THERE,
     :                    BSCALE, COM, NKC, STATUS )
         IF ( .NOT. THERE ) UNSIGN = .FALSE.

*  Get the BSCALE scale factor.
         CALL FTS1_GKEYR( NCARD, HEADER, SCARD, 'BSCALE', 1, THERE,
     :                    BSCALE, COM, NKC, STATUS )
         IF ( .NOT. THERE ) BSCALE = 1.0

*  Next the BZERO offset.
         CALL FTS1_GKEYR( NCARD, HEADER, SCARD, 'BZERO', 1, THERE,
     :                    BZERO, COM, NKC, STATUS )
         IF ( .NOT. THERE ) BZERO = 0.0
      ELSE

*  Negative BITPIX is conventional to indicate IEEE floating-point
*  format.
         IEEE = .TRUE.
      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999

      IF ( .NOT. IEEE ) THEN

*  Get the blank data value.
*  =========================
*
*  Start new error context.
         CALL ERR_MARK

*  Now get the undefined pixel value, BLANK.
         CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'BLANK', 1, BADPIX,
     :                    BLANK, COM, NKC, STATUS )

*  Look for the non-standard case where the BLANK value has been given
*  its original floating-point value rather than the integer value
*  actually stored in the FITS file.
         IF ( STATUS .NE. SAI__OK ) THEN

*  The keyword is present, but not in the correct format.
            IF ( BADPIX ) THEN

*  The error message can be annulled.
               CALL ERR_ANNUL( STATUS )

*  Make the search more efficient.
               DCARD = NKC

*  Read BLANK as floating-point value.
               CALL FTS1_GKEYR( NCARD, HEADER, DCARD, 'BLANK', 1,
     :                          BADPIX, RBLANK, COM, NKC, STATUS )

*  If no problem this time apply scale and zero to derive the true
*  integer value in the FITS file.

               IF ( STATUS .EQ. SAI__OK ) THEN
                  BLANK = NINT( ( RBLANK - BZERO ) / BSCALE )

*  Irritate the user who might contact the source institution to
*  correct their FITS writer.
                  CALL MSG_OUT( 'BLANKERR1',
     :              'The FITS file has a non-standard value for '/
     :              /'the BLANK keyword.  It should be',  STATUS )
                  CALL MSG_OUT( 'BLANKERR2',
     :              'the value actually stored in the FITS file, '/
     :              /'and therefore must be of type integer.',
     :              STATUS )
               END IF
            END IF
         END IF

*  Release the error context.
         CALL ERR_RLSE

*  By definition the BLANK card should be ignored for floating-point
*  data, as this information is stored in the IEEE numbers.
      ELSE
         BADPIX = .FALSE.
      END IF

  999 CONTINUE

      END
