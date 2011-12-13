      SUBROUTINE CON_MANDH( FIRST, NCARD, HEADER, SCARD, BITPIX, NDIM,
     :                      AXIS, DARRAY, NONSDA, SIZE, STATUS )
*+
*  Name:
*     CON_MANDH

*  Purpose:
*     Obtain the values of the mandatory headers in a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_MANDH( FIRST, NCARD, HEADER, SCARD, BITPIX, NDIM, AXIS,
*                     DARRAY, NONSDA, SIZE, STATUS )

*  Description:
*     This routine searches for the mandatory FITS header keywords
*     stored in a buffer, and their values are returned, if they
*     are present. Should an item be missing or have an unsupported
*     value an error is reported, a bad status is set and the routine
*     exits. This version supports mandatory descriptors that are not
*     in the correct order.
*
*     Currently, only simple FITS and group-format FITS are supported.
*
*     The number of dimensions is reduced when the highest dimension is
*     one.

*  Arguments:
*     FIRST = LOGICAL (Given)
*        If true the buffer contains the first header of a FITS file.
*        It is used to validate the header.
*     NCARD = INTEGER (Given)
*        The number of card images in the header.
*     HEADER( NCARD ) = CHARACTER * 80 (Given)
*        The array of headers (80-character cards) to be searched for
*        the mandatory keywords.
*     SCARD = INTEGER (Given)
*        The number of the card from where the searches of the header
*        will begin.  This is needed because the headers make contain a
*        dummy header prior to an extension.
*     BITPIX = INTEGER (Returned)
*        The number of bits per pixel of the data array.
*     NDIM = INTEGER (Returned)
*        The number of active dimensions.
*     AXIS( DAT__MXDIM ) = INTEGER (Returned)
*        The dimensions of the data array.
*     DARRAY = LOGICAL (Returned)
*        If true there is a data array.
*     NONSDA = LOGICAL (Returned)
*        If true the data array is non-standard.
*     SIZE = INTEGER  (Returned)
*        The number of pixels in the (or each) data array.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     If the first header then
*        If file is not SIMPLE, report that, set bad status and return
*     Else
*        If file is not SIMPLE or an extension, report that, set bad
*          status and return
*     Endif
*     If tape is not SIMPLE, report that, set bad status and return
*     Locate and decode BITPIX value and check status - report context
*       and return if bad
*     If BITPIX not present, report error, set bad status and return
*     Check that BITPIX has valid value
*     Locate and decode NAXIS value and check status - report context
*       and return if bad
*     If NAXIS not present, report error, set bad status and return
*     Check that NAXIS has valid value
*     Initialise size counter
*     If NAXIS is not 0 then
*        For each AXIS
*           Generate name of the NAXISn keyword
*           Locate and decode AXISn value, and check status - report
*             context and return if bad
*           If AXISn not present report error, set bad status and return
*           Check that NAXISn has a valid value
*           Look for AXIS1=0, which is group format
*           If group format set nonstandard flag to true
*           If AXISn value is meaningless or non-integer, report error,
*             set bad status and return
*           Increment the number of pixels in the array so far (unless
*             AXIS1=0)
*        Endfor
*     Else
*        Set flag to say no data array is present
*     Endif
*     Remove any redundant (set to 1) higher dimensions
*     End

*  Copyright:
*     Copyright (C) 1988-1992 Science & Engineering Research Council.
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
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1988 September 16 (MJC):
*        First version.
*     1988 September 25 (MJC):
*        Allow BITPIX, NAXIS and NAXISn to appear in any order in the
*        header.
*     1989 July 27 (MJC):
*        Amended extension error message and minor bug fix.
*     1989 July 29 (MJC):
*        Added card number to FITSG* calls.
*     1989 November 22 (MJC):
*        Added FIRST argument to check for extensions in multiple
*        files.
*     1990 November 19 (MJC):
*        Renamed from FITSMH, added SCARD argument, and converted to
*        the SST prologue style.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument for revised FTS1_GKEYx calls.
*     1992 September 18 (MJC):
*        Renamed from FTS1_MANDH for CONVERT.  All FTS1_ calls replaced
*        by CON_ equivalents.
*     2001 August 29 (DSB):
*        Correct CON_GKEYI arguments.
*     2009 June 29 (MJC):
*        Replace cloned CON_GKEYx with KAPLIBS FTS1_GKEYx.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! no default typing allowed

*  Global Constants:
      INCLUDE  'SAE_PAR'         ! SSE global definitions
      INCLUDE  'DAT_PAR'         ! Data-sytem constants
      INCLUDE  'PRM_PAR'         ! Magic-value definitions

*  Arguments Given:
      LOGICAL FIRST

      INTEGER NCARD
      INTEGER SCARD

      CHARACTER*80 HEADER( NCARD )

*  Arguments Returned:
      INTEGER BITPIX
      INTEGER NDIM
      INTEGER SIZE
      INTEGER AXIS( DAT__MXDIM )

      LOGICAL DARRAY
      LOGICAL NONSDA

*  Status:
      INTEGER STATUS

*  Local Variables:
      LOGICAL THERE              ! Card with the requested keyword is in
                                 ! the header

      CHARACTER*80 COM           ! Card comment
      CHARACTER*3 CNDIM          ! Axis number
      INTEGER N                  ! Loop counter
      INTEGER NAX                ! Loop counters
      CHARACTER*8 NAXNAM         ! NAXISn keyword name
      INTEGER NC                 ! Number of characters in axis number
      INTEGER NKC                ! Number of a header card image that
                                 ! contains a specified keyword

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( FIRST ) THEN

*  Check the tape is simple FITS.
         IF ( HEADER( 1 )( 1:6 ) .NE. 'SIMPLE' .OR.
     :        HEADER( 1 )( 30:30 ) .NE. 'T' ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CON_MANDH_SIMPLE',
     :        'SIMPLE not present or in wrong '/
     :        /'order, or the format is not simple', STATUS )
            GOTO 990
         END IF
      ELSE

*  The first header record must be an extension name or SIMPLE = T.
         IF ( .NOT. ( HEADER( 1 )( 1:8 ) .EQ. 'XTENSION' .OR.
     :              ( HEADER( 1 )( 1:6 ) .EQ. 'SIMPLE' .AND.
     :                HEADER( 1 )( 30:30 ) .EQ. 'T' ) )  ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CON_MANDH_NO1ST',
     :        'First keyword is not SIMPLE or XTENSION, '/
     :        /'or the format is not simple', STATUS )
            GOTO 990
         END IF
      END IF

*  Continue checking the mandatory descriptors... BITPIX is
*  number of bits per pixel.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'BITPIX', 1, THERE, BITPIX,
     :                 COM, NKC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CON_MANDH_FBITPI',
     :     'Error evaluating BITPIX', STATUS )
         GOTO 990
      END IF

*  See if there was a BITPIX header card.
      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_MANDH_BITPIX',
     :     'BITPIX not present', STATUS )
         GOTO 990
      END IF

*  BITPIX can currently only have values of 8, 16, and 32 for integer
*  types; and -32 and -64 for floating-point.
      IF ( BITPIX .NE. 8 .AND. BITPIX .NE. 16 .AND.
     :     BITPIX .NE. 32  .AND. BITPIX .NE. -32 .AND.
     :     BITPIX .NE. -64 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_MANDH_IBITPI',
     :     'BITPIX is not 8, 16, 32, -32 or -64.', STATUS )
         GOTO 990
      END IF

*  Now obtain the number of dimensions.
      CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'NAXIS', 1, THERE, NDIM,
     :                 COM, NKC, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CON_MANDH_INAXIS',
     :     'Error converting NAXIS', STATUS )
         GOTO 990
      END IF

      IF ( .NOT. THERE ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CON_MANDH_NAXIS',
     :     'NAXIS not present', STATUS )
         GOTO 990
      END IF

*  Zero dimension means there is no main data array.  Upper limit
*  is imposed by the data system.
      IF ( NDIM .LT. 0 .OR. NDIM .GT. DAT__MXDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NAXIS', NDIM )
         CALL ERR_REP( 'CON_MANDH_INAXIS',
     :     'Cannot process NAXIS = ^NAXIS', STATUS )
         GOTO 990
      END IF

      NONSDA = .FALSE.
      DARRAY = .TRUE.

*  Decode NAXISn values.
      SIZE = 1

*  Allow for the case when there is no data array.
      IF ( NDIM .GT. 0 ) THEN

*  For each dimension
*  ==================
         DO N = 1, NDIM

*  Generate name of the Nth axis dimension, NAXISn.
            CALL CHR_ITOC( N, CNDIM, NC )
            NAXNAM = 'NAXIS' // CNDIM( :NC )

*  Get value of NAXISn.
            CALL FTS1_GKEYI( NCARD, HEADER, SCARD, NAXNAM, 1, THERE,
     :                       AXIS( N ), COM, NKC, STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'AXN', N )
               CALL ERR_REP( 'CON_MANDH_FAXISN',
     :              'Error converting AXIS^AXN value', STATUS )
               GOTO 990
            END IF

            IF ( .NOT. THERE ) THEN
               CALL MSG_SETI( 'AXN', N )
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CON_MANDH_NAXISn',
     :           'NAXIS^AXN not present', STATUS )
               GOTO 990
            END IF


*  File may be in group format, where AXIS1=0.
            IF ( N .EQ. 1 .AND. AXIS( N ) .EQ. 0 ) THEN

*  So there is a non-standard data-array structure present.
               NONSDA = .TRUE.
            END IF

*  Check that it is physical or does not have an integer value.
            IF ( AXIS( N ) .LT. 1 .AND. ( .NOT. NONSDA ) ) THEN
               CALL MSG_SETI( 'AXN', N )
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'AXIS', AXIS( N ) )
               CALL ERR_REP( 'CON_MANDH_AXISIZ',
     :           'Axis ^AXN has invalid dimension ^AXIS', STATUS )

*  Abort.
               GOTO 990
            END IF

*  Evaluate the number of pixels in the array.
            IF ( AXIS ( N ) .GT. 0 ) SIZE = SIZE * AXIS( N )
         END DO
      ELSE

*  No data array is present.
         DARRAY = .FALSE.
         AXIS( 1 ) = VAL__BADI
      END IF

*  Occasionally higher dimensions may be set to one.  Remove such
*  degenerate dimensions.
      IF ( NDIM .GT. 1 ) THEN
         DO NAX = NDIM, 1, -1
            IF ( AXIS( NAX ) .NE. 1 ) GOTO 950
         END DO

         NAX = 1
  950    CONTINUE
         NDIM = NAX
      END IF

  990 CONTINUE

      END
