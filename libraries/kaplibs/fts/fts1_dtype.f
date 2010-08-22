      SUBROUTINE FTS1_DTYPE( DARRAY, NONSDA, BITPIX, SCARD, NCARD,
     :                       HEADER, MXPARM, NDIM, DIMS, BSCALE, BZERO,
     :                       BLANK, BADPIX, IEEE, GCOUNT, PCOUNT, PTYPE,
     :                       PSCALE, PZERO, STATUS )
*+
*  Name:
*     FTS1_DTYPE

*  Purpose:
*     Obtains the input data type, scales and offsets for a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_DTYPE( DARRAY, NONSDA, BITPIX, SCARD, NCARD, HEADER,
*    :                 MXPARM, NDIM, DIMS, BSCALE, BZERO, BLANK, BADPIX,
*    :                 IEEE, GCOUNT, PCOUNT, PTYPE, PSCALE, PZERO,
*    :                 STATUS )

*  Description:
*     This is a server routine for FITSIN.  It packages up the
*     operations required to define the input data types, the scale
*     factor and offset, the data-blank value, the number of bytes per
*     data values.  This includes the group count, number of parameters
*     per group and their group scalings for a non-standard data array.

*  Arguments:
*     DARRAY = LOGICAL (Given)
*        If true there is a data array present if the FITS file.
*     NONSDA = LOGICAL (Given)
*        If true the data array is not standard, i.e. in group format.
*        It is ignored if %DARRAY is false.
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
*     MXPARM = INTEGER (Given)
*        The maximum number of group parameters, and the dimension size
*        for the various group arrays.
*     NDIM = INTEGER (Given and Returned)
*        Dimensionality of the data array.  It may be modified if a)
*        there is no data array, because a valid NDF must have a data
*        array with physical dimensions (set to 1); or b) it is a
*        non-standard array (i.e. groups) where the first dimension is
*        zero, and so the dimensionality is reduced by one.
*     DIMS( DAT__MXDIM ) = INTEGER (Given and Returned)
*        The dimensions of the table.  It may be modified if a)
*        there is no data array, because a valid NDF must have a data
*        array with physical dimensions (set to 2 in 1-d); or b) it is
*        a non-standard array (i.e. groups) where the first dimension
*        is zero, and so the dimension sizes are shifted down one
*        dimension.
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
*     IEEE = LOGICAL (Returned)
*        If true the FITS data are in IEEE floating-point format.
*     GCOUNT = INTEGER (Returned)
*        The number of groups in the FITS sub-file.
*     PCOUNT = INTEGER (Returned)
*        The number of group parameters in each group.
*     PTYPE( MXPARM ) = CHARACTER * ( * ) (Returned)
*        The type (descriptive name) of each group parameter.
*     PSCALE( MXPARM ) = DOUBLE PRECISION (Returned)
*        The scale factors of the group parameters so that the
*        parameters may be converted to the true floating-point values.
*     PZERO( MXPARM ) = DOUBLE PRECISION (Returned)
*        The offsets of the group parameters so that the parameters may
*        be converted to the true floating-point values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The non-standard case of a floating-point data-blank value (BLANK)
*     is handled assuming it is the true blank value as opposed to the
*     blank value in the FITS data.  This is achieved by subtracting the
*     offset and dividing by the scale factor and taking the nearest
*     integer.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59, Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 November 18 (MJC):
*        Original version.
*     1991 February 28 (MJC):
*        Converted BUFFER from an assumed-size to an adjustable array
*        via the NCARD argument for revised FTS1_GKEYx calls.
*     1996 November 24 (MJC):
*        Modern style.  Revised FTS1_GKEYx calls.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      LOGICAL DARRAY
      LOGICAL NONSDA
      INTEGER BITPIX
      INTEGER SCARD
      INTEGER NCARD
      CHARACTER * 80 HEADER( NCARD )
      INTEGER MXPARM

*  Arguments Given and Returned:
      INTEGER NDIM
      INTEGER DIMS( DAT__MXDIM )

*  Arguments Returned:
      REAL BSCALE
      REAL BZERO
      INTEGER BLANK
      LOGICAL BADPIX
      LOGICAL IEEE
      INTEGER GCOUNT
      INTEGER PCOUNT
      CHARACTER * ( * ) PTYPE( MXPARM )
      DOUBLE PRECISION PSCALE( MXPARM )
      DOUBLE PRECISION PZERO( MXPARM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! Comment from card (not used)
      INTEGER DCARD              ! Header card number of BLANK
      LOGICAL GROUPS             ! Data structure has groups format?
      INTEGER N                  ! Loop counter
      REAL NBLANK                ! Normalised floating-point data blank
      INTEGER NCH                ! Number of characters in character
                                 ! form of the group-parameter number
      INTEGER NKC                ! Number of the header card containing
                                 ! the requested FITS keyword
      CHARACTER * ( 4 ) PNUM     ! Number of group-format parameter
      REAL RBLANK                ! Floating-point data blank
      LOGICAL THERE              ! The selected keyword is present in
                                 ! the FITS header?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If there is a data array.
      IF ( DARRAY ) THEN

*  If the data are integers.
         IF ( BITPIX .GT. 0 ) THEN

*  Data are therefore not IEEE.
            IEEE = .FALSE.

*  First get the BSCALE scale factor.
            CALL FTS1_GKEYR( NCARD, HEADER, SCARD, 'BSCALE', 1, THERE,
     :                       BSCALE, COMENT, NKC, STATUS )
            IF ( .NOT. THERE ) BSCALE = 1.0

*  Next the BZERO offset.
            CALL FTS1_GKEYR( NCARD, HEADER, SCARD, 'BZERO', 1, THERE,
     :                       BZERO, COMENT, NKC, STATUS )
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

*  Now the undefined pixel value, BLANK.
            CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'BLANK', 1, BADPIX,
     :                       BLANK, COMENT, NKC, STATUS )

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
     :                             BADPIX, RBLANK, COMENT, NKC, STATUS )

*  If no problem this time apply scale and zero to derive the true
*  integer value in the FITS file.  However, this number has to be
*  checked that it is in the integer range.  If it isn't assume that
*  BLANK is undefined by setting BADPIX to false.
                  IF ( STATUS .EQ. SAI__OK ) THEN
                     NBLANK = ( RBLANK - BZERO ) / BSCALE

                     IF ( NBLANK .GT. REAL( NUM__MAXI ) .OR.
     :                    NBLANK .LT. REAL( NUM__MINI ) ) THEN
                        BADPIX = .FALSE.

*  Irritate the user who might contact the source institution to
*  correct their FITS writer.
                        CALL MSG_OUT( 'BLANKERR',
     :                    'Ignoring the BLANK keyword as its value '/
     :                    /'cannot be scaled to an integer.',  STATUS )

                     ELSE
                        BLANK = NINT( NBLANK )
                     END IF

*  Irritate the user who might contact the source institution to
*  correct their FITS writer.
                     CALL MSG_OUT( 'BLANKERR1',
     :                 'The FITS file has a non-standard value for '/
     :                 /'the BLANK keyword.  It should be',  STATUS )
                     CALL MSG_OUT( 'BLANKERR2',
     :                 'the value actually stored in the FITS file, '/
     :                 /'and therefore must be of type integer.',
     :                 STATUS )
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

         IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Group format
*  ============
*
*  If the data array is non-standard it must have the group format to
*  be processed.
         IF ( NONSDA ) THEN
            CALL FTS1_GKEYL( NCARD, HEADER, SCARD, 'GROUPS', 1, THERE,
     :                       GROUPS, COMENT, NKC, STATUS )
            IF ( .NOT. GROUPS .OR. .NOT. THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FTS1_DTYPE_NOBLO',
     :           'The only supported non-standard data array is the '/
     :           /'blocks (random-group) format.', STATUS )
               GOTO 999
            END IF

*  Now look for the group and parameter counts.
            CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'GCOUNT', 1, THERE,
     :                       GCOUNT, COMENT, NKC, STATUS )
            IF ( .NOT. THERE ) GCOUNT = 1

            CALL FTS1_GKEYI( NCARD, HEADER, SCARD, 'PCOUNT', 1, THERE,
     :                       PCOUNT, COMENT, NKC, STATUS )
            IF ( .NOT. THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FTS1_DTYPE_PCOUNT',
     :           'PCOUNT not defined', STATUS )
               GOTO 999
            END IF

*  Get the group PTYPE keywords.
            DO  N = 1, PCOUNT
               CALL CHR_ITOC( N, PNUM, NCH )
               CALL FTS1_GKEYC( NCARD, HEADER, SCARD,
     :                          'PTYPE'//PNUM( :NCH ), 1, THERE,
     :                          PTYPE( N ), COMENT, NKC, STATUS )
               IF ( .NOT. THERE ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'N', N )
                  CALL ERR_REP( 'FTS1_DTYPE_PTYPEn',
     :              'PTYPE^N not present', STATUS )
                  GOTO 999
               END IF

*  Get group scales and offsets, using double precision to avoid loss
*  of precision, though it may create a false precision in the output
*  data structure.  Routine FTS1_GKEY needs to be designed such that
*  the number of significant figures is returned and FTS1_DTYPE can
*  format the results accordingly.
               CALL FTS1_GKEYD( NCARD, HEADER, SCARD,
     :                          'PSCAL'//PNUM( :NCH ), 1, THERE,
     :                          PSCALE( N ), COMENT, NKC, STATUS )
               IF ( .NOT. THERE ) PSCALE( N ) = 1.0

               CALL FTS1_GKEYD( NCARD, HEADER, SCARD,
     :                          'PZERO'//PNUM( :NCH ), 1, THERE,
     :                          PZERO( N ), COMENT, NKC, STATUS )
               IF ( .NOT. THERE ) PZERO( N ) = 0.0
            END DO

*  Shift the dimensions, if the first is a dummy.
            IF ( DIMS( 1 ) .EQ. 0 ) THEN
               NDIM = NDIM - 1
               DO  N = 1, NDIM
                  DIMS( N ) = DIMS ( N + 1 )
               END DO
            END IF

*  ^^^^^^^^^^^^

*  Standard format, so a there is a single data array.
         ELSE
            GCOUNT = 1
            PCOUNT = 0

         END IF

      ELSE

*  To be a valid NDF, it must have a DATA_ARRAY component so create
*  some dummy dimensions...
         NDIM = 1
         DIMS( 1 ) = 2

*  and there is now a single data array.
         GCOUNT = 1
         PCOUNT = 0

*  End of data-array(s)-present check.
      END IF

  999 CONTINUE

      END
