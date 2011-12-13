      SUBROUTINE COF_MANDH( FUNIT, FIRST, MAXDIM, SIMPLE, BITPIX, NDIM,
     :                      AXIS, PCOUNT, GCOUNT, EXTEND, DARRAY,
     :                      GROUPS, SIZE, STATUS )
*+
*  Name:
*     COF_MANDH

*  Purpose:
*     Obtain the values of the mandatory headers in a FITS file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_MANDH( FUNIT, FIRST, MAXDIM, SIMPLE, BITPIX, NDIM, AXIS,
*                     PCOUNT, GCOUNT, EXTEND, DARRAY, GROUPS, SIZE,
*                     STATUS )

*  Description:
*     This routine searches for the mandatory and standard FITS header
*     keywords stored in a file, and their values are returned, if they
*     are present.  Should an item be missing or have an unsupported
*     value an error is reported, a bad status is set and the routine
*     exits.  This version expects the mandatory descriptors to be in
*     the correct order.
*
*     The number of dimensions is reduced when the highest dimension is
*     one.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The logical unit number of the FITS file.
*     FIRST = LOGICAL (Given)
*        If true the buffer contains the first header of a FITS file.
*        It is used to validate the header.
*     MAXDIM = INTEGER (Returned)
*        The maximum number of dimensions.
*     SIMPLE = LOGICAL (Returned)
*        The value of the SIMPLE flag when FIRST is .TRUE..
*     BITPIX = INTEGER (Returned)
*        The number of bits per pixel of the data array.
*     NDIM = INTEGER (Returned)
*        The number of active dimensions.
*     AXIS( MAXDIM ) = INTEGER (Returned)
*        The dimensions of the data array.
*     PCOUNT = INTEGER (Returned)
*        The number of group parameters in each group.  It defaults to
*        0 if the keyword is absent.
*     GCOUNT = INTEGER (Returned)
*        The number of groups in the FITS sub-file.  It defaults to 1
*        if the keyword is absent.
*     EXTEND = LOGICAL (Returned)
*        The value of the EXTEND keyword if present.  It defaults to
*        .FALSE. when the keyword is absent.
*     DARRAY = LOGICAL (Returned)
*        If .TRUE. there is a data array.
*     GROUPS = LOGICAL (Returned)
*        If .TRUE. there are random groups.
*     SIZE = INTEGER (Returned)
*        The number of pixels in the (or each) data array.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Prior Requirements:
*     The FITS file must already be opened by FITSIO.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     1994 September 15 (MJC):
*        Original version.
*     2000 November 7  (AJC):
*        Set Size, NDIM = 0 and DARRAY=.FALSE. if any NAXISn=0
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard global definitions
      INCLUDE 'PRM_PAR'          ! Magic-value definitions

*  Arguments Given:
      INTEGER FUNIT
      LOGICAL FIRST
      INTEGER MAXDIM

*  Arguments Returned:
      LOGICAL SIMPLE
      INTEGER BITPIX
      INTEGER NDIM
      INTEGER AXIS( MAXDIM )
      INTEGER PCOUNT
      INTEGER GCOUNT
      LOGICAL EXTEND
      INTEGER SIZE
      LOGICAL DARRAY
      LOGICAL GROUPS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER  FITSOK            ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER FSTAT              ! FITSIO status
      INTEGER N                  ! Axis loop counter
      LOGICAL THERE              ! Keyword is present?

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise logical values.
      GROUPS = .FALSE.
      DARRAY = .TRUE.
      SIZE = 1

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Obtain the mandatory FITS headers.
*  ==================================
      CALL FTGHPR( FUNIT, MAXDIM, SIMPLE, BITPIX, NDIM, AXIS, PCOUNT,
     :             GCOUNT, EXTEND, FSTAT )

*  Make some intelligible error report if something went wrong.
*  ============================================================
      IF ( FSTAT .NE. FITSOK ) THEN

*  Set a bad global status so that an error report can be made.
         STATUS = SAI__ERROR

*  SIMPLE keyword
*  ==============
         IF ( FIRST ) THEN
            IF ( FSTAT .EQ. 221 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'COF_MANDH_SIMPLE1',
     :           'The SIMPLE keyword is not present in the primary '/
     :           /'HDU of the FITS file or in the wrong order.',
     :           STATUS )
               GOTO 999

            ELSE IF ( FSTAT .EQ. 220 ) THEN
               CALL ERR_REP( 'COF_MANDH_SIMPLE2',
     :           'The SIMPLE keyword has an illegal value.', STATUS )
               GOTO 999
            END IF

*  XTENSION keyword
*  ================
         ELSE
            IF ( FSTAT .EQ. 225 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'COF_MANDH_XTENS',
     :           'The XTENSION keyword is not present in the '/
     :           /'extension HDU of the FITS file or is in the wrong '/
     :           /'order.', STATUS )
               GOTO 999
            END IF
         END IF

*  BITPIX
*  ======
*
         IF ( FSTAT .EQ. 211 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_BITPIX1',
     :        'The BITPIX keyword has an illegal value.', STATUS )
            GOTO 999

         ELSE IF ( FSTAT .EQ. 222 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_BITPIX2',
     :        'The BITPIX keyword is in the wrong order or not '/
     :        /'present in the FITS header.', STATUS )
            GOTO 999

*  NAXIS
*  =====
         ELSE IF ( FSTAT .EQ. 212 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_NAXIS2',
     :        'The NAXIS keyword has an illegal value.', STATUS )
            GOTO 999

         ELSE IF ( FSTAT .EQ. 223 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_NAXIS',
     :        'Error evaluating NAXIS in FITS header.', STATUS )
            GOTO 999

*  NAXISn
*  ======
         ELSE IF ( FSTAT .EQ. 213 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_NAXISn1',
     :        'An NAXISn keyword has an illegal value.', STATUS )
            GOTO 999

         ELSE IF ( FSTAT .EQ. 224 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_NAXISn2',
     :        'One or more of the NAXISn keywords is missing from the '/
     :        /'FITS header.', STATUS )
            GOTO 999

*  Others
*  ======
         ELSE IF ( FSTAT .EQ. 210 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_END',
     :        'There is no END card terminating the headers.', STATUS )

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_MANDH_ERR',
     :        'Error obtaining a value for one of the standard '/
     :        /'keywords.', STATUS )
            GOTO 999
         END IF
      END IF

*  Assign other returned arguments.
*  ================================

*  Zero dimension means there is no main data array.  The upper limit is
*  imposed by the data system.
      IF ( NDIM .LT. 0 .OR. NDIM .GT. MAXDIM ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NAXIS', NDIM )
         CALL ERR_REP( 'COF_MANDH_INAXIS',
     :     'Cannot process NAXIS = ^NAXIS', STATUS )
         GOTO 999
      END IF

*  Obtain the GROUPS flag.
      CALL COF_GKEYL( FUNIT, 'GROUPS', THERE, GROUPS, COMENT, STATUS )

*  The file may be in group format, where NAXIS1=0 and GCOUNT is
*  positive.  If so, there is a non-standard data-array structure
*  present.
      GROUPS = GROUPS .AND. AXIS( 1 ) .EQ. 0 .AND. GCOUNT .GT. 1

*  Allow for the case when there is no data array.
      IF ( NDIM .GT. 0 ) THEN

*  Loop for each dimension.
         DO N = 1, NDIM
*  Evaluate the number of pixels in the array.
*  and check that not all NAXISn == 0
*  SIZE will not be correct if any NAXISn are zero as in a Random Group header
            DARRAY = .FALSE.
            IF ( AXIS( N ) .GT. 0 ) THEN
               SIZE = SIZE * AXIS( N )
               DARRAY = .TRUE.
            END IF
         END DO

      ELSE
*  No data array is present.
         DARRAY = .FALSE.
         AXIS( 1 ) = VAL__BADI
      END IF

*  Occasionally higher dimensions may be set to one.  Remove such
*  useless dimensions.
      IF ( NDIM .GT. 1 ) THEN
         DO N = NDIM, 1, -1
            IF ( AXIS( N ) .NE. 1 ) GOTO 990
         END DO

         N = 1
  990    CONTINUE
         NDIM = N
      END IF

  999 CONTINUE

      END
