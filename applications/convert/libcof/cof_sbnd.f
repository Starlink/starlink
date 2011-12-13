      SUBROUTINE COF_SBND( FUNIT, NDF, KEYROT, CHECK, STATUS )
*+
*  Name:
*     COF_SBND

*  Purpose:
*     Sets or checks the bounds of an NDF using a FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_SBND( FUNIT, NDF, KEYROT, CHECK, STATUS )

*  Description:
*     If CHECK is FALSE,
*     This routine sets the lower and upper bounds of an NDF, by
*     using the information stored in a FITS header.
*     IF CHECK is TRUE,
*     the upper and lower bounds of the NDF and FITS header are compared
*     and an error reported and STATUS set if they differ.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The FITSIO unit number for the FITS file.
*     NDF = INTEGER (Given)
*        The identifier of the NDF which is to have new bounds.
*     KEYROT = CHARACTER * ( * ) (Given)
*        The root name of the FITS keyword that defines a lower bound.
*        The full keyword being KEYROTn, where is the dimension number.
*        If this is a blank value, the lower bounds are assumed to be
*        one.  It is limited to 7 characters, comprising numbers,
*        letters, underscore and hyphen.  The normal root is 'LBOUND'.
*     CHECK = LOGICAL (Given)
*        If the bounds are to be checked or set
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Prior Requirements:
*     The current header and data unit must either be primary or an
*     IMAGE extension.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1996, 2000, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     1996 January 21 (MJC):
*        Original version.
*     2000 April 5 (AJC):
*        Added the check facility
*     2004 September 10 (TIMJ):
*        Fix valgrind warning with uninitialised KEYWRD on entry
*        to fitsio routine
*        When extension has a symbolic type of IMAGE use that
*        to override the XTENSION value (for compressed images).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER FUNIT
      INTEGER NDF
      CHARACTER * ( * ) KEYROT
      LOGICAL CHECK

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER FITSOK             ! Value of good FITSIO status
      PARAMETER( FITSOK = 0 )

*  Local Variables:
      INTEGER ALBND( NDF__MXDIM ) ! Existing lower bounds of the NDF
      INTEGER AUBND( NDF__MXDIM ) ! Existing upper bounds of the NDF
      INTEGER ANDIM               ! Existing dimensionality of the NDF
      INTEGER BITPIX             ! FITS BITPIX value (not used)
      CHARACTER * ( 80 ) BUFFER  ! Used to form error messages
      CHARACTER * ( 48 ) COMENT  ! Keyword comment
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the NDF
      LOGICAL EXTEND             ! FITS EXTEND value (not used)
      INTEGER FSTAT              ! FITSIO error status
      INTEGER GCOUNT             ! FITS group count value (not used)
      INTEGER I                  ! Loop counter
      CHARACTER * ( 8 ) KEYWRD   ! FITS keyword for a lower bound
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of the NDF
      INTEGER HDUTYP             ! Type of HDU.
      INTEGER NDIM               ! Dimensionality of the NDF
      INTEGER NHDU               ! Number of the current HDU
      INTEGER PCOUNT             ! FITS PCOUNT value (not used)
      LOGICAL SIMPLE             ! Simple FITS (not used)
      LOGICAL THERE              ! Keyword is present?
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of the NDF
      CHARACTER * ( 8 ) XTENS    ! Extension name

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITSIO status.  It's not the same as the Starlink
*  status, which is reset by the fixed part.
      FSTAT = FITSOK

*  Check that the current HDU is the primary or IMAGE extension.
*  At present there is no inquiry routine, so inquire the number of
*  the HDU.  1 is the primary HDU.
      CALL FTGHDN( FUNIT, NHDU )
      IF ( NHDU .GT. 1 ) THEN

*  Obtain the value of the XTENSION keyword.
         CALL COF_GKEYC( FUNIT, 'XTENSION', THERE, XTENS, COMENT,
     :                   STATUS )

         CALL FTGHDT( FUNIT, HDUTYP, STATUS )
         IF ( XTENS .EQ. 'BINTABLE' .AND. HDUTYP .EQ. 0 ) THEN
            XTENS = 'IMAGE'
         END IF

         IF ( .NOT. ( THERE .AND. STATUS .EQ. SAI__OK
     :        .AND. XTENS .EQ. 'IMAGE' ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_SBND',
     :        'Current header and data unit is not primary or IMAGE '/
     :        /'extension, therefore cannot define NDF bounds.',
     :        STATUS )
            GOTO 999
         END IF
      END IF

*  Obtain the bounds and dimensions from the FITS file.
      CALL FTGHPR( FUNIT, NDF__MXDIM, SIMPLE, BITPIX, NDIM, DIMS,
     :             PCOUNT, GCOUNT, EXTEND, FSTAT )

      IF ( FSTAT .NE. FITSOK ) THEN
         CALL COF_FIOER( FSTAT, 'COF_SBND_KEYWORD', 'FTGHPR',
     :     'Error obtaining the dimensions of the NDF from the FITS '/
     :     /'keywords.', STATUS )
         GOTO 999
      END IF

*  Shift the dimensions if the first is zero (meaning a random-groups
*  format).  Reduce the number of dimensions to allow for this
*  convention.
      IF ( DIMS( 1 ) .EQ. 0 ) THEN
         NDIM = NDIM - 1
         DO I = 1, NDIM
            DIMS( I ) = DIMS( I + 1 )
         END DO
      END IF

*  Do not search for keywords.
      IF ( KEYROT .EQ. ' ' ) THEN

         DO I = 1, NDIM
            LBND( I ) = 1
            UBND( I ) = DIMS( I )
         END DO

      ELSE

         DO I = 1, NDIM

*  See if there are lower-bounds keywords.  Use the default value if
*  one is not present.
            KEYWRD = ' '
            CALL FTKEYN( KEYROT, I, KEYWRD, FSTAT )
            IF ( FSTAT .NE. FITSOK ) THEN
               BUFFER = 'FITS keyword for lower bounds is invalid.  '/
     :           /'Root is '//KEYROT//'. (Probable programming error.)'

               CALL COF_FIOER( FSTAT, 'COF_SBND_KEYWORD', 'FTKEYN',
     :                         BUFFER, STATUS )
               GOTO 999
            END IF

*  Obtain the value of the keyword.
            CALL COF_GKEYI( FUNIT, KEYWRD, THERE, LBND( I ), COMENT,
     :                      STATUS )

            IF ( THERE ) THEN
               UBND( I ) = LBND( I ) - 1 + DIMS( I )

*  Use the default bounds if the LBOUNDn keyword is absent or is not
*  integer.
            ELSE
               LBND( I ) = 1
               UBND( I ) = DIMS( I )
            END IF
         END DO
      END IF


*  Set or check the dimensionality, and the bounds of the NDF.
      IF ( CHECK ) THEN
*  A check was requested
*  Get the existing bounds
         CALL NDF_BOUND( NDF, NDF__MXDIM, ALBND, AUBND, ANDIM, STATUS )
*  and compare with the FITS header
         IF ( NDIM .EQ. ANDIM ) THEN
            DO I = 1, NDIM
               IF ( ( LBND(I) .NE. ALBND(I) ) .OR.
     :              ( UBND(I) .NE. AUBND(I) ) ) THEN
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'COF_SBND_BNDER',
     :             'Bounds mismatch within an extension set', STATUS )
               END IF
            END DO

         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'COF_SBND_NDIMS',
     :       'NDIMS mismatch within an extension set', STATUS )
         END IF

      ELSE
*  Requested to set the bounds
         CALL NDF_SBND( NDIM, LBND, UBND, NDF, STATUS )
      END IF

  999 CONTINUE

      END
