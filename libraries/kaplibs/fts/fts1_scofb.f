      SUBROUTINE FTS1_SCOFB ( BSCALE, BZERO, UNDEF, BLANK, SIZE, ARRAY,
     :                        STATUS )
*+
*  Name:
*     FTS1_SCOFB

*  Purpose:
*     Applies scale and zero to a REAL data vector, and substitutes
*     magic values for blank FITS data.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FTS1_SCOFB ( BSCALE, BZERO, UNDEF, BLANK, SIZE, ARRAY,
*    :                  STATUS )

*  Description:
*     This routine applies scale and offset to a 1-d REAL array.  Any
*     pixels with the %BLANK value are substituted by the standard
*     magic value. On output, pixel is input value times scale plus
*     offset.

*  Arguments:
*     BSCALE = REAL (Given)
*        Scale factor to be applied to the array.
*     BZERO = REAL (Given)
*        Offset to be applied to the array.
*     UNDEF = LOGICAL (Given)
*        If true, testing and replacement of undefined data values is
*        to occur.  A blank value, as specified by %BLANK, is replaced
*        by the standard magic value.  If false, the value of %BLANK
*        is ignored.
*     BLANK = INTEGER (Given)
*        Value of an undefined datum.
*     SIZE  = INTEGER (Given)
*        Number of elements in the data array.
*     ARRAY( SIZE ) = REAL (Given and Returned)
*        The data array to which scaling and offset is to be applied.
*     STATUS  = INTEGER (Given)
*        Global status value.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     If replacement of blank pixels required then
*        For all pixels
*           If pixel is undefined then
*              Set pixel to magic value
*           Else
*              Compute output pixel value
*           Endif
*        Endfor
*     Else
*        For all pixels
*           Compute output pixel value
*        Endfor
*     Endif
*     End

*  Copyright:
*     Copyright (C) 1987, 1990 Science & Engineering Research Council.
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
*     1987 Sep 23 (MJC):
*        Original version.
*     1990 November 19 (MJC):
*        Renamed from FITSEX, and converted to the SST prologue style.
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE           ! no default typing allowed


*  Global Constants:
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PRM_PAR'       ! Magic-value definitions


*  Arguments Given:
      REAL
     :  BSCALE,                ! Scale factor
     :  BZERO                  ! Offset

      INTEGER
     :  BLANK,                 ! Undefined pixels have this value on
                               ! input
     :  SIZE                   ! Number of elements in the data array

      LOGICAL                  ! True if:
     :  UNDEF                  ! BLANK has meaning, and undefined pixels
                               ! are located and replaced by the
                               ! standard magic value

*  Arguments Given and Returned:
      REAL
     :  ARRAY( SIZE )          ! Data array


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  I                      ! Loop counter


*.


*    Check for an error on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    There could be blank values.

      IF ( UNDEF ) THEN
         DO  I = 1, SIZE

*          Since values have been converted from integers, NINT will not
*          include defined pixels as undefined.  Set blank values to
*          the standard bad value, otherwise apply the scale and offset.

            IF ( NINT( ARRAY( I ) ) .EQ. BLANK ) THEN
               ARRAY( I ) = VAL__BADR
            ELSE
               ARRAY( I ) = BSCALE * ARRAY( I ) + BZERO
            END IF
         END DO

*    No bad-pixel checking. Just apply the scale and offset in situ.

      ELSE

*       Apply scale and offset to all pixels

         DO  I = 1, SIZE
            ARRAY( I ) = BSCALE * ARRAY( I ) + BZERO
         END DO
      END IF

      END
