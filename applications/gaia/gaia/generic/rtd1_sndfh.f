      SUBROUTINE RTD1_SNDFH( NDF, BITPIX, BAD, NHEAD, IPHEAD, AVAIL,
     :           STATUS )
*+
*  Name:
*     RTD_SNDFH

*  Purpose:
*     Creates the standard headers for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD_SNDFH( NDF, BITPIX, BAD, NHEAD, IPHEAD, STATUS )

*  Description:
*     This routine either creates a fits header block with the standard
*     keywords, SIMPLE, BITPIX, NAXIS, NAXIS1, NAXIS2 and BLANK.

*  Arguments:
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     BITPIX = INTEGER (Given)
*        The FITS data type.
*     BAD = LOGICAL (Given)
*        Whether the NDF may contain bad pixels or not.
*     NHEAD = INTEGER (Given and Returned)
*        On exit this is the number of cards used in HEADER. On entry
*        the maximum size of HEADER
*     IPHEAD = INTEGER (Given and Returned)
*        Pointer to FITS block.
*     AVAIL = INTEGER (Given and Returned)
*        Number of available cards in FITS block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     7-MAR-1996 (PWD):
*        Original version. Based on COF_WNDFH from CONVERT.
*     22-NOV-1996 (PWD):
*        Now just does standard NDF headers.
*     02-DEC-1997 (PWD):
*        Modified to write NDF dimensional headers.
*     18-JUN-1999 (PWD):
*        Removed NHEAD increment. This was a bug.
*     {enter_any_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*     Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'NDF_PAR'         ! NDF_ public constants
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants

*  Arguments Given:
      INTEGER NDF
      INTEGER BITPIX
      LOGICAL BAD

*  Arguments Given and Returned:
      INTEGER IPHEAD
      INTEGER NHEAD
      INTEGER AVAIL

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      CHARACTER KEYWRD * ( 10 ) ! FITS keyword
      CHARACTER VALUE * ( 80 )  ! Accommodates FITS card
      INTEGER I                 ! Loop variable
      INTEGER LBND( NDF__MXDIM )! NDF lower bounds
      INTEGER NCHAR             ! Number of encoded characters
      INTEGER NDIM              ! Number of dimensions
      INTEGER UBND( NDF__MXDIM )! NDF upper bounds

*  Local functions:
      INCLUDE 'NUM_DEC_CVT'     ! Numeric conversion routines.
      INCLUDE 'NUM_DEF_CVT'

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Data is SIMPLE.
      CALL RTD1_WRFTL( 'SIMPLE', 'T', ' ', IPHEAD, NHEAD, AVAIL, STATUS)

*  Add the BIXPIX header.
      CALL RTD1_WRFTI( 'BITPIX', BITPIX, ' ', IPHEAD, NHEAD, AVAIL,
     :                 STATUS)

*  Get the bounds of the NDF.
      CALL NDF_BOUND( NDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Add NAXIS headers.
      CALL RTD1_WRFTI( 'NAXIS', NDIM, ' ', IPHEAD, NHEAD, AVAIL, STATUS)
      KEYWRD = 'NAXIS'
      DO 1 I = 1, NDIM
         CALL CHR_ITOC( I, KEYWRD( 6: ), NCHAR )
         CALL RTD1_WRFTI( KEYWRD, UBND( I ) - LBND( I ) + 1  , ' ',
     :                    IPHEAD, NHEAD, AVAIL, STATUS)
 1    CONTINUE

*  Add the BLANK keyword if required (note using a BLANK for floating
*  point is wrong, should use NaNs..., but we'll go along with things
*  for now).
      IF ( BAD ) THEN
         IF ( BITPIX .EQ. -32 ) THEN
            WRITE( VALUE, '(G24.17)') VAL__BADR
         ELSE IF ( BITPIX .EQ. -64 ) THEN
C            WRITE( VALUE, '(G24.17)') VAL__BADD ! Broken in g77 2.91.66 missing E in exponent
            CALL DENCODE( VAL__BADD, VALUE )
         ELSE IF ( BITPIX .EQ. 32 ) THEN
            WRITE( VALUE, '(I16)') VAL__BADI
         ELSE IF ( BITPIX .EQ. 16 ) THEN
            WRITE( VALUE, '(I16)') NUM_WTOI( VAL__BADW )
         ELSE IF ( BITPIX .EQ. -16 ) THEN
            WRITE( VALUE, '(I16)') NUM_UWTOI( VAL__BADUW )
         ELSE IF ( BITPIX .EQ. 8 ) THEN
            WRITE( VALUE, '(I16)') NUM_BTOI( VAL__BADB )
         ELSE IF ( BITPIX .EQ. -8 ) THEN
            WRITE( VALUE, '(I16)') NUM_UBTOI( VAL__BADUB )
         END IF
         CALL RTD1_WRFTC( 'BLANK', VALUE, ' ', IPHEAD, NHEAD, AVAIL,
     :                    STATUS)
      END IF
      END
