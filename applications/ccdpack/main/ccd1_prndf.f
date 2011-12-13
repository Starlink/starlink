      SUBROUTINE CCD1_PRNDF( IPIN, ITYPE, XDIMN, YDIMN, BAD, XDIMW,
     :                       YDIMW, SCALE, LOWER, UPPER, NRES,
     :                       GENVAL, LOWVAL, UPVAL, OUTPUT, STATUS )
*+
*  Name:
*     CCD1_PRNDF

*  Purpose:
*     Processes an array for display using the CCDPAIR routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PRNDF( IPIN, TYPE, XDIMN, YDIMN, BAD, XDIMW, YDIMW,
*                      SCALE, LOWER, UPPER, NRES, GENVAL, LOWVAL, UpVAL,
*                      OUTPUT, STATUS )

*  Description:
*     This routine resamples and rescales an array of data into a
*     integer array with data range 0 to 255.  The data range
*     corresponds to either a given percentange range or an upper and
*     lower value.  The data is resampled using a given scale factor
*     and then rescaled. The output array is then an image suitable for
*     displaying using IDI.

*  Arguments:
*     IPIN = INTEGER (Given)
*        Pointer to the array of data which is to be resampled and
*        scaled prior to display.
*     TYPE = CHARACTER * ( * ) (Given)
*        The numeric HDS type of the input array. One of _BYTE, _UBYTE,
*        _WORD, _UWORD, _INTEGER, _REAL, _DOUBLE.
*     XDIMN = INTEGER (Given)
*        The first dimension of the input array.
*     YDIMN = INTEGER (Given)
*        The second dimension of the input array.
*     BAD = LOGICAL (Given)
*        True if the input array may contain BAD values.
*     XDIMW = INTEGER (Given)
*        The first dimension of the output array.
*     YDIMW = INTEGER (Given)
*        The second dimension of the output array.
*     SCALE = DOUBLE PRECISION (Given)
*        The scale factor to use when resampling the input data into the
*        output array. Usually this will be set so as to fill at least
*        one dimension of the output array.
*     LOWER = DOUBLE PRECISION (Given)
*        The lower percentile point to use when scaling the data range.
*     UPPER = DOUBLE PRECISION (Given)
*        The upper percentile point to use when scaling the data range.
*     NRES = INTEGER (Given)
*        The number of reserved pens in display.
*     GENVAL = LOGICAL (Given)
*        Whether to use the percentiles ranges to generate the data
*        range or not.
*     LOWVAL = DOUBLE PRECISION (Given and Returned)
*        The lower data value used (maps to 0). If GENVAL is true this
*        is the value determined from the percentiles (returned). If
*        false it is assumed that a value is given.
*     UPVAL = DOUBLE PRECISION (Given and Returned)
*        The upper data value used (maps to 0). If GENVAL is true this
*        is the value determined from the percentiles (returned). If
*        false it is assumed that a value is given.
*     OUTPUT( XDIMW, YDIMW ) = INTEGER (Returned)
*        The output array. This should be integer and will contain on
*        exit data in the range 0 to 255. The 0 corresponding to the
*        LOWER percentile value and 255 to the UPPER percentile value.
*        The data will be resampled using a factor of SCALE in each
*        dimension.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1993 (PDRAPER):
*        Original version.
*     26-MAR-1993 (PDRAPER):
*        Added number of reserved pens.
*     10-JUL-2009 (TIMJ):
*        KAPLIBS version requires the UPPER and LOWER ranges to be
*        the same type as the data array.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER XDIMN
      INTEGER YDIMN
      INTEGER IPIN
      CHARACTER * ( * ) ITYPE
      INTEGER XDIMW
      INTEGER YDIMW
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION UPPER
      DOUBLE PRECISION LOWER
      INTEGER NRES
      LOGICAL GENVAL
      DOUBLE PRECISION UPVAL
      DOUBLE PRECISION LOWVAL
      LOGICAL BAD

*  Arguments Returned:
      INTEGER OUTPUT( XDIMW, YDIMW )
      LOGICAL BADOUT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NHIST              ! Number of bins in histogram
      PARAMETER ( NHIST = 2048 )

*  Local Variables:
      DOUBLE PRECISION WIDTH     ! Width of histogram bin
      DOUBLE PRECISION ZERO      ! Zero point of histogram
      INTEGER MODE               ! Histogram modal bin
      INTEGER NBIN               ! Number of bins in histogram
      INTEGER PEAK               ! Peak count in histogram
      INTEGER IPHIST             ! Pointer to histogram workspace
      INTEGER IPTEMP             ! Pointer to temporary workspace

*  Internal References:
      INCLUDE 'NUM_DEC'          ! NUM declarations
      INCLUDE 'NUM_DEF'          ! NUM definitions

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( GENVAL ) THEN

*  Determine the data range to display. First get space to store a
*  histogram.
         CALL CCD1_MALL( NHIST, '_INTEGER', IPHIST, STATUS )
         CALL CCD1_MKHIS( ITYPE, IPIN, XDIMN * YDIMN, BAD, 1, NHIST,
     :                    %VAL( CNF_PVAL( IPHIST ) ),
     :                    MODE, PEAK, NBIN, ZERO, WIDTH,
     :                    STATUS )

*  Get the actual values.
         CALL CCD1_HISP( %VAL( CNF_PVAL( IPHIST ) ),
     :                   NBIN, ZERO, WIDTH, UPPER,
     :                   UPVAL, STATUS )
         CALL CCD1_HISP( %VAL( CNF_PVAL( IPHIST ) ),
     :                   NBIN, ZERO, WIDTH, LOWER,
     :                   LOWVAL, STATUS )

*  Release the histogram.
         CALL CCD1_MFREE( IPHIST, STATUS )
      ELSE

*  Check that UPVAL and LOWVAL have a range.
         IF ( UPVAL .NE. LOWVAL ) THEN

*  Do nothing. This is ok.
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'NODATARANGE',
     :      '  CCD1_PRNDF: Data has no range', STATUS )
            GO TO 99
         END IF
      END IF

*  Get workspace for intermediate image (of type ITYPE).
      CALL CCD1_MALL( XDIMW * YDIMW, ITYPE, IPTEMP, STATUS )

*  Now resample the data array into the output array.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_RENPB( %VAL( CNF_PVAL( IPIN ) ),
     :                    XDIMN, YDIMN, BAD, XDIMW,
     :                    YDIMW, .FALSE., SCALE,
     :                    %VAL (CNF_PVAL( IPTEMP) ),
     :                    BADOUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_RENPUB( %VAL( CNF_PVAL( IPIN ) ),
     :                     XDIMN, YDIMN, BAD, XDIMW,
     :                     YDIMW, .FALSE., SCALE,
     :                     %VAL( CNF_PVAL( IPTEMP ) ),
     :                     BADOUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_RENPW( %VAL( CNF_PVAL( IPIN ) ),
     :                    XDIMN, YDIMN, BAD, XDIMW,
     :                    YDIMW, .FALSE., SCALE,
     :                    %VAL( CNF_PVAL( IPTEMP ) ),
     :                    BADOUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_RENPUW( %VAL( CNF_PVAL( IPIN ) ),
     :                     XDIMN, YDIMN, BAD, XDIMW,
     :                     YDIMW, .FALSE., SCALE,
     :                     %VAL( CNF_PVAL( IPTEMP ) ),
     :                     BADOUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_RENPI( %VAL( CNF_PVAL( IPIN ) ),
     :                    XDIMN, YDIMN, BAD, XDIMW,
     :                    YDIMW, .FALSE., SCALE,
     :                    %VAL( CNF_PVAL( IPTEMP ) ),
     :                    BADOUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_RENPR( %VAL( CNF_PVAL( IPIN ) ),
     :                    XDIMN, YDIMN, BAD, XDIMW,
     :                    YDIMW, .FALSE., SCALE,
     :                    %VAL( CNF_PVAL( IPTEMP ) ),
     :                    BADOUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_RENPD( %VAL( CNF_PVAL( IPIN ) ),
     :                    XDIMN, YDIMN, BAD, XDIMW,
     :                    YDIMW, .FALSE., SCALE,
     :                    %VAL( CNF_PVAL( IPTEMP ) ),
     :                    BADOUT, STATUS )
      END IF

*  And finally rescale it.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL KPG1_ISCLB( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    .FALSE., NUM_DTOB(LOWVAL), NUM_DTOB(UPVAL),
     :                    NRES, 255, 0, OUTPUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL KPG1_ISCLUB( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP )),
     :                     .FALSE., NUM_DTOUB(LOWVAL), NUM_DTOUB(UPVAL),
     :                     NRES, 255, 0, OUTPUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL KPG1_ISCLW( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    .FALSE., NUM_DTOW(LOWVAL), NUM_DTOW(UPVAL),
     :                    NRES, 255, 0, OUTPUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL KPG1_ISCLUW( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP )),
     :                     .FALSE., NUM_DTOUW(LOWVAL), NUM_DTOUW(UPVAL),
     :                     NRES, 255, 0, OUTPUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL KPG1_ISCLI( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    .FALSE., NUM_DTOI(LOWVAL), NUM_DTOI(UPVAL),
     :                    NRES, 255, 0, OUTPUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL KPG1_ISCLR( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    .FALSE., NUM_DTOR(LOWVAL), NUM_DTOR(UPVAL),
     :                    NRES, 255, 0, OUTPUT, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL KPG1_ISCLD( BAD, XDIMW, YDIMW, %VAL( CNF_PVAL( IPTEMP ) ),
     :                    .FALSE.,
     :                    LOWVAL, UPVAL, NRES, 255, 0, OUTPUT, STATUS )
      END IF

 99   CONTINUE

*  Release the temporary workspace.
      CALL CCD1_MFREE( IPTEMP, STATUS )

      END
* $Id$
