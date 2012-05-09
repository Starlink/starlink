      SUBROUTINE CCD1_MKHIS( ITYPE, ARRAY, NPIX, BAD, MINBIN, NHIST,
     :                       HIST, MODE, PEAK, NBIN, ZERO, WIDTH,
     :                       STATUS )
*+
*  Name:
*     CCD1_MKHIS

*  Purpose:
*     Creates an optimised histogram of a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_MKHIS( ITYPE, ARRAY, NPIX, BAD, MINBIN, NHIST, HIST,
*                      MODE, PEAK, NBIN, ZERO, WIDTH, STATUS )

*  Description:
*     This routine calls CCD1_MKHI<T> to produce a histogram of the
*     input data of type ITYPE> The histogram is formed in such a way
*     that at least MINBIN values are present in the peak counts bin.
*     The first estimate of the histogram is formed using NHIST
*     elements. If more than MINBIN counts are present in at least one
*     bin then no further action is taken, if less than is number of
*     counts are present in the peak bin then the histogram is rebinned
*     to increase the number count until MINBIN is exceeded. On exit
*     the number of bins used to form the histogram is returned
*     together with the bin number which contains the peak count level
*     and the width (in data values) of the bin and a zero point. The
*     original data values are related to the bin number (starting at 1
*     up to NBIN) by
*
*        VALUE = (NBIN-1)*WIDTH+ZERO

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The input data type. Must be one of the HDS non-complex
*        numerics.
*     ARRAY = INTEGER (Given)
*        Pointer to input array of values.
*     NPIX = INTEGER (Given)
*        Number of elements in input array.
*     BAD = LOGICAL (Given)
*        Whether the input array contains BAD values or not.
*     MINBIN = INTEGER (Given)
*        The minimum number of counts which may be in the peak bin on
*        exit.
*     NHIST = INTEGER (Given)
*        Size of the input histogram array. This value is used as the
*        starting point for forming the histogram. The size of this
*        should be sufficient to ensure that the histogram is initially
*        oversampled (so that the MINBIN criterion is NOT meet) if it is
*        vital that the MINBIN count level is just meet.
*     HIST( NHIST ) = INTEGER (Returned)
*        The histogram of the input data values.
*     MODE = INTEGER (Returned)
*        The number of the bin which contains the peak count.
*     PEAK = INTEGER (Returned)
*        The number of counts in the modal bin.
*     NBIN = INTEGER (Returned)
*        The number of bins used to form the histogram (i.e. size of
*        HIST on exit).
*     ZERO = DOUBLE PRECISION (Returned)
*        The zero point of the histogram in data values.
*     WIDTH = DOUBLE PRECISION (Returned)
*        The width of a bin of the histogram in data values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) ITYPE
      INTEGER NPIX
      INTEGER ARRAY
      INTEGER MINBIN
      INTEGER NHIST
      LOGICAL BAD

*  Arguments Returned:
      INTEGER HIST( NHIST )
      INTEGER MODE
      INTEGER PEAK
      INTEGER NBIN
      DOUBLE PRECISION ZERO
      DOUBLE PRECISION WIDTH

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Branch calling version of CCD1_MKHI appropriate to the input data
*  type.
      IF ( ITYPE .EQ. '_BYTE' ) THEN
        CALL CCG1_MKHIB( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
        CALL CCG1_MKHIUB( %VAL( CNF_PVAL( ARRAY ) ),
     :                    NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
        CALL CCG1_MKHIW( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
        CALL CCG1_MKHIUW( %VAL( CNF_PVAL( ARRAY ) ),
     :                    NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
        CALL CCG1_MKHII( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
        CALL CCG1_MKHIR( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
        CALL CCG1_MKHID( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
        CALL CCG1_MKHIK( %VAL( CNF_PVAL( ARRAY ) ),
     :                   NPIX, BAD, MINBIN, NHIST, HIST,
     :                   MODE, NBIN, ZERO, WIDTH, STATUS )
      ELSE

*  Unsupported data type.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', ITYPE )
         CALL ERR_REP( 'CCD1_MKHIST',
     :   '  CCD1_MKHIS: Unsupported data type (^TYPE).', STATUS )

      END IF

*  Extract the peak value.
      IF ( STATUS .EQ. SAI__OK ) PEAK = HIST( MODE )

      END
* $Id$
