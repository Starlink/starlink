      SUBROUTINE CCD1_ESTM( ITYPE, DIRECT, IPIN, IDIM1, IDIM2, BOUNDS,
     :                      NBOUND, IPWORK, MEAN, NOISE, STATUS )
*+
*  Name:
*     CCD1_ESTM

*  Purpose:
*     To estimate the mean and standard deviation of a given set of rows
*     or columns.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ESTM( ITYPE, DIRECT, IPIN, IDIM1, IDIM2, BOUNDS,
*                     NBOUND, IPWORK, MEAN, NOISE, STATUS )

*  Description:
*     This routine is a dummy within which the typing is defered from
*     the previous level. Initially the given columns or rows are
*     extracted, then the mean and standard deviation are formed for the
*     given type.

*  Arguments:
*     ITYPE = CHARACTER * ( * ) (Given)
*        The data type of the given arrays, any non-complex numeric
*        HDS type.
*     DIRECT = INTEGER (Given)
*        The direction of the given bounds:
*        1 : horizontally (rows)
*        2 : vertically (columns)
*     IPIN = INTEGER (Given)
*        Pointer to array containing the data to be extracted and
*        averaged.
*     IDIM1 = INTEGER (Given)
*        First dimension of input array.
*     IDIM2 = INTEGER (Given)
*        Second dimension of input array.
*     BOUNDS( NBOUND ) = INTEGER (Given)
*        The upper and lower bounds of the row or column sections.
*     IPWORK = INTEGER (Given)
*        Pointer to workspace array of size at least IDIM1*IDIM2.
*     MEAN = DOUBLE PRECISION (Returned)
*        The mean value in the required sections.
*     NOISE = DOUBLE PRECISION (Returned)
*        The standard deviation of the required sections.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine uses passed pointers not arrays.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     24-APR-1991 (PDRAPER):
*        Original version.
*     10-DEC-1991 (PDRAPER):
*        Changed to fully generic
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
      CHARACTER * ( * ) ITYPE
      INTEGER DIRECT
      INTEGER IPIN
      INTEGER IDIM1
      INTEGER IDIM2
      INTEGER NBOUND
      INTEGER BOUNDS( NBOUND )
      INTEGER IPWORK

*  Arguments Returned:
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION NOISE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL
      INTEGER VALPIX

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      EL = IDIM1 * IDIM2

*  Extract the sections and perform the statistics for each data type.

      IF ( ITYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_BEXTB( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNB( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                    EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_BEXTUB( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                     IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNUB( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                     EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_WORD' ) THEN
         CALL CCG1_BEXTW( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNW( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                    EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_BEXTUW( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                     IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNUW( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                     EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_BEXTI( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNI( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                    EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_REAL' ) THEN
         CALL CCG1_BEXTR( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNR( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                    EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_BEXTD( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMND( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                    EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE IF ( ITYPE .EQ. '_INT64' ) THEN
         CALL CCG1_BEXTK( DIRECT, %VAL( CNF_PVAL( IPIN ) ),
     :                    IDIM1, IDIM2, BOUNDS,
     :                    NBOUND, %VAL( CNF_PVAL( IPWORK ) ), STATUS )
         CALL CCG1_STMNK( .TRUE., %VAL( CNF_PVAL( IPWORK ) ),
     :                    EL, MEAN, NOISE,
     :                    VALPIX, STATUS )
      ELSE

*  Unsupported numeric type.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_ESTM',
     :   '  CCD1_ESTM : called with unsupported numeric type', STATUS )

      END IF
      END
* $Id$
