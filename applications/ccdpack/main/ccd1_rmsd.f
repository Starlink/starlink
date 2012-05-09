      SUBROUTINE CCD1_RMSD( PTYPE, BAD, IPVEC, SIZE, AVEACC, NOISE,
     :                      VALPIX, STATUS )
*+
*  Name:
*     CCD1_RMSD

*  Purpose:
*     To estimate the mean and standard deviation of a vectorised array
*     of type PTYPE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RMSD( PTYPE, BAD, IPVEC, SIZE, AVEACC, NOISE, VALPIX,
*                     STATUS )

*  Description:
*     This routine is a dummy which just procrastinates the typing of
*     the data. It actually calls CCG1_STMN which forms the sum of all
*     valid pixels in the vectorised array VEC. The mean is then this
*     value divided by the number of valid pixels.  For reasonably
*     small sections of the array estimates of the noise are made. From
*     these estimates a global noise value is derived. This hopefully
*     stops problems with large scale variations and represents the
*     'true' small scale noise.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Flag set if there are bad pixels present in input array.
*     IPVEC = INTEGER (Given)
*        Pointer to the vectorised array of values.
*     SIZE = INTEGER (Given)
*        Size of the array.
*     AVEACC = DOUBLE PRECISION (Returned)
*        The average value of the array expressed in double precision.
*     NOISE = DOUBLE PRECISION (Returned)
*        The noise level in the data.
*     VALPIX = INTEGER (Returned)
*        The number of non-bad pixels in the vectorised array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     23-JUL-1991 (PDRAPER):
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
      LOGICAL BAD
      INTEGER SIZE
      INTEGER IPVEC
      CHARACTER PTYPE * ( * )

*  Arguments Returned:
      DOUBLE PRECISION AVEACC
      DOUBLE PRECISION NOISE
      INTEGER VALPIX

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call the appropriate routine.
      IF ( PTYPE .EQ. '_BYTE' ) THEN
         CALL CCG1_STMNB( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                    NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_UBYTE' ) THEN
         CALL CCG1_STMNUB( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                     NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_WORD' ) THEN
         CALL CCG1_STMNW( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                    NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_UWORD' ) THEN
         CALL CCG1_STMNUW( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                     NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_INTEGER' ) THEN
         CALL CCG1_STMNI( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                    NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_REAL' ) THEN
         CALL CCG1_STMNR( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                    NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_DOUBLE' ) THEN
         CALL CCG1_STMND( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                    NOISE, VALPIX, STATUS )
      ELSE IF ( PTYPE .EQ. '_INT64' ) THEN
         CALL CCG1_STMNK( BAD, %VAL( CNF_PVAL( IPVEC ) ), SIZE, AVEACC,
     :                    NOISE, VALPIX, STATUS )
      END IF

      END
* $Id$
