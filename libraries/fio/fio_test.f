      LOGICAL FUNCTION FIO_TEST( ERRCLS, STATUS )
*+
*  Name:
*     FIO_TEST

*  Purpose:
*     Test if an FIO status value belongs to a certain class of errors

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = FIO_TEST( ERRCLS, STATUS )

*  Description:
*     See if the value of STATUS corresponds one of the FIO error codes
*     that correspond to the error class given as the first argument.

*  Arguments:
*     ERRCLS = CHARACTER * ( * ) (Given)
*        The name of the error class
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     FIO_TEST = LOGICAL
*        Whether STATUS is in the named class of errors.

*  Examples:
*     IF( FIO_TEST( 'OPEN ERROR', STATUS ) ) THEN ...
*        See if the value of STATUS is one of the values associated
*        with the error class 'OPEN ERROR'.

*  Algorithm:
*     -  Search for ERRCLS in the table of error classes.
*     -  If a match is found;
*     -     try to match STATUS against each value for the found class
*           in the table.
*     -  If a match is found, set the function value to true
*     -  otherwise, set it to false.

*  Implementation Deficiencies:
*     This is a quick and dirty release of this routine. It will be
*     replaced with one based on EVT when that is developed to allow
*     users to define their own error classes.

*  External Routines Used:
*     CHR:
*        CHR_SIMLR

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1992 (PMA):
*        Original version.
*     18-FEB-1993 (PMA):
*        Change the name of include files to upper case.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants

*  Arguments Given:
      CHARACTER * ( * ) ERRCLS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Do two strings match except for case?

*  Local Constants:
      INTEGER EVT__MXENT
      PARAMETER( EVT__MXENT = 6 )
      INTEGER EVT__MXVAL
      PARAMETER( EVT__MXVAL = 15 )
      INTEGER EVT__SZENT
      PARAMETER( EVT__SZENT = 16 )

*  Local Variables:
      INTEGER I                  ! Loop counter
      INTEGER J                  ! Loop counter
      CHARACTER * ( EVT__SZENT ) ELIST ( EVT__MXENT ) ! Entity list
      INTEGER NLIST( EVT__MXENT ) ! Actual number of values per entry
      INTEGER VLIST( EVT__MXVAL, EVT__MXENT ) ! Value list

*  Local Data:
      DATA ELIST( 1 ) / 'OPEN ERROR' /
      DATA NLIST( 1 ) / 12 /
      DATA (VLIST( I, 1 ), I = 1, 12) /
     : FIO__FILNF, FIO__CFOLF, FIO__COEXI, FIO__NFEXI, FIO__NAMER,
     : FIO__NODEV, FIO__OPNER, FIO__PTAFD, FIO__PERMD, FIO__ILLOP,
     : FIO__ALOPN, FIO__TOOMF
     : /
      DATA ELIST( 2 ) / 'CLOSE ERROR' /
      DATA NLIST( 2 ) / 3 /
      DATA (VLIST( I, 2 ), I = 1, 3) /
     : FIO__ILLCL, FIO__CLSER, FIO__INCOC
     : /
      DATA ELIST( 3 ) / 'READ ERROR' /
      DATA NLIST( 3 ) / 9 /
      DATA (VLIST( I, 3 ), I = 1, 9) /
     : FIO__RDER, FIO__INPCN, FIO__INREQ, FIO__SYNAM, FIO__TOOMV,
     : FIO__RUNCH, FIO__BLINP, FIO__ILSTI, FIO__IINAM
     : /
      DATA ELIST( 4 ) / 'WRITE ERROR' /
      DATA NLIST( 4 ) / 4 /
      DATA (VLIST( I, 4 ), I = 1, 4) /
     : FIO__WRTER, FIO__REWRT, FIO__OUTCN, FIO__OUTOV
     : /
      DATA ELIST( 5 ) / 'REWIND ERROR' /
      DATA NLIST( 5 ) / 1 /
      DATA (VLIST( I, 5 ), I = 1, 1) /
     : FIO__REWER
     : /
      DATA ELIST( 6 ) / 'BACKSPACE ERROR' /
      DATA NLIST( 6 ) / 2 /
      DATA (VLIST( I, 6 ), I = 1, 2) /
     : FIO__BACER, FIO__CNTBF
     : /

*.

*  Look for ERRCLS in the table.
      DO I = 1, EVT__MXENT
	 IF ( CHR_SIMLR( ERRCLS, ELIST( I ) ) ) THEN

*  Found it. Now look for VALUE in its value list.
	    DO J = 1, NLIST( I )
	       IF ( STATUS .EQ. VLIST( J, I ) ) THEN

*  Found it too. The function is set to true.
                  FIO_TEST = .TRUE.
		  GOTO 999
               END IF
            END DO

*  VALUE not found. Function is set to false.
            FIO_TEST = .FALSE.
            GOTO 999
         END IF
      END DO

*  ERRCLS not found in the table of errors. Function is set to false.
      FIO_TEST = .FALSE.

  999 CONTINUE
      END
