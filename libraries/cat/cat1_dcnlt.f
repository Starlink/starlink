      SUBROUTINE CAT1_DCNLB (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLB
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLB NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  BYTE (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
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
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      BYTE
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      BYTE
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
      SUBROUTINE CAT1_DCNLC (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLC
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLC NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  CHARACTER*(*) (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER*(*)
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      CHARACTER*(CAT__SZVAL)
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
      SUBROUTINE CAT1_DCNLD (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLD
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLD NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  DOUBLE PRECISION (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      DOUBLE PRECISION
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      DOUBLE PRECISION
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
      SUBROUTINE CAT1_DCNLI (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLI
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLI NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  INTEGER (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      INTEGER
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
      SUBROUTINE CAT1_DCNLL (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLL
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLL NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  LOGICAL (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      LOGICAL
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      LOGICAL
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
      SUBROUTINE CAT1_DCNLR (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLR
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLR NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  REAL (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      REAL
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      REAL
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
      SUBROUTINE CAT1_DCNLW (NULCHR, NULVAL, STATUS)
*+
*  Name:
*     CAT1_DCNLW
*  Purpose:
*     Decode a null value from a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_DCNLW NULCHR; (NULVAL; STATUS)
*  Description:
*     Decode a null value from a character string.
*  Arguments:
*     NULCHR  =  CHARACTER*(*) (Given)
*        Character string from which the null value is to be
*        decoded.
*     NULVAL  =  INTEGER*2 (Returned)
*        Null value decoded from the string.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the output value.
*     Copy the input character string to the local string.
*     Copy the local buffer to the return null value.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     27/1/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER*2
     :  NULVAL
*  Arguments Returned:
      CHARACTER
     :  NULCHR*(*)
*  Status:
      INTEGER STATUS         ! Global status
*  Local Variables:
      INTEGER*2
     :  LVAL     ! Local buffer of same data type as input null value.
      CHARACTER
     :  BUFFER*(CAT__SZVAL)  ! Local character buffer.

      EQUIVALENCE (LVAL, BUFFER)
*.

      IF (STATUS .EQ. CAT__OK) THEN

         BUFFER = NULCHR
         NULVAL = LVAL

      END IF

      END
