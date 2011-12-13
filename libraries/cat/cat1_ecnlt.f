      SUBROUTINE CAT1_ECNLB (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLB
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLB (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  BYTE (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
      SUBROUTINE CAT1_ECNLC (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLC
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLC (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  CHARACTER*(*) (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
      SUBROUTINE CAT1_ECNLD (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLD
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLD (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  DOUBLE PRECISION (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
      SUBROUTINE CAT1_ECNLI (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLI
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLI (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  INTEGER (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
      SUBROUTINE CAT1_ECNLL (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLL
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLL (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  LOGICAL (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
      SUBROUTINE CAT1_ECNLR (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLR
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLR (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  REAL (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
      SUBROUTINE CAT1_ECNLW (NULVAL, NULCHR, STATUS)
*+
*  Name:
*     CAT1_ECNLW
*  Purpose:
*     Encode a null value into a character string.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_ECNLW (NULVAL; NULCHR; STATUS)
*  Description:
*     Encode a null value into a character string.
*  Arguments:
*     NULVAL  =  INTEGER*2 (Given)
*        Null value to be encoded into the string.
*     NULCHR  =  CHARACTER*(*) (Returned)
*        Character string into which the null value is to be
*        encoded.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Equivalence a local character string with a local buffer of the
*     same type as the input value.
*     Copy the input null value to the local buffer.
*     Copy the equivalenced string to the return string.
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

         LVAL = NULVAL
         NULCHR = BUFFER

      END IF

      END
