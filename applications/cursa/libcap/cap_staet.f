      SUBROUTINE CAP_STAEC (SIZE, ELEM, VALUE, ARRAY, STATUS)
*+
*  Name:
*     CAP_STAEC
*  Purpose:
*     Set a specified element in an array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_STAEC (SIZE, ELEM, VALUE; ARRAY; STATUS)
*  Description:
*     Set a specified element in an array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Number of elements in the array.
*     ELEM  =  INTEGER (Given)
*        Array element to be set.
*     VALUE  =  CHARACTER*(*) (Given)
*        Value to which the array element is to be set.
*     ARRAY(SIZE)  =  CHARACTER*(*) (Given and Returned)
*        Array of values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the element is inside the array then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version (from CAT5_STAEC).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  SIZE,
     :  ELEM
      CHARACTER*(*)
     :  VALUE
*  Arguments Given and Returned:
      CHARACTER*(*)
     :  ARRAY(SIZE)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

         IF (ELEM .GT. 0  .AND.  ELEM .LE. SIZE) THEN
            ARRAY(ELEM) = VALUE

         ELSE
            STATUS = SAI__ERROR

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: array index ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL ERR_REP ('CAP_STAEC_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_STAED (SIZE, ELEM, VALUE, ARRAY, STATUS)
*+
*  Name:
*     CAP_STAED
*  Purpose:
*     Set a specified element in an array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_STAED (SIZE, ELEM, VALUE; ARRAY; STATUS)
*  Description:
*     Set a specified element in an array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Number of elements in the array.
*     ELEM  =  INTEGER (Given)
*        Array element to be set.
*     VALUE  =  DOUBLE PRECISION (Given)
*        Value to which the array element is to be set.
*     ARRAY(SIZE)  =  DOUBLE PRECISION (Given and Returned)
*        Array of values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the element is inside the array then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version (from CAT5_STAED).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  SIZE,
     :  ELEM
      DOUBLE PRECISION
     :  VALUE
*  Arguments Given and Returned:
      DOUBLE PRECISION
     :  ARRAY(SIZE)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

         IF (ELEM .GT. 0  .AND.  ELEM .LE. SIZE) THEN
            ARRAY(ELEM) = VALUE

         ELSE
            STATUS = SAI__ERROR

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: array index ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL ERR_REP ('CAP_STAED_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_STAEI (SIZE, ELEM, VALUE, ARRAY, STATUS)
*+
*  Name:
*     CAP_STAEI
*  Purpose:
*     Set a specified element in an array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_STAEI (SIZE, ELEM, VALUE; ARRAY; STATUS)
*  Description:
*     Set a specified element in an array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Number of elements in the array.
*     ELEM  =  INTEGER (Given)
*        Array element to be set.
*     VALUE  =  INTEGER (Given)
*        Value to which the array element is to be set.
*     ARRAY(SIZE)  =  INTEGER (Given and Returned)
*        Array of values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the element is inside the array then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version (from CAT5_STAEI).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  SIZE,
     :  ELEM
      INTEGER
     :  VALUE
*  Arguments Given and Returned:
      INTEGER
     :  ARRAY(SIZE)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

         IF (ELEM .GT. 0  .AND.  ELEM .LE. SIZE) THEN
            ARRAY(ELEM) = VALUE

         ELSE
            STATUS = SAI__ERROR

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: array index ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL ERR_REP ('CAP_STAEI_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_STAEL (SIZE, ELEM, VALUE, ARRAY, STATUS)
*+
*  Name:
*     CAP_STAEL
*  Purpose:
*     Set a specified element in an array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_STAEL (SIZE, ELEM, VALUE; ARRAY; STATUS)
*  Description:
*     Set a specified element in an array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Number of elements in the array.
*     ELEM  =  INTEGER (Given)
*        Array element to be set.
*     VALUE  =  LOGICAL (Given)
*        Value to which the array element is to be set.
*     ARRAY(SIZE)  =  LOGICAL (Given and Returned)
*        Array of values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the element is inside the array then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version (from CAT5_STAEL).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  SIZE,
     :  ELEM
      LOGICAL
     :  VALUE
*  Arguments Given and Returned:
      LOGICAL
     :  ARRAY(SIZE)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

         IF (ELEM .GT. 0  .AND.  ELEM .LE. SIZE) THEN
            ARRAY(ELEM) = VALUE

         ELSE
            STATUS = SAI__ERROR

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: array index ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL ERR_REP ('CAP_STAEL_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
      SUBROUTINE CAP_STAER (SIZE, ELEM, VALUE, ARRAY, STATUS)
*+
*  Name:
*     CAP_STAER
*  Purpose:
*     Set a specified element in an array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_STAER (SIZE, ELEM, VALUE; ARRAY; STATUS)
*  Description:
*     Set a specified element in an array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Number of elements in the array.
*     ELEM  =  INTEGER (Given)
*        Array element to be set.
*     VALUE  =  REAL (Given)
*        Value to which the array element is to be set.
*     ARRAY(SIZE)  =  REAL (Given and Returned)
*        Array of values.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the element is inside the array then
*       Set the value.
*     else
*       Set the status.
*       Report an error.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/6/99 (ACD): Original version (from CAT5_STAER).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
*  Arguments Given:
      INTEGER
     :  SIZE,
     :  ELEM
      REAL
     :  VALUE
*  Arguments Given and Returned:
      REAL
     :  ARRAY(SIZE)
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  ERRMSG*75   ! Text of error message.
      INTEGER
     :  ERRLEN      ! Length of ERRMSG (excl. trail. blanks).
*.

      IF (STATUS .EQ. SAI__OK) THEN

         IF (ELEM .GT. 0  .AND.  ELEM .LE. SIZE) THEN
            ARRAY(ELEM) = VALUE

         ELSE
            STATUS = SAI__ERROR

            ERRMSG = ' '
            ERRLEN = 0

            CALL CHR_PUTC ('Error: array index ', ERRMSG, ERRLEN)
            CALL CHR_PUTI (ELEM, ERRMSG, ERRLEN)
            CALL CHR_PUTC (' is out of range.', ERRMSG, ERRLEN)

            CALL ERR_REP ('CAP_STAER_ERR', ERRMSG(1 : ERRLEN),
     :        STATUS)
         END IF

      END IF

      END
