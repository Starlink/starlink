      SUBROUTINE CAT1_CRTAR (SIZE, TYPE, PTR, STATUS)
*+
*  Name:
*     CAT1_CRTAR
*  Purpose:
*     Wrap-around to PSX routine for creating a dynamic array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_CRTAR (SIZE, TYPE; PTR; STATUS)
*  Description:
*     Wrap-around to PSX routine for creating a dynamic array.
*  Arguments:
*     SIZE  =  INTEGER (Given)
*        Size of the array (the number of elements).
*     TYPE  =  CHARACTER*(*) (Given)
*        The data type of the array.
*     PTR  =  INTEGER (Returned)
*        Pointer to the array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Call the appropriate PSX routine.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/4/94 (ACD): Original version.
*     15/4/94 (ACD): First working version.
*     16/7/96 (ACD): Added support for the BYTE, WORD and CHAR*n data
*        types.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'      ! External CAT constants.
      INCLUDE 'CAT_ERR'      ! CAT error codes.
*  Arguments Given:
      INTEGER
     :  SIZE
      CHARACTER
     :  TYPE*(*)
*  Arguments Returned:
      INTEGER
     :  PTR
*  Status:
      INTEGER STATUS         ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      CHARACTER
     :  WRKTYP*(CAT__SZTYP), ! Copy of TYPE (which may be modified).
     :  ERRMSG*75            ! Text of error message.
      INTEGER
     :  CSIZE,   ! Size of character string.
     :  RSIZE,   ! Number of bytes in a character array.
     :  LSTAT,   ! Local status decoding an integer.
     :  ERRLEN,  ! Length of ERRMSG (excl. trail. blanks).
     :  LTYPE    !   "    "  TYPE   ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. CAT__OK) THEN

         WRKTYP = TYPE

         CALL CHR_UCASE (WRKTYP)
         CALL CHR_LDBLK (WRKTYP)

         IF (WRKTYP .EQ. '_BYTE') THEN
            CALL PSX_CALLOC (SIZE, '_INTEGER', PTR, STATUS)

         ELSE IF (WRKTYP .EQ. '_WORD') THEN
            CALL PSX_CALLOC (SIZE, '_INTEGER', PTR, STATUS)

         ELSE IF (WRKTYP(1 : 6) .EQ. '_CHAR*') THEN
            WRKTYP(1 : 6) = '      '

            LSTAT = CAT__OK
            CALL CHR_CTOI (WRKTYP, CSIZE, LSTAT)

            IF (LSTAT .EQ. CAT__OK) THEN
               RSIZE = SIZE * CSIZE
               CALL PSX_CALLOC (RSIZE, '_INTEGER', PTR, STATUS)
            ELSE
               STATUS = CAT__INVDT

               ERRMSG = ' '
               ERRLEN = 0

               CALL CHR_PUTC ('Unknown data type: ', ERRMSG, ERRLEN)
               LTYPE = CHR_LEN(TYPE)
               CALL CHR_PUTC (TYPE(1 : LTYPE), ERRMSG, ERRLEN)
               CALL CHR_PUTC ('.', ERRMSG, ERRLEN)

               CALL CAT1_ERREP ('CAT1_CRTAR_IDT', ERRMSG(1 : ERRLEN),
     :           STATUS)
            END IF

         ELSE
            CALL PSX_CALLOC (SIZE, TYPE, PTR, STATUS)

         END IF

      END IF

      END
