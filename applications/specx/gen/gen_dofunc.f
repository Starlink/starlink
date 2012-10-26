*  History:
*     31 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*        Unused FUNCTION
*-----------------------------------------------------------------------

      SUBROUTINE gen_dofunc (opnd_addr1, opnd_type1,
     &                       opnd_addr2, opnd_type2, fnc_index, ierr)

      IMPLICIT  none
      INCLUDE 'CNF_PAR'

*     Formal parameters

      INTEGER*4 opnd_addr1
      CHARACTER opnd_type1*(*)
      INTEGER*4 opnd_addr2
      CHARACTER opnd_type2*(*)
      INTEGER*4 fnc_index
      INTEGER*4 ierr

*     Local variables

      LOGICAL   positive, negative, zero, exceeds1
      INTEGER*4 nbytes
      INTEGER*4 iresult
      REAL*4    arg
      REAL*4    result
CD     CHARACTER function*4

      EQUIVALENCE (result, iresult)

*     Functions:

      INTEGER*4 gen_ilen

*  ok, go..

      ierr = 0

*     First get the argument in REAL type

      READ (opnd_type2(2:gen_ilen(opnd_type2)), '(I3)') nbytes
      CALL gen_cvt_type (%val(cnf_pval(opnd_addr2)), opnd_type2, nbytes,
     &                    arg,            'R4',        4,       ierr)

*     Evaluate function

      exceeds1 = (arg.gt.1.0 .or. arg.lt.-1.0)
      positive = (arg.gt.0.0)
      negative = (arg.lt.0.0)
      zero     = (arg.eq.0.0)

CD    PRINT *, '-- gen_dofunc --'
CD    PRINT *, '   evaluation of function ', function

      IF (fnc_index .eq. 1) THEN
        IF (positive) THEN
          result = SQRT (arg)
        ELSE
          ierr = 7
        END IF

      ELSE IF (fnc_index .eq. 2) THEN
        result = EXP (arg)

      ELSE IF (fnc_index .eq. 3) THEN
        IF (positive) THEN
          result = ALOG (arg)
        ELSE
          ierr = 7
        END IF

      ELSE IF (fnc_index .eq. 4) THEN
        IF (positive) THEN
          result = ALOG10 (arg)
        ELSE
          ierr = 7
        END IF

      ELSE IF (fnc_index .eq. 5) THEN
        result = ABS (arg)

      ELSE IF (fnc_index .eq. 6) THEN
        result = SIN (arg)

      ELSE IF (fnc_index .eq. 7) THEN
        result = COS (arg)

      ELSE IF (fnc_index .eq. 8) THEN
        result = TAN (arg)

      ELSE IF (fnc_index .eq. 9) THEN
        IF (exceeds1) THEN
          ierr = 7
        ELSE
          result = ASIN (arg)
        END IF

      ELSE IF (fnc_index .eq. 10) THEN
        IF (exceeds1) THEN
          ierr = 7
        ELSE
          result = ACOS (arg)
        END IF

      ELSE IF (fnc_index .eq. 11) THEN
        result = ATAN (arg)

      ELSE IF (fnc_index .eq. 12) THEN
        IF (ABS(arg) .lt. 2147483648.0) THEN
          iresult = NINT (arg)
        ELSE
          ierr = 7
        END IF

      ELSE IF (fnc_index .eq. 13) THEN
        result = arg

      END IF

      IF (ierr.ne.0) GO TO 99

*     transfer result to area of memory reserved for it

      CALL xcopy (4, result, %val(cnf_pval(opnd_addr1)))
      RETURN

*     error return

   99 PRINT *, '-- gen_dofunc --'
      PRINT *, '   argument not in domain of function'

      RETURN
      END
