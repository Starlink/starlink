*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*        Unused LS
*-----------------------------------------------------------------------

      SUBROUTINE gen_decode (intype, instring, retval, istat)

*  Read value of type INTYPE from string INSTRING and return to RETVAL.

      IMPLICIT   NONE

*     Formal parameters:

      CHARACTER  intype*(*)
      CHARACTER  instring*(*)
      INTEGER    retval
      INTEGER    istat

*     Local variables:

      INTEGER   ierr
      INTEGER   idig, fdig, edig
      INTEGER   lf, ll, lt
      INTEGER   nb
      INTEGER   nbytes
      CHARACTER type*4
      CHARACTER form*8

      LOGICAL   lvalue
      INTEGER   ivalue
      REAL*4    rvalue
      REAL*8    dvalue
      EQUIVALENCE (lvalue, ivalue, rvalue, dvalue)

*     Functions

      LOGICAL   gen_digits
      LOGICAL   gen_floating
      LOGICAL   gen_dformat
      LOGICAL   gen_eformat

      INTEGER   gen_ilen
      INTEGER   gen_readnum

*  Make sure routine "falls through" on an error

      IF (istat.ne.0) RETURN

*  Ok, go..

CD    Print *, '-- gen_decode --'

*     Either get the value directly (numeric string) or translate symbol.
*     If result not delayed then value is returned into workspace array
*     at next available location. Otherwise - if for example we need to
*     evaluate the array index for the symbol first - enter symbol on the
*     stack, and the "evaluate array index" operator (@) on the operator stack.

      lt = gen_ilen (intype)
      CALL uucase (intype(1:lt))
      READ (intype(2:lt), '(I3)', IOSTAT=ierr, ERR=99) nbytes
      IF (intype(:1).ne.'C') THEN

        lf = LEN (form)

        IF (gen_digits (instring, idig)) THEN
          type   = 'I4'
          form   = 'I'
          WRITE (form(2:lf), '(I2.2)') idig
          ierr = gen_readnum (instring, type, form, ivalue)
CD        Print *,'     integer value read'
        ELSE IF (gen_floating (instring, idig, fdig)) THEN
          type   = 'R4'
          form   = 'F'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+1,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (instring, type, form, rvalue)
CD        Print *,'     real*4 value read'
        ELSE IF (gen_eformat (instring, idig, fdig, edig)) THEN
          type   = 'R4'
          form   = 'E'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+edig+2,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (instring, type, form, rvalue)
CD        Print *,'     real*4 value read'
        ELSE IF (gen_dformat (instring, idig, fdig, edig)) THEN
          type   = 'R8'
          form   = 'E'
          WRITE (form(2:lf), '(I2.2,''.'',I3.3)') idig+fdig+edig+2,
     &                                             MAX (0, fdig)
          ierr = gen_readnum (instring, type, form, dvalue)
CD        Print *,'     real*8 value read'
        ELSE IF (intype(:1) .eq. 'L') THEN
          type   = 'L4'
          form   = 'L'
          ll     = gen_ilen (instring)
CD        Print *,'     length of input string = ', ll
          WRITE (form(2:lf), '(I2.2)') ll
CD        Print *,'     reading ',instring(:24),' with form = ', form
          ierr = gen_readnum (instring, type, form, lvalue)
CD        Print *,'     logical value read'

        END IF

        lt = gen_ilen (type)
        READ (type(2:lt), '(I3)', IOSTAT=ierr, ERR=99) nb
        CALL gen_cvt_type (ivalue, type,   nb,
     &                     retval, intype, nbytes, istat)

*     Or... Read character string

      ELSE IF (intype(:1) .eq. 'C') THEN
        CALL xcopy (nbytes, %REF(instring), retval)

      END IF
      RETURN

*     Error handling

   99 CONTINUE
      IF (ierr.ne.0) THEN
        CALL GEN_ERMSG (ierr)
        istat = 1
      END IF
      RETURN

      END

*----------------------------------------------------------------------

