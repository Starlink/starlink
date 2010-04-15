*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     28 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Type disagreement - assume logical and integer are interchangeable
*        Unused ERROR1, ERROR2
*-----------------------------------------------------------------------

      SUBROUTINE gen_cvt_type (input,  intype,  nb_in,
     &                         output, outtype, nb_out, ierr)

*  Routine to convert between any two of the GEN/SCL numeric types
*  (I4, R4, R8)

*     Formal parameters

      INTEGER*4 input
      INTEGER*4 output
      CHARACTER intype*(*)
      CHARACTER outtype*(*)
      INTEGER*4 nb_in
      INTEGER*4 nb_out
      INTEGER*4 ierr          ! 0, ok;  1, unknown intype;  2, unknown outtype

*     Local variables

      CHARACTER null_str*128
      PARAMETER (null_str = ' ')

      CHARACTER type1*4, type2*4

      LOGICAL*4 logical
      INTEGER*4 integer
      REAL*4    real
      REAL*8    double

      EQUIVALENCE (logical, integer, real, double)

*  Ok, go..

      IF (ierr.ne.0) RETURN

      type1 = intype
      CALL uucase (type1)
      type2 = outtype
      CALL uucase (type2)

CD    PRINT *, 'converting from ',type1,' to ',type2

      IF (type1.eq.type2) THEN
CD      PRINT *, 'No conversion necessary - copy', nb_in, ' bytes'
        CALL xcopy (nb_in, input, output)

      ELSE IF (type1(1:1).eq.'C' .AND. type2(1:1).eq.'C') THEN
CD      PRINT *, 'String types - copy', MIN(nb_in,nb_out), ' bytes'
        CALL xcopy (nb_out, %REF(null_str), output)
        CALL xcopy (MIN(nb_in,nb_out), input, output)

      ELSE

        CALL xcopy (nb_in, input, logical)
CD      PRINT *, 'Conversion IS necessary'

        IF (type1 .eq. 'L4') THEN
CD        PRINT *,'Input type is L4'
!          IF (type2 .eq. 'I4') THEN
!            integer = logical
!          ELSE IF (type2 .eq. 'R4') THEN
          IF (type2 .eq. 'R4') THEN
             real    = integer
          ELSE IF (type2 .eq. 'R8') THEN
             double  = integer
          ELSE
             ierr = 2
          END IF

        ELSE IF (type1 .eq. 'I4') THEN
CD        PRINT *,'Input type is I4'
!          IF (type2 .eq. 'L4') THEN
!            logical = integer
!          ELSE IF (type2 .eq. 'R4') THEN
          IF (type2 .eq. 'R4') THEN
CD          PRINT *,'Output type is R4'
            real    = integer
          ELSE IF (type2 .eq. 'R8') THEN
            double  = integer
          ELSE
            ierr = 2
          END IF

        ELSE IF (type1 .eq. 'R4') THEN
CD        PRINT *,'Input type is R4'
!          IF (type2 .eq. 'L4') THEN
!            logical = real
!          ELSE IF (type2 .eq. 'I4') THEN
          IF (type2 .eq. 'I4') THEN
            integer = real
          ELSE IF (type2 .eq. 'R8') THEN
            double  = real
          ELSE
            ierr = 2
          END IF

        ELSE IF (type1 .eq. 'R8') THEN
CD        PRINT *,'Input type is R8'
!          IF (type2 .eq. 'L4') THEN
!            logical = double
!          ELSE IF (type2 .eq. 'I4') THEN
          IF (type2 .eq. 'I4') THEN
            integer = double
          ELSE IF (type2 .eq. 'R4') THEN
            real    = double
          ELSE
            ierr = 2
          END IF

        ELSE
          ierr = 1
        END IF

        IF (ierr.eq.0) THEN
          CALL xcopy (nb_out, logical, output)
        ELSE
          PRINT *, '-- gen_cvt_type --'
          PRINT *, '   one or both types unknown'
        END IF

      END IF

      RETURN
      END
