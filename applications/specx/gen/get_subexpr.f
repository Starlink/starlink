*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE GET_SUBEXPR (STRING, ICH1, ICH2, IERR)

*     Subroutine to search for leftmost left bracket in a string and
*     rightmost right bracket. ICH1 and ICH2 are the positions of the
*     first and last characters in the enclosed string. Note that if
*     the contents of the brackets are omitted then ICH2 will be less
*     than ICH1 on return (ICH2 = ICH1 implies a single enclosed character).
*     If there are no brackets then ?

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER STRING*(*)
      INTEGER*4 ICH1, ICH2
      INTEGER*4 IERR

*     Local variables

      INTEGER*4 ILS
      INTEGER*4 NEST

*     Functions

      INTEGER*4 GEN_ILEN

*     OK...

      IERR = 0

      ICH1 = INDEX (STRING,'(')       ! Returns 0 if string '(' not found
      ICH2 = -1

*     If no '(' found then return immediately

      IF (ICH1.EQ.0) RETURN

*     Loop returns ICH2 = 0 if matching ')' not found, else returns
*     the position of the bracket: Decrement it unconditionally at
*     the end of the loop to give position of end of sub-expression

      NEST = 1
      ICH1 = ICH1 + 1
      ICH2 = ICH1
      ILS  = GEN_ILEN (STRING)

*     At the end of the search the loop is not executed.
*     Thus we should exit with ICH2 = pos'n of ")"

      DO WHILE (ICH2.LE.ILS)
        IF (STRING(ICH2:ICH2).EQ.'(') NEST = NEST + 1
        IF (STRING(ICH2:ICH2).EQ.')') NEST = NEST - 1
        IF (NEST.EQ.0) GO TO 99
        ICH2 = ICH2 + 1
      END DO

   99 ICH2 = ICH2 - 1

      IF (NEST.NE.0) THEN
        PRINT *, '-- get_subexpr --'
        PRINT *, '   Mismatched parentheses: ', STRING(:ILS)
        ICH2 = 0
        IERR = 100
      END IF

      RETURN
      END
