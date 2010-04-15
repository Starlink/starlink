      LOGICAL FUNCTION PON_LSELECT( ICOND, CHARSEL, CHARVAL, VALUE,
     :                              SELVAL, SELVAL2, NLIST, LIST )
*+
*  Name:
*     PON_LSELECT

*  Purpose:
*     Select routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PON_LSELECT( ICOND, CHARSEL, CHARVAL, VALUE,
*    :                      SELVAL, SELVAL2, NLIST, LIST )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {argument_data_type} ({argument_access_mode})
*        {argument_description}
*     [argument_spec]...
*     [status_argument_spec]

*  Returned Value:
*     PON_LSELECT = LOGICAL
*        Whether selected.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     19-JUN-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     6-JUN-1994 (PDRAPER):
*        Now uses CHR_LEN to get string lengths.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER ICOND

      CHARACTER CHARSEL * ( * )
      CHARACTER CHARVAL * ( * )

      REAL VALUE
      REAL SELVAL
      REAL SELVAL2

      INTEGER NLIST

      CHARACTER LIST( NLIST ) * ( * )


*  External Definitions:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! String length

*  Local Variables:
      INTEGER I                  ! Loop index

*.

      IF ( ICOND.EQ.0 ) THEN
         PON_LSELECT = .TRUE.
      ELSE
         PON_LSELECT = .FALSE.

         IF ( ICOND.EQ.2 ) THEN

*        Select not equal to value.
            IF ( VALUE.NE.SELVAL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.3 ) THEN

*        Select greater than value.
            IF ( VALUE.GT.SELVAL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.4 ) THEN

*        Select less than value.
            IF ( VALUE.LT.SELVAL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.5 ) THEN

*        Select equal to string.
            IF ( CHARVAL.EQ.CHARSEL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.6 ) THEN

*        Select no equal to string.
            IF ( CHARVAL.NE.CHARSEL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.7 ) THEN

*        Select valuse in range.
            IF ( VALUE.GE.SELVAL
     :           .AND. VALUE.LT.SELVAL2 ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.8 ) THEN

*        Select values from list.
            I = 1

            DO WHILE ( .NOT.PON_LSELECT .AND. ( I.LE.NLIST ) )
               PON_LSELECT = ( LIST( I ).EQ.CHARVAL )
               I = I + 1
            END DO
         ELSE IF ( ICOND.EQ.9 ) THEN

*        Select if substring found in string.
            IF ( INDEX( CHARVAL, CHARSEL(: CHR_LEN( CHARSEL ))).NE.0 )
     :         PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.10 ) THEN

*        Select on greater than the absolute value.
            IF ( ABS( VALUE ).GT.SELVAL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.11 ) THEN

*        Select on less than the absolute value.
            IF ( ABS( VALUE ).LT.SELVAL ) PON_LSELECT = .TRUE.
         ELSE IF ( ICOND.EQ.12 ) THEN

*        Select by excluding values.
            I = 1
            PON_LSELECT = .TRUE.

            DO WHILE ( PON_LSELECT .AND. ( I.LE.NLIST ) )
               PON_LSELECT = ( LIST( I ).NE.CHARVAL )
               I = I + 1
            END DO
         ELSE

*        Select equal to value.
            IF ( VALUE.EQ.SELVAL ) PON_LSELECT = .TRUE.
         END IF
      END IF

      END
* $Id$
