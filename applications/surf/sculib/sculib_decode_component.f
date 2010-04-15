      SUBROUTINE SCULIB_DECODE_COMPONENT (COMPONENT, N, SELECT, STATUS)
*+
*  Name:
*     SCULIB_DECODE_COMPONENT

*  Purpose:
*     This routine decodes the value of a single component in a SCUBA
*     data-spec.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_DECODE_COMPONENT (COMPONENT, N, SELECT, STATUS)

*  Description:
*     This routine decodes the value of a single component in a SCUBA
*     data-spec. It is called by SCULIB_DECODE_SPEC. The component can be:-
*
*     -[1]   "*"                     select all data
*     -[2]   "<index>"               select data at position <index>
*     -[3]   "<index1>,<index2>"     select data at positions <index1> and
*                               <index2>
*     -[4]   "<index1>:<index2>"     select all data in the range <index1> to
*                               <index2>
*
*     or any combination of 2, 3 and 4 separated by commas. Example
*     component values are:-
*
*      - "*"                     select all data
*      - "1,5"                   select data at indices 1 and 5
*      - "5:10,16"               select data at indices 5 through 10 and 16
*
*     Errors will occur if:-
*
*       - You mix the * format with selection by index.
*       - Any index selected lies outside the range 1 to N.
*       - In a selection range <index2> is less than <index1>.
*       - The component does not conform to the design syntax.
*
*     Output from the routine is in the form of a mask array SELECT.
*     Data indices selected will be marked as 1 in SELECT, while indices
*     not selected will be zeroes.

*  Arguments:
*     COMPONENT                        = CHARACTER*(*) (Given)
*        the component  to be decoded
*     N                                = INTEGER (Given)
*        the number of items that can be selected
*     SELECT (N)                       = INTEGER (Returned)
*        the items selected; 1 for selected, 0 for not selected
*     STATUS                           = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.4  1999/08/19 03:37:06  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.3  1999/08/03 19:34:54  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.2  1999/07/30 19:58:14  timj
*     Minor header tweaks.
*
*     Revision 1.1  1997/04/02 03:07:37  jfl
*     Initial revision
*

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE                        ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                    ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) COMPONENT
      INTEGER       N

*  Arguments Returned:
      INTEGER       SELECT (N)

*  Status:
      INTEGER STATUS

*  External routines:
      INTEGER CHR_LEN                      ! CHR used-string-length
                                           ! function

*  Local Constants:

*  Local Variables:
      CHARACTER*80 ATOM                  ! section of COMPONENT
      INTEGER      COLON                 ! index of : in string
      INTEGER      COMMA                 ! index of , in string
      INTEGER      I                     ! DO loop index
      INTEGER      I1                    ! lower limit to range
      INTEGER      I2                    ! upper limit to range
      INTEGER      LEN                   ! used-length of COMPONENT
      LOGICAL      LOOPING               ! .TRUE. while looping
      CHARACTER*80 NEUTRON               ! second index in ATOM
      CHARACTER*80 PROTON                ! first index in ATOM
      CHARACTER*80 STRING                ! copy of COMPONENT

*  local data
*.

      IF (STATUS .NE. SAI__OK) RETURN

*  initialise output array

      DO I = 1, N
         SELECT (I) = 0
      END DO

*  work on a copy of the component

      STRING = COMPONENT
      LEN = CHR_LEN (STRING)
      IF (LEN .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_DECODE_COMPONENT: zero length '//
     :     'component value', STATUS)
      END IF

*  check first for all items selected

      IF (STATUS .EQ. SAI__OK) THEN

         IF (STRING(:MAX(1,LEN)) .EQ. '*') THEN
            DO I = 1, N
               SELECT (I) = 1
            END DO
         ELSE

*  loop through the component

            LOOPING = .TRUE.

            DO WHILE (LOOPING)

*  look for a ,

               COMMA = INDEX (STRING,',')

               IF (COMMA .EQ. 0) THEN

*  the component only contains one atom

                  ATOM = STRING
                  LOOPING = .FALSE.
               ELSE IF (COMMA .EQ. 1) THEN
                  LOOPING = .FALSE.
                  STATUS = SAI__ERROR
                  CALL MSG_SETC ('C', COMPONENT)
                  CALL ERR_REP (' ', 'SCULIB_DECODE_COMPONENT: zero '//
     :              'length sub-component in ^C', STATUS)
               ELSE

*  copy the atom and blank this section of the component ready
*  for the next turn around the loop

                  ATOM = STRING (:COMMA-1)
                  CALL CHR_RMBLK (ATOM)
                  CALL CHR_FILL (' ', STRING(:COMMA))
               END IF

*  analyse the atom

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL CHR_RMBLK (ATOM)

*  look for a :

                  COLON = INDEX (ATOM,':')

                  IF (COLON .EQ. 0) THEN

*  a single number, try to decode it

                     CALL CHR_CTOI (ATOM, I, STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        LOOPING = .FALSE.
                        STATUS = SAI__ERROR
                        CALL MSG_SETC ('A', ATOM)
                        CALL MSG_SETC ('C', COMPONENT)
                        CALL ERR_REP (' ', 'SCULIB_DECODE_COMPONENT: '//
     :                    'error decoding ^A in component ^C',
     :                    STATUS)
                     ELSE

*  if the number is within range, set the SELECT array

                        IF ((I .LT. 1) .OR. (I .GT. N)) THEN
                           LOOPING = .FALSE.
                           STATUS = SAI__ERROR
                           CALL MSG_SETI ('I', I)
                           CALL MSG_SETI ('N', N)
                           CALL MSG_SETC ('C', COMPONENT)
                           CALL ERR_REP (' ',
     :                       'SCULIB_DECODE_COMPONENT: number ^I '//
     :                       'in component ^C is outside allowed '//
     :                       'range 1-^N', STATUS)
                        ELSE
                           SELECT (I) = 1
                        END IF
                     END IF

                  ELSE IF (COLON .EQ. 1) THEN

                     LOOPING = .FALSE.
                     STATUS = SAI__ERROR
                     CALL MSG_SETC ('C', COMPONENT)
                     CALL ERR_REP (' ', 'SCULIB_DECODE_COMPONENT: '//
     :                 'invalid range in component ^C', STATUS)

                  ELSE

*  a range of numbers, split the atom and try to decode them

                     PROTON = ATOM (:COLON-1)
                     CALL CHR_RMBLK (PROTON)
                     NEUTRON = ATOM (COLON+1:)
                     CALL CHR_RMBLK (NEUTRON)

                     CALL CHR_CTOI (PROTON, I1, STATUS)
                     IF (STATUS .NE. SAI__OK) THEN
                        LOOPING = .FALSE.
                        STATUS = SAI__ERROR
                        CALL MSG_SETC ('A', PROTON)
                        CALL MSG_SETC ('C', COMPONENT)
                        CALL ERR_REP (' ',
     :                    'SCULIB_DECODE_COMPONENT: '//
     :                    'error decoding ^A in component ^C',
     :                    STATUS)
                     ELSE
                        CALL CHR_CTOI (NEUTRON, I2, STATUS)
                        IF (STATUS .NE. SAI__OK) THEN
                           LOOPING = .FALSE.
                           STATUS = SAI__ERROR
                           CALL MSG_SETC ('A', NEUTRON)
                           CALL MSG_SETC ('C', COMPONENT)
                           CALL ERR_REP (' ',
     :                       'SCULIB_DECODE_COMPONENT: '//
     :                       'error decoding ^A in component ^C',
     :                        STATUS)
                        ELSE

*  if the numbers are within range, set the SELECT array

                           IF ((I1.LT.1) .OR. (I1.GT.N)) THEN
                              LOOPING = .FALSE.
                              STATUS = SAI__ERROR
                              CALL MSG_SETI ('I', I1)
                              CALL MSG_SETI ('N', N)
                              CALL MSG_SETC ('C', COMPONENT)
                              CALL ERR_REP (' ',
     :                          'SCULIB_DECODE_COMPONENT: number '//
     :                          '^I in component ^C is outside '//
     :                          'allowed range 1 to ^N', STATUS)
                           ELSE IF ((I2.LT.1) .OR. (I2.GT.N)) THEN
                              LOOPING = .FALSE.
                              STATUS = SAI__ERROR
                              CALL MSG_SETI ('I', I2)
                              CALL MSG_SETI ('N', N)
                              CALL MSG_SETC ('C', COMPONENT)
                              CALL ERR_REP (' ',
     :                          'SCULIB_DECODE_COMPONENT: number '//
     :                          '^I in component ^C is outside '//
     :                          'allowed range 1-^N', STATUS)
                           ELSE IF (I2 .LT. I1) THEN
                              LOOPING = .FALSE.
                              STATUS = SAI__ERROR
                              CALL MSG_SETC ('C', COMPONENT)
                              CALL ERR_REP (' ',
     :                          'SCULIB_DECODE_COMPONENT: range '//
     :                          'reversed in component ^C', STATUS)
                           ELSE

                              DO I = I1, I2
                                 SELECT (I) = 1
                              END DO

                           END IF
                        END IF
                     END IF
                  END IF
               END IF

            END DO

         END IF
      END IF

      END
