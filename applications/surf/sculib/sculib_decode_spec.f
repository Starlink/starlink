      SUBROUTINE SCULIB_DECODE_SPEC (SPEC, DEMOD_POINTER,
     :     N_SWITCHES,N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :     N_POS, N_BOLS, SWITCH_EXPECTED, POS_SELECTED, POS_S,
     :     SWITCH_S, EXP_S, INT_S, MEAS_S, BOL_S, STATUS)
*+
*  Name:
*     SCULIB_DECODE_SPEC

*  Purpose:
*     Decode SCUBA-style data specifications

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_DECODE_SPEC (SPEC, DEMOD_POINTER,
*    :     N_SWITCHES,N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
*    :     N_POS, N_BOLS, SWITCH_EXPECTED, POS_SELECTED, POS_S,
*    :     SWITCH_S, EXP_S, INT_S, MEAS_S, BOL_S, STATUS)

*  Description:
*     This routine decodes a SCUBA-style data specification. The data-spec
*     will be of the form {<component>;<component>;...}, where components
*     are one of the following:-
*
*     - B<index_spec>   - specifying bolometer indices
*     - P<index_spec>   -            position indices
*     - S<index_spec>   -            switch indices
*     - E<index_spec>   -            exposure indices
*     - I<index_spec>   -            integration indices
*     - M<index_spec>   -            measurement indices
*
*     and the <index_spec> is a list like, for example, 2,5:7,17 to select
*     indices 2, 5 through 7 and 17. Alternatively, <index_spec> can be *
*     which will select all data in that component coordinate.
*     The data spec can be passed in as an array of data specs. The
*     number is specified by N_SPEC.
*     By default all components in a dataset are selected. Thus the
*     empty data-spec {} will return all components selected. Example
*     data-specs are:-
*
*      - "{}               -         select all data
*      - "{B7,12;P57}"     -          select data for bolometers 7 and 12 at
*                                 measurement position 57
*      - "{S2;E1;I3:M2}"   -          select data for all bolometers in
*                                 switch 2 of exposure 1 of integration 3
*                                 in measurement 2 of the observation
*      - "{B29}"           -          select all data for bolometer 29
*      - "{B29;E1}"        -          select data for bolometer 29 in the
*                                 first exposure of each integration
*
*     The data-spec is case-insensitive and blanks are ignored.
*
*     Errors will occur:-
*
*      - If you attempt to select indices outside the dimensions input to
*       the routine.
*      - If you attempt to select data both by position Pxxx and by switch
*       Sxxx, exposure Exxx, integration Ixxx or measurement Mxxx.
*      - If you attempt to select by switch Sxxx when the SWITCH_EXPECTED
*       flag is input .FALSE.
*
*     Output consists of a flag POS_SELECTED, to say whether or not the
*     data were selected by position, and mask arrays that are set
*     to 1 at the coordinate of selected data and 0 otherwise. Even if the
*     data were not selected by position the POS_S array, which is the mask
*     for the position coordinate, will be set correctly. Conversely,
*     however, the MEAS_S, INT_S, EXP_S and SWITCH_S arrays, which are
*     masks for the measurement, integration, exposure and switch will not
*     be set to sensible values if the data are position selected.

*  Arguments:
*     N_SPEC                           = INTEGER (Given)
*       Number of specifications supplied in SPEC
*     SPEC( N_SPEC )                   = CHARACTER*(*) (Given)
*       the specification to be decoded
*     DEMOD_POINTER (N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,  N_MEASUREMENTS)
*     = INTEGER (Given)
*       the pointer to the location in the main data array of the data
*       for each switch of the observation
*     N_SWITCHES                       = INTEGER (Given)
*       the number of switches per exposure
*     N_EXPOSURES                      = INTEGER (Given)
*       the number of exposures per integration
*     N_INTEGRATIONS                   = INTEGER (Given)
*       the number of integrations per measurement
*     N_MEASUREMENTS                   = INTEGER (Given)
*       the number of measurements in the observation
*     N_POS                            = INTEGER (Given)
*       the number of positions measured in the observation
*     N_BOLS                           = INTEGER (Given)
*       the number of bolometers measured in the observation
*     SWITCH_EXPECTED                  = LOGICAL (Given)
*       .TRUE. if a switch component is allowed in the data-spec
*     POS_SELECTED                     = LOGICAL (Returned)
*       .TRUE. if the P component is used in the data-spec
*     POS_S (N_POS)                    = INTEGER (Returned)
*       the position mask array; 1 for selected positions, 0 otherwise
*     SWITCH_S (N_SWITCHES)            = INTEGER (Returned)
*       the switch mask array; 1 for selected switches, 0 otherwise
*     EXP_S (N_EXPOSURES)              = INTEGER (Returned)
*       the exposure mask array; 1 for selected exposures, 0 otherwise
*     INT_S (N_INTEGRATIONS)           = INTEGER (Returned)
*       the integration mask array; 1 for selected integrations, 0
*       otherwise
*     MEAS_S (N_MEASUREMENTS)          = INTEGER (Returned)
*       the measurement mask array; 1 for selected measurements, 0
*       otherwise
*     BOL_S (N_BOLS)                   = INTEGER (Returned)
*       the bolometer mask array; 1 for selected bolometers, 0 otherwise
*     STATUS                           = INTEGER (Given and Returned)
*       The global status.

*  Authors:
*     John Lightfoot (jfl@roe.ac.uk)
*     Tim Jenness (t.jenness@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.7  1999/08/19 03:37:07  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.6  1999/08/03 19:34:55  timj
*     Add copyright message to header.
*     Convert old header style to new.
*
*     Revision 1.5  1999/07/29 23:44:25  timj
*     Header tidy up.
*
*     Revision 1.4  1997/05/20 23:41:13  timj
*     Forgot to delete var NSPEC
*
*     Revision 1.3  1997/05/20 23:39:46  timj
*     Can't decode more than one SPEC at any one call!!
*     Go back to how it was in v1.1
*
*     Revision 1.2  1997/05/15 21:32:15  timj
*     Loop over a SPEC array rather than just one spec.
*
*     Revision 1.1  1997/04/02 02:42:30  jfl
*     Initial revision
*

*  Bugs:

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! for VAL__BADI

*  Arguments Given:
      CHARACTER*(*) SPEC
      INTEGER       N_SWITCHES
      INTEGER       N_EXPOSURES
      INTEGER       N_INTEGRATIONS
      INTEGER       N_MEASUREMENTS
      INTEGER       DEMOD_POINTER (N_SWITCHES, N_EXPOSURES,
     :     N_INTEGRATIONS, N_MEASUREMENTS)
      INTEGER       N_POS
      INTEGER       N_BOLS
      LOGICAL       SWITCH_EXPECTED

*  Arguments Returned:
      LOGICAL       POS_SELECTED
      INTEGER       POS_S (N_POS)
      INTEGER       SWITCH_S (N_SWITCHES)
      INTEGER       EXP_S (N_EXPOSURES)
      INTEGER       INT_S (N_INTEGRATIONS)
      INTEGER       MEAS_S (N_MEASUREMENTS)
      INTEGER       BOL_S (N_BOLS)

*  Status:
      INTEGER STATUS

*  External routines:

*  Local Constants:
      INTEGER      MAX__COMP    ! max. number of components in
      PARAMETER (MAX__COMP = 8) ! spec

*  Local Variables:
      LOGICAL      BOL_SELECTED ! .TRUE. if bolometers
                                ! selected
      CHARACTER*80 COMPONENT (MAX__COMP) ! components in spec
      CHARACTER*1  COMP_TYPE    ! component type
      CHARACTER*70 COMP_VALUE   ! component value
      INTEGER      DATA_END     ! index of end of data section
      INTEGER      DATA_START   ! index of start of data
                                ! section
      INTEGER      END_COMP     ! index of end of a component
      INTEGER      EX           ! exposure number in DO loop
      LOGICAL      EXP_SELECTED ! .TRUE. if exposure selected
      INTEGER      FIRST        ! index of { at start of spec
      INTEGER      I            ! DO loop index
      INTEGER      IN           ! integration number in DO loop
      LOGICAL      INT_SELECTED ! .TRUE. if integration
                                ! selected
      INTEGER      I_COMP       ! component index in DO loop
      INTEGER      LAST         ! index of } at end of spec
      LOGICAL      LOOPING      ! .TRUE. while looping
      INTEGER      ME           ! measurement number in DO loop
      LOGICAL      MEAS_SELECTED ! .TRUE. if measurement
                                ! selected
      INTEGER      NEXT         ! index of ; in spec
      INTEGER      N_COMP       ! number of components in spec
      INTEGER      START_COMP   ! index of start of a component
      CHARACTER*80 STRING       ! internal copy of SPEC
      INTEGER      SW           ! switch number in DO loop
      LOGICAL      SWITCH_SELECTED ! .TRUE. if switch specified

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     initialise variables

      POS_SELECTED = .FALSE.
      SWITCH_SELECTED = .FALSE.
      EXP_SELECTED = .FALSE.
      INT_SELECTED = .FALSE.
      MEAS_SELECTED = .FALSE.

*     default all data to selected

      DO I = 1, N_BOLS
         BOL_S (I) = 1
      END DO

      DO I = 1, N_SWITCHES
         SWITCH_S (I) = 1
      END DO

      DO I = 1, N_EXPOSURES
         EXP_S (I) = 1
      END DO

      DO I = 1, N_INTEGRATIONS
         INT_S (I) = 1
      END DO

      DO I = 1, N_MEASUREMENTS
         MEAS_S (I) = 1
      END DO

      DO I = 1, N_POS
         POS_S (I) = 1
      END DO

*     Take a copy of the current string and tidy it up a bit
      STRING = SPEC
      CALL CHR_RMBLK (STRING)
      CALL CHR_UCASE (STRING)

*     an empty {} means select everything, so just return here. Otherwise...

      IF (STRING(1:2) .NE. '{}') THEN

*     search for {} delimiters

         FIRST = INDEX (STRING, '{')
         IF (FIRST .EQ. 0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: opening { is '//
     :           'missing', STATUS)
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            LAST = INDEX (STRING, '}')
            IF (LAST .EQ. 0) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: closing '//
     :              '} is missing', STATUS)
            END IF
         END IF

*     now go through string and separate its component specifications

         IF (STATUS .EQ. SAI__OK) THEN
            N_COMP = 0
            START_COMP = FIRST + 1
            LOOPING = .TRUE.

            DO WHILE (LOOPING)

*     look for delimiter

               NEXT = INDEX (STRING, ';')

               IF (NEXT .NE. 0) THEN

*     there was one, so a component ends with the character before it, remove
*     the ; ready for the next pass through the string

                  END_COMP = NEXT - 1
                  STRING (NEXT:NEXT) = ' '
               ELSE

*     no delimiter found, the last } must indicate the end of the component

                  END_COMP = LAST - 1
               END IF

               IF (END_COMP .LT. START_COMP) THEN
                  LOOPING = .FALSE.
                  STATUS = SAI__ERROR
                  CALL MSG_SETC ('S', SPEC)
                  CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: zero '//
     :                 'length component in ^S', STATUS)
               ELSE

*     store the component

                  N_COMP = N_COMP + 1
                  IF (N_COMP .GT. MAX__COMP) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETC ('S', SPEC)
                     CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: '//
     :                    'too many components in ^S', STATUS)
                  ELSE
                     COMPONENT (N_COMP) = STRING(START_COMP:END_COMP)
                  END IF

*     move the start pointer to where the beginning of the next component
*     will be if there is one, then check that we haven't overrun the
*     the end of the string

                  START_COMP = END_COMP + 2
                  IF (START_COMP .GE. LAST) THEN
                     LOOPING = .FALSE.
                  END IF
               END IF

            END DO

         END IF


         IF (STATUS .EQ. SAI__OK) THEN
            IF (N_COMP .EQ. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC ('S', SPEC)
               CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: no '//
     :              'components in ^S', STATUS)
            END IF
         END IF

*     OK, we should now have a set of components

         IF (STATUS .EQ. SAI__OK) THEN
            I_COMP = 0
            LOOPING = .TRUE.

            DO WHILE (LOOPING)
               I_COMP = I_COMP + 1

               IF (I_COMP .GT. N_COMP) THEN
                  LOOPING = .FALSE.
               ELSE

*     get name and value of component

                  COMP_TYPE = COMPONENT (I_COMP) (1:1)
                  COMP_VALUE = COMPONENT (I_COMP) (2:)

                  IF (COMP_TYPE .EQ. 'B') THEN
                     BOL_SELECTED = .TRUE.
                     CALL SCULIB_DECODE_COMPONENT (COMP_VALUE,
     :                    N_BOLS, BOL_S, STATUS)
                  ELSE IF (COMP_TYPE .EQ. 'P') THEN
                     POS_SELECTED = .TRUE.
                     CALL SCULIB_DECODE_COMPONENT (COMP_VALUE,
     :                    N_POS, POS_S, STATUS)
                  ELSE IF (COMP_TYPE .EQ. 'S') THEN
                     IF (SWITCH_EXPECTED) THEN
                        SWITCH_SELECTED = .TRUE.
                        CALL SCULIB_DECODE_COMPONENT (COMP_VALUE,
     :                       N_SWITCHES, SWITCH_S, STATUS)
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETC ('C', COMPONENT(I_COMP))
                        CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: '//
     :                       'switch specified when it should '//
     :                       'not be - ^C', STATUS)
                     END IF
                  ELSE IF (COMP_TYPE .EQ. 'E') THEN
                     EXP_SELECTED = .TRUE.
                     CALL SCULIB_DECODE_COMPONENT (COMP_VALUE,
     :                    N_EXPOSURES, EXP_S, STATUS)
                  ELSE IF (COMP_TYPE .EQ. 'I') THEN
                     INT_SELECTED = .TRUE.
                     CALL SCULIB_DECODE_COMPONENT (COMP_VALUE,
     :                    N_INTEGRATIONS, INT_S, STATUS)
                  ELSE IF (COMP_TYPE .EQ. 'M') THEN
                     MEAS_SELECTED = .TRUE.
                     CALL SCULIB_DECODE_COMPONENT (COMP_VALUE,
     :                    N_MEASUREMENTS, MEAS_S, STATUS)
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETC ('C', COMPONENT(I_COMP))
                     CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: bad '//
     :                    'component type - ^C', STATUS)
                     LOOPING = .FALSE.
                  END IF
               END IF

            END DO

         END IF

*     check the validity of the selection

         IF (STATUS .EQ. SAI__OK) THEN
            IF (POS_SELECTED) THEN
               IF (SWITCH_SELECTED .OR. EXP_SELECTED .OR.
     :              INT_SELECTED .OR. MEAS_SELECTED) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETC ('S', SPEC)
                  CALL ERR_REP (' ', 'SCULIB_DECODE_SPEC: spec '//
     :                 'selects by both P and (S,E,I,M) - ^S',STATUS)
               END IF
            END IF
         END IF

*     if the selection is valid and was done by S, E, I or M instead of
*     P, then set the POS array appropriately

         IF (STATUS .EQ. SAI__OK) THEN
            IF (SWITCH_SELECTED .OR. EXP_SELECTED .OR.
     :           INT_SELECTED .OR. MEAS_SELECTED) THEN

               DO I = 1, N_POS
                  POS_S (I) = 0
               END DO

               DO ME = 1, N_MEASUREMENTS
                  IF (MEAS_S (ME) .EQ. 1) THEN

                     DO IN = 1, N_INTEGRATIONS
                        IF (INT_S (IN) .EQ. 1) THEN

                           DO EX = 1, N_EXPOSURES
                              IF (EXP_S (EX) .EQ. 1) THEN

                                 DO SW = 1, N_SWITCHES
                                    IF (SWITCH_S (SW) .EQ. 1) THEN
                                       CALL SCULIB_FIND_SWITCH (
     :                                      DEMOD_POINTER,N_SWITCHES,
     :                                      N_EXPOSURES,
     :                                      N_INTEGRATIONS,
     :                                      N_MEASUREMENTS, N_POS,
     :                                      SW, EX, IN, ME,
     :                                      DATA_START, DATA_END,
     :                                      STATUS)

                                       IF ((DATA_START.NE.0) .AND.
     :                                      (DATA_START .NE.
     :                                      VAL__BADI)) THEN
                                          DO I = DATA_START, DATA_END
                                             POS_S (I) = 1
                                          END DO
                                       END IF

                                    END IF
                                 END DO

                              END IF
                           END DO

                        END IF
                     END DO

*     aargh!

                  END IF
               END DO

            END IF
         END IF
      END IF

      END
