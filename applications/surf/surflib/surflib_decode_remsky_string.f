      SUBROUTINE SURFLIB_DECODE_REMSKY_STRING( SUB_INSTRUMENT, 
     :     N_ELEMENTS, BOL_DESC, N_BOLS, BOL_ADC, BOL_CHAN,
     :     BOL_LIST, N_BOLS_OUT, 
     :     STATUS )
*+
*  Name:
*     SURFLIB_DECODE_REMSKY_STRING
 
*  Purpose:
*     Calculate bolometer list from remsky input
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
 
 
*  Description:
*     Given an array of bolometer descriptions, return an array
*     of bolometer numbers.
*     Input array can contain a variety of forms for identifying bolometers
*     or lists of bolometers.

*  Arguments:
*     SUB_INSTRUMENT = CHARACTER (Given)
*        Sub instrument name. This governs the bolometers that
*        are allowed to be returned.
*     N_ELEMENTS = INTEGER (Given)
*        Number of elements in the input array
*     BOL_DESC( N_ELEMENTS ) = CHARACTER*(*) (Given)
*        Array of bolometer descriptions
*        Can take the form of:
*          n      -   bolometer number
*          id     -   bolometer id (eg H7)
*          all    -   all bolometers
*          rn     -   ring of bolometers
*        Can be prefixed with a minus sign to indicate that a bolometer
*        should be removed from the final list.
*     N_BOLS = INTEGER (Given)
*        Maximum allowed size for return array containing bolometer numbers
*        An error is given if this number is exceeded. In effect this
*        is equivalent to the actual number of bolometers on the array.
*     BOL_ADC( N_BOLS ) = INTEGER (Given)
*        ADC numbers for identifying bolometers
*     BOL_CHAN( N_BOLS ) = INTEGER (Given)
*        Channel number for identifying bolometers
*     BOL_LIST( N_BOLS ) = INTEGER (Returned)
*        List of bolometer numbers corresponding to the supplied strings.
*     N_BOLS_OUT = INTEGER (Returned)
*        Actual number of bolometers returned in BOL_LIST
*     STATUS = INTEGER (Given & Returned)
*        Global status

*  Authors:
*     Tim Jenness (timj@jach.hawaii.edu)

*  History:
*     $Log$
*     Revision 1.1  1997/11/06 01:56:57  timj
*     Initial revision
*
*

*-
 
*  Type Definitions:
      IMPLICIT NONE                              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'                          ! Standard SAE constants
      INCLUDE 'PRM_PAR'                          ! Bad values
      INCLUDE 'MSG_PAR'                          ! MSG__ constants
 
*  Arguments Given:
      INTEGER N_ELEMENTS
      CHARACTER*(*) BOL_DESC ( N_ELEMENTS )
      INTEGER N_BOLS
      INTEGER BOL_ADC(N_BOLS)
      INTEGER BOL_CHAN(N_BOLS)
      CHARACTER *(*) SUB_INSTRUMENT

*  Arguments Returned:
      INTEGER N_BOLS_OUT
      INTEGER BOL_LIST ( N_BOLS )

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Local Constants:
      INTEGER MAX_N_BOLS              ! Storage space for bolometers in list
      PARAMETER ( MAX_N_BOLS = 500 )
      INTEGER N_SUBS                  ! Number of sub_instruments
      PARAMETER ( N_SUBS = 2 )        ! with different arrays
      INTEGER N_RINGS                 ! Max number of rings of all subs_insts
      PARAMETER ( N_RINGS = 5 )      
      INTEGER N_BPR                   ! Max number of bols per ring
      PARAMETER (N_BPR = 30 )         ! Determined by SHORT array

*  Local Variables:
      INTEGER ADC                     ! ADC number for bolometer
      CHARACTER * (10) ARRAYS (N_SUBS)! Names of supported sub_instruments
      INTEGER BOL                     ! Bolometer number
      INTEGER CHAN                    ! CHANnel number for bolometer
      INTEGER COUNTER                 ! Keep track of position in array
      INTEGER CURRENT                 ! Current bolometer in loop
      LOGICAL FOUND                   ! Found bolometer in sub instrument
      INTEGER I                       ! Loop variable
      LOGICAL MINUS                   ! Is the string positive or negative
      INTEGER N                       ! Loop variable
      INTEGER NEGB(MAX_N_BOLS)        ! Work space of negative bolometers
      INTEGER NEG_BOLS (MAX_N_BOLS)   ! Bolometers to be removed from list
      INTEGER N_NEGATIVE              ! Number of negative bolometers 
      INTEGER N_POSITIVE              ! Number of positive bolometers
      INTEGER POSB(MAX_N_BOLS)        ! Work space of positive bolometers
      INTEGER POS                     ! Position in negative array
      INTEGER POS_BOLS(MAX_N_BOLS)    ! Bolometers to be kept in list
      INTEGER RINGS(N_BPR,0:N_RINGS,N_SUBS)
                                      ! Bolometers in each ring and sub_inst
      INTEGER RNUM                    ! Ring number
      CHARACTER*(10) STEMP            ! Modified remsky string
      INTEGER SUB_INDEX               ! Index to specify sub instrument in RINGS

*  Local Data:
      DATA ARRAYS / 'LONG','SHORT'/

      DATA (RINGS(I,0,1),I=1,N_BPR) /19, 29*0/    ! LONG array
      DATA (RINGS(I,1,1),I=1,N_BPR) / 12,13,20,26,25,18, 24*0 /
      DATA (RINGS(I,2,1),I=1,N_BPR) 
     :     /  6, 7, 8,14,21,27,32,31,30,24,17,11, 18*0/ 
      DATA (RINGS(I,3,1),I=1,N_BPR) 
     :     /  1, 2, 3, 4, 9,15,22,28,33,37,36,35,34,29,23,16,10, 5, 
     :     12*0/
      DATA (RINGS(I,4,1),I=1,N_BPR) /30*0/
      DATA (RINGS(I,5,1),I=1,N_BPR) /30*0/

      DATA (RINGS(I,0,2),I=1,N_BPR) /46, 29*0/    ! SHORT array
      DATA (RINGS(I,1,2),I=1,N_BPR) /35,36,47,57,56,45, 24*0/
      DATA (RINGS(I,2,2),I=1,N_BPR) 
     :     /25,26,27,37,48,58,67,66,65,55,44,34, 18*0/
      DATA (RINGS(I,3,2),I=1,N_BPR) 
     :     /16,17,18,19,28,38,49,59,68,76,75,74,73,64,54,43,33,24,12*0/
      DATA (RINGS(I,4,2),I=1,N_BPR) 
     :     /8,9,10,11,12,20,29,39,50,60,69,77,84,83,82,81,
     :     80,72,63,53,42,32,23,15, 6*0/
      DATA (RINGS(I,5,2),I=1,N_BPR) 
     :     /1,2,3,4,5,6,13,21,30,40,51,61,70,78,85,91,90,
     :     89,88,87,86,79,71,62,52,41,31,22,14,7/

      DATA NEGB / MAX_N_BOLS * 0 /
      DATA POSB / MAX_N_BOLS * 0 /
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the counters
      N_POSITIVE = 0
      N_NEGATIVE = 0
      N_BOLS_OUT = 0

*     Set the sub_instrument index (compare upper case)

      STEMP = SUB_INSTRUMENT
      CALL CHR_UCASE(STEMP)

      I = 1
      DO WHILE (ARRAYS(I) .NE. STEMP .AND. I .LE. N_SUBS)
         I = I + 1
      END DO
      SUB_INDEX = I

      IF (SUB_INDEX .GT. N_SUBS) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('SUB', SUB_INSTRUMENT)
         CALL ERR_REP(' ','DECODE_STRING: Sub instrument (^SUB)'//
     :        ' is not supported.', STATUS)
      END IF

      
*     Loop through each element

      IF (STATUS .EQ. SAI__OK) THEN

         DO I = 1, N_ELEMENTS

*     First see if we can convert to a number straightaway
            CALL CHR_CTOI (BOL_DESC(I), BOL, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN
*     We did have a number. Check it for range.

               IF ((ABS(BOL) .LE. N_BOLS) .AND. (BOL .NE. 0)) THEN
                  
                  IF (BOL .LT. 0) THEN
                     
                     N_NEGATIVE = N_NEGATIVE + 1
                     IF (N_NEGATIVE .LE. MAX_N_BOLS) THEN
                        NEG_BOLS(N_NEGATIVE) = BOL
                     END IF
                     
                  ELSE
                  
                     N_POSITIVE = N_POSITIVE + 1
                     IF (N_POSITIVE .LE. MAX_N_BOLS) THEN
                        POS_BOLS(N_POSITIVE) = BOL
                     END IF

                  END IF

               ELSE 
                  CALL MSG_SETI('BOL',BOL)
                  CALL MSG_SETI('NB',N_BOLS)
                  CALL MSG_OUTIF(MSG__NORM, ' ',
     :                 'DECODE_STRING: ^BOL is out of range (1:^NB)', 
     :                 STATUS)

               END IF


            ELSE

*     No number. First annul the bad status.
               CALL ERR_ANNUL (STATUS)

*     Clean up the string just in case
               CALL CHR_CLEAN(BOL_DESC(I))

*     Remove any blanks from the string
               CALL CHR_RMBLK(BOL_DESC(I))

*     First decide whether this is to be a positive bolometer 
*     or a negative. Search for a minus sign in the first character.
*     Remove the minus sign if we do find one.

               IF (BOL_DESC(I)(1:1) .EQ. '-') THEN
                  MINUS = .TRUE.
                  STEMP = BOL_DESC(I)(2:)
               ELSE
                  MINUS = .FALSE.
                  STEMP = BOL_DESC(I)
               END IF

*     Upper case
               CALL CHR_UCASE(STEMP)

*     Now go through and look at the string.
*     Do this the long hand way.
            
               IF (STEMP .EQ. 'ALL') THEN
*     This is all the bolometers. Get them by simply copying all the
*     indices. Note that the '-all' option is irrelevant since it
*     will remove everything from the list

                  IF (MINUS) THEN
                     DO N = 1, N_BOLS
                        N_NEGATIVE = N_NEGATIVE + 1
                        IF (N_NEGATIVE .LE. MAX_N_BOLS) THEN
                           NEG_BOLS( N_NEGATIVE ) = N
                        END IF
                     END DO     
                     CALL MSG_OUTIF(MSG__QUIET, ' ',
     :                    'WARNING: The  -all option will remove '//
     :                    'all bolometers from the input list',
     :                    STATUS)
                  ELSE
                     DO N = 1, N_BOLS
                        N_POSITIVE = N_POSITIVE + 1
                        IF (N_POSITIVE .LE. MAX_N_BOLS) THEN
                           POS_BOLS( N_POSITIVE ) = N
                        END IF
                     END DO
                  END IF

*     If the string starts with an 'R' then it is a ring
               ELSE IF (STEMP(1:1) .EQ. 'R') THEN

*     Now I must convert the second character onwards to an integer
*     and get the ring number
                  
                  CALL CHR_CTOI(STEMP(2:), RNUM, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
*     We have a number. Now check the range

                     IF (RNUM .LT. 0 .OR. RNUM .GT. N_RINGS) THEN
*     Oh dear. Out of range so report

                        CALL MSG_SETI('RN', RNUM)
                        CALL MSG_OUTIF(MSG__QUIET, ' ',
     :                       'DECODE_STRING: Ring ^RN is not '//
     :                       'available', STATUS)

                     ELSE
*     Can now process this ring.

                        IF (MINUS) THEN

*     Negative bolometers
                           DO N = 1, N_BPR

                              IF (RINGS(N, RNUM, SUB_INDEX) .NE. 0) THEN
                                 N_NEGATIVE = N_NEGATIVE + 1
                                 IF (N_NEGATIVE .LE. MAX_N_BOLS) THEN
                                    NEG_BOLS(N_NEGATIVE) = 
     :                                   RINGS(N, RNUM, SUB_INDEX)
                                 END IF

                              END IF

                           END DO

                        ELSE

*     Positive bolometers
                           DO N = 1, N_BPR

                              IF (RINGS(N, RNUM, SUB_INDEX) .NE. 0) THEN
                                 N_POSITIVE = N_POSITIVE + 1
                                 IF (N_POSITIVE .LE. MAX_N_BOLS) THEN
                                    POS_BOLS(N_POSITIVE) = 
     :                                   RINGS(N, RNUM, SUB_INDEX)
                                 END IF

                              END IF

                           END DO


                        END IF


                     END IF
                  ELSE
*     Could not find a number so report a message and continue
*     round loop
                     CALL ERR_ANNUL(STATUS)
                     CALL MSG_SETC('RNG',STEMP)
                     CALL MSG_OUTIF(MSG__QUIET, ' ',
     :                    'DECODE_STRING: Error converting '//
     :                    ' ^RNG to a ring number', STATUS)

                  END IF

*     Else it may be an explicit bolometer name
               ELSE

                  CALL SCULIB_BOLDECODE (STEMP, ADC, CHAN, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN
*     This means the string is a proper bolometer.
*     Have to check that it is in the selected array

*     Compare the ADC and CHAN with the sub instrument data

                     FOUND = .FALSE.
                     N = 1
                     DO WHILE ((.NOT.FOUND) .AND. (N .LE. N_BOLS))

                        IF ((ADC .EQ. BOL_ADC(N)) .AND.
     :                       (CHAN .EQ. BOL_CHAN(N))) THEN

                           FOUND = .TRUE.
*     found it. Now I can put it into the array
                           IF (MINUS) THEN
                              N_NEGATIVE = N_NEGATIVE + 1
                              IF (N_NEGATIVE .LE. MAX_N_BOLS) THEN
                                 NEG_BOLS(N_NEGATIVE) = N
                              END IF
                           ELSE
                              N_POSITIVE = N_POSITIVE + 1
                              IF (N_POSITIVE .LE. MAX_N_BOLS) THEN
                                 POS_BOLS(N_POSITIVE) = N
                              END IF                              
                           END IF
                        END IF
*     increment counter
                        N = N + 1
                     END DO

*     If I didnt found a bolometer in this sub then report an error

                     IF (.NOT. FOUND) THEN

                        CALL MSG_SETC('BOL', STEMP)
                        CALL MSG_SETC('SUB', SUB_INSTRUMENT)
                        CALL MSG_OUTIF(MSG__QUIET, ' ',
     :                       'DECODE_STRING: ^BOL could not be found'//
     :                       ' in the ^SUB SUB_INSTRUMENT', STATUS)
                     END IF

                  ELSE
*     Bolometer could not be deciphered
                     CALL ERR_FLUSH(STATUS)
                     CALL MSG_SETC('BOL', STEMP)
                     CALL MSG_OUTIF(MSG__QUIET, ' ',
     :                    'DECODE_STRING: Bolometer ^BOL not found',
     :                    STATUS)
                  END IF


               END IF

            END IF

         END DO

      END IF


*     Make sure we did not exceed our storage capacity

      IF ((N_POSITIVE .GT. MAX_N_BOLS) .OR. 
     :     (N_NEGATIVE .GT. MAX_N_BOLS)) THEN

         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','DECODE_STRING: Scratch space exceeded. '//
     :           'Please try a shorter bolometer list.', STATUS)
         END IF

      END IF

*     Now we have got all the required bolometers in either a
*     positive array or a negative array.

*     First we need to make sure that the positive array does not contain
*     duplicate numbers

      IF (STATUS .EQ. SAI__OK) THEN

*     Sort the arrays

         CALL PDA_QSAI(N_NEGATIVE, NEG_BOLS)
         CALL PDA_QSAI(N_POSITIVE, POS_BOLS)

*     Copy the data into new arrays checking for copies

*     Start with negative data
*     Set initial conditions
         CURRENT = -1     ! To compare with
         NEGB(1) = 0      ! If no negatives we need to initialise to zero
         COUNTER = 0

         DO I = 1, N_NEGATIVE
            
            IF (NEG_BOLS(I) .NE. CURRENT) THEN
*     Not the same as the previous entry so we can copy this one

               COUNTER = COUNTER + 1
               CURRENT = POS_BOLS(I)
               NEGB(COUNTER) = NEG_BOLS(I)

            END IF

         END DO

*     Change N_NEGATIVE to the actual number of unique bolometers
         N_NEGATIVE = COUNTER

*     Now deal with the positive array
*     Set initial conditions
         CURRENT = -1     ! To compare with
         COUNTER = 0

         DO I = 1, N_POSITIVE
            
            IF (POS_BOLS(I) .NE. CURRENT) THEN
*     Not the same as the previous entry so we can copy this one

               COUNTER = COUNTER + 1
               CURRENT = POS_BOLS(I)
               POSB(COUNTER) = POS_BOLS(I)

            END IF

         END DO

*     Change N_NEGATIVE to the actual number of unique bolometers
         N_POSITIVE = COUNTER


*     Second, we have to transfer the positives to the output array
*     so long as they are not in the input array.

*     For each of the positive bolometers see if there is a negative
*     that negates it.
*     Remember that this list is sorted.

         N_BOLS_OUT = 0 ! Number of output bolometers
         POS = 1  ! Position in negative array

         DO I = 1, N_POSITIVE


            DO WHILE ( POS .LT. N_NEGATIVE .AND. NEGB(POS) .LT. POSB(I))

               POS = POS + 1

            END DO

*     The postive bolometer does not match the negative bolometer
*     so we can copy this to the output
*     This works even if N_NEGATIVE is zero since I have initialised
*     NEGB to 0.

            IF (POSB(I) .NE. NEGB(POS)) THEN

               N_BOLS_OUT = N_BOLS_OUT + 1
               BOL_LIST(N_BOLS_OUT) = POSB(I)

            END IF

         END DO

      END IF

      END
