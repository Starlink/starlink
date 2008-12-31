*+FORM_INPUT       Gets the field values input in Line mode
*  History
*     1987 Aug	M Harris	1st version
*     1989 Jan	M Ricketts	Edits - passing text for library
*     1992 Apr  M. Duesterhaus  modifications for UNIX port
*     1992 Dec  M. Duesterhaus  modifications for astro-d
*     1993 Jun  P. Brisco       Added # option to skip similar fields.
*     1993 Jun  P. Brisco       Added & option to go to previous field.
*     1993 Jun  P. Brisco       Added ?? option to list menu help.
********************************************************************
      SUBROUTINE FORM_INPUT( KFORM, VAL, FIELDS, UNITS, FRMT, FLENTH,
     &        CONDOFF, CONDNULL, N, HELPLIB, HELP_PREFIX, FORM_FIELD)
      IMPLICIT NONE

*  Calling Arguments
      INTEGER KFORM		! In	Form number
      CHARACTER*(*) FIELDS(*)	! 	The field names.
     & ,            UNITS(*)	! 	Field units.
     & ,            FRMT(*)	! 	Field formats.
     & ,            HELPLIB	! 	Help library.
      INTEGER       FLENTH(*)   !       Field length
      INTEGER CONDOFF(*)	!	conditional offset
      CHARACTER*(*) CONDNULL(*)	!	null value for array skip
      CHARACTER*(*) HELP_PREFIX	!
      INTEGER       N		! 	Form number and number of fields.
      INTEGER FORM_FIELD	! 	Field at which to start 
      CHARACTER*(*) VAL(*)	! I/Out	The field values.

*  Functions
      INTEGER INDEX
      INTEGER MDH_ENDWORD
      REAL   MDH_CTOR

*-
* Local Variables
      INTEGER STATUS
      INTEGER CHAR_END
      CHARACTER*60 CDEF
      CHARACTER*48 CHFORM
      CHARACTER*132 TEMP
      CHARACTER*10 VLAST, VNULL
      CHARACTER*1 CLOCK,MDATA,BIT
      REAL RATE
      INTEGER end_pos
      INTEGER I			! Loop variable.
     & ,      STRT		! Start position in output string.
     & ,      COND_FIELD	! conditional field
     & ,      NCLAST, NCNULL, INLENTH
      LOGICAL PROCEED

*  Subroutines
*     MYLIB MDH_HELP	! Gives access to help library.

*  ___________________________ Executable Code ____________________________
	CALL key_help (127)
      i = form_field

      DO WHILE (i .LE. n)

        PROCEED = .TRUE.
        IF (CONDOFF(I) .GT. 0) THEN
          COND_FIELD = I - CONDOFF(I)
          IF ( VAL(COND_FIELD) .EQ. 'N') PROCEED = .FALSE.
        ELSE IF (CONDOFF(I) .LT. 0) THEN
          COND_FIELD = I - 1
          VLAST = ' '
          VNULL = ' '
          CALL STRIM(VAL(COND_FIELD), VLAST, NCLAST)
          CALL STRIM(CONDNULL(COND_FIELD), VNULL, NCNULL)
          IF ( VLAST(:NCLAST) .EQ. VNULL(:NCNULL)) PROCEED = .FALSE.
        END IF

        IF (PROCEED) THEN

         IF (fields(i)(1:1) .EQ. '$' ) THEN
            WRITE(*,*)      !   Skip a field.
            strt = 2            !   New output line to be started.
          ELSE                   !  Else.
            strt = 1           !   Continue the present line.
         END IF                 !  End if.

         IF (units(i) .NE. ' ') THEN !  If units not blank then.
            WRITE(chform,'( A )')
     &          ',Format ' // frmt(i) // ', Units ' // units(i) 		!   Output units and format.
          ELSE
           WRITE(chform,'( A )') ',Format' // FRMT( I ) 			!   Just output format.
         END IF                !  End if.

         cdef = val(i)

         IF (status .EQ. 4) THEN
            end_pos = INDEX(fields(i),'(')
            IF (end_pos .EQ. 0 .OR. temp(1:end_pos) .NE.
     &           fields(i)(1:end_pos)) status = 0
         END IF

         IF (status .NE. 4) THEN
            temp = fields(i)(strt:)//chform
            CALL H_GET(temp, frmt(i), cdef, val(i)(:flenth(i)), status) 	      ! Get field value - specific length
            IF (FRMT(I)(:1) .EQ. 'A') THEN
               INLENTH = MDH_ENDWORD( VAL(I)(:flenth(I)) )
               IF (INLENTH .EQ. FLENTH(I) .and. (INLENTH .GT.3) )
     &		write(*,*) ' Field may have been truncated, ',inlenth,' chars. returned'
             END IF
         END IF

         IF (status .EQ. 1) THEN
            i = n
          ELSE IF (status .EQ. 2) THEN ! Help
            char_end = INDEX(fields(i),'(') - 1
            IF (char_end .LT. 0) char_end = MDH_ENDWORD(fields(i))
            status = MDH_ENDWORD(help_prefix) + 2
*	write(*,*) ' calling helplib '//helplib//': '
*	write(*,*) ' fld '//help_prefix(:status) //fields(i)(strt:char_end)
            CALL MDH_HELP(helplib,help_prefix(:status) 
     &           //fields(i)(strt:char_end)) ! Access help library.
            i = i - 1
          ELSE IF (status .EQ. 3) THEN ! exit
            i = n
          ELSE IF (status .EQ. 5) THEN

            IF (i .GT. 1) THEN
               i = i - 1
               if (condoff(i) .lt. 0) then
                  do while (condoff(i) .lt.0 .and. (i .gt. 1))      ! to skip back to start of array
                     i = i - 1
                  end do
               end if
               i = i - 1
             ELSE
               WRITE(*,*)' Cannot back up beyond the first field in '//
     &               'the form.'
               i = i - 1
            END IF

          ELSE IF (status .EQ. 6) THEN
	    CALL key_help (127)
            i = i - 1
	  ELSE IF (status .EQ. 7) THEN
	    CALL MDH_HELP(helplib,help_prefix) ! Access help library
	    i = i - 1
         END IF

        END IF							! proceed

        i = i + 1
      ENDDO

      WRITE(*,*) ! Skip a line.

      END
