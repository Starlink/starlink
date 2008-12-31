*ABSTRACT_EDIT -- Does the creation and/or edit of the abstract.
*     1993 Jun  Phillip Brisco.   Original
*
      SUBROUTINE abstract_edit

      INCLUDE 'com_form_files.inc'

	INTEGER*4 num_recs, record_length, file_name_len
	INTEGER*4 chan, status, i, j, k, current_input, strng_siz
	INTEGER*4 search_len, replace_len, find_pos, MDH_ENDWORD
	PARAMETER (num_recs = 12,record_length = 69,strng_siz=100)
	CHARACTER*128 file_name
	CHARACTER*100 buffer(num_recs), out_put
	LOGICAL edit_abstract, tmp_edit_abstract
c------------------------------------------------------------------------------
      file_name = ' '
      WRITE (*,*)
      WRITE (*,*) '                    ABSTRACT EDIT                  '
      WRITE (*,*)
      WRITE (*,*) '%	Exits when entered at the beginning of a line.'
      WRITE (*,*) '*	Keeps previously entered information unchanged.'
      WRITE (*,*) '^	Blanks out a line when entered at the '//
     1    'beginning of a line.'
      WRITE (*,*) '&	Edits previous line when entered at the '//
     1    'beginning of a line.'
      WRITE (*,*) 'r	Replaces one string with another.  Of the form '//
     1    '"r/x/y/" or'
      WRITE (*,*) '	"r$x$y$" where y replaces x.  The "r/" or "r$" '//
     1    'must go in the first'
      WRITE (*,*) '	two columns.'
      WRITE (*,*) '@	Displays column ruler when entered at the '//
     1    'beginning of a line.'
      WRITE (*,*) '?	Displays this help message when entered '//
     1    'at the beginning of a line.'
      WRITE (*,*)
      j = 0
      file_name = form
      j = MDH_ENDWORD(file_name)

      IF (j .GT. 0) THEN
	  file_name = form(1:j)//'.abstract'
	ELSE
	  WRITE(*,'(A$)' )' Ready for the name of the abstract'//
     1      ' file: '
	  READ (*,'(A)') file_name
	  j = mdh_endword(file_name)

c
c        Check for an extension of .abstract. If there is no such extension,
c        append it to the end of the filename.
c
	  IF (j .GT. 0) THEN
	     IF (INDEX(file_name, '.abstract') .EQ. 0) file_name(j + 1:) =
     1      '.abstract '

	  ENDIF

       ENDIF

       j = 0

       DO i = 1,32
	  IF (file_name(i:i) .GT. ' ') j = i
       ENDDO

       j = j + 1
       file_name_len = j

c
c     If the file name entered by the user is non-blank, we continue, else
c     we exit the program.
c
      IF (file_name(1:1) .GT. ' ') THEN
	  CALL GETLUN(chan)
	  OPEN (chan, FILE=file_name(1:j),
     1	      STATUS='old', FORM='FORMATTED', CARRIAGECONTROL = 'LIST',
     2        iostat=status)

c
c        If file does not already exist, create it.
c
	  IF (status .NE. 0) THEN
	     edit_abstract = .FALSE.
	     OPEN (chan, FILE=file_name(1:j),
     1		 STATUS='NEW', FORM='FORMATTED', RECL=record_length,
     2           CARRIAGECONTROL='LIST', IOSTAT=status)

	     DO i = 1,num_recs
		 buffer(i) = ' '
	     ENDDO

	   ELSE
	     CLOSE (chan)
	     CALL FORMAT_ABSTRACT(file_name(1:j - 10), .TRUE.)
	     OPEN (chan, FILE=file_name(1:j),
     1	       STATUS='old', FORM='FORMATTED', CARRIAGECONTROL = 'LIST',
     2         iostat=status)
	     edit_abstract = .TRUE.

	     DO i = 1,num_recs
		 buffer(i) = ' '
		 READ (chan,'(A)',END=10) buffer(i)
	     ENDDO

	  ENDIF

c
c	 If the file is less than num_recs in length, get rid of any strange
c	 characters which might be hanging around.
c
10	  IF (i .LT. num_recs) THEN

		 DO j = i - 1,num_recs
			 buffer(j) = ' '
		 ENDDO

	  ENDIF

	  IF (status .NE. 0) THEN
	     WRITE (*,*) 'Could not create the abstract file'
	   ELSE
	     WRITE (*,*)
	     WRITE (*,*) 'The abstract file name is: ',file_name(1:
	1	file_name_len)
	     WRITE (*,*)
	     WRITE (*,*), 'Column            1         2         3       '//
	1	'  4         5         6'
	     WRITE (*,*), 'ruler    1234567890123456789012345678901234567'//
	1	'89012345678901234567890123456789'
	     WRITE (*,*), '----------------------------------------------'//
	1	'--------------------------------'
	     i = 1
	     current_input = 0
	     tmp_edit_abstract = .FALSE.

	     DO WHILE (i .LE. num_recs)

		IF (edit_abstract .OR. tmp_edit_abstract) THEN

		   IF (i .LT. 10) THEN
		      WRITE(*,'(A,I1,A)') ' Line(', i, '): '//buffer(i)
	1		(1:record_length)
		    ELSE
		      WRITE(*,'(A,I2,A)') ' Line(', i, '): '//buffer(i)
	1		(1:record_length)
		   ENDIF

		   IF (i .LT. 10) THEN
		      WRITE(*,'(A,I1,A,$)') ' Edit(', i, '): '
		    ELSE
		      WRITE(*,'(A,I2,A,$)') ' Edit(', i, '): '
		   ENDIF

		 ELSE

		   IF (i .LT. 10) THEN
		      WRITE(*,'(A,I1,A,$)') ' Line(', i, '): '
		    ELSE
		      WRITE(*,'(A,I1,A,$)') ' Line(', i, '): '
		   ENDIF

		ENDIF

		out_put = ' '
		READ (*,'(A)') out_put
		k = 0

		DO j = 1,79
		   IF (out_put(1:1) .GT. ' ') k = j
		ENDDO

		IF ((k .EQ. 0 .AND. .NOT. edit_abstract .AND.
     1             .NOT. tmp_edit_abstract) .OR. out_put(1:1) .EQ. '%')
     2             THEN		! End input
		   i = num_recs
		 ELSE IF (out_put(1:1) .EQ. '&') THEN	! previous line

		   IF (i .GT. 1) THEN
		      IF (current_input .EQ. 0) current_input = i
		      i = i - 2
		      tmp_edit_abstract = .TRUE.
		    ELSE
		      i = i - 1
		   ENDIF

		 ELSE IF (out_put(1:1) .EQ. '^') THEN	! erase line
		   buffer(i) = ' '

		 ELSE IF (out_put(1:1) .EQ. '@') THEN
		     WRITE (*,*)
		     WRITE (*,*), 'Column            1         2         3'//
	1		'         4         5         6'
		     WRITE (*,*), 'ruler    123456789012345678901234567890'//
	1		'123456789012345678901234567890123456789'
		     WRITE (*,*), '---------------------------------------'//
	1		'---------------------------------------'
		     i = i - 1

		 ELSE IF (out_put(1:1) .EQ. '?') THEN	! Help message
		   i = i - 1
		   WRITE (*,*)
		   WRITE (*,*) '                    ABSTRACT EDITOR'
		   WRITE (*,*)
		   WRITE (*,*) '%	Exits when entered at the '//
     1		      'beginning of a line.'
		   WRITE (*,*) '*	Keeps previously entered '//
     1                'information unchanged.'
		   WRITE (*,*) '^	Blanks out a line when entered '//
     1                'at the beginning of a line.'
		   WRITE (*,*) '&	Edits previous line when '//
     1                'entered at the beginning of a line.'
		   WRITE (*,*) 'r	Replaces one string with '//
     1                  'another.  Of the form "r/x/y/" or'
		   WRITE (*,*) '	"r$x$y$" where y replaces x.  '//
     1                  'The "r/" or "r$" must go in the first'
                   WRITE (*,*) '	two columns.'
                   WRITE (*,*) '@	Displays column ruler when '//
     1		      'entered at the beginning of a line.'
                   WRITE (*,*) '?	Displays this help message '//
     1                'when entered at the beginning of a line.'
                   WRITE (*,*)
		ELSE IF (out_put(1:2) .EQ. 'r/' .OR. out_put(1:2) .EQ.
	1		'R/' .OR. out_put(1:2) .EQ. 'r$' .OR.
	2		out_put(1:2) .EQ. 'R$') THEN	! Replace
		  search_len = INDEX(out_put(3:),out_put(2:2)) + 1

		  IF (search_len - 1 .GT. 0) THEN
		     replace_len = INDEX(out_put(search_len + 2:),
	1		out_put(2:2)) + search_len

		     IF (replace_len - search_len .GT. 0) THEN
			find_pos = INDEX(buffer(i),out_put(3:search_len))

			IF (find_pos .NE. 0) THEN
			   buffer(i) = buffer(i)(0:find_pos - 1)//
	1		      out_put(search_len + 2:replace_len)//
	2		      buffer(i)(find_pos + search_len - 2:)
			 ELSE
			   WRITE (*,*)
			   WRITE (*,*) '*** Could not find "'//
	1			out_put(3:search_len)//'" in target. '//
	2				'***'
			   WRITE (*,*)
			ENDIF

		      ELSE
			WRITE (*,*)
			WRITE (*,*) '*** "'//out_put(2:2)//'" is not '//
	1			'used as a closing delimiter for the '//
	2			'replace parameter. ***'
			WRITE (*,*)
		     ENDIF

		   ELSE
		     WRITE (*,*)
		     WRITE (*,*) '*** "'//out_put(2:2)//'" is not '//
	1		'used as a closing delimiter for the search '//
	2		'parameter. ***'
		     WRITE (*,*)
		  ENDIF

		  i = i - 1
                ELSE
		  IF (k .GT. 0) THEN

                     DO j = 1,strng_siz

                        IF (out_put(j:j) .NE. '*') THEN	! Don't erase chars
                           buffer(i)(j:j) = out_put(j:j)
                        ENDIF

                     ENDDO

                   ENDIF

               ENDIF

	       out_put = ' '
               i = i + 1

c
c	If the user backed up while in create mode, he is put into edit
c	mode.  We need to know when to exit edit mode and go back to create
c	mode.
               IF (current_input .GT. 0 .AND. current_input .EQ. i) THEN
                  current_input = 0
                  tmp_edit_abstract = .FALSE.
               ENDIF

            END DO

            IF (edit_abstract) REWIND (chan)

	    CALL FORMAT_ABSTRACT(buffer, .FALSE.)
            WRITE (*,*)
            WRITE (*,'(XA)')'+--------------------------------------'//
	1	'---------------------------------+'
            WRITE (*,'(XA)')'|                                      '//
	1	'                                 |'
            WRITE (*,'(X,A,T74,A)')'|                      '//file_name(
	1	1:file_name_len),'|'
	    WRITE (*,'(X,A,T74,A)'), '|','|'

	    DO j = 1,num_recs
               WRITE (*,'(X,2A,T74,A)') '| ',buffer(j)(1:record_length),
	1	'|'
               WRITE (chan,'(A)'), buffer(j)(1:record_length)
	    ENDDO

	    WRITE (*,'(X,A,T74,A)'), '|','|'
            WRITE (*,'(XA)')'+--------------------------------------'//
	1	'---------------------------------+'
            WRITE(*,'(XA$)')'Please hit <RETURN> to continue...'
            READ (*,'(A)') file_name
            WRITE (*,*)
            CLOSE (chan)
            CALL FRELUN (chan)
        ENDIF

       ENDIF

       RETURN

       END
