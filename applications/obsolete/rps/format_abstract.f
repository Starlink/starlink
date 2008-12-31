*- FORMAT_ABSTRACT	Formats the abstract into 69 character lines
*	Jul 1993	Phillip C. Brisco
*
	SUBROUTINE format_abstract (abstract_file,do_file)

	INCLUDE 'com_form_files.inc'

	INTEGER*4 chan, status, mdh_endword, work_arr_var, num_recs
	INTEGER*4 buff_arr_count, i, j, k, len_buffer
	CHARACTER*(*) abstract_file(*)
	CHARACTER*69 work_array(12), blank_out/' '/
	CHARACTER*1000 buffer
	LOGICAL end_of_file, do_file
	PARAMETER (num_recs = 12)

c---------------------------------------------------------------------------
	DO i = 1,num_recs
		work_array(i) = ' '
	ENDDO

	IF (do_file) THEN
		CALL GETLUN(chan)
		OPEN (chan, FILE=abstract_file(1)//
	1		'.abstract', STATUS='old', FORM='FORMATTED',
	2		CARRIAGECONTROL = 'LIST', iostat=status)
	ENDIF

	end_of_file = .FALSE.
	len_buffer = 0
	buffer = ' '
	i = 2
	buff_arr_count = 1

c
c	Read in the entire abstract file.  If do_file is true, then we are
c	formatting a file, otherwise we are fomatting a passed array.
c
	DO WHILE (.NOT. end_of_file)

		IF (do_file) THEN
			READ(chan,'(A)',END=10,ERR=10) buffer(
	1			len_buffer + i:)
		 ELSE
			buffer(len_buffer + i:) = abstract_file(
	1			buff_arr_count)
			IF (buff_arr_count .EQ. num_recs)
	1			end_of_file = .TRUE.
			buff_arr_count = buff_arr_count + 1
		ENDIF

		len_buffer = mdh_endword(buffer)

c
c		If the last character was a '.', '?', or '!' then we can
c		assume that it marks the end of a sentence.  So put two
c		characters vice one between the appropriate words.
c
		IF (buffer(len_buffer:len_buffer) .EQ. '.' .OR.
	1		buffer(len_buffer:len_buffer) .EQ. '?' .OR.
	2		buffer(len_buffer:len_buffer) .EQ. '!') THEN
			i = 3
		 ELSE
			i = 2
		ENDIF

		IF (len_buffer .GE. 900) end_of_file = .TRUE.
	ENDDO

10	len_buffer = mdh_endword(buffer)
	i = 1
	work_arr_var = 1

c
c	Do until end of file.
c
	DO WHILE (i .LE. len_buffer .AND. work_arr_var .LE. num_recs)
c
c		Strip off the leading blanks.
c
		DO WHILE (buffer(i:i) .LE. ' ' .AND. i .LE. len_buffer)
			i = i + 1
		ENDDO

		j = i

c
c		Find the nearest blank to the end of the line.
c
		DO WHILE (i .LE. j + 69)
			IF (buffer(i:i) .EQ. ' ') k = i
			i = i + 1
		ENDDO

		work_array(work_arr_var) = buffer(j:k)
		work_arr_var = work_arr_var + 1
		i = k + 1
	ENDDO

	IF (do_file) THEN
		CLOSE (chan)
		OPEN (chan, FILE=abstract_file(1)//
	1		'.ABSTRACT', STATUS='NEW', FORM='FORMATTED',
	2		CARRIAGECONTROL = 'LIST', RECL = 69,
	3		iostat=status)					! appear to status = 117 here, creates fort.99!
		DO i=1,work_arr_var
			WRITE (chan,'(A)') work_array(i)
		ENDDO

		CLOSE (chan)
		CALL FRELUN(chan)
	 ELSE

		DO i=1,num_recs
			abstract_file(i) = ' '
			abstract_file(i) = work_array(i)
			IF (abstract_file(i) .LT. ' ')
	1			abstract_file(i) = ' '
		ENDDO

	ENDIF

	RETURN

	END
