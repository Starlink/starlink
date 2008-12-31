* KEY_HELP	Prints the keyhelp information for each screen.
*	1993 Jul	P. Brisco	Original
*------------------------------------------------------------------------------
	subroutine key_help (bit_mask)

	INTEGER bit_mask, bit_off

	IF (bit_mask .GT. 0) THEN
		WRITE(*,*)
		WRITE(*,'(XA)') '+---------------------------------------'//
	1		'------------------------------------+'
	ENDIF

	bit_off = bit_mask .AND. 1
	IF (bit_off .GT. 0)
	1	WRITE(*,'(XA,T78,A)')'| Enter % to exit '//
	2		'out of the form at any time.','|'

	bit_off = bit_mask .AND. 2
	IF (bit_off .GT. 0) THEN
		WRITE(*,'(XA,T78,A)')'| Enter ^ to erase information '//
	1		'in character fields - entering spaces ','|'
		WRITE(*,'(XA,T78,A)')'|         in a character field '//
	1		'will NOT erase the information!','|'
	ENDIF

	bit_off = bit_mask .AND. 4
	IF (bit_off .GT. 0)
	1	WRITE(*,'(XA,T78,A)')'| Enter # to skip '//
	2	'groups of similar fields.','|'
	bit_off = bit_mask .AND. 8
	IF (bit_off .GT. 0)
	1	WRITE(*,'(XA,T78,A)')'| Enter & to go '//
	2	'to the previous field.','|'
	bit_off = bit_mask .AND. 16
	IF (bit_off .GT. 0)
	1	WRITE(*,'(XA,T78,A)')'| Enter ? for field help.','|'
	bit_off = bit_mask .AND. 32
	IF (bit_off .GT. 0)
	1	WRITE(*,'(XA,T78,A)')'| Enter ?? for key help.','|'
	bit_off = bit_mask .AND. 64

	IF (bit_mask .GT. 0) THEN
		WRITE(*,*) '+---------------------------------------'//
	1		'------------------------------------+'
	ENDIF

	RETURN

	END
