	Integer Function JULIAN( Year, Month, Day )
C
	Implicit none
C
	Integer Year, Month, Day
C
	Integer YY, MM, DD
C
	If( Month .eq. 1 .or. Month .eq. 2 ) Then
	  MM = Month + 9
	  YY = Year - 1
	Else If( Month .ge. 3 .or. Month .le. 12 ) Then
	  MM = Month - 3
	  YY = Year
	Else
	  Stop 'ERROR:: Non-existent month!'
	End If
	DD = Day + ( YY + 4712 ) * 1461 / 4
     1          + ( MM * 306 + 5 ) / 10 + 59
	DD = DD - ( YY / 100 + 49 ) * 3 / 4 + 38
C
	JULIAN = DD
C
	End
