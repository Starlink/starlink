
	subroutine convert_local2ut( local_time, timediff, ut_time)

	implicit none

*     14-JUL-1994  Changed LIB$ to CHR_ (SKL@JACH)
*      9-AUG-2004  Should not use .EQ. for logical comparison (TIMJ@JACH)

      INCLUDE 'CHR_ERR'
      INCLUDE 'SAE_PAR'

	integer
     :    STATUS,
     :	  timediff,
     :	  year,
     :	  day,
     :	  hour,
     :	  minute,
     :	  second,
     :	  msecond,
     :    NCHAR

	character
     :	  local_time*( *),
     :	  ut_time*( *),
     :	  month*3,
     :	  cyear*4,
     :	  cday*2,
     :	  chour*2,
     :	  cminute*2,
     :	  csecond*2,
     :	  cmsecond*2

	logical
     :	  leap_year

	month = local_time( 4:6)

	if( month( 1:1) .eq. ' ') month( 1:1) = 'X'
	if( month( 2:2) .eq. ' ') month( 2:2) = 'X'
	if( month( 3:3) .eq. ' ') month( 3:3) = 'X'

        CALL CHR_CTOI( local_time( 1:2), day, STATUS )
        CALL CHR_CTOI( local_time( 8:11), YEAR, STATUS )
        CALL CHR_CTOI( local_time( 13:14), HOUR, STATUS )
        CALL CHR_CTOI( local_time( 16:17), MINUTE, STATUS )
        CALL CHR_CTOI( local_time( 19:20), SECOND, STATUS )
        CALL CHR_CTOI( local_time( 22:23), MSECOND, STATUS )

	if( ifix( year/4.0 + 0.5)*4 .eq. year) then
	  leap_year = .true.
	else
	  leap_year = .false.
	end if

	hour = hour + timediff

	if( hour .ge. 24) then
	  hour = hour - 24
	  day = day + 1
	end if

	if( month .eq. 'JAN' .and. day .gt. 31) then

	  day = day - 31
	  month = 'FEB'

	else if( month .eq. 'FEB') then

	  if( leap_year  .and. day .gt. 29) then

	    day = day - 29
	    month = 'MAR'

	  else if( .NOT. leap_year .and. day .gt. 28) then

	    day = day - 28
	    month = 'MAR'

	  end if

	else if( month .eq. 'MAR' .and. day .gt. 31) then

	  day = day - 31
	  month = 'APR'

	else if( month .eq. 'APR' .and. day .gt. 30) then

	  day = day - 30
	  month = 'MAY'

	else if( month .eq. 'MAY' .and. day .gt. 31) then

	  day = day - 31
	  month = 'JUN'

	else if( month .eq. 'JUN' .and. day .gt. 30) then

	  day = day - 30
	  month = 'JUL'

	else if( month .eq. 'JUL' .and. day .gt. 31) then

	  day = day - 31
	  month = 'AUG'

	else if( month .eq. 'AUG' .and. day .gt. 31) then

	  day = day - 31
	  month = 'SEP'

	else if( month .eq. 'SEP' .and. day .gt. 30) then

	  day = day - 30
	  month = 'OCT'

	else if( month .eq. 'OCT' .and. day .gt. 31) then

	  day = day - 31
	  month = 'NOV'

	else if( month .eq. 'NOV' .and. day .gt. 30) then

	  day = day - 30
	  month = 'DEC'

	else if( month .eq. 'DEC' .and. day .gt. 31) then

	  day = day - 31
	  month = 'JAN'
          year = year + 1

	end if

        CALL CHR_ITOC( day, cday, NCHAR )
        CALL CHR_ITOC( YEAR, CYEAR, NCHAR )
        CALL CHR_ITOC( HOUR, CHOUR, NCHAR )
        CALL CHR_ITOC( MINUTE, CMINUTE, NCHAR )
        CALL CHR_ITOC( SECOND, CSECOND, NCHAR )
        CALL CHR_ITOC( MSECOND, CMSECOND, NCHAR )

	if( cyear( 4:4) .eq. ' ') then

	  cyear( 4:4) = cyear( 1:1)
	  cyear( 1:1) = ' '
	  cyear( 2:2) = ' '
	  cyear( 3:3) = ' '

	end if

	if( cyear( 1:1) .eq. ' ') cyear( 1:1) = '0'
	if( cyear( 2:2) .eq. ' ') cyear( 2:2) = '0'
	if( cyear( 3:3) .eq. ' ') cyear( 3:3) = '0'
	if( cyear( 4:4) .eq. ' ') cyear( 4:4) = '0'

	if( cday( 2:2) .eq. ' ') then

	  cday( 2:2) = cday( 1:1)
	  cday( 1:1) = ' '

	end if

	if( cday( 1:1) .eq. ' ') cday( 1:1) = '0'
	if( cday( 2:2) .eq. ' ') cday( 2:2) = '0'

	if( chour( 2:2) .eq. ' ') then

	  chour( 2:2) = chour( 1:1)
	  chour( 1:1) = ' '

	end if

	if( chour( 1:1) .eq. ' ') chour( 1:1) = '0'
	if( chour( 2:2) .eq. ' ') chour( 2:2) = '0'

	if( cminute( 2:2) .eq. ' ') then

	  cminute( 2:2) = cminute( 1:1)
	  cminute( 1:1) = ' '

	end if

	if( cminute( 1:1) .eq. ' ') cminute( 1:1) = '0'
	if( cminute( 2:2) .eq. ' ') cminute( 2:2) = '0'

	if( csecond( 2:2) .eq. ' ') then

	  csecond( 2:2) = csecond( 1:1)
	  csecond( 1:1) = ' '

	end if

	if( csecond( 1:1) .eq. ' ') csecond( 1:1) = '0'
	if( csecond( 2:2) .eq. ' ') csecond( 2:2) = '0'

	if( cmsecond( 2:2) .eq. ' ') then

	  cmsecond( 2:2) = cmsecond( 1:1)
	  cmsecond( 1:1) = ' '

	end if

	if( cmsecond( 1:1) .eq. ' ') cmsecond( 1:1) = '0'
	if( cmsecond( 2:2) .eq. ' ') cmsecond( 2:2) = '0'

	ut_time = cday//'-'//month//'-'//cyear//' '//chour//':'//cminute//
     :	          ':'//csecond//'.'//cmsecond

	end
