*+SUN_CNSTR
        subroutine SUN_CNSTR( in_ra, in_dec, epoch, lo_rang, hi_rang,
     &                        year, month, day, no_days, m_pairs,
     &                        fm_strng, to_strng, n_pairs )

        implicit none

        double precision in_ra, in_dec, epoch
        real lo_rang, hi_rang
        integer year, month, day, no_days
        integer m_pairs
        character*11 fm_strng( m_pairs ), to_strng( m_pairs )
        integer n_pairs

*       Description
*         Given the object position and the range of solar angle
*         permissible, and a starting date and duration of the
*         relevant period (e.g., an AO period), this returns pairs
*         of dates during which the source is observable, as strings.
*
*       Arguments
*         in_ra, in_dec       (i) : source position in decimal degrees
*         epoch               (i) : epoch of the above coordinate
*         lo_rang, hi_rang    (i) : range of solar angle allowed
*         year, month, day    (i) : starting day of the AO period
*         no_days             (i) : number of days of the AO period
*         m_pairs             (i) : dimension of the return arrays;
*                                   Should be 3 if n_days is 365 or less,
*                                   add 2 more for every year (or fraction)
*                                   you add to this.
*         fm_strng, to_strng  (o) : source observability windows
*                                           --- of the form "1992 Aug 14"
*         n_pairs             (o) : number of pairs of dates, if positive or 0;
*                                   error occured if negative
*
*       Dependencies
*         JULIAN     Converts calendar date into julian date
*         DEJUL      The reverse of the above
*         PREC       Precesses celestial coordinates
*         TOECLP     Converts from celestial to ecliptic
*         SUN_ELONG  Calculates ecliptic longitude of the sun
*         ANG_SEP    Angle between two points on the sky
*
*       Origin
*         program "beta" by T. Naylor
*
*       Author
*         Koji Mukai     (1992 Dec 30), original version
*-SUN_CNSTR

        double precision jdstrt, jd
        double precision alpha, delta
        double precision lambda, beta, sun_lambda
        double precision angle
        integer year1, month1, day1, j
        logical obsvbl

        integer JULIAN
        double precision SUN_ELONG, ANG_SEP

        character*3 mnname( 12 )
        data mnname / 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     &                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /

*       First, calculate the julian dates at the beginning and middle
*       of the AO period
        jdstrt = dble( JULIAN( year, month, day ) ) - 0.5D+00
        jd = jdstrt + dble( no_days ) * 0.5D+00

*       Precess coordinates to the middle of the AO period
	call PREC( in_ra, in_dec, epoch, jd, alpha, delta )
*       ...then convert to ecliptic coordinates
        call TOECLP( alpha, delta, jd, lambda, beta )

*       Special treatment for the first day:
        sun_lambda = SUN_ELONG( jdstrt )
*                 Sun is at (sun_lambda,0.0) in the ecliptic coordinates
        angle = ANG_SEP( lambda, beta, sun_lambda, 0.0D+00 )
*                 Thus sun and objects are angle (degrees) apart
        if( angle .ge. lo_rang .and. angle .le. hi_rang ) then
*         The object is observable at the beginning of the AO period
          n_pairs = 1
          write( fm_strng( n_pairs ), '(I4,A5,I2.2)' )
     &                                 year, ' '//mnname(month)//' ', day
*         ...written the AO start date into string
          obsvbl = .true.
        else
*         Object is not observable at the beginning of the AO period
          n_pairs = 0
          obsvbl = .false.
        end if

        do j = 1, no_days

*         Calculate the sun-object distance at 1-day intervals

          jd = jdstrt + dble( j )
          sun_lambda = SUN_ELONG( jd )
          angle = ANG_SEP( lambda, beta, sun_lambda, 0.0D+00 )
*         Current jd, sun_lambda and angle calculated
          if( angle .ge. lo_rang .and. angle .le. hi_rang ) then
*           Object is now observable
            if( .not. obsvbl ) then
*             ...but wasn't yesterday.  Today is therefore the first day of
*             an observing window.
              n_pairs = n_pairs + 1
              if( n_pairs .gt. m_pairs ) then
*               Oh dear, more observing windows than the parent program
*               is prepared to handle.  Set flag and return.
                n_pairs = -n_pairs
                return
              end if
              jd = jd - 0.5D+00
              call DEJUL( jd, year1, month1, day1 )
*             Decoded the Julian date back into normal calendar days
              write( fm_strng( n_pairs ), '(I4,A5,I2.2)' )
     &                                 year1, ' '//mnname( month1 )//' ', day1
              obsvbl = .true.
            end if
          else
*           Object is not observable
            if( obsvbl ) then
*             ...but was, yesterday.  Today is the last day of the current
*             window
              jd = jd - 0.5D+00
              call DEJUL( jd, year1, month1, day1 )
              write( to_strng( n_pairs ), '(I4,A5,I2.2)' )
     &                                 year1, ' '//mnname( month1 )//' ', day1
              obsvbl = .false.
            end if
          end if
        end do

        if( obsvbl ) then
*         Object is observable on the last day of AO period.
*         Close out by filling the "to_string" for the last window
          jd = jd + 0.5D+00
          call DEJUL( jd, year1, month1, day1 )
          write( to_strng( n_pairs ), '(I4,A5,I2.2)' )
     &                                 year1, ' '// mnname( month1 ) //' ', day1
        end if

        end
