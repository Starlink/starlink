*+DEJUL
        subroutine DEJUL( jd, year, month, day )

        implicit none

        double precision day_year
        parameter( day_year = 3.6525D+02 )
*                  Number of days in a Julian year

        double precision jd
        integer year, month, day

*       Description
*         Given a (unmodified) Julian date, returns the year, month and day
*         Probably only works for Gregorian calendar; guaranteed only for
*         modern astronomical use.
*
*       Arguments
*         jd                (i) : Julian date
*         year, month, day  (o) : Gregorian calendar date
*
*       Dependences
*         none
*
*       Origin
*         Lost in the mist of time, probably the Monroe book on Fortran 77
*
*       Author
*         Koji Mukai, (1992 Dec 30) re-coded original
*-DEJUL

        double precision jday2, g

        jday2 = nint( jd )
        g = int( ( jday2 - 4479.5D+00 ) / 36524.25D+00 )
        g = int( g * 0.75D+00 + 0.5D+00 ) - 37.0D+00
        jday2 = jday2 + g
        year = int( jday2 / day_year ) - 4712
        g = idint( dmod( jday2 - 5.925D+01, day_year ) ) + 5.0D-01
        month = mod( idint( g / 3.06D+01 ) + 2, 12 ) + 1
        day = idint( dmod( g, 3.06D+01 ) ) + 1

        end
