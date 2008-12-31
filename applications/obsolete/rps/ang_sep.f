*+ANG_SEP
        double precision function ANG_SEP( ra1, dec1, ra2, dec2 )

        implicit none

        double precision pi, radian
        parameter( pi = 3.1415926536D+00 )
        parameter( radian = 57.29577951D+00 )

        double precision ra1, dec1
        double precision ra2, dec2

*       Description
*         Calculates the angular distance between two celestial points
*
*       Arguments
*         ra1, dec1      (i) : 1st position (decimal degrees)
*         ra2, dec2      (i) : 2nd position
*         <ANG_SEP>      (r) : angular separation (degrees)
*
*       Dependencies
*         TO_XYZ (included)
*
*       Origin
*         Common sense
*
*       Author
*         Koji Mukai (1992 Dec 30), original version
*-ANG_SEP

        double precision x1, y1, z1
        double precision x2, y2, z2
        double precision inner, xd, yd, zd, temp, ang

        call TO_XYZ( ra1, dec1, x1, y1, z1 )
        call TO_XYZ( ra2, dec2, x2, y2, z2 )
        inner = x1 * x2 + y1 * y2 + z1 * z2

        if( inner .gt. 0.71d+00 ) then
*            Use sine, which is more accurate in this regime
*            Length( vector1 - vector2 ) * 0.5 = sin( angle * 0.5 )
*            (Cosine of small x is ~1.0-x^2/2, whereas sin x ~ x)
          xd = x2 - x1
          yd = y2 - y1
          zd = z2 - z1
          temp = sqrt( xd * xd + yd * yd + zd * zd ) * 0.5d+00
          ang = asin( temp ) * 2.0d+00
        else if( inner .ge. -0.71d+00 ) then
*            Use the more familiar cosine formula
          ang = acos( inner )
        else
*            Back to sine, but two vectors are ~180 degrees apart
          xd = x1 + x2
          yd = y1 + y2
          zd = z1 + z2
          temp = sqrt( xd * xd + yd * yd + zd * zd ) * 0.5d+00
          ang = pi - asin( temp ) * 2.0d+00
        end if

*       finally, convert to degree
        ANG_SEP = ang * radian

        end



*+TO_XYZ
        subroutine TO_XYZ( ra, dec, x, y, z )

        implicit none

        double precision pi_180
        parameter( pi_180 = 0.01745329252D+00 )

        double precision ra, dec
        double precision x, y, z

*       Description
*         Converts RA and DEC (decimal degrees) to a unit vector.
*
*       Arguments
*         ra, dec      (i) : Input RA & Dec (or longitude and latitude, ...)
*         x, y, z      (o) : Unit vector describing the same direction
*
*       Dependencies
*         none
*
*       Origin
*         high school mathematics
*
*       Author
*         Koji Mukai   (1992 Dec 30), original version
*-TO_XYZ
        
        double precision alpha, delta, temp

        alpha = ra * pi_180
        delta = dec * pi_180

        temp = cos( delta )
        x = temp * cos( alpha )
        y = temp * sin( alpha )
        z = sin( delta )

        end
