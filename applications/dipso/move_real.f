       SUBROUTINE MOVE_REAL ( n , from , to )

       INTEGER i , n
       REAL    from ( n )
       REAL    to  ( n )

       DO i = 1 , n
         to ( i ) = from ( i )
       END DO

       END
