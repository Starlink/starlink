      PROGRAM SORTTEST
*
*  Test program for PDA sorting routines.
*
*  Written by: P.W. Draper 10th November 1995.
*

*  Number of values to test.
      INTEGER NVAL
      PARAMETER ( NVAL = 12 )

*  Local variables.
      INTEGER RAN( NVAL )
      INTEGER IA( NVAL )
      INTEGER IFAIL
      REAL RA( NVAL )
      DOUBLE PRECISION DA( NVAL )
      INTEGER INDEX( NVAL )

*  "Random" data.
      DATA RAN/201,5,7,8,1,3,6,9,12,67,100,2/

*  Fill all data arrays.
      DO I = 1, NVAL
        IA( I ) = RAN( I )
        RA( I ) = REAL( RAN( I ) )
        DA( I ) = DBLE( RAN( I ) )
      END DO

*  Show initial arrangement
      WRITE(*,*) 'Initial value arrangement'
      WRITE(*,'(5X,I3)') RAN
      WRITE(*,*) '-------------------------'

*  In place ascending sort.
      CALL PDA_QSAI( NVAL, IA )
      WRITE(*,*) 'After INTEGER ascending in place sort'
      WRITE(*,'(5X,I3)') IA
      WRITE(*,*) '-------------------------'
      CALL PDA_QSAR( NVAL, RA )
      WRITE(*,*) 'After REAL ascending in place sort'
      WRITE(*,'(5X,F4.0)') RA
      WRITE(*,*) '-------------------------'
      CALL PDA_QSAD( NVAL, DA )
      WRITE(*,*) 'After DOUBLE PRECISION ascending in place sort'
      WRITE(*,'(5X,F4.0)') DA
      WRITE(*,*) '-------------------------'

*  Re-fill all data arrays.
      DO I = 1, NVAL
        IA( I ) = RAN( I )
        RA( I ) = REAL( RAN( I ) )
        DA( I ) = DBLE( RAN( I ) )
      END DO

*  In place descending sort.
      CALL PDA_QSDI( NVAL, IA )
      WRITE(*,*) 'After INTEGER descending in place sort'
      WRITE(*,'(5X,I3)') IA
      WRITE(*,*) '-------------------------'
      CALL PDA_QSDR( NVAL, RA )
      WRITE(*,*) 'After REAL descending in place sort'
      WRITE(*,'(5X,F4.0)') RA
      WRITE(*,*) '-------------------------'
      CALL PDA_QSDD( NVAL, DA )
      WRITE(*,*) 'After DOUBLE PRECISION descending in place sort'
      WRITE(*,'(5X,F4.0)') DA
      WRITE(*,*) '-------------------------'

*  Re-fill all data arrays.
      DO I = 1, NVAL
        IA( I ) = RAN( I )
        RA( I ) = REAL( RAN( I ) )
        DA( I ) = DBLE( RAN( I ) )
      END DO

*  Indexed ascending sort.
      CALL PDA_QSIAI( NVAL, IA, INDEX )
      WRITE(*,*) 'After INTEGER ascending indexed sort'
      WRITE(*,'(5X,I3)') (IA(INDEX(I)),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIAR( NVAL, RA, INDEX )
      WRITE(*,*) 'After REAL ascending indexed sort'
      WRITE(*,'(5X,F4.0)') (RA(INDEX(I)),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIAD( NVAL, DA, INDEX )
      WRITE(*,*) 'After DOUBLE PRECISION ascending indexed sort'
      WRITE(*,'(5X,F4.0)') (DA(INDEX(I)),I=1,NVAL)
      WRITE(*,*) '-------------------------'

*  Re-fill all data arrays.
      DO I = 1, NVAL
        IA( I ) = RAN( I )
        RA( I ) = REAL( RAN( I ) )
        DA( I ) = DBLE( RAN( I ) )
      END DO

*  Indexed descending sort.
      CALL PDA_QSIDI( NVAL, IA, INDEX )
      WRITE(*,*) 'After INTEGER descending indexed sort'
      WRITE(*,'(5X,I3)') (IA(INDEX(I)),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIDR( NVAL, RA, INDEX )
      WRITE(*,*) 'After REAL descending indexed sort'
      WRITE(*,'(5X,F4.0)') (RA(INDEX(I)),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIDD( NVAL, DA, INDEX )
      WRITE(*,*) 'After DOUBLE PRECISION descending indexed sort'
      WRITE(*,'(5X,F4.0)') (DA(INDEX(I)),I=1,NVAL)
      WRITE(*,*) '-------------------------'

*  Index and rank RAN.
      CALL PDA_QSIAI( NVAL, RAN, INDEX )
      CALL PDA_IPERM( NVAL, INDEX )
      CALL PDA_IPERM( NVAL, INDEX )
      WRITE(*,*) 'Random value,   Index vector,     I'
      DO I = 1, NVAL
         WRITE(*,'(5X,I3,12X,I3,10X,I3)') RAN( I ), INDEX( I ), I
      END DO
      WRITE(*,*) '-------------------------'
      CALL PDA_IPERM( NVAL, INDEX )
      WRITE(*,*) 'Random value,    Rank vector'
      DO I = 1, NVAL
         WRITE(*,'(5X,I3,12X,I3)') RAN( I ), INDEX( I )
      END DO
      WRITE(*,*) '-------------------------'

*  Re-fill all data arrays.
      DO I = 1, NVAL
        IA( I ) = RAN( I )
        RA( I ) = REAL( RAN( I ) )
        DA( I ) = DBLE( RAN( I ) )
      END DO

*  Reorder arrays in place with descending sort.
      CALL PDA_QSIDI( NVAL, IA, INDEX )
      CALL PDA_RINPI( INDEX, NVAL, IA, IFAIL )
      WRITE(*,*) 'After INTEGER indexed descending in place sort'
      WRITE(*,'(5X,I3)') (IA(I),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIDR( NVAL, RA, INDEX )
      CALL PDA_RINPR( INDEX, NVAL, RA, IFAIL )
      WRITE(*,*) 'After REAL indexed descending in place sort'
      WRITE(*,'(5X,F4.0)') (RA(I),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIDD( NVAL, DA, INDEX )
      CALL PDA_RINPD( INDEX, NVAL, DA, IFAIL )
      WRITE(*,*)
     :     'After DOUBLE PRECISION indexed descending in place sort'
      WRITE(*,'(5X,F4.0)') (DA(I),I=1,NVAL)
      WRITE(*,*) '-------------------------'

*  Re-fill all data arrays.
      DO I = 1, NVAL
        IA( I ) = RAN( I )
        RA( I ) = REAL( RAN( I ) )
        DA( I ) = DBLE( RAN( I ) )
      END DO

*  Reorder arrays in place with ascending sort.
      CALL PDA_QSIAI( NVAL, IA, INDEX )
      CALL PDA_RINPI( INDEX, NVAL, IA, IFAIL )
      WRITE(*,*) 'After INTEGER indexed ascending in place sort'
      WRITE(*,'(5X,I3)') (IA(I),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIAR( NVAL, RA, INDEX )
      CALL PDA_RINPR( INDEX, NVAL, RA, IFAIL )
      WRITE(*,*) 'After REAL indexed ascending in place sort'
      WRITE(*,'(5X,F4.0)') (RA(I),I=1,NVAL)
      WRITE(*,*) '-------------------------'
      CALL PDA_QSIAD( NVAL, DA, INDEX )
      CALL PDA_RINPD( INDEX, NVAL, DA, IFAIL )
      WRITE(*,*)
     :     'After DOUBLE PRECISION indexed ascending in place sort'
      WRITE(*,'(5X,F4.0)') (DA(I),I=1,NVAL)
      WRITE(*,*) '-------------------------'

*  Matrix ranking tests.
      WRITE(*,*)
      WRITE(*,*)'Matrix ranking tests'
      WRITE(*,*)'--------------------'
      CALL MATRIXI()
      CALL MATRIXR()
      CALL MATRIXD()
      END

      SUBROUTINE MATRIXR
      PARAMETER ( N = 8 )
      PARAMETER ( M = 8 )
      REAL ARR(N+2,M+2)
      INTEGER IP(N+2)
      INTEGER IPORI(N+2)
      INTEGER IFAIL

*  Fill the array with data.
      K = M * N
      DO J = 1, M
        DO I = 1, N
          K = K - 1
          ARR(I,J) = 5.0*ATAN(REAL(K))
          ARR(I,J) = NINT(10.0*(ARR(I,J) - INT(ARR(I,J))))
        ENDDO
      ENDDO
      WRITE(*,*)'Initial REAL matrix row-wise:'
      DO I =1, N
         WRITE(*,'(1X,I4,3X,8F3.0)') I, ( ARR(I,J), J=1,M )
      END DO

*  Sort by row.
      CALL PDA_SAARR( ARR, N+2, N, M, IP, IPORI, IFAIL )
      WRITE(*,*) 'Final state row:'
      DO I =1, N
         WRITE(*,'(1X,I4,3X,8F3.0)') IP(I), ( ARR(IP(I),J), J=1,M )
      END DO

*  Fill the array with data.
      K = M * N
      DO J = 1, M
        DO I = 1, N
          K = K - 1
          ARR(I,J) = 5.0*ATAN(REAL(K))
          ARR(I,J) = NINT(10.0*(ARR(I,J) - INT(ARR(I,J))))
        ENDDO
      ENDDO
      WRITE(*,*)'Initial REAL matrix column-wise:'
      DO I =1, M
         WRITE(*,'(1X,I4,3X,8F3.0)') I, ( ARR(I,J), J=1,N )
      END DO

*  Sort by column.
      CALL PDA_SAACR( ARR, N+2, N, M, IP, IPORI, IFAIL )
      WRITE(*,*) 'Final state column:'
      DO J =1, M
         WRITE(*,'(1X,I4,3X,8F3.0)') IP(J), ( ARR( I,IP(J)), I=1,N )
      END DO
      END

      SUBROUTINE MATRIXI
      PARAMETER ( N = 8 )
      PARAMETER ( M = 8 )
      INTEGER ARR(N+2,M+2)
      INTEGER IP(N+2)
      INTEGER IPORI(N+2)
      INTEGER IFAIL

*  Fill the array with data.
      K = M * N
      DO J = 1, M
        DO I = 1, N
          K = K - 1
          R = 5.0*ATAN(REAL(K))
          ARR(I,J) = NINT(10.0*(R - INT(R)))
        ENDDO
      ENDDO
      WRITE(*,*)'Initial INTEGER matrix row-wise:'
      DO I =1, N
         WRITE(*,'(1X,I4,3X,8I3.0)') I, ( ARR(I,J), J=1,M )
      END DO

*  Sort by row.
      CALL PDA_SAARI( ARR, N+2, N, M, IP, IPORI, IFAIL )
      WRITE(*,*) 'Final state row:'
      DO I =1, N
         WRITE(*,'(1X,I4,3X,8I3)') IP(I), ( ARR(IP(I),J), J=1,M )
      END DO

*  Fill the array with data.
      K = M * N
      DO J = 1, M
        DO I = 1, N
          K = K - 1
          R = 5.0*ATAN(REAL(K))
          ARR(I,J) = NINT(10.0*(R - INT(R)))
        ENDDO
      ENDDO
      WRITE(*,*)'Initial INTEGER matrix column-wise:'
      DO I =1, M
         WRITE(*,'(1X,I4,3X,8I3)') I, ( ARR(I,J), J=1,N )
      END DO

*  Sort by column.
      CALL PDA_SAACI( ARR, N+2, N, M, IP, IPORI, IFAIL )
      WRITE(*,*) 'Final state column:'
      DO J =1, M
         WRITE(*,'(1X,I4,3X,8I3)') IP(J), ( ARR( I,IP(J)), I=1,N )
      END DO
      END

      SUBROUTINE MATRIXD
      PARAMETER ( N = 8 )
      PARAMETER ( M = 8 )
      DOUBLE PRECISION ARR(N+2,M+2)
      INTEGER IP(N+2)
      INTEGER IPORI(N+2)
      INTEGER IFAIL

*  Fill the array with data.
      K = M * N
      DO J = 1, M
        DO I = 1, N
          K = K - 1
          ARR(I,J) = 5.0D0*ATAN(REAL(K))
          ARR(I,J) = NINT(10.0D0*(ARR(I,J) - INT(ARR(I,J))))
        ENDDO
      ENDDO
      WRITE(*,*)'Initial DOUBLE PRECISION matrix row-wise:'
      DO I =1, N
         WRITE(*,'(1X,I4,3X,8F3.0)') I, ( ARR(I,J), J=1,M )
      END DO

*  Sort by row.
      CALL PDA_SAARD( ARR, N+2, N, M, IP, IPORI, IFAIL )
      WRITE(*,*) 'Final state row:'
      DO I =1, N
         WRITE(*,'(1X,I4,3X,8F3.0)') IP(I), ( ARR(IP(I),J), J=1,M )
      END DO

*  Fill the array with data.
      K = M * N
      DO J = 1, M
        DO I = 1, N
          K = K - 1
          ARR(I,J) = 5.0D0*ATAN(REAL(K))
          ARR(I,J) = NINT(10.0D0*(ARR(I,J) - INT(ARR(I,J))))
        ENDDO
      ENDDO
      WRITE(*,*)'Initial DOUBLE PRECISION matrix column-wise:'
      DO I =1, M
         WRITE(*,'(1X,I4,3X,8F3.0)') I, ( ARR(I,J), J=1,N )
      END DO

*  Sort by column.
      CALL PDA_SAACD( ARR, N+2, N, M, IP, IPORI, IFAIL )
      WRITE(*,*) 'Final state column:'
      DO J =1, M
         WRITE(*,'(1X,I4,3X,8F3.0)') IP(J), ( ARR( I,IP(J)), I=1,N )
      END DO
      END

* @(#)Sorttest.f   1.6   97/02/20 10:30:14   97/02/24 14:56:42
