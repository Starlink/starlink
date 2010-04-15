      program testrebin
      implicit none
      include 'SAE_PAR'
      external test1, test2, test3, test4, test5, test6, test7, test8,
     :         test9
      integer status
      status = sai__ok

      call ast_begin( status )

      call tester( test7, status )
      call tester( test8, status )
      call tester( test9, status )
      call tester( test1, status )
      call tester( test2, status )
      call tester( test3, status )
      call tester( test4, status )
      call tester( test5, status )
      call tester( test6, status )

      call ast_end( status )
      call ast_flushmemory( 1 )


      if( status .eq. sai__ok ) then
         write(*,*) 'All AST_REBIN tests passed'
      else
         write(*,*) 'AST_REBIN tests failed'
      end if

      end




*
*  Do a given test with a all data types and spread functions.
*
      subroutine tester( testfun, status )
      implicit none
      include 'SAE_PAR'
      include 'AST_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'

      integer m, lbnd_in(10), ubnd_in(10), ipin, ipin_var,
     :        lbnd_out(10), ubnd_out(10), lbnd(10), ubnd(10), ipout,
     :        ipout_var, status, nin, nout, i, nel_in, nel_out,
     :        spreads(6), j
      character types(3)*15, name*20
      double precision tol, params(20)
      external testfun

      data types/ '_DOUBLE', '_REAL', '_INTEGER' /

      data spreads/ AST__SINC, AST__NEAREST, AST__LINEAR,
     :              AST__SINCSINC, AST__SINCCOS, AST__SINCGAUSS /


      if( status .ne. sai__ok ) return

*  Get the scalar properties of the test.
      call testfun( 0, name, types(1),
     :              lbnd_in, ubnd_in, ipin, ipin_var,
     :              lbnd_out, ubnd_out, ipout, ipout_var,
     :              lbnd, ubnd, m, params, tol, j, status )

*  Get the number of input and output axes.
      nin = ast_geti( m, 'Nin', status )
      nout = ast_geti( m, 'Nout', status )

*  Get no. of pixels in entire input array.
      nel_in = 1
      do i = 1, nin
         nel_in = nel_in*( ubnd_in( i ) - lbnd_in( i ) + 1 )
      end do

*  Get no. of pixels in entire output array.
      nel_out = 1
      do i = 1, nout
         nel_out = nel_out*( ubnd_out( i ) - lbnd_out( i ) + 1 )
      end do

*  Loop round all data types.
      do i = 1, 3

*  Allocate memory for input and output data and variance arrays
         call psx_calloc( nel_in, types(i), ipin, status )
         call psx_calloc( nel_in, types(i), ipin_var, status )

         call psx_calloc( nel_out, types(i), ipout, status )
         call psx_calloc( nel_out, types(i), ipout_var, status )

*  Loop round all spread functions
         do j = 1, 6

*  Get the scalar properties of the test. This may change the Mapping.
            call testfun( 0, name, types(i),
     :              lbnd_in, ubnd_in, ipin, ipin_var,
     :              lbnd_out, ubnd_out, ipout, ipout_var,
     :              lbnd, ubnd, m, params, tol, spreads(j), status )

*  Fill the input data and variance arrays using the supplied function.
            call testfun( 1, name, types(i),
     :                 lbnd_in, ubnd_in, ipin, ipin_var,
     :                 lbnd_out, ubnd_out, ipout, ipout_var,
     :                 lbnd, ubnd, m, params, tol, spreads(j),
     :                 status )

*  Rebin the input data using the AST function appropriate to the
*  supplied data type.
            if( types(i) .eq. '_REAL' ) then
               call ast_rebinr( m, 0.0D0, nin, lbnd_in, ubnd_in,
     :              %val( cnf_pval( ipin )), %val( cnf_pval(ipin_var )),
     :              spreads(j), params,
     :              AST__USEBAD+AST__USEVAR, tol, 100, VAL__BADR,
     :              nout, lbnd_out, ubnd_out,
     :              lbnd, ubnd, %val( cnf_pval( ipout )),
     :              %val( cnf_pval( ipout_var )), status )

            else if( types(i) .eq. '_DOUBLE' ) then
               call ast_rebind( m, 0.0D0, nin, lbnd_in, ubnd_in,
     :              %val( cnf_pval( ipin )), %val( cnf_pval(ipin_var )),
     :              spreads(j), params,
     :              AST__USEBAD+AST__USEVAR, tol, 100, VAL__BADD,
     :              nout, lbnd_out, ubnd_out,
     :              lbnd, ubnd, %val( cnf_pval( ipout ) ),
     :              %val( cnf_pval( ipout_var )), status )

            else if( types(i) .eq. '_INTEGER' ) then
               call ast_rebini( m, 0.0D0, nin, lbnd_in, ubnd_in,
     :              %val( cnf_pval( ipin )), %val( cnf_pval(ipin_var )),
     :              spreads(j), params,
     :              AST__USEBAD+AST__USEVAR, tol, 100, VAL__BADI,
     :              nout, lbnd_out, ubnd_out,
     :              lbnd, ubnd, %val( cnf_pval( ipout )),
     :              %val( cnf_pval( ipout_var )), status )

            else if( status .eq. sai__ok ) then
               status = SAI__ERROR
               call msg_setc( 'T', types(i) )
               call err_rep( ' ', 'Bad data type (^T) supplied to '//
     :                       'rebin', status )
            end if

*  Call the supplied function to test the results.
            call testfun( 2, name, types(i),
     :                    lbnd_in, ubnd_in, ipin, ipin_var,
     :                    lbnd_out, ubnd_out, ipout, ipout_var,
     :                    lbnd, ubnd, m, params, tol,
     :                    spreads(j), status )

*  Report the data type and spread function if an error occurred, and
*  abort.
            if( status .ne. sai__ok ) then
               call msg_seti( 'sf', j )
               call msg_setc( 'dt', types( i ) )
               call msg_setc( 't', name )
               call err_rep( ' ', '^t failed: Spread function ^sf '//
     :                       'data type ^dt', status )
               go to 999
            end if

         end do

*  Free resources.
         call psx_free( ipout, status )
         call psx_free( ipout_var, status )
         call psx_free( ipin, status )
         call psx_free( ipin_var, status )
      end do

 999  continue

      end

      LOGICAL FUNCTION EQUALB( A, B )
      IMPLICIT NONE
      BYTE A, B
      EQUALB = ( A .EQ. B )
      END

      LOGICAL FUNCTION EQUALD( A, B )
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
      DOUBLE PRECISION A, B
      IF( A .NE. 0.0D0 .AND. B .NE. 0.0D0 ) THEN
         EQUALD = ( ABS( A - B ) .LE. 1.0E9*ABS( A + B )*VAL__EPSD )
      ELSE
         EQUALD = ( ABS( A + B ) .LE. 1.0D-11 )
      END IF

      END

      LOGICAL FUNCTION MYEQUALD( A, B )
      IMPLICIT NONE
      DOUBLE PRECISION A, B
      LOGICAL EQUALD
      MYEQUALD = EQUALD( A, B )
      END

      LOGICAL FUNCTION EQUALI( A, B )
      IMPLICIT NONE
      INTEGER A, B
      EQUALI = ( A .EQ. B )

      END

      LOGICAL FUNCTION EQUALR( A, B )
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
      REAL A, B

      IF( A .NE. 0.0 .AND. B .NE. 0.0 ) THEN
         EQUALR = ( ABS( A - B ) .LE. 50.0*ABS( A + B )*VAL__EPSR )
      ELSE
         EQUALR = ( ABS( A + B ) .LE. 1.0E-11 )
      END IF

      END

      LOGICAL FUNCTION EQUALW( A, B )
      IMPLICIT NONE
      INTEGER*2 A, B
      EQUALW = ( A .EQ. B )
      END




* -----------------------------------------------
*  Test 7
*

      SUBROUTINE TEST7( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST7'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST7R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST7D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST7I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST7',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST7D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, NZ
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), SUM, KT
      DOUBLE PRECISION TOL, PARAMS(*), K
      LOGICAL EQUALD, MYEQUALD, GOOD

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_SHIFTMAP( 1, 1.5D0, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = 0
            IN_VAR( I - LBND_IN(1) + 1 ) = K
         END DO
         IN( 14 - LBND_IN(1) + 1 ) = K

*  Otherwise check output data and variance arrays look right.
      ELSE

         SUM = 0
         DO I = LBND_OUT(1), UBND_OUT(1)
            IF( OUT( I - LBND_OUT(1) + 1) .NE. VAL__BADD ) THEN
               SUM = SUM + OUT( I - LBND_OUT(1) + 1)
            END IF
         END DO

         KT = K


         IF( 'D' .EQ. 'R' .OR. 'D' .EQ. 'D' ) THEN
            GOOD = EQUALD( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 3 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST7D Data sum is ^S should be ^K',
     :                    STATUS )
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            NZ = 0
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( OUT( I - LBND_OUT(1) + 1) .NE. 0 .AND.
     :             OUT( I - LBND_OUT(1) + 1) .NE. VAL__BADD ) THEN
                  IF( NZ .EQ. 0 ) THEN
                     NZ = NZ + 1
                     IF( OUT( I - LBND_OUT(1) + 1) .NE. KT ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', I )
                        CALL MSG_SETD( 'D1',
     :                      DBLE( OUT( I - LBND_OUT(1) + 1)))
                        CALL MSG_SETD( 'K', dble( KT ) )
                        CALL ERR_REP( ' ', 'TEST7D ^I: ^D1 ^K',
     :                                STATUS )
                     END IF
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', I )
                     CALL MSG_SETD( 'D1',
     :                   DBLE( OUT( I - LBND_OUT(1) + 1)))
                     CALL ERR_REP( ' ', 'TEST7D ^I: ^D1',
     :               STATUS )
                  END IF
               END IF
            END DO

         ELSE
            DO I = 0, 3
               IF( .NOT. EQUALD( OUT( 15 - I - LBND_OUT(1) + 1 ),
     :                     OUT( 16 + I - LBND_OUT(1) + 1 ) ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'I1', 15 - I )
                  CALL MSG_SETI( 'I2', 16 + I )
                  CALL MSG_SETD( 'D1',
     :                            DBLE( OUT( 15 - I - LBND_OUT(1) + 1)))
                  CALL MSG_SETD( 'D2',
     :                            DBLE( OUT( 16 + I - LBND_OUT(1) + 1)))
                  CALL ERR_REP( ' ', 'TEST7D ^I1 (^D1) != '//
     :                          '^I2 (^D2)', STATUS )
               END IF
            END DO
         END IF

      END IF

      END


      SUBROUTINE TEST7I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, NZ
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), SUM, KT
      DOUBLE PRECISION TOL, PARAMS(*), K
      LOGICAL EQUALI, MYEQUALD, GOOD

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_SHIFTMAP( 1, 1.5D0, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = 0
            IN_VAR( I - LBND_IN(1) + 1 ) = K
         END DO
         IN( 14 - LBND_IN(1) + 1 ) = K

*  Otherwise check output data and variance arrays look right.
      ELSE

         SUM = 0
         DO I = LBND_OUT(1), UBND_OUT(1)
            IF( OUT( I - LBND_OUT(1) + 1) .NE. VAL__BADI ) THEN
               SUM = SUM + OUT( I - LBND_OUT(1) + 1)
            END IF
         END DO

         KT = K


         IF( 'I' .EQ. 'R' .OR. 'I' .EQ. 'D' ) THEN
            GOOD = EQUALI( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 3 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST7I Data sum is ^S should be ^K',
     :                    STATUS )
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            NZ = 0
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( OUT( I - LBND_OUT(1) + 1) .NE. 0 .AND.
     :             OUT( I - LBND_OUT(1) + 1) .NE. VAL__BADI ) THEN
                  IF( NZ .EQ. 0 ) THEN
                     NZ = NZ + 1
                     IF( OUT( I - LBND_OUT(1) + 1) .NE. KT ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', I )
                        CALL MSG_SETD( 'D1',
     :                      DBLE( OUT( I - LBND_OUT(1) + 1)))
                        CALL MSG_SETD( 'K', dble( KT ) )
                        CALL ERR_REP( ' ', 'TEST7I ^I: ^D1 ^K',
     :                                STATUS )
                     END IF
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', I )
                     CALL MSG_SETD( 'D1',
     :                   DBLE( OUT( I - LBND_OUT(1) + 1)))
                     CALL ERR_REP( ' ', 'TEST7I ^I: ^D1',
     :               STATUS )
                  END IF
               END IF
            END DO

         ELSE
            DO I = 0, 3
               IF( .NOT. EQUALI( OUT( 15 - I - LBND_OUT(1) + 1 ),
     :                     OUT( 16 + I - LBND_OUT(1) + 1 ) ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'I1', 15 - I )
                  CALL MSG_SETI( 'I2', 16 + I )
                  CALL MSG_SETD( 'D1',
     :                            DBLE( OUT( 15 - I - LBND_OUT(1) + 1)))
                  CALL MSG_SETD( 'D2',
     :                            DBLE( OUT( 16 + I - LBND_OUT(1) + 1)))
                  CALL ERR_REP( ' ', 'TEST7I ^I1 (^D1) != '//
     :                          '^I2 (^D2)', STATUS )
               END IF
            END DO
         END IF

      END IF

      END


      SUBROUTINE TEST7R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, NZ
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), SUM, KT
      DOUBLE PRECISION TOL, PARAMS(*), K
      LOGICAL EQUALR, MYEQUALD, GOOD

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_SHIFTMAP( 1, 1.5D0, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = 0
            IN_VAR( I - LBND_IN(1) + 1 ) = K
         END DO
         IN( 14 - LBND_IN(1) + 1 ) = K

*  Otherwise check output data and variance arrays look right.
      ELSE

         SUM = 0
         DO I = LBND_OUT(1), UBND_OUT(1)
            IF( OUT( I - LBND_OUT(1) + 1) .NE. VAL__BADR ) THEN
               SUM = SUM + OUT( I - LBND_OUT(1) + 1)
            END IF
         END DO

         KT = K


         IF( 'R' .EQ. 'R' .OR. 'R' .EQ. 'D' ) THEN
            GOOD = EQUALR( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 3 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST7R Data sum is ^S should be ^K',
     :                    STATUS )
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            NZ = 0
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( OUT( I - LBND_OUT(1) + 1) .NE. 0 .AND.
     :             OUT( I - LBND_OUT(1) + 1) .NE. VAL__BADR ) THEN
                  IF( NZ .EQ. 0 ) THEN
                     NZ = NZ + 1
                     IF( OUT( I - LBND_OUT(1) + 1) .NE. KT ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', I )
                        CALL MSG_SETD( 'D1',
     :                      DBLE( OUT( I - LBND_OUT(1) + 1)))
                        CALL MSG_SETD( 'K', dble( KT ) )
                        CALL ERR_REP( ' ', 'TEST7R ^I: ^D1 ^K',
     :                                STATUS )
                     END IF
                  ELSE
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', I )
                     CALL MSG_SETD( 'D1',
     :                   DBLE( OUT( I - LBND_OUT(1) + 1)))
                     CALL ERR_REP( ' ', 'TEST7R ^I: ^D1',
     :               STATUS )
                  END IF
               END IF
            END DO

         ELSE
            DO I = 0, 3
               IF( .NOT. EQUALR( OUT( 15 - I - LBND_OUT(1) + 1 ),
     :                     OUT( 16 + I - LBND_OUT(1) + 1 ) ) ) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'I1', 15 - I )
                  CALL MSG_SETI( 'I2', 16 + I )
                  CALL MSG_SETD( 'D1',
     :                            DBLE( OUT( 15 - I - LBND_OUT(1) + 1)))
                  CALL MSG_SETD( 'D2',
     :                            DBLE( OUT( 16 + I - LBND_OUT(1) + 1)))
                  CALL ERR_REP( ' ', 'TEST7R ^I1 (^D1) != '//
     :                          '^I2 (^D2)', STATUS )
               END IF
            END DO
         END IF

      END IF

      END




* -----------------------------------------------
*  Test 8
*

      SUBROUTINE TEST8( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST8'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST8R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST8D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST8I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST8',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST8D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, NZ,
     :        II, JJ, KK
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), SUM, KT
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(2)
      LOGICAL EQUALD, GOOD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 10000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -2
         UBND_IN( 1 ) = 3
         LBND_OUT( 1 ) = -2
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -2
         UBND( 1 ) = 3
         LBND_IN( 2 ) = 0
         UBND_IN( 2 ) = 5
         LBND_OUT( 2 ) = 0
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 0
         UBND( 2 ) = 5
         SHIFTS(1) = 0.5D0
         SHIFTS(2) = -0.5D0
         M = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = 0
               IN_VAR( K ) = KFAC
               K = K + 1
            END DO
         END DO
         IN( 21 ) = KFAC

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 0
         SUM = 0
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               K = K + 1
               IF( OUT( K ) .NE. VAL__BADD ) THEN
                  SUM = SUM + OUT( K )
               END IF
            END DO
         END DO

         KT = KFAC

         IF( 'D' .EQ. 'R' .OR. 'D' .EQ. 'D' ) THEN
            GOOD = EQUALD( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 5 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST8D Data sum is ^S should be ^K',
     :                    STATUS )
            GO TO 999
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            NZ = 0
            K = 0
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  IF( OUT( K ) .NE. 0 .AND.
     :                OUT( K ) .NE. VAL__BADD ) THEN
                     IF( NZ .EQ. 0 ) THEN
                        NZ = NZ + 1
                        IF( OUT( K ) .NE. KT ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETI( 'K', K )
                           CALL MSG_SETD( 'D1', DBLE( OUT( K )))
                           CALL MSG_SETD( 'KT', DBLE( KT ) )
                           CALL ERR_REP( ' ', 'TEST8D ^K: ^D1 ^KT',
     :                                   STATUS )
                           GO TO 999
                        END IF
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'K', K )
                        CALL MSG_SETD( 'D1', DBLE( OUT( K )))
                        CALL ERR_REP( ' ', 'TEST8D ^K: ^D1',
     :                                STATUS )
                        GO TO 999
                     END IF
                  END IF
               END DO
            END DO
         ELSE
            K = 0
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  II = 1 - I
                  JJ = 5 - J
                  IF( II .GE. LBND_OUT(1) .AND.
     :                II .LE. UBND_OUT(1) .AND.
     :                JJ .GE. LBND_OUT(2) .AND.
     :                JJ .LE. UBND_OUT(2) ) THEN
                     KK = 6*JJ + ( II + 3 )

                     IF( .NOT. EQUALD( OUT( KK ), OUT( K ) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'KK', KK )
                        CALL MSG_SETI( 'K', K )
                        CALL MSG_SETD( 'D1', DBLE( OUT(KK) ) )
                        CALL MSG_SETD( 'D2', DBLE( OUT(K) ) )
                        CALL ERR_REP( ' ', 'TEST8D ^KK (^D1) != '//
     :                                '^K (^D2)', STATUS )
                        GO TO 999
                     END IF
                  END IF
               END DO
            END DO
         END IF

      END IF

 999  CONTINUE

      END



      SUBROUTINE TEST8I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, NZ,
     :        II, JJ, KK
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), SUM, KT
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(2)
      LOGICAL EQUALI, GOOD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 10000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -2
         UBND_IN( 1 ) = 3
         LBND_OUT( 1 ) = -2
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -2
         UBND( 1 ) = 3
         LBND_IN( 2 ) = 0
         UBND_IN( 2 ) = 5
         LBND_OUT( 2 ) = 0
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 0
         UBND( 2 ) = 5
         SHIFTS(1) = 0.5D0
         SHIFTS(2) = -0.5D0
         M = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = 0
               IN_VAR( K ) = KFAC
               K = K + 1
            END DO
         END DO
         IN( 21 ) = KFAC

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 0
         SUM = 0
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               K = K + 1
               IF( OUT( K ) .NE. VAL__BADI ) THEN
                  SUM = SUM + OUT( K )
               END IF
            END DO
         END DO

         KT = KFAC

         IF( 'I' .EQ. 'R' .OR. 'I' .EQ. 'D' ) THEN
            GOOD = EQUALI( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 5 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST8I Data sum is ^S should be ^K',
     :                    STATUS )
            GO TO 999
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            NZ = 0
            K = 0
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  IF( OUT( K ) .NE. 0 .AND.
     :                OUT( K ) .NE. VAL__BADI ) THEN
                     IF( NZ .EQ. 0 ) THEN
                        NZ = NZ + 1
                        IF( OUT( K ) .NE. KT ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETI( 'K', K )
                           CALL MSG_SETD( 'D1', DBLE( OUT( K )))
                           CALL MSG_SETD( 'KT', DBLE( KT ) )
                           CALL ERR_REP( ' ', 'TEST8I ^K: ^D1 ^KT',
     :                                   STATUS )
                           GO TO 999
                        END IF
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'K', K )
                        CALL MSG_SETD( 'D1', DBLE( OUT( K )))
                        CALL ERR_REP( ' ', 'TEST8I ^K: ^D1',
     :                                STATUS )
                        GO TO 999
                     END IF
                  END IF
               END DO
            END DO
         ELSE
            K = 0
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  II = 1 - I
                  JJ = 5 - J
                  IF( II .GE. LBND_OUT(1) .AND.
     :                II .LE. UBND_OUT(1) .AND.
     :                JJ .GE. LBND_OUT(2) .AND.
     :                JJ .LE. UBND_OUT(2) ) THEN
                     KK = 6*JJ + ( II + 3 )

                     IF( .NOT. EQUALI( OUT( KK ), OUT( K ) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'KK', KK )
                        CALL MSG_SETI( 'K', K )
                        CALL MSG_SETD( 'D1', DBLE( OUT(KK) ) )
                        CALL MSG_SETD( 'D2', DBLE( OUT(K) ) )
                        CALL ERR_REP( ' ', 'TEST8I ^KK (^D1) != '//
     :                                '^K (^D2)', STATUS )
                        GO TO 999
                     END IF
                  END IF
               END DO
            END DO
         END IF

      END IF

 999  CONTINUE

      END



      SUBROUTINE TEST8R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, NZ,
     :        II, JJ, KK
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), SUM, KT
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(2)
      LOGICAL EQUALR, GOOD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 10000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -2
         UBND_IN( 1 ) = 3
         LBND_OUT( 1 ) = -2
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -2
         UBND( 1 ) = 3
         LBND_IN( 2 ) = 0
         UBND_IN( 2 ) = 5
         LBND_OUT( 2 ) = 0
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 0
         UBND( 2 ) = 5
         SHIFTS(1) = 0.5D0
         SHIFTS(2) = -0.5D0
         M = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = 0
               IN_VAR( K ) = KFAC
               K = K + 1
            END DO
         END DO
         IN( 21 ) = KFAC

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 0
         SUM = 0
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               K = K + 1
               IF( OUT( K ) .NE. VAL__BADR ) THEN
                  SUM = SUM + OUT( K )
               END IF
            END DO
         END DO

         KT = KFAC

         IF( 'R' .EQ. 'R' .OR. 'R' .EQ. 'D' ) THEN
            GOOD = EQUALR( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 5 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST8R Data sum is ^S should be ^K',
     :                    STATUS )
            GO TO 999
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            NZ = 0
            K = 0
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  IF( OUT( K ) .NE. 0 .AND.
     :                OUT( K ) .NE. VAL__BADR ) THEN
                     IF( NZ .EQ. 0 ) THEN
                        NZ = NZ + 1
                        IF( OUT( K ) .NE. KT ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETI( 'K', K )
                           CALL MSG_SETD( 'D1', DBLE( OUT( K )))
                           CALL MSG_SETD( 'KT', DBLE( KT ) )
                           CALL ERR_REP( ' ', 'TEST8R ^K: ^D1 ^KT',
     :                                   STATUS )
                           GO TO 999
                        END IF
                     ELSE
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'K', K )
                        CALL MSG_SETD( 'D1', DBLE( OUT( K )))
                        CALL ERR_REP( ' ', 'TEST8R ^K: ^D1',
     :                                STATUS )
                        GO TO 999
                     END IF
                  END IF
               END DO
            END DO
         ELSE
            K = 0
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  II = 1 - I
                  JJ = 5 - J
                  IF( II .GE. LBND_OUT(1) .AND.
     :                II .LE. UBND_OUT(1) .AND.
     :                JJ .GE. LBND_OUT(2) .AND.
     :                JJ .LE. UBND_OUT(2) ) THEN
                     KK = 6*JJ + ( II + 3 )

                     IF( .NOT. EQUALR( OUT( KK ), OUT( K ) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'KK', KK )
                        CALL MSG_SETI( 'K', K )
                        CALL MSG_SETD( 'D1', DBLE( OUT(KK) ) )
                        CALL MSG_SETD( 'D2', DBLE( OUT(K) ) )
                        CALL ERR_REP( ' ', 'TEST8R ^KK (^D1) != '//
     :                                '^K (^D2)', STATUS )
                        GO TO 999
                     END IF
                  END IF
               END DO
            END DO
         END IF

      END IF

 999  CONTINUE

      END





* -----------------------------------------------
*  Test 9
*

      SUBROUTINE TEST9( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST9'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST9R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST9D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST9I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST9',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST9D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), KT, SUM
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(3), G(3), W
      LOGICAL EQUALD, GOOD, MYEQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 10000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 0
         UBND_IN( 1 ) = 6
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 6
         LBND( 1 ) = 0
         UBND( 1 ) = 6
         LBND_IN( 2 ) = 0
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 0
         UBND_OUT( 2 ) = 6
         LBND( 2 ) = 0
         UBND( 2 ) = 6
         LBND_IN( 3 ) = 0
         UBND_IN( 3 ) = 6
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 6
         LBND( 3 ) = 0
         UBND( 3 ) = 6

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            SHIFTS(1) = 1.7D0
            SHIFTS(2) = 2.1D0
            SHIFTS(3) = -1.2D0
         ELSE
            SHIFTS(1) = 0.5D0
            SHIFTS(2) = 0.0D0
            SHIFTS(3) = -0.5D0
         END IF

         M = AST_SHIFTMAP( 3, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = 0
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO
         IN( 172 ) = KFAC

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 0
         SUM = 0
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  IF( OUT( K ) .NE. VAL__BADD ) THEN
                     SUM = SUM + OUT( K )
                  END IF
               END DO
            END DO
         END DO

         KT = KFAC

         IF( 'D' .EQ. 'R' .OR. 'D' .EQ. 'D' ) THEN
            GOOD = EQUALD( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 5 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST9D Data sum is ^S should be ^K',
     :                    STATUS )
            GO TO 999
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            K = 0
            DO L = LBND_OUT(3), UBND_OUT(3)
               DO J = LBND_OUT(2), UBND_OUT(2)
                  DO I = LBND_OUT(1), UBND_OUT(1)
                     K = K + 1
                     IF( K .EQ. 139 ) THEN
                        IF( .NOT. EQUALD( OUT(K), KT ) ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETD( 'K', DBLE( KT ) )
                           CALL MSG_SETD( 'O', DBLE( OUT(K) ) )
                           CALL ERR_REP( ' ', 'TEST9D El. 139 is '//
     :                                   '^O should be ^K', STATUS )
                           GO TO 999
                        END IF
                     ELSE
                        IF( .NOT. EQUALD( OUT(K), 0.0D0 ) ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETI( 'K', K )
                           CALL MSG_SETD( 'O', DBLE( OUT(K) ) )
                           CALL ERR_REP( ' ', 'TEST9D El. ^K is '//
     :                                   '^O should be zero', STATUS )
                           GO TO 999
                        END IF
                     END IF
                  END DO
               END DO
            END DO
         ELSE

            G(1) = 0.0
            G(2) = 0.0
            G(3) = 0.0
            W = 0.0
            K = 0
            DO L = LBND_OUT(3), UBND_OUT(3)
               DO J = LBND_OUT(2), UBND_OUT(2)
                  DO I = LBND_OUT(1), UBND_OUT(1)
                     K = K + 1
                     G(1) = G(1) + DBLE( I*OUT( k ) )
                     G(2) = G(2) + DBLE( J*OUT( K ) )
                     G(3) = G(3) + DBLE( L*OUT( K ) )
                     W = W + DBLE( OUT( K ) )
                  END DO
               END DO
            END DO

            IF( .NOT. MYEQUALD( G(1)/W, 3.5D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(1)/W )
               CALL ERR_REP( ' ', 'TEST9D Mean X is ^A '//
     :                       ' should be 3.5', STATUS )
               GO TO 999
            ELSE IF( .NOT. MYEQUALD( G(2)/W, 3.0D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(2)/W )
               CALL ERR_REP( ' ', 'TEST9D Mean Y is ^A '//
     :                       ' should be 3.0', STATUS )
               GO TO 999
            ELSE IF( .NOT. MYEQUALD( G(3)/W, 2.5D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(3)/W )
               CALL ERR_REP( ' ', 'TEST9D Mean Z is ^A '//
     :                       ' should be 2.5', STATUS )
               GO TO 999
            END IF

         END IF
      END IF

 999  CONTINUE

      END



      SUBROUTINE TEST9I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), KT, SUM
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(3), G(3), W
      LOGICAL EQUALI, GOOD, MYEQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 10000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 0
         UBND_IN( 1 ) = 6
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 6
         LBND( 1 ) = 0
         UBND( 1 ) = 6
         LBND_IN( 2 ) = 0
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 0
         UBND_OUT( 2 ) = 6
         LBND( 2 ) = 0
         UBND( 2 ) = 6
         LBND_IN( 3 ) = 0
         UBND_IN( 3 ) = 6
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 6
         LBND( 3 ) = 0
         UBND( 3 ) = 6

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            SHIFTS(1) = 1.7D0
            SHIFTS(2) = 2.1D0
            SHIFTS(3) = -1.2D0
         ELSE
            SHIFTS(1) = 0.5D0
            SHIFTS(2) = 0.0D0
            SHIFTS(3) = -0.5D0
         END IF

         M = AST_SHIFTMAP( 3, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = 0
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO
         IN( 172 ) = KFAC

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 0
         SUM = 0
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  IF( OUT( K ) .NE. VAL__BADI ) THEN
                     SUM = SUM + OUT( K )
                  END IF
               END DO
            END DO
         END DO

         KT = KFAC

         IF( 'I' .EQ. 'R' .OR. 'I' .EQ. 'D' ) THEN
            GOOD = EQUALI( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 5 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST9I Data sum is ^S should be ^K',
     :                    STATUS )
            GO TO 999
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            K = 0
            DO L = LBND_OUT(3), UBND_OUT(3)
               DO J = LBND_OUT(2), UBND_OUT(2)
                  DO I = LBND_OUT(1), UBND_OUT(1)
                     K = K + 1
                     IF( K .EQ. 139 ) THEN
                        IF( .NOT. EQUALI( OUT(K), KT ) ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETD( 'K', DBLE( KT ) )
                           CALL MSG_SETD( 'O', DBLE( OUT(K) ) )
                           CALL ERR_REP( ' ', 'TEST9I El. 139 is '//
     :                                   '^O should be ^K', STATUS )
                           GO TO 999
                        END IF
                     ELSE
                        IF( .NOT. EQUALI( OUT(K), 0  ) ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETI( 'K', K )
                           CALL MSG_SETD( 'O', DBLE( OUT(K) ) )
                           CALL ERR_REP( ' ', 'TEST9I El. ^K is '//
     :                                   '^O should be zero', STATUS )
                           GO TO 999
                        END IF
                     END IF
                  END DO
               END DO
            END DO
         ELSE

            G(1) = 0.0
            G(2) = 0.0
            G(3) = 0.0
            W = 0.0
            K = 0
            DO L = LBND_OUT(3), UBND_OUT(3)
               DO J = LBND_OUT(2), UBND_OUT(2)
                  DO I = LBND_OUT(1), UBND_OUT(1)
                     K = K + 1
                     G(1) = G(1) + DBLE( I*OUT( k ) )
                     G(2) = G(2) + DBLE( J*OUT( K ) )
                     G(3) = G(3) + DBLE( L*OUT( K ) )
                     W = W + DBLE( OUT( K ) )
                  END DO
               END DO
            END DO

            IF( .NOT. MYEQUALD( G(1)/W, 3.5D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(1)/W )
               CALL ERR_REP( ' ', 'TEST9I Mean X is ^A '//
     :                       ' should be 3.5', STATUS )
               GO TO 999
            ELSE IF( .NOT. MYEQUALD( G(2)/W, 3.0D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(2)/W )
               CALL ERR_REP( ' ', 'TEST9I Mean Y is ^A '//
     :                       ' should be 3.0', STATUS )
               GO TO 999
            ELSE IF( .NOT. MYEQUALD( G(3)/W, 2.5D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(3)/W )
               CALL ERR_REP( ' ', 'TEST9I Mean Z is ^A '//
     :                       ' should be 2.5', STATUS )
               GO TO 999
            END IF

         END IF
      END IF

 999  CONTINUE

      END



      SUBROUTINE TEST9R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*), KT, SUM
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(3), G(3), W
      LOGICAL EQUALR, GOOD, MYEQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 10000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 0
         UBND_IN( 1 ) = 6
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 6
         LBND( 1 ) = 0
         UBND( 1 ) = 6
         LBND_IN( 2 ) = 0
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 0
         UBND_OUT( 2 ) = 6
         LBND( 2 ) = 0
         UBND( 2 ) = 6
         LBND_IN( 3 ) = 0
         UBND_IN( 3 ) = 6
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 6
         LBND( 3 ) = 0
         UBND( 3 ) = 6

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            SHIFTS(1) = 1.7D0
            SHIFTS(2) = 2.1D0
            SHIFTS(3) = -1.2D0
         ELSE
            SHIFTS(1) = 0.5D0
            SHIFTS(2) = 0.0D0
            SHIFTS(3) = -0.5D0
         END IF

         M = AST_SHIFTMAP( 3, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = 0
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO
         IN( 172 ) = KFAC

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 0
         SUM = 0
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)
                  K = K + 1
                  IF( OUT( K ) .NE. VAL__BADR ) THEN
                     SUM = SUM + OUT( K )
                  END IF
               END DO
            END DO
         END DO

         KT = KFAC

         IF( 'R' .EQ. 'R' .OR. 'R' .EQ. 'D' ) THEN
            GOOD = EQUALR( SUM, KT )
         ELSE
            GOOD = ( ABS( SUM - KT ) .LT. 5 )
         END IF

         IF( .NOT. GOOD ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'K', dble( KT ) )
            CALL MSG_SETD( 'S', dble( SUM ) )
            CALL ERR_REP( ' ', 'TEST9R Data sum is ^S should be ^K',
     :                    STATUS )
            GO TO 999
         END IF

         IF( SPREAD .EQ. AST__NEAREST ) THEN
            K = 0
            DO L = LBND_OUT(3), UBND_OUT(3)
               DO J = LBND_OUT(2), UBND_OUT(2)
                  DO I = LBND_OUT(1), UBND_OUT(1)
                     K = K + 1
                     IF( K .EQ. 139 ) THEN
                        IF( .NOT. EQUALR( OUT(K), KT ) ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETD( 'K', DBLE( KT ) )
                           CALL MSG_SETD( 'O', DBLE( OUT(K) ) )
                           CALL ERR_REP( ' ', 'TEST9R El. 139 is '//
     :                                   '^O should be ^K', STATUS )
                           GO TO 999
                        END IF
                     ELSE
                        IF( .NOT. EQUALR( OUT(K), 0.0E0 ) ) THEN
                           STATUS = SAI__ERROR
                           CALL MSG_SETI( 'K', K )
                           CALL MSG_SETD( 'O', DBLE( OUT(K) ) )
                           CALL ERR_REP( ' ', 'TEST9R El. ^K is '//
     :                                   '^O should be zero', STATUS )
                           GO TO 999
                        END IF
                     END IF
                  END DO
               END DO
            END DO
         ELSE

            G(1) = 0.0
            G(2) = 0.0
            G(3) = 0.0
            W = 0.0
            K = 0
            DO L = LBND_OUT(3), UBND_OUT(3)
               DO J = LBND_OUT(2), UBND_OUT(2)
                  DO I = LBND_OUT(1), UBND_OUT(1)
                     K = K + 1
                     G(1) = G(1) + DBLE( I*OUT( k ) )
                     G(2) = G(2) + DBLE( J*OUT( K ) )
                     G(3) = G(3) + DBLE( L*OUT( K ) )
                     W = W + DBLE( OUT( K ) )
                  END DO
               END DO
            END DO

            IF( .NOT. MYEQUALD( G(1)/W, 3.5D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(1)/W )
               CALL ERR_REP( ' ', 'TEST9R Mean X is ^A '//
     :                       ' should be 3.5', STATUS )
               GO TO 999
            ELSE IF( .NOT. MYEQUALD( G(2)/W, 3.0D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(2)/W )
               CALL ERR_REP( ' ', 'TEST9R Mean Y is ^A '//
     :                       ' should be 3.0', STATUS )
               GO TO 999
            ELSE IF( .NOT. MYEQUALD( G(3)/W, 2.5D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETD( 'A', G(3)/W )
               CALL ERR_REP( ' ', 'TEST9R Mean Z is ^A '//
     :                       ' should be 2.5', STATUS )
               GO TO 999
            END IF

         END IF
      END IF

 999  CONTINUE

      END





* -----------------------------------------------
*  Test 1
*

      SUBROUTINE TEST1( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST1'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST1R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST1D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST1I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST1',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST1D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), STATUS, M, I, SPREAD
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      LOGICAL EQUALD, IGNORE
      DOUBLE PRECISION TOL, PARAMS(*), K

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_UNITMAP( 1, ' ', STATUS )
         IF( SPREAD .EQ. AST__GAUSS ) THEN
            PARAMS(1) = 2.0
            PARAMS(2) = 2.0
         ELSE
            PARAMS(1) = 2.0
            PARAMS(2) = 0.5
         END IF
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = I*K
            IN_VAR( I - LBND_IN(1) + 1 ) = I
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         DO I = LBND_OUT(1), UBND(1)
            IGNORE = ( SPREAD .EQ. AST__GAUSS .AND.
     :               ( I .LE. LBND_OUT(1) + 1 .OR.
     :                 I .GE. UBND(1) - 1 ) )
            IF( IGNORE ) THEN

            ELSE IF( .NOT. EQUALD( OUT( I - LBND_OUT(1) + 1 ),
     :                          IN( I - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1 ) ) )
               CALL MSG_SETD( 'B', DBLE( IN( I - LBND_IN(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST1D ^I: data ^V != ^B', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALD( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                            IN_VAR( I - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1) ) )
               CALL MSG_SETD( 'B', DBLE( IN_VAR(I-LBND_IN(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST1D ^I: variance ^V != ^B',
     :                       STATUS )
               RETURN
            END IF
         END DO
         DO I = UBND(1) + 1, UBND_OUT(1)
            IF( .NOT. EQUALD( OUT( I - LBND_OUT(1) + 1 ),
     :                          0.0D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1)))
               CALL ERR_REP( ' ', 'TEST1D ^I: ^V != 0.0', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALD( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                               0.0D0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1)))
               CALL ERR_REP( ' ', 'TEST1D ^I: variance ^V != 0.0',
     :                       STATUS )
               RETURN
            END IF
         END DO

      END IF

      END



      SUBROUTINE TEST1I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), STATUS, M, I, SPREAD
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      LOGICAL EQUALI, IGNORE
      DOUBLE PRECISION TOL, PARAMS(*), K

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_UNITMAP( 1, ' ', STATUS )
         IF( SPREAD .EQ. AST__GAUSS ) THEN
            PARAMS(1) = 2.0
            PARAMS(2) = 2.0
         ELSE
            PARAMS(1) = 2.0
            PARAMS(2) = 0.5
         END IF
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = I*K
            IN_VAR( I - LBND_IN(1) + 1 ) = I
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         DO I = LBND_OUT(1), UBND(1)
            IGNORE = ( SPREAD .EQ. AST__GAUSS .AND.
     :               ( I .LE. LBND_OUT(1) + 1 .OR.
     :                 I .GE. UBND(1) - 1 ) )
            IF( IGNORE ) THEN

            ELSE IF( .NOT. EQUALI( OUT( I - LBND_OUT(1) + 1 ),
     :                          IN( I - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1 ) ) )
               CALL MSG_SETD( 'B', DBLE( IN( I - LBND_IN(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST1I ^I: data ^V != ^B', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALI( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                            IN_VAR( I - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1) ) )
               CALL MSG_SETD( 'B', DBLE( IN_VAR(I-LBND_IN(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST1I ^I: variance ^V != ^B',
     :                       STATUS )
               RETURN
            END IF
         END DO
         DO I = UBND(1) + 1, UBND_OUT(1)
            IF( .NOT. EQUALI( OUT( I - LBND_OUT(1) + 1 ),
     :                          0  ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1)))
               CALL ERR_REP( ' ', 'TEST1I ^I: ^V != 0.0', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALI( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                               0  ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1)))
               CALL ERR_REP( ' ', 'TEST1I ^I: variance ^V != 0.0',
     :                       STATUS )
               RETURN
            END IF
         END DO

      END IF

      END



      SUBROUTINE TEST1R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), STATUS, M, I, SPREAD
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      LOGICAL EQUALR, IGNORE
      DOUBLE PRECISION TOL, PARAMS(*), K

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_UNITMAP( 1, ' ', STATUS )
         IF( SPREAD .EQ. AST__GAUSS ) THEN
            PARAMS(1) = 2.0
            PARAMS(2) = 2.0
         ELSE
            PARAMS(1) = 2.0
            PARAMS(2) = 0.5
         END IF
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = I*K
            IN_VAR( I - LBND_IN(1) + 1 ) = I
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         DO I = LBND_OUT(1), UBND(1)
            IGNORE = ( SPREAD .EQ. AST__GAUSS .AND.
     :               ( I .LE. LBND_OUT(1) + 1 .OR.
     :                 I .GE. UBND(1) - 1 ) )
            IF( IGNORE ) THEN

            ELSE IF( .NOT. EQUALR( OUT( I - LBND_OUT(1) + 1 ),
     :                          IN( I - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1 ) ) )
               CALL MSG_SETD( 'B', DBLE( IN( I - LBND_IN(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST1R ^I: data ^V != ^B', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALR( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                            IN_VAR( I - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1) ) )
               CALL MSG_SETD( 'B', DBLE( IN_VAR(I-LBND_IN(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST1R ^I: variance ^V != ^B',
     :                       STATUS )
               RETURN
            END IF
         END DO
         DO I = UBND(1) + 1, UBND_OUT(1)
            IF( .NOT. EQUALR( OUT( I - LBND_OUT(1) + 1 ),
     :                          0.0E0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1)))
               CALL ERR_REP( ' ', 'TEST1R ^I: ^V != 0.0', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALR( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                               0.0E0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1)))
               CALL ERR_REP( ' ', 'TEST1R ^I: variance ^V != 0.0',
     :                       STATUS )
               RETURN
            END IF
         END DO

      END IF

      END





* -----------------------------------------------
*  Test 2
*

      SUBROUTINE TEST2( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST2'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST2R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST2D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST2I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST2',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST2D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), STATUS, M, I, J, K, SPREAD
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC
      LOGICAL EQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         M = AST_UNITMAP( 2, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = K*KFAC
               IN_VAR( K ) = K
               K = K + 1
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( K .LE. 4 .OR. MOD( K, 4 ) .EQ. 0 .OR.
     :                           MOD( K, 4 ) .EQ. 3 ) THEN
                  IF( .NOT. EQUALD( OUT( K ), 0.0D0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     CALL ERR_REP( ' ', 'TEST2D ^I: ^V != 0',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALD( OUT_VAR( K ), 0.0D0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                     CALL ERR_REP( ' ', 'TEST2D ^I: variance ^V '//
     :                             '!= 0', STATUS )
                     RETURN
                  END IF
               ELSE
                  IF( .NOT. EQUALD( OUT( K ), IN( K - 3 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADD ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN( K - 3 ) ) )
                     CALL ERR_REP( ' ', 'TEST2D ^I: data ^V != ^B',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALD( OUT( K ), IN( K-3 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADD ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN_VAR( K - 3 ) ) )
                     CALL ERR_REP( ' ',
     :                             'TEST2D ^I: variance ^V != ^B',
     :                             STATUS )
                     RETURN
                  END IF
               END IF
               K = K + 1
            END DO
         END DO

      END IF

      END



      SUBROUTINE TEST2I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), STATUS, M, I, J, K, SPREAD
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC
      LOGICAL EQUALI

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         M = AST_UNITMAP( 2, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = K*KFAC
               IN_VAR( K ) = K
               K = K + 1
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( K .LE. 4 .OR. MOD( K, 4 ) .EQ. 0 .OR.
     :                           MOD( K, 4 ) .EQ. 3 ) THEN
                  IF( .NOT. EQUALI( OUT( K ), 0  ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     CALL ERR_REP( ' ', 'TEST2I ^I: ^V != 0',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALI( OUT_VAR( K ), 0  ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                     CALL ERR_REP( ' ', 'TEST2I ^I: variance ^V '//
     :                             '!= 0', STATUS )
                     RETURN
                  END IF
               ELSE
                  IF( .NOT. EQUALI( OUT( K ), IN( K - 3 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADI ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN( K - 3 ) ) )
                     CALL ERR_REP( ' ', 'TEST2I ^I: data ^V != ^B',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALI( OUT( K ), IN( K-3 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADI ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN_VAR( K - 3 ) ) )
                     CALL ERR_REP( ' ',
     :                             'TEST2I ^I: variance ^V != ^B',
     :                             STATUS )
                     RETURN
                  END IF
               END IF
               K = K + 1
            END DO
         END DO

      END IF

      END



      SUBROUTINE TEST2R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), STATUS, M, I, J, K, SPREAD
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC
      LOGICAL EQUALR

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         M = AST_UNITMAP( 2, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = K*KFAC
               IN_VAR( K ) = K
               K = K + 1
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( K .LE. 4 .OR. MOD( K, 4 ) .EQ. 0 .OR.
     :                           MOD( K, 4 ) .EQ. 3 ) THEN
                  IF( .NOT. EQUALR( OUT( K ), 0.0E0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     CALL ERR_REP( ' ', 'TEST2R ^I: ^V != 0',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALR( OUT_VAR( K ), 0.0E0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                     CALL ERR_REP( ' ', 'TEST2R ^I: variance ^V '//
     :                             '!= 0', STATUS )
                     RETURN
                  END IF
               ELSE
                  IF( .NOT. EQUALR( OUT( K ), IN( K - 3 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADR ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN( K - 3 ) ) )
                     CALL ERR_REP( ' ', 'TEST2R ^I: data ^V != ^B',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALR( OUT( K ), IN( K-3 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADR ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN_VAR( K - 3 ) ) )
                     CALL ERR_REP( ' ',
     :                             'TEST2R ^I: variance ^V != ^B',
     :                             STATUS )
                     RETURN
                  END IF
               END IF
               K = K + 1
            END DO
         END DO

      END IF

      END





* -----------------------------------------------
*  Test 3
*

      SUBROUTINE TEST3( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST3'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST3R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST3D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST3I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST3',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST3D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :  SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC
      LOGICAL EQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         LBND_IN( 3 ) = -1
         UBND_IN( 3 ) = 1
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 2
         LBND( 3 ) = -1
         UBND( 3 ) = 1
         M = AST_UNITMAP( 3, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = K*KFAC
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)

                  K2 = MOD( K - 1, 16 ) + 1
                  IF( K2 .LE. 4 .OR. MOD( K2, 4 ) .EQ. 0 .OR.
     :                               MOD( K2, 4 ) .EQ. 3 .OR.
     :                               L .EQ. 2 ) THEN
                     IF( .NOT. EQUALD( OUT( K ), 0.0D0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST3D ^I: ^V != 0',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALD( OUT_VAR( K ),
     ;                                        0.0D0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT_VAR( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST3D ^I: variance ^V '//
     :                                '!= 0', STATUS )
                        RETURN
                     END IF
                  ELSE
                     IF( .NOT. EQUALD( OUT( K ), IN( K + 13 ))) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN( K+13 ) ) )
                        CALL ERR_REP( ' ', 'TEST3D ^I: data ^V != ^B',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALD( OUT( K ), IN(K+13) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN_VAR( K+13 ) ) )
                        CALL ERR_REP( ' ',
     :                                'TEST3D ^I: variance ^V != ^B',
     :                                STATUS )
                        RETURN
                     END IF
                  END IF
                  K = K + 1
               END DO
            END DO
         END DO
      END IF

      END



      SUBROUTINE TEST3I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :  SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC
      LOGICAL EQUALI

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         LBND_IN( 3 ) = -1
         UBND_IN( 3 ) = 1
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 2
         LBND( 3 ) = -1
         UBND( 3 ) = 1
         M = AST_UNITMAP( 3, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = K*KFAC
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)

                  K2 = MOD( K - 1, 16 ) + 1
                  IF( K2 .LE. 4 .OR. MOD( K2, 4 ) .EQ. 0 .OR.
     :                               MOD( K2, 4 ) .EQ. 3 .OR.
     :                               L .EQ. 2 ) THEN
                     IF( .NOT. EQUALI( OUT( K ), 0  ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST3I ^I: ^V != 0',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALI( OUT_VAR( K ),
     ;                                        0  ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT_VAR( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST3I ^I: variance ^V '//
     :                                '!= 0', STATUS )
                        RETURN
                     END IF
                  ELSE
                     IF( .NOT. EQUALI( OUT( K ), IN( K + 13 ))) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN( K+13 ) ) )
                        CALL ERR_REP( ' ', 'TEST3I ^I: data ^V != ^B',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALI( OUT( K ), IN(K+13) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN_VAR( K+13 ) ) )
                        CALL ERR_REP( ' ',
     :                                'TEST3I ^I: variance ^V != ^B',
     :                                STATUS )
                        RETURN
                     END IF
                  END IF
                  K = K + 1
               END DO
            END DO
         END DO
      END IF

      END



      SUBROUTINE TEST3R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :  SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC
      LOGICAL EQUALR

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         LBND_IN( 3 ) = -1
         UBND_IN( 3 ) = 1
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 2
         LBND( 3 ) = -1
         UBND( 3 ) = 1
         M = AST_UNITMAP( 3, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = K*KFAC
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)

                  K2 = MOD( K - 1, 16 ) + 1
                  IF( K2 .LE. 4 .OR. MOD( K2, 4 ) .EQ. 0 .OR.
     :                               MOD( K2, 4 ) .EQ. 3 .OR.
     :                               L .EQ. 2 ) THEN
                     IF( .NOT. EQUALR( OUT( K ), 0.0E0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST3R ^I: ^V != 0',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALR( OUT_VAR( K ),
     ;                                        0.0E0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT_VAR( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST3R ^I: variance ^V '//
     :                                '!= 0', STATUS )
                        RETURN
                     END IF
                  ELSE
                     IF( .NOT. EQUALR( OUT( K ), IN( K + 13 ))) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN( K+13 ) ) )
                        CALL ERR_REP( ' ', 'TEST3R ^I: data ^V != ^B',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALR( OUT( K ), IN(K+13) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN_VAR( K+13 ) ) )
                        CALL ERR_REP( ' ',
     :                                'TEST3R ^I: variance ^V != ^B',
     :                                STATUS )
                        RETURN
                     END IF
                  END IF
                  K = K + 1
               END DO
            END DO
         END DO
      END IF

      END





* -----------------------------------------------
*  Test 4
*

      SUBROUTINE TEST4( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST4'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST4R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST4D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST4I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST4',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST4D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), K
      LOGICAL EQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_SHIFTMAP( 1, 3.0D0, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = I*K
            IN_VAR( I - LBND_IN(1) + 1 ) = I
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE

         DO I = LBND_OUT(1), LBND(1) + 2
            IF( .NOT. EQUALD( OUT( I - LBND_OUT(1) + 1 ),
     :                          0.0D0) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST4D ^I: ^V != BAD', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALD( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                          0.0D0) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST4D ^I: variance ^V != BAD',
     :                       STATUS )
               RETURN
            END IF
         END DO

         DO I =  LBND(1) + 3, UBND_OUT(1)
            IF( .NOT. EQUALD( OUT( I - LBND_OUT(1) + 1 ),
     :                          IN( I - 3 - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               IF( OUT( I - LBND_OUT(1) + 1 ) .NE. VAL__BADD ) THEN
                  CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1)))
               ELSE
                  CALL MSG_SETC( 'V', 'BAD' )
               END IF
               CALL MSG_SETD( 'B', DBLE( IN( I -3 - LBND_IN(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST4D ^I: data ^V != ^B', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALD( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                         IN_VAR( I - 3 - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               IF( OUT_VAR(I-LBND_OUT(1)+1) .NE. VAL__BADD ) THEN
                  CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1)))
               ELSE
                  CALL MSG_SETC( 'V', 'BAD' )
               END IF
               CALL MSG_SETD( 'B', DBLE( IN_VAR(I-3-LBND_IN(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST4D ^I: variance ^V != ^B',
     :                       STATUS )
               RETURN
            END IF
         END DO

      END IF

      END

      SUBROUTINE TEST4I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), K
      LOGICAL EQUALI

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_SHIFTMAP( 1, 3.0D0, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = I*K
            IN_VAR( I - LBND_IN(1) + 1 ) = I
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE

         DO I = LBND_OUT(1), LBND(1) + 2
            IF( .NOT. EQUALI( OUT( I - LBND_OUT(1) + 1 ),
     :                          0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST4I ^I: ^V != BAD', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALI( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                          0 ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST4I ^I: variance ^V != BAD',
     :                       STATUS )
               RETURN
            END IF
         END DO

         DO I =  LBND(1) + 3, UBND_OUT(1)
            IF( .NOT. EQUALI( OUT( I - LBND_OUT(1) + 1 ),
     :                          IN( I - 3 - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               IF( OUT( I - LBND_OUT(1) + 1 ) .NE. VAL__BADI ) THEN
                  CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1)))
               ELSE
                  CALL MSG_SETC( 'V', 'BAD' )
               END IF
               CALL MSG_SETD( 'B', DBLE( IN( I -3 - LBND_IN(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST4I ^I: data ^V != ^B', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALI( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                         IN_VAR( I - 3 - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               IF( OUT_VAR(I-LBND_OUT(1)+1) .NE. VAL__BADI ) THEN
                  CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1)))
               ELSE
                  CALL MSG_SETC( 'V', 'BAD' )
               END IF
               CALL MSG_SETD( 'B', DBLE( IN_VAR(I-3-LBND_IN(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST4I ^I: variance ^V != ^B',
     :                       STATUS )
               RETURN
            END IF
         END DO

      END IF

      END

      SUBROUTINE TEST4R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), K
      LOGICAL EQUALR

      IF( STATUS .NE. SAI__OK ) RETURN

      K = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = 10
         UBND_IN( 1 ) = 19
         LBND_OUT( 1 ) = 12
         UBND_OUT( 1 ) = 20
         LBND( 1 ) = 11
         UBND( 1 ) = 17
         M = AST_SHIFTMAP( 1, 3.0D0, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.1

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         DO I = LBND_IN(1), UBND_IN(1)
            IN( I - LBND_IN(1) + 1 ) = I*K
            IN_VAR( I - LBND_IN(1) + 1 ) = I
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE

         DO I = LBND_OUT(1), LBND(1) + 2
            IF( .NOT. EQUALR( OUT( I - LBND_OUT(1) + 1 ),
     :                          0.0E0) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST4R ^I: ^V != BAD', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALR( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                          0.0E0) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST4R ^I: variance ^V != BAD',
     :                       STATUS )
               RETURN
            END IF
         END DO

         DO I =  LBND(1) + 3, UBND_OUT(1)
            IF( .NOT. EQUALR( OUT( I - LBND_OUT(1) + 1 ),
     :                          IN( I - 3 - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               IF( OUT( I - LBND_OUT(1) + 1 ) .NE. VAL__BADR ) THEN
                  CALL MSG_SETD( 'V', DBLE( OUT( I - LBND_OUT(1) + 1)))
               ELSE
                  CALL MSG_SETC( 'V', 'BAD' )
               END IF
               CALL MSG_SETD( 'B', DBLE( IN( I -3 - LBND_IN(1) + 1 ) ) )
               CALL ERR_REP( ' ', 'TEST4R ^I: data ^V != ^B', STATUS )
               RETURN
            ELSE IF( .NOT. EQUALR( OUT_VAR( I - LBND_OUT(1) + 1 ),
     :                         IN_VAR( I - 3 - LBND_IN(1) + 1 ) ) ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI( 'I', I )
               IF( OUT_VAR(I-LBND_OUT(1)+1) .NE. VAL__BADR ) THEN
                  CALL MSG_SETD( 'V', DBLE( OUT_VAR(I-LBND_OUT(1)+1)))
               ELSE
                  CALL MSG_SETC( 'V', 'BAD' )
               END IF
               CALL MSG_SETD( 'B', DBLE( IN_VAR(I-3-LBND_IN(1)+1) ) )
               CALL ERR_REP( ' ', 'TEST4R ^I: variance ^V != ^B',
     :                       STATUS )
               RETURN
            END IF
         END DO

      END IF

      END



* -----------------------------------------------
*  Test 5
*

      SUBROUTINE TEST5( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST5'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST5R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST5D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST5I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST5',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST5D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(2)
      LOGICAL EQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         SHIFTS(1) = 3.0D0
         SHIFTS(2) = -1.0D0
         M = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = K*KFAC
               IN_VAR( K ) = K
               K = K + 1
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( MOD( K - 1, 4 ) .LT. 2 ) THEN
                  IF( .NOT. EQUALD( OUT( K ), 0.0D0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADD ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL ERR_REP( ' ', 'TEST5D ^I: ^V != BAD',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALD( OUT_VAR( K ),
     :                                     0.0D0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT_VAR( K ) .NE. VAL__BADD ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL ERR_REP( ' ', 'TEST5D ^I: variance ^V '//
     :                             '!= BAD', STATUS )
                     RETURN
                  END IF
               ELSE
                  IF( .NOT. EQUALD( OUT( K ), IN( K - 2 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADD ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN( K - 2 ) ) )
                     CALL ERR_REP( ' ', 'TEST5D ^I: data ^V != ^B',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALD( OUT( K ), IN( K-2 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADD ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN_VAR( K - 2 ) ) )
                     CALL ERR_REP( ' ',
     :                             'TEST5D ^I: variance ^V != ^B',
     :                             STATUS )
                     RETURN
                  END IF
               END IF
               K = K + 1
            END DO
         END DO

      END IF

      END



      SUBROUTINE TEST5I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(2)
      LOGICAL EQUALI

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         SHIFTS(1) = 3.0D0
         SHIFTS(2) = -1.0D0
         M = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = K*KFAC
               IN_VAR( K ) = K
               K = K + 1
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( MOD( K - 1, 4 ) .LT. 2 ) THEN
                  IF( .NOT. EQUALI( OUT( K ), 0  ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADI ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL ERR_REP( ' ', 'TEST5I ^I: ^V != BAD',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALI( OUT_VAR( K ),
     :                                     0  ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT_VAR( K ) .NE. VAL__BADI ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL ERR_REP( ' ', 'TEST5I ^I: variance ^V '//
     :                             '!= BAD', STATUS )
                     RETURN
                  END IF
               ELSE
                  IF( .NOT. EQUALI( OUT( K ), IN( K - 2 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADI ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN( K - 2 ) ) )
                     CALL ERR_REP( ' ', 'TEST5I ^I: data ^V != ^B',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALI( OUT( K ), IN( K-2 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADI ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN_VAR( K - 2 ) ) )
                     CALL ERR_REP( ' ',
     :                             'TEST5I ^I: variance ^V != ^B',
     :                             STATUS )
                     RETURN
                  END IF
               END IF
               K = K + 1
            END DO
         END DO

      END IF

      END



      SUBROUTINE TEST5R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(2)
      LOGICAL EQUALR

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         SHIFTS(1) = 3.0D0
         SHIFTS(2) = -1.0D0
         M = AST_SHIFTMAP( 2, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO J = LBND_IN(2), UBND_IN(2)
            DO I = LBND_IN(1), UBND_IN(1)
               IN( K ) = K*KFAC
               IN_VAR( K ) = K
               K = K + 1
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO J = LBND_OUT(2), UBND_OUT(2)
            DO I = LBND_OUT(1), UBND_OUT(1)
               IF( MOD( K - 1, 4 ) .LT. 2 ) THEN
                  IF( .NOT. EQUALR( OUT( K ), 0.0E0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADR ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL ERR_REP( ' ', 'TEST5R ^I: ^V != BAD',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALR( OUT_VAR( K ),
     :                                     0.0E0 ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT_VAR( K ) .NE. VAL__BADR ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL ERR_REP( ' ', 'TEST5R ^I: variance ^V '//
     :                             '!= BAD', STATUS )
                     RETURN
                  END IF
               ELSE
                  IF( .NOT. EQUALR( OUT( K ), IN( K - 2 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADR ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN( K - 2 ) ) )
                     CALL ERR_REP( ' ', 'TEST5R ^I: data ^V != ^B',
     :                             STATUS )
                     RETURN
                  ELSE IF( .NOT. EQUALR( OUT( K ), IN( K-2 ) ) ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'I', K )
                     IF( OUT( K ) .NE. VAL__BADR ) THEN
                        CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                     ELSE
                        CALL MSG_SETC( 'V', 'BAD' )
                     END IF
                     CALL MSG_SETD( 'B', DBLE( IN_VAR( K - 2 ) ) )
                     CALL ERR_REP( ' ',
     :                             'TEST5R ^I: variance ^V != ^B',
     :                             STATUS )
                     RETURN
                  END IF
               END IF
               K = K + 1
            END DO
         END DO

      END IF

      END





* -----------------------------------------------
*  Test 6
*

      SUBROUTINE TEST6( DO, NAME, TYPE,
     :                  LBND_IN, UBND_IN, IPIN, IPIN_VAR,
     :                  LBND_OUT, UBND_OUT, IPOUT, IPOUT_VAR,
     :                  LBND, UBND, M, PARAMS, TOL, J, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'

      INTEGER M, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        LBND(*), UBND(*), IPIN, IPIN_VAR, IPOUT, IPOUT_VAR,
     :        STATUS, DO, J
      DOUBLE PRECISION TOL, PARAMS(*)
      CHARACTER TYPE*(*), NAME*(*)

      IF( STATUS .NE. SAI__OK ) RETURN

      NAME = 'TEST6'

*  Fill the input data and variance arrays if required.
      IF( TYPE .EQ. '_REAL' ) THEN
         CALL TEST6R( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_DOUBLE' ) THEN
         CALL TEST6D( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( TYPE .EQ. '_INTEGER' ) THEN
         CALL TEST6I( DO, LBND_IN, UBND_IN, %VAL(CNF_PVAL(IPIN)),
     :                %VAL(CNF_PVAL(IPIN_VAR)), LBND_OUT, UBND_OUT,
     :                %VAL(CNF_PVAL(IPOUT)),%VAL(CNF_PVAL(IPOUT_VAR)),
     :                LBND, UBND, M, PARAMS, TOL, J, STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) then
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( ' ', 'Bad data type (^T) supplied to TEST6',
     :                    STATUS )
      END IF

      END




      SUBROUTINE TEST6D( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      DOUBLE PRECISION IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(3)
      LOGICAL EQUALD

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_DTOD( VAL__MAXD )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         LBND_IN( 3 ) = -1
         UBND_IN( 3 ) = 1
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 2
         LBND( 3 ) = -1
         UBND( 3 ) = 1
         SHIFTS(1) = 3.0D0
         SHIFTS(2) = -1.0D0
         SHIFTS(3) = 1.0D0
         M = AST_SHIFTMAP( 3, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = K*KFAC
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)

                  K2 = MOD( K - 1, 16 ) + 1
                  IF( MOD( K2 - 1, 4 ) .LT. 2 ) THEN
                     IF( .NOT. EQUALD( OUT( K ), 0.0D0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST6D ^I: ^V != BAD',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALD( OUT_VAR( K ),
     :                                        0.0D0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT_VAR( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST6D ^I: variance ^V '//
     :                                '!= BAD', STATUS )
                        RETURN
                     END IF
                  ELSE
                     IF( .NOT. EQUALD( OUT( K ), IN( K - 2 ))) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN( K-2 ) ) )
                        CALL ERR_REP( ' ', 'TEST6D ^I: data ^V != ^B',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALD( OUT( K ), IN(K-2) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADD ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN_VAR( K-2 ) ) )
                        CALL ERR_REP( ' ',
     :                                'TEST6D ^I: variance ^V != ^B',
     :                                STATUS )
                        RETURN
                     END IF
                  END IF
                  K = K + 1
               END DO
            END DO
         END DO
      END IF

      END



      SUBROUTINE TEST6I( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      INTEGER IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(3)
      LOGICAL EQUALI

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_ITOD( VAL__MAXI )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         LBND_IN( 3 ) = -1
         UBND_IN( 3 ) = 1
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 2
         LBND( 3 ) = -1
         UBND( 3 ) = 1
         SHIFTS(1) = 3.0D0
         SHIFTS(2) = -1.0D0
         SHIFTS(3) = 1.0D0
         M = AST_SHIFTMAP( 3, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = K*KFAC
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)

                  K2 = MOD( K - 1, 16 ) + 1
                  IF( MOD( K2 - 1, 4 ) .LT. 2 ) THEN
                     IF( .NOT. EQUALI( OUT( K ), 0  ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST6I ^I: ^V != BAD',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALI( OUT_VAR( K ),
     :                                        0  ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT_VAR( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST6I ^I: variance ^V '//
     :                                '!= BAD', STATUS )
                        RETURN
                     END IF
                  ELSE
                     IF( .NOT. EQUALI( OUT( K ), IN( K - 2 ))) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN( K-2 ) ) )
                        CALL ERR_REP( ' ', 'TEST6I ^I: data ^V != ^B',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALI( OUT( K ), IN(K-2) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADI ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN_VAR( K-2 ) ) )
                        CALL ERR_REP( ' ',
     :                                'TEST6I ^I: variance ^V != ^B',
     :                                STATUS )
                        RETURN
                     END IF
                  END IF
                  K = K + 1
               END DO
            END DO
         END DO
      END IF

      END



      SUBROUTINE TEST6R( DO, LBND_IN, UBND_IN, IN, IN_VAR, LBND_OUT,
     :                  UBND_OUT, OUT, OUT_VAR, LBND, UBND, M,
     :                  PARAMS, TOL, SPREAD, STATUS )

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'NUM_DEC'
      INCLUDE 'NUM_DEF'

      INTEGER DO, LBND_IN(*), UBND_IN(*), LBND_OUT(*), UBND_OUT(*),
     :        SPREAD, LBND(*), UBND(*), STATUS, M, I, J, K, L, K2
      REAL IN(*), IN_VAR(*), OUT(*), OUT_VAR(*)
      DOUBLE PRECISION TOL, PARAMS(*), KFAC, SHIFTS(3)
      LOGICAL EQUALR

      IF( STATUS .NE. SAI__OK ) RETURN

      KFAC = MIN( 1000.0D0, NUM_RTOD( VAL__MAXR )/20.0 )

*  Return the scalar parameters of the test if required.
      IF( DO .EQ. 0 ) THEN
         LBND_IN( 1 ) = -1
         UBND_IN( 1 ) = 2
         LBND_OUT( 1 ) = 0
         UBND_OUT( 1 ) = 3
         LBND( 1 ) = -1
         UBND( 1 ) = 1
         LBND_IN( 2 ) = 3
         UBND_IN( 2 ) = 6
         LBND_OUT( 2 ) = 2
         UBND_OUT( 2 ) = 5
         LBND( 2 ) = 3
         UBND( 2 ) = 6
         LBND_IN( 3 ) = -1
         UBND_IN( 3 ) = 1
         LBND_OUT( 3 ) = 0
         UBND_OUT( 3 ) = 2
         LBND( 3 ) = -1
         UBND( 3 ) = 1
         SHIFTS(1) = 3.0D0
         SHIFTS(2) = -1.0D0
         SHIFTS(3) = 1.0D0
         M = AST_SHIFTMAP( 3, SHIFTS, ' ', STATUS )
         PARAMS(1) = 2.0
         PARAMS(2) = 2.0
         TOL = 0.0

*  Fill the input data and variance arrays if required.
      ELSE IF( DO .EQ. 1 ) THEN
         K = 1
         DO L = LBND_IN(3), UBND_IN(3)
            DO J = LBND_IN(2), UBND_IN(2)
               DO I = LBND_IN(1), UBND_IN(1)
                  IN( K ) = K*KFAC
                  IN_VAR( K ) = K
                  K = K + 1
               END DO
            END DO
         END DO

*  Otherwise check output data and variance arrays look right.
      ELSE
         K = 1
         DO L = LBND_OUT(3), UBND_OUT(3)
            DO J = LBND_OUT(2), UBND_OUT(2)
               DO I = LBND_OUT(1), UBND_OUT(1)

                  K2 = MOD( K - 1, 16 ) + 1
                  IF( MOD( K2 - 1, 4 ) .LT. 2 ) THEN
                     IF( .NOT. EQUALR( OUT( K ), 0.0E0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST6R ^I: ^V != BAD',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALR( OUT_VAR( K ),
     :                                        0.0E0 ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT_VAR( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K )))
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL ERR_REP( ' ', 'TEST6R ^I: variance ^V '//
     :                                '!= BAD', STATUS )
                        RETURN
                     END IF
                  ELSE
                     IF( .NOT. EQUALR( OUT( K ), IN( K - 2 ))) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN( K-2 ) ) )
                        CALL ERR_REP( ' ', 'TEST6R ^I: data ^V != ^B',
     :                                STATUS )
                        RETURN
                     ELSE IF( .NOT. EQUALR( OUT( K ), IN(K-2) ) ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'I', K )
                        IF( OUT( K ) .NE. VAL__BADR ) THEN
                           CALL MSG_SETD( 'V', DBLE( OUT_VAR( K ) ) )
                        ELSE
                           CALL MSG_SETC( 'V', 'BAD' )
                        END IF
                        CALL MSG_SETD( 'B', DBLE( IN_VAR( K-2 ) ) )
                        CALL ERR_REP( ' ',
     :                                'TEST6R ^I: variance ^V != ^B',
     :                                STATUS )
                        RETURN
                     END IF
                  END IF
                  K = K + 1
               END DO
            END DO
         END DO
      END IF

      END



