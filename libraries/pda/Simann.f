* ======================================================================
* NIST Guide to Available Math Software.
* Source for module SIMANN from package OPT.
* Retrieved from NETLIB on Fri Feb 10 04:53:47 1995.
* ======================================================================
C ABSTRACT:
C   Simulated annealing is a global optimization method that distinguishes
C   between different local optima. Starting from an initial point, the
C   algorithm takes a step and the function is evaluated. When minimizing a
C   function, any downhill step is accepted and the process repeats from this
C   new point. An uphill step may be accepted. Thus, it can escape from local
C   optima. This uphill decision is made by the Metropolis criteria. As the
C   optimization process proceeds, the length of the steps decline and the
C   algorithm closes in on the global optimum. Since the algorithm makes very
C   few assumptions regarding the function to be optimized, it is quite
C   robust with respect to non-quadratic surfaces. The degree of robustness
C   can be adjusted by the user. In fact, simulated annealing can be used as
C   a local optimizer for difficult functions.
C
C   This implementation of simulated annealing was used in "Global Optimization
C   of Statistical Functions with Simulated Annealing," Goffe, Ferrier and
C   Rogers, Journal of Econometrics, vol. 60, no. 1/2, Jan./Feb. 1994, pp.
C   65-100. Briefly, we found it competitive, if not superior, to multiple
C   restarts of conventional optimization routines for difficult optimization
C   problems.
C
C   For more information on this routine, contact its author:
C   Bill Goffe, bgoffe@whale.st.usm.edu
C
      PROGRAM SIMANN
C  This file is an example of the Corana et al. simulated annealing
C  algorithm for multimodal and robust optimization as implemented
C  and modified by Goffe, Ferrier and Rogers. Counting the above line
C  ABSTRACT as 1, the routine itself (PDA_SA), with its supplementary
C  routines, is on lines 232-990. A multimodal example from Judge et al.
C  (FCN) is on lines 150-231. The rest of this file (lines 1-149) is a
C  driver routine with values appropriate for the Judge example. Thus, this
C  example is ready to run.
C
C  To understand the algorithm, the documentation for PDA_SA on lines 236-
C  484 should be read along with the parts of the paper that describe
C  simulated annealing. Then the following lines will then aid the user
C  in becomming proficient with this implementation of simulated
C  annealing.
C
C  Learning to use PDA_SA:
C      Use the sample function from Judge with the following suggestions
C  to get a feel for how PDA_SA works. When you've done this, you should be
C  ready to use it on most any function with a fair amount of expertise.
C    1. Run the program as is to make sure it runs okay. Take a look at
C       the intermediate output and see how it optimizes as temperature
C       (T) falls. Notice how the optimal point is reached and how
C       falling T reduces VM.
C    2. Look through the documentation to PDA_SA so the following makes a
C       bit of sense. In line with the paper, it shouldn't be that hard
C       to figure out. The core of the algorithm is described on pp. 68-70
C       and on pp. 94-95. Also see Corana et al. pp. 264-9.
C    3. To see how it selects points and makes decisions about uphill
C       and downhill moves, set IPRINT = 3 (very detailed intermediate
C       output) and MAXEVL = 100 (only 100 function evaluations to limit
C       output).
C    4. To see the importance of different temperatures, try starting
C       with a very low one (say T = 10E-5). You'll see (i) it never
C       escapes from the local optima (in annealing terminology, it
C       quenches) & (ii) the step length (VM) will be quite small. This
C       is a key part of the algorithm: as temperature (T) falls, step
C       length does too. In a minor point here, note how VM is quickly
C       reset from its initial value. Thus, the input VM is not very
C       important. This is all the more reason to examine VM once the
C       algorithm is underway.
C    5. To see the effect of different parameters and their effect on
C       the speed of the algorithm, try RT = .95 & RT = .1. Notice the
C       vastly different speed for optimization. Also try NT = 20. Note
C       that this sample function is quite easy to optimize, so it will
C       tolerate big changes in these parameters. RT and NT are the
C       parameters one should adjust to modify the runtime of the
C       algorithm and its robustness.
C    6. Try constraining the algorithm with either LB or UB.

      PARAMETER (N = 2, NEPS = 4)

      DOUBLE PRECISION  LB(N), UB(N), X(N), XOPT(N), C(N), VM(N),
     1                  FSTAR(NEPS), XP(N), T, EPS, RT, FOPT

      INTEGER  NACP(N), NS, NT, NFCNEV, IER, ISEED1, ISEED2,
     1         MAXEVL, IPRINT, NACC, NOBDS

      LOGICAL  MAX

      EXTERNAL FCNXMP

C  Set underflows to zero on IBM mainframes.
C     CALL XUFLOW(0)

C  Set input parameters.
      MAX = .FALSE.
      EPS = 1.0D-6
      RT = .5
      ISEED1 = 1
      ISEED2 = 2
      NS = 20
      NT = 5
      MAXEVL = 100 000
      IPRINT = 1
      DO 10, I = 1, N
         LB(I) = -1.0D25
         UB(I) =  1.0D25
         C(I) = 2.0
10    CONTINUE

C  Note start at local, but not global, optima of the Judge function.
      X(1) =  2.354471
      X(2) = -0.319186

C  Set input values of the input/output parameters.
      T = 5.0
      DO 20, I = 1, N
         VM(I) = 1.0
 20   CONTINUE

      WRITE(*,1000) N, MAX, T, RT, EPS, NS, NT, NEPS, MAXEVL, IPRINT,
     1              ISEED1, ISEED2

      CALL PDA_PRTVEC(X,N,'STARTING VALUES')
      CALL PDA_PRTVEC(VM,N,'INITIAL STEP LENGTH')
      CALL PDA_PRTVEC(LB,N,'LOWER BOUND')
      CALL PDA_PRTVEC(UB,N,'UPPER BOUND')
      CALL PDA_PRTVEC(C,N,'C VECTOR')
      WRITE(*,'(/,''  ****   END OF DRIVER ROUTINE OUTPUT   ****''
     1          /,''  ****   BEFORE CALL TO PDA_SA.         ****'')')

      CALL PDA_SA(FCNXMP,
     3        N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,ISEED1,
     1        ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     2        FSTAR,XP,NACP)

      WRITE(*,'(/,''  ****   RESULTS AFTER PDA_SA   ****   '')')
      CALL PDA_PRTVEC(XOPT,N,'SOLUTION')
      CALL PDA_PRTVEC(VM,N,'FINAL STEP LENGTH')
      WRITE(*,1001) FOPT, NFCNEV, NACC, NOBDS, T, IER

1000  FORMAT(/,' SIMULATED ANNEALING EXAMPLE',/,
     1       /,' NUMBER OF PARAMETERS: ',I3,'   MAXIMAZATION: ',L5,
     2       /,' INITIAL TEMP: ', G8.2, '   RT: ',G8.2, '   EPS: ',G8.2,
     3       /,' NS: ',I3, '   NT: ',I2, '   NEPS: ',I2,
     4       /,' MAXEVL: ',I10, '   IPRINT: ',I1, '   ISEED1: ',I4,
     5       '   ISEED2: ',I4)
1001  FORMAT(/,' OPTIMAL FUNCTION VALUE: ',G20.13
     1       /,' NUMBER OF FUNCTION EVALUATIONS:     ',I10,
     2       /,' NUMBER OF ACCEPTED EVALUATIONS:     ',I10,
     3       /,' NUMBER OF OUT OF BOUND EVALUATIONS: ',I10,
     4       /,' FINAL TEMP: ', G20.13,'  IER: ', I3)

      STOP
      END

      SUBROUTINE FCNXMP(N,THETA,H)
C  This subroutine is from the example in Judge et al., The Theory and
C  Practice of Econometrics, 2nd ed., pp. 956-7. There are two optima:
C  F(.864,1.23) = 16.0817 (the global minumum) and F(2.35,-.319) = 20.9805.

      DOUBLE PRECISION THETA(2), H

      DOUBLE PRECISION Y(20), X2(20), X3(20)

      Y(1) = 4.284
      Y(2) = 4.149
      Y(3) = 3.877
      Y(4) = 0.533
      Y(5) = 2.211
      Y(6) = 2.389
      Y(7) = 2.145
      Y(8) = 3.231
      Y(9) = 1.998
      Y(10) = 1.379
      Y(11) = 2.106
      Y(12) = 1.428
      Y(13) = 1.011
      Y(14) = 2.179
      Y(15) = 2.858
      Y(16) = 1.388
      Y(17) = 1.651
      Y(18) = 1.593
      Y(19) = 1.046
      Y(20) = 2.152

      X2(1) =  .286
      X2(2) =  .973
      X2(3) =  .384
      X2(4) =  .276
      X2(5) =  .973
      X2(6) =  .543
      X2(7) =  .957
      X2(8) =  .948
      X2(9) =  .543
      X2(10) =  .797
      X2(11) =  .936
      X2(12) =  .889
      X2(13) =  .006
      X2(14) =  .828
      X2(15) =  .399
      X2(16) =  .617
      X2(17) =  .939
      X2(18) =  .784
      X2(19) =  .072
      X2(20) =  .889

      X3(1) = .645
      X3(2) = .585
      X3(3) = .310
      X3(4) = .058
      X3(5) = .455
      X3(6) = .779
      X3(7) = .259
      X3(8) = .202
      X3(9) = .028
      X3(10) = .099
      X3(11) = .142
      X3(12) = .296
      X3(13) = .175
      X3(14) = .180
      X3(15) = .842
      X3(16) = .039
      X3(17) = .103
      X3(18) = .620
      X3(19) = .158
      X3(20) = .704

      H = 0.0
      DO 100, I = 1, 20
         H = (THETA(1) + THETA(2)*X2(I) + (THETA(2)**2)*X3(I) - Y(I))**2
     1       + H
100   CONTINUE

      RETURN
      END
