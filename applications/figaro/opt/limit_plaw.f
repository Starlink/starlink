      subroutine LIMIT_PLAW( XC, n_Par)
*+
* Name:
*    LIMIT_PLAW

* Invocation:
*    CALL LIMIT_PLAW( XC, N_PAR)

* Purpose:
*   Subroutine to set the default limits for parameters

* Description:
*   Subroutine to set the default limits for parameters. These require
*   that the AMPLITUDE most be POSITVE and so must the INDEX
      Implicit None
*
* Include OPT

      include 'opt_cmn'
      Integer n_Par
      Double Precision XC( n_Par )
*-
      integer INDEX
      integer AMPLITUDE
      integer CENTRE
      Parameter (CENTRE = 3)
      Parameter (AMPLITUDE = 2)
      Parameter (INDEX = 1)
*
      If( lower_bound(AMPLITUDE) .eq. neg_large .and.
     :                   upper_bound(AMPLITUDE) .eq. pos_large ) Then
        Call LIMIT_PAR( AMPLITUDE, 0.0, pos_large )
*                                   Normalization > 0.0
      End If

      If( lower_bound(INDEX) .eq. neg_large
     :       .and. upper_bound(INDEX) .eq. pos_large) Then
        Call LIMIT_PAR( INDEX, 0.0, pos_large )
*                             Power Law Index > 0.0
      End If
*
      End
