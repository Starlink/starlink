*.......................................................................
*   Common block storage for interactions

*   Number of active interactors
      INTEGER CINTN

*   Interactor type
      INTEGER CINTTY( 0 : MAXINT - 1 )

*   Interactor identifier
      INTEGER CINTID( 0 : MAXINT - 1 )

*   Onject type
      INTEGER COBJTY( 0 : MAXINT - 1 )

*   Object identifier
      INTEGER COBJID( 0 : MAXINT - 1 )

*   Interactive operation
      INTEGER CINTOP( 0 : MAXINT - 1 )

*   Exit trigger number
      INTEGER CEXTRN( 0 : MAXINT - 1 )

*   Interaction flags. Set to true by IKNENI.
      INTEGER CINTFL( 0 : MAXINT - 1 )

      COMMON / IKN_COMINT / CINTN, CINTTY, CINTID, COBJTY, COBJID,
     :                      CINTOP, CEXTRN, CINTFL

      SAVE / IKN_COMINT /

