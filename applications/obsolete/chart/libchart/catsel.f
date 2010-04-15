      SUBROUTINE CATSEL(STRING,NARR,N,INIT,IDNAM,IDBYTE, STATUS )
*+
*   Checks through the string to recognise catalogue
*   names by their abbreviations held in 'CATNAM' list
*
*   Gets
*   ----
*      STRING - The list of catalogue abbreviations
*      INIT   - 1st character to be tested in search for names
*      IDNAM  - The array of catalogue name abbreviations
*      IDBYTE - The number of bytes in the logical*1 array IDLIST
*
*   Returns
*   -------
*      NARR   - Integer array to hold the list of bit numbers
*      N      - Number of catalogue abbrevs. recognised
*
*   Gets and Returns
*   ----------------
*      STATUS - The global status.
*
*   History
*   -------
*     2-MAR-1993 (AJJB):
*        Added STATUS argument.
*     2-JUN-1993 (AJJB):
*        Removed IFAIL argument as it was never used, and STATUS
*        is used throughout Chart now to report success/failure.
*     3-JUN-1993 (AJJB):
*        Added a check to set STATUS to an error value and output an
*        error message if no catalogue abbreviations are recognised.
*-

      INCLUDE 'SAE_PAR'             ! Standard SAE constants

      INTEGER STATUS
      INTEGER NARR(*)
      CHARACTER IDNAM(*)*(*),STRING*(*)

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      N=0
      DO K=1,80
         IF (IDNAM(K).NE.' ') THEN
            IF (INDEX(STRING(INIT:),
     :         IDNAM(K)(:LENG(IDNAM(K)))).NE.0) THEN
                  N=N+1
                  NARR(N)=K
            ENDIF
         ENDIF
      ENDDO

* If no catalogue abbreviations recognised, output error message

      IF ( N .EQ. 0 ) THEN
         STATUS = SAI__ERROR

* If catalogue string equal to 'ALL', output a little message-ette to
* the user. This is really just an excuse to set STATUS to an error
* value, to suppress the message that CATINF (the calling routine) will
* otherwise output about which catalogues will be used.

         IF ( STRING(:3) .EQ. 'ALL' ) THEN
            CALL ERR_REP( ' ', 'CATALOGUE string set to ALL - ' //
     :                     'selecting from all catalogues. It is' //
     :                     ' better to do this by setting ' //
     :                     'SELECTION to ALL.', STATUS )

         ELSE

            CALL ERR_REP( ' ', 'Catalogue selection ' //
     :                 'string not recognised, will select from all' //
     :                 ' catalogues.', STATUS )

         ENDIF
      ENDIF

      END


