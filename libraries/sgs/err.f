      SUBROUTINE sgs_1ERR (NERR, RNAME, MES, JSTAT)
*+
*   - - - -
*    E R R     (internal routine)
*   - - - -
*
*   Report SGS error.
*
*   Given:
*      NERR      i         SGS internal error number
*      RNAME     c*(*)     name of routine reporting error
*      MES       c*(*)     error message
*
*   Returned:
*      JSTAT     i         SGS error status
*
*  Externals:
*      ems_REP
*
*   Remarks:
*
*      The SGS internal error number and the SGS error status are the
*      same in this implementation.
*
*      The entire message (text plus routine name etc.) will be
*      truncated to 72 characters.
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      INTEGER NERR
      CHARACTER*(*) RNAME,MES
      INTEGER JSTAT

      CHARACTER LINE*72, ID*10



      JSTAT = NERR
      LINE =  'SGS_'//RNAME//' - '//MES
      ID = 'SGS__'//RNAME
      CALL EMS_REP(ID, LINE, JSTAT)

      END
