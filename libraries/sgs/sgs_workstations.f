      PROGRAM WKSTN
*+
*   Program:
*
*      WKSTN
*
*   Description:
*
*      Lists the names and descriptions of all the available SGS
*      workstations on FORTRAN unit 6 (usually the terminal).
*
*   Usage:
*
*      RUN SGS_DIR:WORKSTATIONS
*+
*   Libraries Used:
*    
*      SGS
*    
*   D L Terrett   22-NOV-1988 

      IMPLICIT NONE

      CALL SGS_WLIST(6)
      
      END
