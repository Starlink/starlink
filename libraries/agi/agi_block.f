************************************************************************
*+   AGI_BLOCK - Block data for intialising counters

      BLOCK DATA AGI_BLOCK

*   Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*   Include the common blocks
      INCLUDE 'agi_cref'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*   Initialise the values
      DATA CREF / 0 /
      DATA CNEST / 1 /
      DATA CURPID / 0 /

      END

