*  This include file details the variables used by the 'context'
*  management routine ECH_SET_CONTEXT.  These variables are used
*  by ECH_REPORT, ECH_PROVIDE_HINTS etc.
      INTEGER CTX_ECHOMOP_SHELL
      PARAMETER ( CTX_ECHOMOP_SHELL = -1 )

      INTEGER CTX_FTRDB_CREATOR
      PARAMETER ( CTX_FTRDB_CREATOR = -2 )

      INTEGER CONTEXT_MODE

      COMMON / ECH_CONTEXT / CONTEXT_MODE
