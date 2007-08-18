CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ST_ADAM.INC
C
C Include file to define common block for file and workspace s/routines
C
C  alan penny           ral         1994 Jul


        integer                 WS_NSIZE		!
        parameter               ( WS_NSIZE = 20 )
        integer                 WORK_LIMIT		!
        parameter               ( WORK_LIMIT = 100 )
        character*(WS_NSIZE)	WS_NAME(WORK_LIMIT)	! name of workspace
        integer                 CPNTR(WORK_LIMIT)	! pointer to workspace in char form
        integer                 WS_COUNTER		!

        integer                 NDF_LIMIT		!
        parameter               ( NDF_LIMIT = 250 )
        character*(WS_NSIZE)	NDF_NAME(NDF_LIMIT)     ! name of  NDF
        integer                 SNDF(NDF_LIMIT)		! NDF identifier
        integer                 NDF_COUNTER


        common / sman_wscha / ws_name
        common / sman_wsint / ws_counter, cpntr

        common / sman_ndfcha / ndf_name
        common / sman_ndfint / sndf, ndf_counter
