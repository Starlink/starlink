.TITLE       EMS
.FACILITY    EMS,104/PREFIX=EMS__


.SEVERITY	ERROR

.BASE		200
OPTER	<Error encountered during message output.>
NOMSG	<No error messages pending output.>

.BASE		300
UNSET	<Status not set in call to EMS_REP (improper use of EMS_REP).>
BADOK	<Status set to SAI__OK in call to EMS_REP (improper use of EMS_REP).>
NSTER	<Error in nested calls to EMS_BEGIN and EMS_END (improper use of EMS).>
BDKEY	<Invalid keyword will be ignored (improper use of EMS_TUNE).>
BTUNE	<Conflicting actions (improper use of EMS_TUNE).>
NOENV	<Error encountered reading environment variable EMS_TUNE.>

.BASE		400
EROVF	<Error stack overflow (EMS fault).>
CXOVF	<Context stack overflow (EMS fault).>
TEST    <Test facility error check.>
.END
