.TITLE       DTASK
.FACILITY    DTASK,120/PREFIX=DTASK__


.SEVERITY	INFORMATIONAL

.BASE		100
REJECTED	<Action rejected - already in progress>
NOTFOUND	<Action not found in action list>
ACTACTIVE	<Action already active>
NOTACTIVE	<No obey active>
ACTSTART	<Action started>
ACTCOMPLETE	<Action complete>
ACTCANCEL	<Action cancelled>
ACTINFORM	<Action complete - text in value string>
ACTCONSTR	<Declared constraints on obey action not met>
CANCONSTR	<Declared constraints on cancel action not met>
IVACTSTAT	<Invalid status returned from ACT routine>
ILLCONTEXT	<Illegal ADAM message context>
TASKEXIT	<Task exiting>
NOSUBSIDACT	<ACT__MESSAGE status returned when no subsidiary actions>
INVCONTROL	<Invalid task control message received>
BADDEF	<Failed to set new default directory>
CMDLIN	<Failed to get command line>
ACTPAR	<Invalid parameter specification for GET/SET to monolith>

.SEVERITY	WARNING

.BASE		200
ACTREMOVED	<Action already removed from action list>
ACTNOTCANCEL	<Stage/wait/end status returned from cancel ACT>
ACTUNIMP	<Action not implemented in ACT>
BADSEQ	<Bad sequence number returned from ACT>

.SEVERITY	ERROR

.BASE		300
SCHEDTIMER	<Error in setting reschedule timer>
ILLACTSTATUS	<Illegal status in action list>
ACTOVF	<Action list overflow>
DUMP	<stack dump requested>
.END
