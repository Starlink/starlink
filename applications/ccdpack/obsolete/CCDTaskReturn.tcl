proc CCDTaskReturn { args } {
   global CCDtaskout
   global CCDtaskreturn
   puts stdout "Task exited"
   puts stdout "Having written: $CCDtaskout"
}
# $Id$
