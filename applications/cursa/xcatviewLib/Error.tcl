proc Error error_text {

#+ Error

#  Report a message to the error window.

#  error_text (Given)
#    Name of the action in the ADAM application to be invoked.

#  Author:
#   ACD: A C Davenhall (Leicester)

#  History:
#   7/6/94  (ACD): First dated version.
#   1/11/01 (ACD): Added a space to the start of the message.  Adding
#     the space makes messages from the tcl scripts line up properly
#     with those from ADAM applications.  However, starting them with a
#     space rather than an exclamation mark makes it easy to distinguish
#     the two types of messsage.
#-

    .messages.output  configure  -state normal

    .messages.output insert end " "
    .messages.output insert end $error_text
    .messages.output insert end "\n"

    .messages.output  configure  -state disabled

    .messages.output  yview      -pickplace end

}
