proc OpenRemote { } {

#+ OpenRemote
#
#  Extract and open a subset selected from a remote database.
#
#  Given:
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   26:3/97  (ACD): Original version.
#   26/3/98  (ACD): Explicitly specified the spawn id.  when closing
#     catview in order to avoid a core dump on Linux.
#   22/11/99 (ACD): Modified to use the standard tcl/tk 'exec' rather
#      than 'expect/spawn'.
#-

#
#   Declare the global variables.
#   ----------------------------

     global catviewExe

     global remoteCatsShort
     global remoteQueryCat

     global catalogueName




#
#   Attempt to determine the list of databases available on the remote
#   server and proceed if ok.  Note that the remote server is only
#   queried for the list if it is not already available.

     if {$remoteCatsShort == ""} then {
        RemoteList
     }

     if {$remoteCatsShort != ""} then {

#
#      Attempt to define the query and proceed if ok.

        GetRemoteQuery

        if {$remoteQueryCat != ""} then {
           RemoteQuery

           if {$catalogueName != ""} then {
              OpenCat remote
           } else {
              Error "Failure querying the remote catalogue."
           }

        } else {
           Error "Query not specified or invalid; the remote catalogue will not be accessed."
           set catalogueName  ""
        }

     } else {
        Error "Failed to obtain list of catalogues on the remote server."
        set catalogueName  ""
     }

}
