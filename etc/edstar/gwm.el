(new-routine "GWM_CLOSE"
             "Close the X client-server connection"
             '("STATUS"))

(new-routine "GWM_CRWIN"
             "Create a GWM window"
             '("WNAME" "STATUS"))

(new-routine "GWM_DSWIN"
             "Destroy a GWM window"
             '("WNAME" "STATUS"))

(new-routine "GWM_EXIST"
             "Inquire if a GWM window of the given name exists"
             '("WNAME" "EXISTS" "STATUS"))

(new-routine "GWM_GETCI"
             "Inquire the number of colours and the colour indices allocated to the given window"
             '("WNAME" "IDIM" "INDEXS" "NCOLS" "STATUS"))

(new-routine "GWM_OPEN"
             "Establish the X client-server connection"
             '("DISPLY" "USEDEF" "STATUS"))

(new-routine "GWM_WSETC"
             "Set an character string window option"
             '("OPTION" "VALUE" "STATUS"))

(new-routine "GWM_WSETI"
             "Set an integer window option"
             '("OPTION" "VALUE" "STATUS"))

(new-routine "GWM_WSETL"
             "Set a logical window option"
             '("OPTION" "VALUE" "STATUS"))
