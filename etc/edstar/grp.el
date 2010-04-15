(new-routine "GRP_COPY"
             "Copy a section of an existing group to a new group"
             '("IGRP" "INDXLO" "INDXHI" "REJECT" "IGRP2" "STATUS"))

(new-routine "GRP_DELET"
             "Delete a group from the GRP system"
             '("IGRP" "STATUS"))

(new-routine "GRP_GET"
             "Returns a set of names contained in a group"
             '("IGRP" "INDEX" "SIZE" "NAMES" "STATUS"))

(new-routine "GRP_GETCC"
             "Returns requested control characters for the specified group"
             '("IGRP" "CCLIST" "CC" "STATUS"))

(new-routine "GRP_GETCS"
             "Determine the case sensitivity of a group"
             '("IGRP" "SENSIT" "STATUS"))

(new-routine "GRP_GROUP"
             "Append a list of names obtained from the environment to a previously created group"
             '("PARAM" "IGRP1" "IGRP2" "SIZE" "ADDED" "FLAG" "STATUS"))

(new-routine "GRP_GRPEX"
             "Append a list of names contained within a supplied group expression to a previously created group"
             '("GRPEXP" "IGRP1" "IGRP2" "SIZE" "ADDED" "FLAG" "STATUS"))

(new-routine "GRP_GRPSZ"
             "Returns the number of names in a group"
             '("IGRP" "SIZE" "STATUS"))

(new-routine "GRP_GTYPE"
             "Retrieve the type string stored with a group"
             '("IGRP" "TYPE" "STATUS"))

(new-routine "GRP_INDEX"
             "Searches for a given name and if found, returns its index"
             '("NAME" "IGRP" "START" "INDEX" "STATUS"))

(new-routine "GRP_INFOC"
             "Retrieve an item of character information about a name"
             '("IGRP" "INDEX" "ITEM" "VALUE" "STATUS"))

(new-routine "GRP_INFOI"
             "Retrieve an item of integer information about a name"
             '("IGRP" "INDEX" "ITEM" "VALUE" "STATUS"))

(new-routine "GRP_LIST"
             "Write names to a text file specified by the environment"
             '("PARAM" "INDXLO" "INDXHI" "COMNT" "IGRP" "STATUS"))

(new-routine "GRP_LISTF"
             "Write names to a specified text file"
             '("FILENM" "INDXLO" "INDXHI" "COMNT" "IGRP" "STATUS"))

(new-routine "GRP_NEW"
             "Create a new empty group"
             '("TYPE" "IGRP" "STATUS"))

(new-routine "GRP_OWN"
             "Returns the identifier of the group which owns the specified group"
             '("IGRP1" "IGRP2" "STATUS"))

(new-routine "GRP_PTYPE"
             "Associate a new type string with a group"
             '("IGRP" "TYPE" "STATUS"))

(new-routine "GRP_PURGE"
             "Purge duplicate entries from a group"
             '("IGRP1" "IGRP2" "STATUS"))

(new-routine "GRP_PUT"
             "Put a given set of literal names into a group"
             '("IGRP" "SIZE" "NAMES" "INDEX" "STATUS"))

(new-routine "GRP_REMOV"
             "Remove all occurrences of a given name from a group"
             '("IGRP1" "NAME" "IGRP2" "STATUS"))

(new-routine "GRP_SETCC"
             "Sets requested control characters for the specified group"
             '("IGRP" "CCLIST" "CC" "STATUS"))

(new-routine "GRP_SETCS"
             "Establish the case sensitivity of a group"
             '("IGRP" "SENSIT" "STATUS"))

(new-routine "GRP_SETSZ"
             "Reduce the size of a group"
             '("IGRP" "SIZE" "STATUS"))

(new-routine "GRP_SOWN"
             "Establish one group as the owner of another group"
             '("IGRP1" "IGRP2" "STATUS"))

(new-routine "GRP_VALID"
             "Determine if a group identifier is valid"
             '("IGRP" "VALID" "STATUS"))
