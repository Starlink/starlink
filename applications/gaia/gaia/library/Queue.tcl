#+
#  Name:
#     Queue

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class for implementing a queue of strings.

#  Description:
#     This class provides a basic queue mechanism for storing strings.

#  Invocations:
#
#        Queue object_name
#
#     This creates an instance of a Queue object. The return is
#     the name of the object.
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Methods:
#     
#     push string
#
#   Push a string onto the queue.
#
#     pop
#
#   Retrieve a string from the queue. If queue is empty this is
#   returned as "", which may not be added to the queue.
#
#     size
#
#   Returns the number of strings in the queue.

#  Inheritance:
#     This class inherits no other classes.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     23-APR-1996 (PDRAPER):
#        Original version.
#     9-JUL-1996 (PDRAPER):
#        Converted to itcl2.0.
#     20-MAR-1998 (PDRAPER):
#        Modified from a Stack to a proper (FIFO) queue.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::Queue {
   constructor {} {}
   destructor {}

   #  Methods:
   #  --------

   public method push string {
      set queue_($high_) $string
      incr high_
   }
   public method pop {} {
      if { $high_ > $low_ } { 
         set value $queue_($low_)
         unset queue_($low_)
         incr low_
         return $value
      } else {
         return {}
      }
   }
   public method size {} {
      return [expr $high_-$low_]
   }

   #  Protected variables: (available to instance)
   #  --------------------
   protected variable low_ 0
   protected variable high_ 0
   protected variable queue_

#  End of class definition.
}
