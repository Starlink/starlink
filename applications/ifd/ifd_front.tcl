# History:
#    24-JUL-1996 (AJC):
#       Add procs version and alias/start/end
#    25-JUL-1996 (AJC):
#       Add helplib to package level
#    AUG/SEP-1996 (AJC):
#       Add Current prefix to proc package
#       Move exepath to package level
#       Fix up conditional evaluation
#    14-JAN-1996 (AJC):
#       Rename task parameter procedure to taskparam
#    18-MAR-1997 (AJC):
#       Add icl and csh procedure dummies
#    17-APR-1997 (AJC):
#       Add outputpar
#    22-APR-1997 (AJC):
#       Add repeated
#    25-APR-1997 (AJC):
#       Initialise Current(version) to PKG_VERS
#     1-MAY-1997 (AJC):
#       Add dynamic
#     9-JUN-1997 (AJC):
#       Add cl
#     9-OCT-1997 (AJC):
#       Initialise position to 0
#    14-OCT-1997 (AJC):
#       Add some comments in procedure unknown
#       Make Universe adam equivalent to star
#    20-OCT-1997 (AJC):
#       Add taskinherit
#-
proc package_start_hook {name} {}
proc package_end_hook {name} {}

proc comment_start_hook {comment} {}

proc display_start_hook {message} {}
proc display_end_hook {message} {}

proc executable_start_hook {name} {}
proc executable_end_hook {name} {}

proc action_start_hook {name} {}
proc action_end_hook {name} {}

proc alias_start_hook {name} {}
proc alias_end_hook {name} {}

proc parameter_start_hook {name} {}
proc parameter_end_hook {name} {}

proc message_start_hook {name} {}
proc message_end_hook {name} {}

proc helplib_start_hook {name} {}

proc defhelp_start_hook {topic entry lib} {}

proc command_start_hook {name} {}
proc command_end_hook {name} {}

proc print_start_hook {mess} {}
proc obey_start_hook {command} {}
proc task_start_hook {action} {}
proc task_end_hook {action} {}
proc taskparam_start_hook {action} {}
proc taskinherit_start_hook {action} {}
proc obsolete_start_hook {mess} {}


proc icl {text} {}
proc csh {text} {}
proc sh {text} {}
proc cl {text} {}

proc unknown {name args} {
    global Universe
    if [regexp :|! $name sep] {
       set temp [split $name :!]
       set name [lindex $temp 1]
       if {$name==""} {
          set name [lindex $args 0]
          set args [lreplace $args 0 0]
       }
       set tag [split [lindex $temp 0] ,]
#   Add star if adam is in tag
#   This will allow conditionals specifying the old adam: key or
#   the new star: key.
       if {[lsearch $tag "adam"] >= 0 } {
          lappend tag "star"
       }
       if { $sep == "!"} {
#       Only do this code if $Universe not in $tag
	  if {[lsearch $tag $Universe] == -1} {
#             eval [join [list $name $args] " "]
             eval $name $args
          }
       } {
#       Only do this code if $Universe is in $tag
	  if {[lsearch $tag $Universe] != -1} {
# eval $name [join $args ""]  good
# eval $name \{$args\}  bad
# eval eval $args    good
 eval $name $args
#puts "evaluating [join [list $name $args] {} ]"
#             eval [join [list $name $args] " "]
          }
       }
    } {
	global Context
	puts stderr "Unknown [lindex $Context 0] entity \"$name\""
    }
}

proc comment {comment} {
   comment_start_hook $comment
}

proc package {name defn} {
    global Context
    set Context {package}
    global Current
    set Current(package) $name
    set Current(exepath) "\$[string toupper $Current(package)]_DIR"
    set Current(helplib) "\$[string toupper $Current(package)]_HELP"
    set Current(version) "PKG_VERS"

    package_start_hook $name

    proc version {version_num} {
        global Current
	set Current(version) $version_num
    }

    proc exepath {path} {
       global Current
       set Current(exepath) $path
    }

    proc prefix {prefix} {
        global Current
	set Current(prefix) $prefix
    }

    proc display {message} {
        display_start_hook $message
        display_end_hook $message
    }

    proc helplib {par} {
	global Current
	set Current(helplib) $par
        helplib_start_hook $par
    }

    proc defhelp {topic entry {lib ""}} {
        defhelp_start_hook $topic $entry $lib
    }

    proc executable {name defn} {
        global Context
	set Context [linsert $Context 0 executable]
	global Current
        executable_start_hook $name
	set Current(executable) $name

	proc action {name defn} {
	    global Context
	    set Context [linsert $Context 0 action]
	    global Current
            global OutputPars
            set OutputPars {}
            global DynPars
            set DynPars {}
            global Aliases
            set Aliases {}

            set Current(action) $name
	    action_start_hook $name

            proc alias {name} {
  	        alias_start_hook $name
	        alias_end_hook $name
            }

	    proc parameter {name defn} {
		global Context
	        set Context [linsert $Context 0 parameter]
		global Current

		parameter_start_hook $name
		set Current(parameter) $name

		set Current(association) ""
		set Current(type) ""
		set Current(keyword) ""
		set Current(helpkey) ""
		set Current(default) ""
		set Current(prompt) ""
		set Current(ppath) ""
		set Current(vpath) ""
		set Current(access) ""
		set Current(ptype) ""
		set Current(help) ""
		set Current(in) ""
		set Current(range) ""
		set Current(size) ""
		set Current(position) 0
                set Current(outputpar) 0
                set Current(repeated) 0
                set Current(dynamic) ""

		proc association {args} {
		    global Current; set Current(association) $args
		}
		proc type {args} {global Current; set Current(type) $args}
		proc keyword {args} {global Current; set Current(keyword) $args}
		proc helpkey {args} {global Current; set Current(helpkey) $args}
		proc default {args} {global Current; set Current(default) $args}
		proc prompt {args} {global Current; set Current(prompt) $args}
		proc ppath {args} {global Current; set Current(ppath) $args}
		proc vpath {args} {global Current; set Current(vpath) $args}
		proc position {args} {
		    global Current; set Current(position) $args
		}
		proc access {args} {global Current; set Current(access) $args}
		proc ptype {args} {global Current; set Current(ptype) $args}
		proc help {args} {global Current; set Current(help) $args}
		proc in {args} {global Current; set Current(in) $args}
		proc range {args} {global Current; set Current(range) $args}
		proc size {args} {global Current; set Current(size) $args}
                proc outputpar {} {global Current; set Current(outputpar) 1}
                proc repeated {} {global Current; set Current(repeated) 1}
                proc dynamic {args} {
                    global Current;  set Current(dynamic) $args
                }

		eval $defn

		rename association {}
		rename type {}
		rename keyword {}
		rename helpkey {}
		rename default {}
		rename prompt {}
		rename ppath {}
		rename vpath {}
		rename position {}
		rename access {}
		rename ptype {}
		rename help {}
		rename in {}
		rename range {}
		rename size {}
                rename outputpar {}
                rename repeated {}
                rename dynamic {}

		parameter_end_hook $name

                set Context [lreplace $Context 0 0]
	    }
	    proc message {name defn} {
		global Context
	        set Context [linsert $Context 0 message]
		global Current
		message_start_hook $name
		set Current(message) $name
		set Current(text) ""

		proc text {args} {
		    global Current
		    set Current(text) $args
		}

		eval $defn

		rename text {}
		message_end_hook $name
                set Context [lreplace $Context 0 0]
	    }


	    eval $defn

            rename alias {}
	    rename parameter {}
	    rename message {}
	    action_end_hook $name

            set Context [lreplace $Context 0 0]
	}

	eval $defn

	rename action {}

	executable_end_hook $name

        set Context [lreplace $Context 0 0]
    }

    proc command {name defn} {
        global Context
        global Current
        global Aliases

	set Context [linsert $Context 0 command]
        set Current(command) $name
        set Aliases {}

        proc alias {name} {
           alias_start_hook $name
           alias_end_hook $name
        }

        proc print {mess} {
           print_start_hook $mess
        }

        proc obey {command} {
           obey_start_hook $command
        }

        proc task {action defn} {
           global Paramlist

           proc taskparam {val} {
              taskparam_start_hook $val
           }
           proc taskinherit {param} {
              taskinherit_start_hook $param
           }
           task_start_hook $action

           set Paramlist {}

           eval $defn

           task_end_hook $action
           rename taskparam {}
           rename taskinherit {}
        }

        proc obsolete {mess} {
           obsolete_start_hook $mess
        }

        command_start_hook $name

        eval $defn

        command_end_hook $name

        rename alias {}
        rename print {}
        rename obey {}
        rename task {}

        set Context [lreplace $Context 0 0]

    }

    eval $defn

    rename executable {}
    rename exepath {}
    rename version {}
    rename helplib {}
    rename display {}
    rename command {}

    package_end_hook $name

    set Context [lreplace $Context 0 0]
}
