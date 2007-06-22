proc cgs3drLoadTask {taskname} {
   global env
   if {$taskname=="figaro1"} {
     set taskname $env(PID)figaro1
     adamtask $taskname $env(FIG_DIR)/figaro1
   } elseif {$taskname=="red3"} {
     set taskname $env(PID)red3
     adamtask $taskname $env(RED3_DIR)/red3
   } elseif {$taskname=="cgs3dr"} {
     set taskname $env(PID)cgs3dr
     adamtask $taskname $env(CGS3DR_DIR)/cgs3dr
   } elseif {$taskname=="tsp"} {
     set taskname $env(PID)tsp
     adamtask $taskname $env(TSP_DIR)/tsp_mon
   }
   cgs3drPath $taskname
   return $taskname
}
