
#

user=${USER}

# Kill red3
echo "Killing red3 task ..."
for pid in \
   `ps -a -f -u ${user} | grep /red3 | grep -v grep | nawk '{print $2}'` ;\
   do kill -9 ${pid} ;\
done
echo " OK"

# Kill figaro
echo "Killing figaro task ..."
for pid in \
   `ps -a -f -u ${user} | grep /figaro | grep -v grep | nawk '{print $2}'` ;\
   do kill -9 ${pid} ;\
done
echo " OK"

# Kill tsp
echo "Killing tsp task ..."
for pid in \
   `ps -a -f -u ${user} | grep /tsp | grep -v grep | nawk '{print $2}'` ;\
   do kill -9 ${pid} ;\
done
echo " OK"

# Kill cgs3dr
echo "Killing cgs3dr task ..."
for pid in \
   `ps -a -f -u ${user} | grep /cgs3dr | grep -v grep | nawk '{print $2}'` ;\
   do kill -9 ${pid} ;\
done
echo " OK"

# Exit script
exit 0
#.
