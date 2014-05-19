set terminal png size 600,400
set termoption enhanced

unset key
set xtics nomirror out rotate 90
set ytics nomirror
set yrange [0:]
set format y "%g"
set border 3

set style data histogram 
set boxwidth 0.5
set style histogram cluster gap 1
set style fill solid border -1

set label at graph 0.24, graph 0.95 tc rgb "#cc0000" "Float" 
set label at graph 0.72, graph 0.95 tc rgb "#009900" "Double" 

set ylabel "runtime (seconds)"
set xlabel " "

set title "runtime of calculating l2 distance on vectors of {/Verdana-Bold length 16}" font ", 20"
set output "summary16.png"
plot "summary16.dat" every ::0::4 using 1:3:xtic(2) ls 1 with boxes,\
     "summary16.dat" every ::4::7 using 1:3:xtic(2) ls 2 with boxes

set title "runtime of calculating l2 distance on vectors of length 160" font ", 20"
set output "summary160.png"
plot "summary160.dat" every ::0::4 using 1:3:xtic(2) ls 1 with boxes,\
     "summary160.dat" every ::4::7 using 1:3:xtic(2) ls 2 with boxes

set title "runtime of calculating l2 distance on vectors of length 1600" font ", 20"
set output "summary1600.png"
plot "summary1600.dat" every ::0::4 using 1:3:xtic(2) ls 1 with boxes,\
     "summary1600.dat" every ::4::7 using 1:3:xtic(2) ls 2 with boxes

set title "runtime of calculating l2 distance on vectors of length 16000" font ", 20"
set output "summary16000.png"
plot "summary16000.dat" every ::0::4 using 1:3:xtic(2) ls 1 with boxes,\
     "summary16000.dat" every ::4::7 using 1:3:xtic(2) ls 2 with boxes
