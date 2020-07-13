set title "wave at time t"
set xlabel "X"
set ylabel "Z"
set yrange [:] reverse     # to set origin at top left

plot 'vp.txt' matrix with image
pause(1)