#!usr/bin/gnuplot
set terminal postscript enhanced color eps 
set terminal pngcairo enhanced size 1280,960 font "Arial, 20" transparent lw 2

set encoding utf8

# For EPS
# set style line 1 pt 7 ps 0.5 lc rgbcolor "#118ab2"  # Recovered
# set style line 2 pt 7 ps 0.5 lc rgbcolor "#ef476f"  # Infected
# set style line 3 pt 7 ps 0.5 lc rgbcolor "#06d6a0"  # Susceptible

# For pngcairo
set style line 1 pt 7 ps 1 lc rgbcolor "#118ab2"  # Recovered
set style line 2 pt 7 ps 1 lc rgbcolor "#ef476f"  # Infected
set style line 3 pt 7 ps 1 lc rgbcolor "#06d6a0"  # Susceptible

# set output "results/plots/evolution.eps"
set output "results/plots/evolution.png"

set xlabel "t (adim.)"
set ylabel "# Infected"

unset key

plot "results/evolution.dat" u 1:3 w lp ls 1 lw 2

# set output "results/plots/histrograms.eps"
set output "results/plots/histrograms.png"

set xlabel "t (adim.)"
unset ylabel 

set xrange [*:*]
set yrange [*:*] 

set key

plot "results/evolution_histo.dat" u 1:2 w lp ls 1 t "Susceptible", "" u 1:3 w lp ls 2 t "Infected", "" u 1:4 w lp ls 3 t "Recovered"

# set output "results/plots/rec_lambda.eps"
set output "results/plots/rec_lambda.png"

set xlabel "λ (adim.)"
set ylabel "R (t→∞)" 

set xrange [*:*]
set yrange [*:*] 

unset key

plot "results/rec_lambda.dat" u 1:2 w lp ls 1 lw 2 ps 1.5

set output "results/plots/lambda_change.png"

set xrange [0:20]
set key inside top right vertical
set ylabel "# Infected"

set palette defined ( 0 "#118ab2", 0.5 "#06d6a0", 1 "#ef476f" )

plot for [i=1:23:2] sprintf("results/lambda_%5.3f_evolution_histo.dat", i/100.) u 1:3:0 \
                    w l lw 1.5 lc palette frac (i / 23.) t sprintf("λ = %5.2f", i/100.)


# set output "results/plots/filled_hist.eps"
set output "results/plots/lambda_all_hist.png"

unset xlabel
unset ylabel 

set multiplot layout 4,3 upwards
# set size 2560,2560
unset key
set font ",2"

set lmargin 1.1
set rmargin 1.1
set tmargin 1.1
set bmargin 1.1
set format ""
set tics front

set title offset 0,-0.5

# unset xtics; unset ytics
# set xtics format " "
# set ytics format " "

do for [i=1:23:2] {
    set title sprintf("λ = %5.2f", i/100.) font ",12"
    plot sprintf("results/lambda_%5.3f_evolution_histo.dat", i/100.) u 1:($2+$3+$4) w filledcurves x1 ls 1 t "Susceptible", \
                                "" u 1:($3+$4)    w filledcurves x1 ls 3 t "Recovered", \
                                "" u 1:3          w filledcurves x1 ls 2 t "Infected"
}