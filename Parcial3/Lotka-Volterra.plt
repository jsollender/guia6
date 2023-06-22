set term x11 persist

# EULER y RUNGE-KUTTA 4
# MN2023 - 3º parcial
# Sollender, Jazmín
##################################################
##################################################
##################################################

    set title  "Evolución temporal poblaciones (RK-4)"
    set xlabel "Población"
    set ylabel "Tiempo"
    set grid

    
#	G R A F.   D A T O S
################################################## 
	
	plot "LV_rk4.dat" u 1:2 title "Presas" w l linetype 2
	replot "LV_rk4.dat" u 1:3 title "Depredadores" w l linetype 7


#	E X P O R T A R 
##################################################

    set terminal png size 1200,900
    set output 'Lokta-Volterra-RK4.png'
    replot
    
set term x11 persist    
##################################################
##################################################
##################################################

    set title  "Evolución temporal poblaciones (Euler)"
    set xlabel "Población"
    set ylabel "Tiempo"
    set grid
    
#	G R A F.   D A T O S
################################################## 
	
	plot "LV_euler.dat" u 1:2 title "Presas" w l linetype 3
	replot "LV_euler.dat" u 1:3 title "Depredadores" w l linetype 6

#	E X P O R T A R 
##################################################

    set terminal png size 1200,900
    set output 'Lokta-Volterra-EULER.png'
    replot

set term x11 persist    
##################################################
##################################################
##################################################

    set title  "Pobacion presas vs poblacion predadores (Euler)"
    set xlabel "Poblacion presas"
    set ylabel "Poblacion depredadores"
    set grid
    
#	G R A F.   D A T O S
################################################## 
	
	plot "LV_euler.dat" u 2:3 title "EULER" w l linetype 3
	replot "LV_rk4.dat" u 2:3 title "RK-4" w l linetype 4
	
#	E X P O R T A R 
##################################################

    set terminal png size 1200,900
    set output 'XvsY.png'
    replot
exit
