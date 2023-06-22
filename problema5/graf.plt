set term x11 persist

# parcial 2 - ejercicio 2

# MERCADO, Javier
# SOLLENDER, Jazmín

# parte II.C
# Gráfico de comparación entre métodos.

##################################################
####            h o l i s    ( :            ######
##################################################

    set title  "RK4 "
    set xlabel "Posicion"
    set ylabel "Velocidad"
    set grid
    #set logscale xy
    #set sample 500

##################################################
######      G R A F.   D A T O S            ######
##################################################

    # grafico error relativo biseccion    

          plot "datos_rk4.dat" u 1:2 title "Datos v vs z" 
		replot "datos_rk4.dat" u 1:3 title "Datos v vs z" 
		replot "datos_rk4.dat" u 1:4 title "Datos v vs z"


##################################################
######          E X P O R T A R             ######
##################################################
############       P   N   G         #############

    set terminal png size 1200,900
    set output 'figura1d.png'
    replot

exit
