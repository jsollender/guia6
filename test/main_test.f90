program main_test

use mod_prec
use funciones
use metodos

implicit none
  !definición de variables
	real(wp)				:: a, b		!extremos del intervalo
	integer(il)				:: N		!cantidad de intervalos
	real(wp), dimension(2)	:: w, alfa	!aproximación y cond. inicial
  !inicialización de variables
  	a = 0._wp
  	b = 16._wp
  	alfa = (0._wp,0._wp)
	N = 16
  !llamamos a la subrutina
  	print*,'%%%%% E U L E R %%%%%'
	call euler (a, b, N, alfa, w)
	print*,'salió:', w(1), 'y', w(2)
	
	print*,'#### R - K   4 ####'
	call rk4 (a, b, N, alfa, w)
	print*,'salió:', w(1), 'y',  w(2)
	
end program main_test
