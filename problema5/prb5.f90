program main_test

use mod_prec
use funciones
use metodos

implicit none
  !definición de variables
	real(wp)				:: a, b, h	!extremos del intervalo
	!integer(il)				:: cant_int	!cantidad de intervalos
	real(wp), dimension(3)	:: w, alfa	!aproximación y cond. inicial
!	integer(il)				:: N		!poblacion total
  !inicialización de variables
  	a = 0._wp		!dato del problema
  	b = 200._wp		!dato del problema
!  	N = 1000		!dato del problema
  	alfa(1) = 990._wp
  	alfa(2) = 10._wp
  	alfa(3) = 0._wp
  	
  	h = 0.05_wp
  	
  	!cant_int = 15	!porque por que no
  	
  	
  	!func(1)+func(2)+func(3) = N
  	
  !llamamos a la subrutina
!  	print*,'%%%%% E U L E R %%%%%'
!	call euler (a, b, h, alfa, w)
!	print*,'salió:', w
	
	print*,'#### R - K   4 ####'
	call rk4 (a, b, h, alfa, w)
	print*,'salió:', w
	
end program main_test
