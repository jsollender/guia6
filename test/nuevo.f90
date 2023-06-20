module metodos

use mod_prec
use funciones

implicit none
contains

	subroutine euler (a, b, N, alfa, w)
	  
	  !definimos las variables que entran y salen
	  	real(wp), intent(in)				:: a, b		!extremos del intervalo
	  	real(wp), dimension(:), intent(in)	:: alfa	!condición inicial (t,y(t))
	  	integer(il), intent(in)				:: N		!cantidad de intervalos
	  	real(wp), dimension(:), intent(out)	:: w	!aprox. de solucion exacta
	  
	  !definimos las variables auxiliares
	  	real(wp)						:: t, h	!ptos. del intervalo y su tamaño
	  	integer(wp)						:: i
	  
	  !inicializamos las variables
	  	t = a
	  	w = alfa
	  	h = (b-a)/N
	  
	  !proceso
	  	do i = 1, N+1, 1	
	  		w = w + h * func(t,w)
	  		print*, t, w(1), w(2)
	  		t = a + i*h
	  	end do
	  	
	end subroutine euler
end module metodos