module metodos

use mod_prec
use funciones

implicit none
contains

	subroutine euler (a, b, N, alfa, w)
	
	  !definimos las variables que entran y salen
	  	real(wp), intent(in)				:: a, b	!extremos del intervalo
	  	real(wp), dimension(:), intent(in)	:: alfa	!condición inicial (t,y(t))	(*)
	  	integer(il), intent(in)				:: N	!cantidad de intervalos
	  	real(wp), dimension(:), intent(out)	:: w	!aprox. de solucion exacta	(*)
	  
	  !	(*) la dimensión viene desde afuera. Acá la dejo abierta así la subrutina se
	  !		puede comer cualquier cosa
	  
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
	  		t = a + i*h
	  		print*, t, w(1), w(2)
	  	end do	  	
	end subroutine euler
	
	subroutine
end module metodos
