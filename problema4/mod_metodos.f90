module metodos

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%					Euler y Runge-Kutta 4				 %%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	  	integer(wp)						:: i	!para el do
	  
	  !inicializamos las variables
	  	t = a
	  	w = alfa
	  	h = (b-a)/N
	  !print de control
	  		print*, t, w(1), w(2)
	  		
	  !proceso
	  	do i = 1, N, 1	
	  		w = w + h * func(t,w)
	  		t = a + i*h
	  	  !print de control
	  		print*, t, w(1), w(2)
	  	end do	  	
	end subroutine euler
	
	subroutine rk4 (a, b, N, alfa, w)
	
  	  !definimos variables que entran o salen
  		real(wp), intent(in)				:: a, b	!extremos del intervalo
	  	real(wp), dimension(:), intent(in)	:: alfa	!condición inicial (t,y(t))	(*)
	  	integer(il), intent(in)				:: N	!cantidad de intervalos
	  	real(wp), dimension(:), intent(out)	:: w	!aprox. de solucion exacta	(*)
	  	
	  	!	(*) la dimensión viene desde afuera. Acá la dejo abierta así la subrutina se
	  	!		puede comer cualquier cosa
	  
	  !definimos las variables auxiliares
	  	real(wp)					 :: t, h			!ptos. del intervalo y su tamaño
	  	real(wp), dimension(size(w)) :: k1, k2, k3, k4	!auxiliares
	  	integer(wp)				 	 :: i				!para el do
	
	  !inicializamos las variables
	  	t = a
	  	w = alfa
	  	h = (b-a)/N	
	  !print de control
	  	print*, t, w(1), w(2)
	  	
	  !proceso
	  	do i = 1, N, 1
	  	  !inicializamos los k
		  	k1 = h * func( t , w )
		  	k2 = h * func( t + h * 0.5_wp , w + k1 * 0.5_wp ) 
		  	k3 = h * func( t + h * 0.5_wp , w + k2 * 0.5_wp )
		  	k4 = h * func( t + h , w + k3 )
	  		
	  		w = w + ( k1 + 2._wp * k2 + k3 * 2._wp + k4 )/6._wp
	  		
	  		t = a + i*h
	  		!print de control
	  		print*, t, w(1), w(2)
	  	end do
	end subroutine rk4
	
end module metodos
