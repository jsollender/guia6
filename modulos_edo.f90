module metodos
use mod_prec
!use funciones

!aca voy a intentar hacer los modulos vectoriales. no se que estoy haciendo.
!tengo sueño :(

!creo que habria que tener:

!rk4 vectorial
!rk3 heun vectorial
!rk2 vectorial
!euler vectorial
!euler modificado vectorial

implicit none

	subroutine euler (t, y, h, w)
	implicit none
	
	!tenemos a la func vec y, al punto t entre a y b, a la func f=y', a la cond inicial
	!alfa. f viene del modulo de funciones. el t y el y se modifican afuera. y ser{ia
	!la aprox anterior.
	
		real(wp), intent(in)				:: 	t, h	
		real(wp), dimension(:), intent(in)	:: 	y
		real(wp), dimension(:), intent(out)	::	w
		
		w = y + h * f(t,y) 		
		
	end subroutine euler
	

end module metodos
