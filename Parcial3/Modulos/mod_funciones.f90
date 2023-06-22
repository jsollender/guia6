module funciones

!hola usé el ancho del tab = 4. Con otro se va a ver descuajeringado todo

! EULER y RUNGE-KUTTA 4
! MN2023 - 3º parcial
! Sollender, Jazmín

use mod_prec
implicit none
contains 

	function func(t, y)
		implicit none
		real(wp), intent(in)				:: t
		real(wp), dimension(2), intent(in)	:: y
		real(wp),dimension(2)				:: func
		
		func(1) = ka1 * y(1) - ka2 * y(2) * y(1)
        func(2) = ka2 * y(2) * y(1) - ka3 * y(2)
	
	end function func

end module funciones
