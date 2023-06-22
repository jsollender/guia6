module funciones
use mod_prec
implicit none
contains

	function func(t, y)
		implicit none
		real(wp), intent(in)				:: t
		real(wp), dimension(3), intent(in)	:: y
		real(wp),dimension(3)				:: func
		       
        func(1) = -(( beta * y(1) * y(2) ) / pt )
        func(2) = (( beta * y(1) * y(2) ) / pt ) - ( gamma * y(2) )
        func(3) = gamma * y(2)
	
	end function func
	
end module funciones
