module funciones
use mod_prec
implicit none
contains

	function func(t, y)
		implicit none
		real(wp), intent(in)				:: t
		real(wp), dimension(2), intent(in)	:: y
		real(wp),dimension(2)				:: func
		
		func(1) = y(2)
        func(2) = -1.0_wp * g * exp((-1.0_wp*t)/af)
	
	end function func

	function y (t)
		real(wp), intent(in)		:: t
		real(wp), dimension(2)		:: y
		
		y = (t + 1._wp)**2._wp - 0.5_wp*exp(t)
    
	end function y

end module funciones
