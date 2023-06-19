module rk4mod

use ISOprec
use funciones

contains

! -----------------------------------------------------

subroutine rk4(t ,w ,h , w_h )

implicit none

real ( kind = wp ) , intent ( in ) :: t ,w , h
real ( kind = wp ) , intent ( out ) :: w_h
real ( kind = wp )          :: k1 , k2 , k3 , k4


k1 = f (t , w )
k2 = f ( t + 0.5_wp *h , w + 0.5_wp * h * k1 )
k3 = f ( t + 0.5_wp *h , w + 0.5_wp * h * k2 )
k4 = f ( t + h , w + h * k3 )


w_h = w + h *( k1 + 2._wp *( k2 + k3 ) + k4 )/6._wp

return

end subroutine

! ---------------------------------------------

end module rk4mod
