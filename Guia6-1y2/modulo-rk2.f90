module rk2mod

use ISOprec
use funciones

contains

! --------------------------------------------------

subroutine rk2(t , w , h , w_h )
implicit none

real ( kind = wp ) , intent ( in ) :: t , w , h
real ( kind = wp ) , intent ( out ) :: w_h
real ( kind = wp )          :: k1


k1 = h * f (t , w )
w_h = w + h * f ( t + 0.5_wp *h , w +0.5_wp * k1 )

end subroutine rk2


!-------------------------------------------------

subroutine rk2_o(t , w , h , w_h )
implicit none

real ( kind = wp ) , intent ( in ) :: t , w , h
real ( kind = wp ) , intent ( out ) :: w_h
real ( kind = wp )        :: k1 , k2 , a , b , p1 , p2

a = 2._wp /3._wp
b = 2._wp /3._wp
p1 = 0.25_wp
p2 = 0.75_wp

k1 = h * f (t , w )
k2 = h * f ( t + a *h , w + b * k1 )
w_h = w + p1 * k1 + p2 * k2

end subroutine rk2_o

! ---------------------------------------------


end module rk2mod
