module eulermod

use ISOprec
use funciones

contains
subroutine euler(t , w , h , w_h )

implicit none

real ( kind = wp ) , intent ( in ) :: t , w , h
real ( kind = wp ) , intent ( out ) :: w_h

w_h = w + h * f (t,w)

end subroutine euler

subroutine euler_modificado(t, w, h, w_h, f)

implicit none

real ( kind = wp ) , intent ( in ) :: t , w , h
real ( kind = wp ) , intent ( out ) :: w_h
real ( kind = wp )                  :: f, k1, k2 


k1 = h * f (t , w )
k2 = h * f ( t + h , w + k1 )

w_h = w + 0.5_wp * ( k1 + k2 ) 

end subroutine euler_modificado





end module eulermod
