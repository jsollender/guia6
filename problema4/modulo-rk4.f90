module rk4mod

use ISOprec
use funciones

contains

! -----------------------------------------------------

subroutine rk4_vec(t ,w ,h , w_h)

implicit none

real ( kind = wp ) , intent ( in ) :: t , h
real ( kind = wp ) , dimension (:) ,  intent ( in ) :: w
real ( kind = wp ) , dimension (:) ,  intent ( out ) :: w_h
real ( kind = wp ) ,  dimension(size(w))   :: k1 , k2 , k3 , k4


k1 = derive( t , w )
k2 = derive( t + 0.5_wp *h , w + 0.5_wp * h * k1 )
k3 = derive( t + 0.5_wp *h , w + 0.5_wp * h * k2 )
k4 = derive( t + h , w + h * k3 )


w_h = w + h *( k1 + 2._wp *( k2 + k3 ) + k4 )/6._wp

!print *, w_h

return

end subroutine

! ---------------------------------------------
subroutine rk4_vec_e(t ,w ,h , w_h)

implicit none

real ( kind = wp ) , intent ( in ) :: t , h
real ( kind = wp ) , dimension (:) ,  intent ( inout ) :: w
real ( kind = wp ) , dimension (:) ,  intent ( out ) :: w_h
real ( kind = wp ) ,  dimension(size(w))   :: k1 , k2 , k3 , k4


k1 = derive_PO( t , w )
k2 = derive_PO( t + 0.5_wp *h , w + 0.5_wp * h * k1 )
k3 = derive_PO( t + 0.5_wp *h , w + 0.5_wp * h * k2 )
k4 = derive_PO( t + h , w + h * k3 )


w_h = w + h *( k1 + 2._wp *( k2 + k3 ) + k4 )/6._wp

!print *, w_h

return

end subroutine

! ---------------------------------------------

end module rk4mod
