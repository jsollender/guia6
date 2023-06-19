module funciones

use ISOprec
use constantes

contains

! -----------------------------------------------------------------------------------------

function f(t , y )

implicit none
real ( kind = wp ) , intent ( in )  :: t , y
real ( kind = wp )               :: f


 f = -y + sin(2._wp*pi*t)

 end function

 ! ------------------------------------------------------------------------------------------
 function y_ex( t )

 implicit none

 real ( kind = wp ) , intent ( in )         :: t
 real ( kind = wp )                         :: y_ex


y_ex =( 1._wp + 2._wp * pi /(1._wp + 4._wp * pi **2) ) * exp (-t) +   &
       ( sin (2. _wp * pi * t ) - 2._wp * pi * cos (2._wp * pi * t ) ) /(1._wp + 4._wp * pi **2)


end function

! ------------------------------------------------------------------------------------------------



end module
