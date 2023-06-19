module funciones

use ISOprec
use constantes

contains

   ! ------------------------------------------------------------------------------------------
   function tita_ex ( t, tita0 )

   implicit none

   real ( kind = wp ) , intent ( in )         :: t, tita0
   real ( kind = wp )                         :: tita_ex, pi

  pi = acos(-1._wp )

  tita_ex = (tita0*cos(sqrt(10.0_wp)*t))


  end function

! -----------------------------------------------------------------------------------------

  function derive(t , y )

  implicit none

  real ( kind = wp ) , intent ( in )  :: t
  real ( kind = wp ) , dimension (1:2) , intent ( in ) :: y
  real ( kind = wp ) , dimension (1:2) :: derive

  !nrhs = nrhs +1 ! por si se quisiese contar las evaluaciones

  derive(1) = y(2)        ! u = dtita/dt
  derive(2) = -(g/l) * sin(y(1))   ! sin(tita)

  end function derive


! -----------------------------------------------------------------------------------------

function derive_PO(t , y )

implicit none

real ( kind = wp ) , intent ( in )  :: t
real ( kind = wp ) , dimension (1:2) , intent ( in ) :: y
real ( kind = wp ) , dimension (1:2) :: derive_PO

!nrhs = nrhs +1 ! por si se quisiese contar las evaluaciones

derive_PO(1) = y(2)        ! u = dtita/dt
derive_PO(2) = -(g/l) * y(1)   ! sin(tita)

end function derive_PO


 ! ------------------------------------------------------------------------------------------

 function y_ex ( t )

 implicit none

 real ( kind = wp ) , intent ( in )         :: t
 real ( kind = wp )                         :: y_ex, pi

pi = acos(-1._wp )

y_ex = ( 1._wp + 2._wp * pi /(1._wp + 4._wp * pi **2) ) * exp (-t) -   &
       ( sin (2. _wp * pi * t ) - 2._wp * pi * cos (2._wp * pi * t ) ) /(1._wp + 4._wp * pi **2)


end function

! ------------------------------------------------------------------------------------------------





end module
