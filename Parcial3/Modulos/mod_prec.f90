module mod_prec

!!!hola usé el ancho del tab = 4. Con otro se va a ver descuajeringado todo

! EULER y RUNGE-KUTTA 4
! MN2023 - 3º parcial
! Sollender, Jazmín

use ISO_FORTRAN_ENV

implicit none

	integer(kind=int8), parameter :: is = int8, id = int16, il = int32 , ix = int64
	integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

	integer(kind=int8), parameter :: wp = rd

  !constantes K1, K2 y K3 del enunciado (les cambié el nombre por paz mental)
	real(kind=wp), parameter :: ka1 = 1._wp 
	real(kind=wp), parameter :: ka2 = 0.002_wp
	real(kind=wp), parameter :: ka3 = 1._wp

end module mod_prec
