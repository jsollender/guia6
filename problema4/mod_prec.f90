module mod_prec
use ISO_FORTRAN_ENV

implicit none

	integer(kind=int8), parameter :: is = int8, id = int16, il = int32 , ix = int64
	integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

	integer(kind=int8), parameter :: wp = rd

  !constantes
	real(kind=wp), parameter :: g = 10._wp	!aceleración de la gravedad
	real(kind=wp), parameter :: l = 1.0_wp	!longitud del péndulo

end module mod_prec
