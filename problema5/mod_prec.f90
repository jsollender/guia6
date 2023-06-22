module mod_prec
use ISO_FORTRAN_ENV

implicit none

	integer(kind=int8), parameter :: is = int8, id = int16, il = int32 , ix = int64
	integer(kind=int8), parameter :: rs = real32, rd = real64, rl = real128

	integer(kind=int8), parameter :: wp = rd

  !constantes
	real(kind=wp), parameter	:: gamma = 0.1_wp	!indice 
	real(kind=wp), parameter 	:: beta = 0.5_wp	!indice
	real(kind=wp), parameter 	:: pt = 1000		!poblacion total
	
end module mod_prec
