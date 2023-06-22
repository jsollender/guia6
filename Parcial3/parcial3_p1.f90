program parcial3_p1

!!!hola usé el ancho del tab = 4. Con otro se va a ver descuajeringado todo

! EULER y RUNGE-KUTTA 4
! MN2023 - 3º parcial
! Sollender, Jazmín

use mod_prec
use funciones
use metodos

implicit none
  !definición de variables
	real(wp)				:: a, b		!extremos del intervalo
	real(wp)				:: h		!ancho del intervalo
	real(wp), dimension(2)	:: w, alfa	!aproximación y cond. inicial

  !inicialización de variables
  	a = 0._wp
  	b = 60._wp
  	h = 0.01_wp
  	alfa(1) = 800._wp
  	alfa(2) = 200._wp
  	
  !llamamos a las subrutinas
  
  ! la profe nos dijo que podíamos usar los módulos con el do adentro
  ! ambas subrutinas generan los archivos de datos.
  	
	call euler (a, b, h, alfa, w)
	
	call rk4 (a, b, h, alfa, w)
	
  !esto para imprimir por pantalla porque sí nomás.	

print*,'		MN2023 - 3er. Parcial'
print*,''
print*,'    () ()'
print*,'   ( =º·ª)=  < hola'
print*,' *(   u u)'
	
end program parcial3_p1
