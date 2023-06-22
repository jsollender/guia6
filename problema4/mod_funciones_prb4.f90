module funciones
use mod_prec
implicit none
contains
  
  ! la funcion 'y' se la damos de comer adentro del módulo.
  ! serian las soluciones tita y omega que buscamos y se 
  ! van dando los valores que se van aproximando.
  
  ! la funcion 'deriv' es la funcion F del apunte, y la
  ! derivada de las componentes de y.
  
  ! en este caso, y(t) = (tita(t), omega(t))
  ! 		 F(t,y(t)) = (deriv.de-tita(t), deriv.de-omega(t))
  !					   = (omega(t), -(g/l)sen(tita(t)))
  
  ! me duele el cerebro :(
  
	function deriv(t, y)
		implicit none
		real(wp), intent(in)				:: t
		real(wp), dimension(2), intent(in)	:: y
		real(wp), dimension(2)				:: deriv
		
		deriv(1) = y(2)					
        deriv(2) = -(g/l) * sin(y(1)))	
	
	end function deriv
	
 !para pequeñas oscilaciones (po)
!	function deriv_po(t, y)
!		real(wp), intent(in)				:: t
!		real(wp), dimension(2), intent(in)	:: y
!		real(wp), dimension(2)				:: deriv_po	
!	end function y

end module funciones
