program odes1

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
! uso de tres métodos para resolver una ecuación
! en derivadas ordinarias ; Euler , RK2 y RK4.
! De RK2 damos dos versiones.
! Elegir :
! metodo = 1 para Euler
! metodo = 2 para rk2
! metodo = 22 para rk2_o
! metodo = 4 para rk4
! que se lee del archivo datosent/paramscmp-p1y2.in
! Los tiempos inicial ti , final tf , el incremento h y el
! valor inicial y_0 se leen del
! archivo datosent/paramsinic-p1y2.in
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use ISOprec
use constantes
use eulermod
use rk2mod
use rk4mod


implicit none

integer      :: metodo , p , p_max , pmax2 , j , nn, fu, fu1
real ( kind = wp ) :: t , ti , tf , h , y_0 , y_h
character ( len =50) :: hache , hache0
character ( len =90) :: archivo

! lectura de parametros -----------------------------------------------------
namelist / parametroscmp / &
               metodo ,      &
         / parametrosinic /   &
              ti , tf , h , y_0


open ( newunit = fu , file = "datosent/paramscmp-p1y2.in" , status = "old" )
read ( unit = fu , nml = parametroscmp )
close ( unit = fu)

open ( newunit = fu , file = "datosent/paramsinic-p1y2.in" , status = "old" )
read ( unit = fu , nml = parametrosinic )
close ( unit = fu)

! ---------------------------------------------------
open ( newunit = fu1 , file = "datossal/salidas-p1y2.f" , status = "unknown" )

! constantes ---------------------------------------------
pi = acos(-1._wp ) ! definido globalmente para que lo puedan usar
! todas las funciones y subrutinas.

! h ------------------------------------------------------------------------

write ( hache0 , '( F10.3) ') h

hache = 'h'//trim(adjustl(hache0))

nn = (tf-ti)/h


! chequeos básicos ---------------------------------------------------------

write (* ,*) " "
write (fu1 ,*) " "


write (fu1 ,*) " pi = " , pi
write (* ,*) " pi = " , pi

write (* ,*)  " hache0 = " , hache0
write (fu1 ,*) " hache0 = " , hache0

write (* ,*)  " hache = " , hache
write (fu1 ,*) " hache = " , hache

write (* ,*) "  ti = " , ti
write (* ,*) "  tf = " , tf
write (* ,*) "   h = " ,  h
write (* ,*) "  y_0 = ", y_0
write (fu1 ,*)  "  ti = " , ti
write (fu1 ,*)  "  tf = " , tf
write (fu1 ,*)  "   h = " ,  h
write (fu1 ,*)  "  y_0 = ", y_0

write (* ,*) " "
write (fu1 ,*) " "

write (* ,*)   " f (0. _wp , y_0 ) = ",  f (0._wp , y_0 )
write (fu1 ,*) " f (0. _wp , y_0 ) = ",  f (0._wp , y_0 )
write (* ,*)   " f (1. _wp , y_0 ) = ",  f (1._wp , y_0 )
write (fu1 ,*) " f (1. _wp , y_0 ) = ",  f (1._wp , y_0 )

write (* ,*)   " y_ex (0._wp ) = ",  y_ex(0._wp)
write (fu1 ,*) " y_ex (0._wp ) = ",  y_ex(0._wp)
write (* ,*)   " y_ex (1._wp ) = ",  y_ex(1._wp)
write (fu1 ,*) " y_ex (1._wp ) = ",  y_ex(1._wp)

!--------------------------------------------------------------------------

select case (metodo)

   case (1)

        archivo = 'datossal10p/p2-euler-'//trim(hache)//'.dat'
        open (newunit=fu , file =archivo )
        write (fu ,*) " #         t          y_h            y_ex(t)" ,&
        "      abs(y_h - y_ex(t)) "

        t = ti

             write (fu , '(4( E17.9) )') ti , y_0 , y_ex( ti ) , abs( y_0 - y_ex( ti ))

        do while ( t < (tf-h) )

            call euler(t , y_0 ,h , y_h )
            t = t + h
            write (fu , '(4( E17.9) )') t , y_h , y_ex( t ) , abs( y_h - y_ex( t ))
            y_0 = y_h

        end do

        close (fu)

!----------------------------------------------------------------------------------

case (2)

        archivo = 'datossal10p/p2-rk2-'//trim(hache)//'.dat'
        open ( newunit =fu , file =archivo )
        write (fu ,*) " #          t            y_h          y_ex(t)" ,&
        "     abs(y_h - y_ex(t)) "

        t = ti

              write (fu , ' (4( E17.9) )') ti , y_0 , y_ex( ti ) , abs( y_0 - y_ex( ti ))


        do while ( t < (tf-h) )

            call rk2(t , y_0 ,h , y_h )
            t = t + h
            write (fu , '(4( E17.9))') t , y_h , y_ex( t ) , abs( y_h - y_ex( t ))
            y_0 = y_h

        end do

        close (fu)

!---------------------------------------------------------------------------------

case (22) ! 22

          archivo = 'datossal10p/p2-rk2_o-'//trim(hache)//'.dat'
          open ( newunit =fu , file =archivo )
          write (fu ,*) " #            t            y_h            y_ex(t)" ,&
          "    abs(y_h - y_ex(t)) "

         t = ti


             write (fu , '(4( E17.9))') t , y_0 , y_ex( ti ) , abs( y_0 - y_ex( ti ))

          do while ( t < (tf-h) )

             call rk2_o(t , y_0 ,h , y_h )
             t = t + h
             write (fu, '(4( E17.9))') t , y_h , y_ex( t ) , abs( y_h - y_ex( t ))
             y_0 = y_h

          end do

          close (fu)

!-------------------------------------------------------------------------------------

case (4) !4

     archivo = 'datossal10p/p2-rk4-'//trim(hache)//'.dat'
     open ( newunit =fu , file = archivo )
     write (fu ,*) " #             t                y_h                y_ex(t)" ,&
     "      abs(y_h - y_ex(t)) "

     t = ti

              write (fu , '(4( E17.9))') ti , y_0 , y_ex(ti) , abs(y_0 - y_ex(ti))

          do while ( t < (tf-h) )


              call rk4(t , y_0, h  , y_h )
              t = t + h
              write (fu , '(4( E17.9))') t , y_h , y_ex(t) , abs(y_h - y_ex(t))
              y_0 = y_h

           end do

           close (fu)

!---------------------------------------------------------------------------------

    case default

        write (*,*) " Error : metodo solo puede valer 1 , 2 , 22 o 4 "



!-----------------------------------------------------------------------------------


  end select

  end program
