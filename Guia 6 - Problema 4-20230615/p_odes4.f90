program odes4
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Solución del péndulo simple por Runge - Kutta de 4 to order .
! d2 tita / dt2 = - ( g / l ) sin ( tita )
! definimos
! y1 = tita
! y2 = dtita / dt
! luego
! dy1 / dt = y2
! dy2 / dt = - ( g / l ) sin ( y1 )
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use ISOprec
use constantes
use funciones

use rk4mod


implicit none

integer      :: nsteps,fu, fu1, fu2, fu3, fu4
real ( kind = wp ) :: t , ti , tf , h, N0, pi 
real  (kind = wp ) ::  En, En0, tita0, Ec, Ep, Etot
real ( kind = wp ) ::  y_0(1:2), y_h(1:2)
character ( len =50) :: hache , hache0
character ( len =90) :: archivo

! lectura de parametros -----------------------------------
namelist /parametrosinic/   &
              N0, ti , tf , h

open ( newunit = fu , file = "datosent/paramsinic-p4.in" , status = "old" )
read ( unit = fu , nml = parametrosinic )
close ( unit = fu)

! ---------------------------------------------------
open ( newunit = fu1 , file = "datossal/salidas-p4.f" , status = "unknown" )

! constantes ---------------------------------------------
pi = acos(-1._wp ) ! definido globalmente para que lo puedan usar
! todas las funciones y subrutinas.
! ---------------------------------------------------
! cuando se estiman los errores
! allocate ( err_rk45_sys ( size ( y_0 ) ) , err_rk45_sys_local ( size ( y_0 ) ) )


y_0 = (/0.1_wp , 0.0_wp /)  !    PO tita=0.01 normal tita = 0.5 , d(tita)/dt = 0.0 conciciones inicales

g = 10._wp
l = 1._wp


! h --------------------------------------------------------------

write ( hache0 , '( F10.3) ') h

hache = 'h'//trim(adjustl(hache0))

! chequeos básicos -----------------------------------------------

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

write (* ,*)" derive (0_wp , y_0 ) = " , derive (0._wp , y_0 )
write (fu1 ,*) " derive (0._wp , y_0 ) = " , derive (0._wp , y_0 )

En0 = 0.5_wp * l**2 * y_0(2)**2 - g * l * (cos(y_0(1)) - 1.0_wp )

write (* ,*) " En0 = " , En0
write (fu1 ,*) " En0 = " , En0

close (fu1)

!-----------------------------------------------------------------------




        archivo = 'datossal/p4-'//trim(hache)//'.dat'
        open (newunit=fu1 , file =archivo )
        write (fu1, *) " #         t                  tita                   dtita/dt= u"


        archivo = 'datossal/p4-ener-'//trim(hache)//'.dat'
        open (newunit=fu2 , file =archivo )
        write (fu2, *) " #          t                energía                       En - En0"

!-------------------------------------------------------------------------------------------



     t = ti

        write (30 , '( E16.8, 3( E29.20) )') t , y_0, tita_ex(t,tita0)

          do while ( t <= tf )

              call rk4_vec(t , y_0 ,h , y_h)

              En = 0.5 _wp * l**2 * y_h(2)**2 - g * l * ( cos(y_h(1)) - 1.0_wp )


              write (fu1 , '( E16.8, 2( E29.20) )') t , y_h !
              write (fu2 , '( E16.8 ,2( E29.20) ) ') t , En , ( En-En0 )

              t = t + h

              y_0 = y_h

           end do

           close (fu1)
           close (fu2)


!  deallocate ( err_rk45_sys , err_rk45_sys_local )
!---------------------------------------------------------------------------------------------
! caso aprox sen(tita)=tita

      tita0 = y_0(1)     !      tita = 0.01 , d(tita)/dt = 0.0 conciciones inicales

      print *, "tita0 = ", tita0

        archivo = 'datossal/p4-PO'//trim(hache)//'.dat'
        open (newunit=fu3 , file =archivo )
        write (fu3,*) " #         t                  tita                   dtita/dt= u               tita_ex"


        archivo = 'datossal/p4-POener-'//trim(hache)//'.dat'
        open (newunit=fu4 , file =archivo )
        write (fu4,*) " #          t       Ec     Ep       DeltaE                       En - En0"

!-------------------------------------------------------------------------------------------



     t = ti

     write (fu3 , '( E16.8, 3( E29.20) )') t , y_0, tita_ex(t,tita0)  !


          do while ( t <= tf )

              call rk4_vec_e(t , y_0 ,h , y_h)

              Ec = 0.5_wp * l**2 * y_h(2)**2 
              Ep = - g * l * (cos(y_h(1)) - 1.0_wp )
              Etot= Ec + Ep


              write (fu3 , '( E16.8, 3( E29.20) )') t , y_h, tita_ex(t,tita0)  !
              write (fu4 , '( E16.8 ,3( E29.20) ) ') t , Ec, Ep , ( En-En0 )


              t = t + h
              y_0 = y_h

           end do

           close (fu3)
           close (fu4)



  end program
