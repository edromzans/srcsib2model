subroutine load_aeropars(nlinha_zlt, digsig, zlt_sib2, &
     ha, z0d, dd, g2, g3, cc1, cc2, corb1, corb2)

  ! nlinha_zlt = numero de linhas de zlt no arquivo
  ! de parametros calibrados
  implicit none
  real (kind=8) :: zlt_sib2
  real (kind=8) :: zlt_in
  !
  real (kind=8) :: ha_par
  real (kind=8) :: z0d_par
  real (kind=8) :: dd_par
  real (kind=8) :: g2_par
  real (kind=8) :: g3_par
  real (kind=8) :: cc1_par
  real (kind=8) :: cc2_par
  real (kind=8) :: corb1_par
  real (kind=8) :: corb2_par
  !
  real (kind=8) :: ha
  real (kind=8) :: z0d
  real (kind=8) :: dd
  real (kind=8) :: g2
  real (kind=8) :: g3
  real (kind=8) :: cc1
  real (kind=8) :: cc2
  real (kind=8) :: corb1
  real (kind=8) :: corb2
  !
  integer :: ios=0
  integer :: nzlt
  integer :: nlinha_zlt
  integer :: digsig
  
  open(82,file='params_calibrado.dat', status='old')
  read(82, *, iostat=ios)

  do nzlt = 1, nlinha_zlt !while (ios == 0)
     
     read(82, *, iostat=ios) zlt_in, ha_par, z0d_par, &
          dd_par, g2_par, g3_par, cc1_par, cc2_par, corb1_par, corb2_par
     
     if ((int(zlt_sib2*digsig)).eq.(int(zlt_in*digsig))) then
        print *, int(zlt_sib2*digsig), int(zlt_in*digsig)

        ha = ha_par
        z0d = z0d_par
        dd = dd_par
        g2 = g2_par
        g3 = g3_par
        cc1 = cc1_par
        cc2 = cc2_par
        corb1 = corb1_par
        corb2 = corb2_par
        
        exit
     end if
     
     ! print *,                zlt_in(nzlt), z0d_par(nzlt), &
     !      dd_par(nzlt), cc1_par(nzlt), cc2_par(nzlt)

     !nzlt = nzlt + 1
     
  end do

  close(82)

end subroutine load_aeropars

! program main
!   implicit none
!   integer :: nlinha_zlt
!   real (kind=8) :: zlt_in(21)

!   nlinha_zlt = 21
  
!   call load_aeropars(nlinha_zlt, &
!        zlt_in, z0d_par, dd_par, cc1_par, cc2_par)
!   print *, (int( zlt_in * 10)) 

! end program main
