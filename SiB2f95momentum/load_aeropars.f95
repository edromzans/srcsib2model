subroutine load_aeropars(nlinha_zlt, digsig, zlt_sib2, &
  z0d, dd, cc1, cc2)
  ! nlinha_zlt = numero de linhas de zlt no arquivo
  ! de parametros calibrados
  implicit none
  real (kind=8) :: zlt_sib2
  real (kind=8) :: zlt_in
  real (kind=8) :: z0d_par
  real (kind=8) :: dd_par
  real (kind=8) :: cc1_par
  real (kind=8) :: cc2_par
  !
  real (kind=8) :: z0d
  real (kind=8) :: dd
  real (kind=8) :: cc1
  real (kind=8) :: cc2
  
  integer :: ios=0
  integer :: nzlt
  integer :: nlinha_zlt
  integer :: digsig
  
  open(82,file='params_calibrado.dat', status='old')
  read(82, *, iostat=ios)

  do nzlt = 1, nlinha_zlt !while (ios == 0)
     
     read(82, *, iostat=ios) zlt_in, z0d_par, &
          dd_par, cc1_par, cc2_par
     
     if ((int(zlt_sib2*digsig)).eq.(int(zlt_in*digsig))) then
        print *, int(zlt_sib2*digsig), int(zlt_in*digsig)
        z0d = z0d_par
        dd = dd_par
        cc1 = cc1_par
        cc2 = cc2_par 
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
