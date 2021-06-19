program test_net
use network
use sir
use mtmod
implicit none
real*8,allocatable :: histo(:),histo_acu(:),knn(:)
real*8             :: t
integer            :: i,j

delta = 1.d0
lambda = 0.5d0

call sgrnd(879465132)

open(1,file="out.moreno_zebra_zebra")

call init_net(1,2)
close(1)

open(1,file="moreno_zebra.info")
call print_info(1)
close(1)

call init_sir()
call init_states(5)

open(1,file="results/evolution.dat")
write(1,*)"Time  Inactive  Infected  Recovered  Total(ct.)"

open(2,file="results/log.txt")
call print_info(2)

t = 0.d0
i = 0
call print_sir_info(2)
do while(.true.)
      call sir_step(t)
      i = i + 1

      write(1,*)t,N_ina,N_inf,N_rec,N_ina+N_inf+N_rec

      if(N_inf==0) exit
end do

write(2,*)""
write(2,*)"---End of the infection"
write(2,*)"Iterations",i
write(2,*)"Time",t
call print_sir_info(2)

close(1)
close(2)

! open(1,file="results/test.dat")
! do i=1,N_net
!       do j=1,N_net
!             if(j==N_net) then
!                   if(any(V_net(P_ini(i):P_fin(i))==j)) then
!                         write(1,"(I1,X)")1
!                   else
!                         write(1,"(I1,X)")0
!                   end if
!             else
!                   if(any(V_net(P_ini(i):P_fin(i))==j)) then
!                         write(1,"(I1,X)",advance="no")1
!                   else
!                         write(1,"(I1,X)",advance="no")0
!                   end if
!             end if
!       end do
! end do

end program test_net