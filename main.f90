program test_net
use network
use sir
use mtmod
implicit none
real*8,allocatable :: histo(:),histo_acu(:),knn(:)
real*8             :: t
integer            :: i

delta = 1.d0
lambda = 0.5d0

call sgrnd(879465132)

open(1,file="out.moreno_zebra_zebra")

call init_net(1,2)
close(1)

open(1,file="moreno_zebra.info")
call print_info(1)
close(1)
call print_info(6)

call init_sir()
call init_states(5)

t = 0.d0
call print_sir_info(6)
! call recover_node(17)
! call infect_node(12)
! call infect_node(15)
do i = 1, 2
    call sir_step(t, i)
end do
call print_sir_info(6)
print*,"Time",t

end program test_net