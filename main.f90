program test_net
use network
use sir
use mtmod
implicit none
real*8,allocatable :: histo(:),histo_acu(:),knn(:)
integer            :: i

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

call print_sir_info(6)

end program test_net