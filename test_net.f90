program test_net
use network
implicit none
real*8,allocatable :: histo(:),histo_acu(:),knn(:)
integer            :: i

! open(1,file="out.moreno_zebra_zebra")

! call ini_net(1,2)
! close(1)

! open(1,file="moreno_zebra.info")
! call print_info(1)
! close(1)
! call print_info(6)

open(1,file="out.twin")

call ini_net(1,1)
close(1)

open(1,file="results/twin.info")
call print_info(1)
close(1)

allocate(histo(D_max),histo_acu(D_max))

call get_degree_histogram(histo,histo_acu)

open(1,file="results/histo.dat")
open(2,file="results/acuhisto.dat")
do i=1,D_max
      write(1,*)i,histo(i)
      write(2,*)i,histo_acu(i)
end do
close(1)
close(2)

allocate(knn(D_max))

call get_knn(knn)

open(1,file="results/knn.dat")
do i=1,D_max
      write(1,*)i,knn(i)
end do
close(1)


end program test_net