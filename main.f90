program main
use network
use sir
use mtmod
implicit none
real*8,parameter   :: dt = 0.1d0
integer,parameter  :: Nt = 1000
integer            :: N_sample, offset
integer,allocatable:: N_sus_histo(:,:),N_inf_histo(:,:),N_rec_histo(:,:)
real*8             :: N_sus_histo_acu(Nt),N_inf_histo_acu(Nt),N_rec_histo_acu(Nt)

real*8             :: t
integer            :: i,j,sample,k_t,lambda_i

real*8,parameter   :: dlambda = 0.01d0
integer,parameter  :: N_lambda = int((0.25d0-0.001d0)/dlambda) + 1
real*8             :: lambda_array(N_lambda)
real*8             :: rec_all(N_lambda)

character :: filename*90,fmtlabel*90, dir_net*90, N_char*10, offset_char*10

call get_command_argument(number=1, value=dir_net)
call get_command_argument(number=2, value=N_char)
read(N_char,*) N_sample
call get_command_argument(number=3, value=offset_char)
read(offset_char,*) offset

allocate(N_sus_histo(Nt,N_sample),N_inf_histo(Nt,N_sample),N_rec_histo(Nt,N_sample))

delta = 1.d0

do i = 1, N_lambda
      lambda_array(i) = 0.001 + (i - 1) * dlambda
end do

call sgrnd(879465132)

open(1,file="networks/"//trim(adjustl(dir_net)))

call init_net(1,offset)
close(1)

open(1,file="networks/"//trim(adjustl(dir_net))//".info")
call print_info(1)
close(1)

call init_sir()

do lambda_i=1,N_lambda
      lambda = lambda_array(lambda_i)

      N_sus_histo = 0.d0
      N_inf_histo = 0.d0
      N_rec_histo = 0.d0

      ! open(1,file="results/evolution.dat")
      ! write(1,*)"Time  Inactive  Infected  Recovered  Total(ct.)"

      ! open(2,file="results/log.txt")

      do sample=1,N_sample
            write(*,"(I2,A,I2, A ,I5,A,I5,A)",advance="no")lambda_i,"of",N_lambda,"|",sample,"of",N_sample,"samples"
            call execute_command_line('echo "\033[A"')
            call init_states(20)

            ! N_sus_histo(1) = N_sus_histo(1) + N_sus
            ! N_inf_histo(1) = N_inf_histo(1) + N_inf
            ! N_rec_histo(1) = N_rec_histo(1) + N_rec

            N_sus_histo(1,sample) = N_sus
            N_inf_histo(1,sample) = N_inf
            N_rec_histo(1,sample) = N_rec
            
            ! call print_info(2)

            t = 0.d0
            i = 0
            ! call print_sir_info(2)
            do while(.true.)
                  call sir_step(t)
                  i = i + 1

                  k_t = int(t/dt) + 2

                  ! N_sus_histo(k_t) = N_sus_histo(k_t) + N_sus
                  ! N_inf_histo(k_t) = N_inf_histo(k_t) + N_inf
                  ! N_rec_histo(k_t) = N_rec_histo(k_t) + N_rec

                  N_sus_histo(k_t,sample) = N_sus
                  N_inf_histo(k_t,sample) = N_inf
                  N_rec_histo(k_t,sample) = N_rec

                  ! write(1,*)t,N_sus,N_inf,N_rec,N_sus+N_inf+N_rec

                  if(N_inf==0) then
                        ! N_sus_histo(k_t+1:Nt) = N_sus_histo(k_t+1:Nt) + N_sus
                        ! N_inf_histo(k_t+1:Nt) = N_inf_histo(k_t+1:Nt) + N_inf
                        ! N_rec_histo(k_t+1:Nt) = N_rec_histo(k_t+1:Nt) + N_rec
                        N_sus_histo(k_t+1:Nt,sample) = N_sus
                        N_inf_histo(k_t+1:Nt,sample) = N_inf
                        N_rec_histo(k_t+1:Nt,sample) = N_rec
                        
                        do j = 2, k_t
                              if (N_sus_histo(j,sample) == 0) N_sus_histo(j,sample) = N_sus_histo(j-1,sample)
                              if (N_inf_histo(j,sample) == 0) N_inf_histo(j,sample) = N_inf_histo(j-1,sample)
                              if (N_rec_histo(j,sample) == 0) N_rec_histo(j,sample) = N_rec_histo(j-1,sample)
                        end do
                        exit
                  end if
            end do

            ! write(2,*)""
            ! write(2,*)"---End of the infection"
            ! write(2,*)"Iterations",i
            ! write(2,*)"Time",t
            ! call print_sir_info(2)
      end do

      N_sus_histo_acu = dble(sum(N_sus_histo,2))/N_sample
      N_inf_histo_acu = dble(sum(N_inf_histo,2))/N_sample
      N_rec_histo_acu = dble(sum(N_rec_histo,2))/N_sample

      rec_all(lambda_i) = N_rec_histo_acu(Nt)/N_net

      ! open(3,file="results/evolution_histo.dat")
      write(filename,"(A,F5.3,A)") trim(adjustl(dir_net))//"/lambda_",lambda,"_evolution_histo.dat"
      open(3,file=filename)
      write(3,*)"#Time  Inactive  Infected  Recovered  Total(ct.)"
      write(3,*)0.d0,N_sus_histo_acu(1),N_inf_histo_acu(1),N_rec_histo_acu(1),&
      N_sus_histo_acu(1) + N_inf_histo_acu(1) + N_rec_histo_acu(1)

      do i=1,Nt-1
            write(3,*)dt*i,N_sus_histo_acu(i+1),N_inf_histo_acu(i+1),N_rec_histo_acu(i+1),&
            N_sus_histo_acu(i+1) + N_inf_histo_acu(i+1) + N_rec_histo_acu(i+1)
      end do
      close(3)
end do

open(4,file=trim(adjustl(dir_net))//"/rec_lambda.dat")
write(4,*) "# lambda  recovered"
do i = 1, N_lambda
      write(4,*) lambda_array(i), rec_all(i)
end do
close(4)

! close(1)
! close(2)

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


open(1,file=trim(adjustl(dir_net))//"/adj_list.dat")
do i=1,N_net
      write(1,*)i,V_net(P_ini(i):P_fin(i))
end do
end program main